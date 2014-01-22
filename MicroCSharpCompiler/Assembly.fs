module Assembly

open Ast
open Definitions
open Class
open Interface
open Enum

open Reflection

open System
open System.Reflection
open System.Reflection.Emit

let GetTypeFromLoadedAssemblies usings name =
    let t = Type.GetType name
    match t with
    | null ->
        let usings = usings |>List.rev //We reverse the order as the last defined takes greatest precedence
        let possibleTypeNames = usings |> List.map(fun using -> using + "." + name)
        let loadedAssemblies = AppDomain.CurrentDomain.GetAssemblies()

        let possibleFullyQualifiedTypes =
            possibleTypeNames
            |> List.collect (fun t -> loadedAssemblies |> Seq.map(fun a -> t + "," + a.FullName) |> Seq.toList)
            |> List.map(fun tn -> Type.GetType(tn))
            |> List.filter(fun t -> t <> null)
        match possibleFullyQualifiedTypes with
        | head::_ -> head
        | _ -> null

    | t -> t


let ResolveType usings userdefinition name= 
    match resolveTypeIntegral name with
    | Some(t) -> t
    | None ->
        match userdefinition, name with
        | GetClass(cd) -> cd.Type :> Type
        | GetEnum(eb) -> eb :> Type
        | _ -> let t = GetTypeFromLoadedAssemblies usings name
               if t <> null then t else failwith "Unknown Type"

let CompileFile filename = 
    IO.File.Delete (filename + ".dll")
    let aName = AssemblyName(filename)
    let ab = AppDomain.CurrentDomain.DefineDynamicAssembly(aName, AssemblyBuilderAccess.RunAndSave)
    let mb = ab.DefineDynamicModule(aName.Name, aName.Name, true) //true means debug it seems
    ab,  mb, filename

let CompileNamespaceBody name types mb = 
    let rec buildType types classes interfaces structs enums = 
        match types with
        | t::tail -> 
            let classes, interfaces, structs, enums  = buildType tail classes interfaces structs enums
            match name, t, mb with
            | BuildClass(classDefinition) -> 
                [classDefinition]@classes, interfaces, structs, enums
            | BuildEnum(enumDefinition) -> 
                classes, interfaces, structs, [enumDefinition]@enums
            | BuildInterface(interfaceDefinition) -> 
                classes, [interfaceDefinition]@interfaces, structs, enums
            | _ -> classes, interfaces, structs, enums
        | [] -> classes, interfaces, structs, enums
    buildType types [] [] [] []

let CompileBody (File(body)) (ab, mb:ModuleBuilder, filename) =
    let usings = body|>List.map(function Using(name) -> Some(name) |_-> None) |> List.choose id
    let definitions = body |> List.map(function Namespace(name, body) -> Some(name, body) | _ -> None) |> List.choose id
    let body = definitions |> List.map(fun (name, bodies) -> CompileNamespaceBody name bodies mb) 
    let classes = body |> List.map (fun (t,_,_,_) -> t) |> List.collect id
    let enums = body |> List.map (fun (_,_,_,e) -> e) |> List.collect id
    let interfaces = body |> List.map (fun (_,_,i,_) -> i) |> List.collect id
    
    let (userdefinitions:UserDefinitions) = classes, interfaces, [], enums

    let ResolveType name = ResolveType usings userdefinitions name
    
    //Methods - TODO - this code is fugly
    let classes = classes 
                  |> List.map(fun c -> c.Ast 
                                       |> List.fold(fun c body ->
                                                        match body with
                                                        | ClassBody.Method(access, returntype, name, parameters, body) ->
                                                             WithMethod c access returntype name parameters ResolveType body
                                                        | _ -> failwith "Unsupported") c
                                                             )
    let userdefinitions = classes, interfaces, [], enums

    classes|>List.iter(fun c -> c.Methods |> List.iter(fun (_,_,b) -> b userdefinitions))
    //Compile bodies
    userdefinitions, ab, mb, filename

let Finish (userdefinitions:UserDefinitions, ab, mb, filename) =
    let classes, interfaces, structs, enums = userdefinitions
    //Compile bodies
    classes|> List.map(fun t -> t.Type.CreateType()) |> ignore
    enums|> List.map(fun e -> e.CreateType()) |> ignore
    interfaces|> List.map(fun i -> i.Type.CreateType()) |> ignore
    ab, mb, filename


let Save (ab:AssemblyBuilder, _, filename) = 
    ab.Save(filename)