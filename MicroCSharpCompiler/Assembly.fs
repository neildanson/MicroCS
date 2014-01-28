module Assembly

open Ast
open Definitions
open Class
open Interface
open Struct
open Enum
open Reflection
open TypeResolver

open System
open System.Reflection
open System.Reflection.Emit


let CompileFile name = 
    let filename = name + ".dll"
    IO.File.Delete (filename)
    let aName = AssemblyName(name)

    let ab = AppDomain.CurrentDomain.DefineDynamicAssembly(aName, AssemblyBuilderAccess.RunAndSave)
    let mb = ab.DefineDynamicModule(aName.Name, filename, true) //true means debug it seems
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
            | BuildStruct(structDefinition) -> classes, interfaces, [structDefinition]@structs, enums
            | _ -> classes, interfaces, structs, enums
        | [] -> classes, interfaces, structs, enums
    buildType types [] [] [] []

let CompileTypeStubs (File(body)) (ab, mb:ModuleBuilder, filename) =
    let usings = body|>List.map(function Using(name) -> Some(name) |_-> None) |> List.choose id
    let definitions = body |> List.map(function Namespace(name, body) -> Some(name, body) | _ -> None) |> List.choose id
    let body = definitions |> List.map(fun (name, bodies) -> CompileNamespaceBody name bodies mb) 
    let classes = body |> List.map (fun (t,_,_,_) -> t) |> List.collect id
    let interfaces = body |> List.map (fun (_,i,_,_) -> i) |> List.collect id
    let enums = body |> List.map (fun (_,_,_,e) -> e) |> List.collect id
    let structs = body |> List.map (fun (_,_,s,_) -> s) |> List.collect id

    let (userdefinitions:UserDefinitions) = classes, interfaces, structs, enums

    let ResolveType name = ResolveType usings userdefinitions name
    
    //Methods - TODO - this code is fugly
    (*let buildClassContent classDef  = function
        | ClassBody.Method(access, returntype, name, parameters, body) ->
            Class.WithMethod classDef access returntype name parameters ResolveType body
        | ClassBody.Field(access, fieldtype, name) -> 
            WithField classDef access fieldtype name ResolveType
        | _ -> failwith "Unsupported"    
    
    let buildInterfaceContent interfaceDef  = function
        | InterfaceBody.Method(returntype, name, parameters) ->
            Interface.WithMethod interfaceDef returntype name parameters ResolveType
        | _ -> failwith "Unsupported"                                                       

    let classes = classes 
                  |> List.map(fun classDef -> classDef.Ast 
                                              |> List.fold(fun classDef body -> buildClassContent classDef body) classDef)
    let interfaces = interfaces|> List.map(fun classDef -> classDef.Ast 
                                                           |> List.fold(fun classDef body -> buildInterfaceContent classDef body) classDef)
                                                           *)
    //let userdefinitions = classes, interfaces, structs, enums

    //classes|>List.iter(fun c -> c.Methods |> List.iter(fun (_,_,b) -> b userdefinitions))
    //Compile bodies
    userdefinitions, ab, mb, filename, ResolveType

let CompileMethodStubs (userdefinitions:UserDefinitions, ab, mb, filename, resolveType)= 
    let classes, interfaces, structs, enums = userdefinitions    
    let buildClassContent classDef  = function
        | ClassBody.Method(access, returntype, name, parameters, body) ->
            Class.WithMethod classDef access returntype name parameters resolveType body
        | ClassBody.Field(access, fieldtype, name) -> 
            WithField classDef access fieldtype name resolveType
        | _ -> failwith "Unsupported"    
    
    let buildInterfaceContent interfaceDef  = function
        | InterfaceBody.Method(returntype, name, parameters) ->
            Interface.WithMethod interfaceDef returntype name parameters resolveType
        | _ -> failwith "Unsupported"                                                       

    let classes = classes 
                  |> List.map(fun classDef -> classDef.Ast 
                                              |> List.fold(fun classDef body -> buildClassContent classDef body) classDef)
    let interfaces = interfaces|> List.map(fun classDef -> classDef.Ast 
                                                           |> List.fold(fun classDef body -> buildInterfaceContent classDef body) classDef)

    let userdefinitions = classes, interfaces, structs, enums

    classes|>List.iter(fun c -> c.Methods |> List.iter(fun (_,_,b) -> b userdefinitions))
    //Compile bodies
    userdefinitions, ab, mb, filename


let Finish (userdefinitions:UserDefinitions, ab, mb, filename) =
    let classes, interfaces, structs, enums = userdefinitions
    //Compile bodies
    classes|> List.map(fun t -> t.Type.CreateType()) |> ignore
    enums|> List.map(fun e -> e.CreateType()) |> ignore
    interfaces|> List.map(fun i -> i.Type.CreateType()) |> ignore
    structs |> List.map(fun s -> s.Type.CreateType()) |> ignore
    ab, mb, filename


let Save (ab:AssemblyBuilder, _, filename) = 
    ab.Save(filename)