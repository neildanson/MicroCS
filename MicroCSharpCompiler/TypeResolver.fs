module TypeResolver

open Class
open Enum

open System

let ResolveTypeIntegral name =
    match name with
    | "void" -> typeof<System.Void> |> Some
    | "int" -> typeof<int> |> Some
    | "float" -> typeof<float32> |> Some
    | "double" -> typeof<float> |> Some
    | "string" -> typeof<string> |> Some
    | "bool" -> typeof<bool> |> Some
    | "decimal" -> typeof<decimal> |> Some
    | "object" -> typeof<obj> |> Some
    | _ -> None //TODO other integral types

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
    match ResolveTypeIntegral name with
    | Some(t) -> t
    | None ->
        match userdefinition, name with
        | GetClass(cd) -> cd.Type :> Type
        | GetEnum(eb) -> eb :> Type
        | _ -> let t = GetTypeFromLoadedAssemblies usings name
               if t <> null then t else failwith "Unknown Type"