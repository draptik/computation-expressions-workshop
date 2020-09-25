module Options

open Expecto

let opt1 = Some 1
let opt2 = Some 2
let opt3 = Some 3
let opt4 = Some 4

let sum4 w x y z = w + x + y + z

let nested =
    match opt1 with
    | Some w ->
        match opt2 with
        | Some x ->
            match opt3 with
            | Some y ->
                match opt4 with
                | Some z ->
                    let result = sum4 w x y z
                    printfn "Nested: %d" result
                    Some result
                | None -> None
            | None -> None
        | None -> None
    | None -> None
    
let composed =
    opt1
    |> Option.bind (fun w ->
        opt2
        |> Option.bind (fun x ->
            opt3
            |> Option.bind (fun y ->
                opt4
                |> Option.map (fun z ->
                    let result = sum4 w x y z
                    printfn "Composed: %d" result
                    result
                )
            )
        )
    )

type OptionBuilder() =
    member __.Return(value) =
        printfn "maybe.Return(%A)" value
        Some value
    member __.Bind(m, f) =
        printfn "maybe.Bind(%A, %A)" m f
        Option.bind f m
    
let maybe = OptionBuilder()
    
[<Tests>]
let tests =
    testList "OptionBuilder" [
        test "nested = composed" {
            Expect.equal nested composed "Expected nested to equal composed"
        }
        
        test "OptionBuilder returns value" {
            let expected = 1
            let actual = maybe { return expected }
            Expect.equal actual (Some expected) "Expected Some 1"
        }
        
        test "OptionBuilder can bind option values" {
            let actual = maybe {
                let! w = opt1
                let! x = opt2
                let! y = opt3
                let! z = opt4
                let result = sum4 w x y z
                printfn "Result: %d" result
                return result
            }
            Expect.equal actual nested "Actual should sum to the same value as nested"
        }
        
        test "OptionBuilder instance can be used directly" {
            let actual =
                maybe.Bind(opt1, fun w ->
                    maybe.Bind(opt2, fun x ->
                        maybe.Bind(opt3, fun y ->
                            maybe.Bind(opt4, fun z ->
                                let result = sum4 w x y z
                                printfn "Result: %d" result
                                maybe.Return(result)
                            )
                        )
                    )
                )
            Expect.equal actual composed "Actual should sum to the same value as nested"
        }
    ]
    