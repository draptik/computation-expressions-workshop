module Choose

open Expecto
open Options

type ChoiceBuilder() =
    inherit OptionBuilder()
    
    member __.Combine(m: 'a option, f: unit -> 'a option) =
        printfn "choose.Combine(%A, %A)" m f
        match m with
        | Some _ -> m
        | None -> f ()
        
    member __.Delay(f: unit -> 'a option) =
        printfn "choose.Delay(%A)" f
        f
        
    member __.Run(f: unit -> 'a option) =
        printfn "choose.Run(%A)" f
        f ()

let choose = ChoiceBuilder()

[<Tests>]
let tests =
    testList "choices" [
        test "choose returns first value if it is Some" {
            let actual = choose {
                return! Some 1
                printfn "returning first value?"
                return! Some 2
            }
            Expect.equal actual (Some 1) "Expected the first value to be returned."
        }
        
        test "expanding choose for the fourth attempt runs the same way" {
            let actual =
                choose.Run(
                    choose.Delay(fun () ->
                        choose.Combine(
                            choose.ReturnFrom(Some 1),
                            choose.Delay(fun () ->
                              printfn "returning first value?"
                              choose.ReturnFrom(Some 2)
                            )
                        )
                    )
                )
            Expect.equal actual (Some 1) "Expected the first value to be returned."
        }
    ]
    