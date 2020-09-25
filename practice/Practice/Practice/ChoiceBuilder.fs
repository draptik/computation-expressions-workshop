module Choose

open Expecto
open Options

type ChoiceBuilder() =
    inherit OptionBuilder()
    
    member __.Combine(m1: 'a option, m2: 'a option) =
        printfn "choose.Combine(%A, %A)" m1 m2
        match m1 with
        | Some _ -> m1
        | None -> m2
        
    member __.Delay(f: unit -> 'a option) =
        printfn "choose.Delay(%A)" f
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
    ]
    