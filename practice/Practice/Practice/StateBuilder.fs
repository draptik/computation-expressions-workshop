module States

open Expecto
open System.Text

type StateBuilder () = class end

let state = StateBuilder ()

[<Tests>]
let tests =
    testList "states" [
        test "StringBuilder as state" {
            let printA (sb: StringBuilder) = sb.Append("A")
            let printB (sb: StringBuilder) = sb.Append("B")
            let printC (sb: StringBuilder) = sb.Append("C")
            let run (sb: StringBuilder) = sb.ToString()
            let sb = StringBuilder()
            let actual = sb |> printA |> printB |> printC |> run
            Expect.equal actual "ABC" "Expected ABC"
        }
    ]

// State is a function type that takes a state and returns a value and the new state    
type State<'a, 's> = 's -> 'a * 's

// Alternative: Single case union of the same type
//type State<'a, 's> = State of ('s -> 'a * 's)

    