module Sequences

open Expecto

type Stack<'a> =
    | Empty
    | Cons of top: 'a * rest: Stack<'a>
    
module Stack =
    /// Pushed a new value on top of the stack
    let push v s = Cons(v, s)
    
    /// Pops the top off the stack,
    /// returning both the value and remaining stack.
    /// Throws an error if there are no remaining values.
    let pop s =
        match s with
        | Cons(v, c) -> v, c
        | _ -> failwith "Nothing to pop!"
        
    /// Converts the Stack<'a> to an 'a list.
    let toList s =
        let rec loop s cont =
            match s with
            | Cons(head, tail) ->
                loop tail (fun rest -> cont(head::rest))
            | Empty -> cont []
        loop s id
        
    /// Pushed a value onto a new stack.
    let lift v = push v Empty
    
type StackBuilder() =
    member __.Yield(value) = Stack.lift value
    member __.YieldFrom(m: Stack<'a>) = m

let stack = StackBuilder()

[<Tests>]
let tests =
    testList "sequences" [
        test "Stack.toList generates a matching list" {
            let actual = Cons(1, Cons(2, Cons(3, Empty))) |> Stack.toList
            Expect.equal actual [1;2;3] "Expected list containing [1;2;3]"
        }
        
        test "stack can return one item" {
            let actual = stack { yield 1 }
            Expect.equal (Stack.toList actual) [1] "Expected a stack containing 1"
        }
        
        test "stack can yield an empty stack" {
            let actual = stack { yield! Empty }
            Expect.equal actual Empty "Expected an empty stack"
        }
    ]
    