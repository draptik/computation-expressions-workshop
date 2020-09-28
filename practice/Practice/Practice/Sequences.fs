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
    
    let append s1 s2 =
        let rec loop s1 cont =
            match s1 with
            | Cons(top, rem) ->
                loop rem (fun rest -> cont(Cons(top, rest)))
            | Empty -> cont s2
        loop s1 id
        
    let collect (f: 'a -> Stack<'b>) (m: Stack<'a>) =
        let rec loop s cont =
            match s with
            | Cons(top, rem) ->
                loop rem (fun rest -> cont(append (f top) rest))
            | Empty -> cont Empty
        loop m id
    
type StackBuilder() =
    member __.Yield(value) = Stack.lift value
    member __.YieldFrom(m: Stack<'a>) = m
    member __.Combine(s1: Stack<'a>, s2: Stack<'a>) = Stack.append s1 s2
    member __.Delay(f: unit -> Stack<'a>) = f()
    member __.For(m, f) = Stack.collect f m

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
        
        test "stack can return multiple items" {
            let actual = stack {
                yield 1
                yield 2
                yield 3
            }
            Expect.equal (Stack.toList actual) [1;2;3] "Actual should match expected"
        }
        
        test "stack can iterate and yield" {
            let expected = stack { yield 1; yield 2; yield 3 }
            let actual = stack {
                for x in expected do
                    yield x
            }
            Expect.equal actual expected "Expected iterating and yielding to return the same result"
        }
    ]
    