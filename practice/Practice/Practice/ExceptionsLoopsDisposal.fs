module ExceptionsLoopsDisposal

open Expecto

type MyTestBuilder(name) =
    member __.Delay(f) = f
    member __.Run(f) = testCase name f
    member __.Zero() = ()
    member __.TryWith(tryBlock, withBlock) =
        try tryBlock()
        with e -> withBlock e
    member __.TryFinally(tryBlock, finallyBlock) =
        try tryBlock()
        finally finallyBlock()
    member __.Combine(f1: 'a, f2: unit -> unit) =
        f2()
        f1

let myTest name = MyTestBuilder(name)

[<Tests>]
let tests =
    testList "my tests" [
        myTest "A simple test" {
            let expected = 4
            Expect.equal expected (2+2) "2+2 = 4"
        }
        
        myTest "myTest supports try...with" {
            try
                raise(System.Exception("Failed!"))
            with
            | e -> Expect.equal e.Message "Failed!" "Expected an exception with message \"Failed!\""
        }
        
        myTest "myTest supports try...finally" {
            let mutable calledFinally = false
            try
                try
                    raise(System.Exception("Failed!"))
                // without this, the test will fail anyway,
                // as the exception will trigger a test failure.
                with _ -> ()
            finally
                calledFinally <- true
            Expect.isTrue calledFinally "Expected test to call finally block"
        }
    ]
    
    