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
        
    // doesn't seem to be needed
    member __.Using(disposable: #System.IDisposable, f) =
        try
            f disposable
        finally
            match disposable with
            | null -> ()
            | disp -> disp.Dispose()
            
    member __.For(items: seq<_>, f) =
        __.Using(items.GetEnumerator(), fun enum ->
            __.While((fun () -> enum.MoveNext()),
                     __.Delay(fun () -> f enum.Current)))

    member __.Bind(m: unit -> unit, f) =
        m()
        f()
        
    member __.While(pred, body) =
        if pred() then
            __.Bind(body, (fun () -> __.While(pred, body)))
        else __.Zero()
            
let myTest name = MyTestBuilder(name)

[<AllowNullLiteral>]
type ObservableDisposable() =
    member val IsDisposed = false with get, set
    interface System.IDisposable with
        member this.Dispose() =
            this.IsDisposed <- true

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
        
        myTest "myTest support use" {
            let disp = new ObservableDisposable()
            do use d = disp
               ()
            Expect.isTrue disp.IsDisposed "Expected the instance to be disposed"
        }
        
        myTest "myTest supports for" {
            for i in [1..10] do
                Expect.equal i i "i should equal itself within a for loop"
        }
        
        myTest "myTest supports while" {
            let mutable i = 1
            while i <= 10 do
                Expect.equal i i "i should equal itself within a for loop"
                i <- i + 1
        }
    ]
    
    