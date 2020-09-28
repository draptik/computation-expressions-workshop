module Extensions

open Expecto
open Options

type Microsoft.FSharp.Control.AsyncBuilder with
    member this.Bind(task, f) =
        this.Bind(Async.AwaitTask task, f)
        
    member this.Bind(task: System.Threading.Tasks.Task, f) =
        this.Bind(Async.AwaitTask task, f)
        
[<Tests>]
let tests =
    testList "extensions" [
        test "async can bind Task<'T>" {
            let expected = 0
            let task = System.Threading.Tasks.Task.FromResult expected
            let actual =
                async {
                    let! res = task
                    return res
                }
                |> Async.RunSynchronously
            Expect.equal actual expected "Expected async to bind Task<'T>"
        }
        
        test "async can bind Task" {
            let task =
                    System.Threading.Tasks.Task.FromResult ()
                    :> System.Threading.Tasks.Task
            let actual =
                async {
                    do! task
                }
                |> Async.RunSynchronously
            Expect.equal actual () "Expected async to bind Task"
        }
        
    ]

