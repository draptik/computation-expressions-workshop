module Queries

open System
open System.Linq
open System.Reactive.Linq
open System.Reactive.Concurrency
open Microsoft.FSharp.Linq
open Expecto

type Microsoft.FSharp.Linq.QueryBuilder with

    [<CustomOperation("headOrNone")>]
    member __.HeadOrNone(source: QuerySource<'T, 'Q>) =
        Seq.tryHead source.Source
        
    [<CustomOperation("exactlyOneOrNone")>]
    member __.ExactlyOneOrNone(source: QuerySource<'T, 'Q>) =
        if Seq.length source.Source = 1 then
            Enumerable.Single(source.Source) |> Some
        else
            None

type RxQueryBuilder() =
    member __.For (s: IObservable<_>, body: _ -> IObservable<_>) = s.SelectMany(body)
    member __.Yield (value) = Observable.Return(value)
    member __.Zero () = Observable.Empty(Scheduler.CurrentThread :> IScheduler)
    
    [<CustomOperation("select")>]
    member __.Select(s: IObservable<_>, [<ProjectionParameter>] selector: _ -> _) =
        s.Select(selector)
        
    [<CustomOperation("head")>]
    member __.Head (s: IObservable<_>) = s.FirstAsync()
    
    [<CustomOperation("exactlyOne")>]
    member __.ExactlyOne (s: IObservable<_>) = s.SingleAsync()
    
    [<CustomOperation("where", MaintainsVariableSpace=true)>]
    member __.Where (s: IObservable<_>, [<ProjectionParameter>] predicate: _ -> bool) =
        s.Where(predicate)
    
    [<CustomOperation("groupBy", AllowIntoPattern=true)>]
    member __.GroupBy (s: IObservable<_>, [<ProjectionParameter>] keySelector: _ -> _) =
        s.GroupBy(Func<_,_>(keySelector))

let rxquery = RxQueryBuilder()


type TestRec = { Value: int }


[<Tests>]
let tests =
    testList "queries" [
        test "query supports F# types with headOrDefault" {
            let actual : TestRec =
                query {
                    for x in Seq.empty<TestRec> do
                    headOrDefault
                }
            Expect.equal actual (Unchecked.defaultof<TestRec>) "Expected default value of TestRec"
        }
        
        test "query supports F# types with headOrNone" {
            let actual =
                query {
                    for x in Seq.empty<TestRec> do
                    headOrNone
                }
            Expect.equal actual None "Expected default value of TestRec"
        }
                 
        test "query exactlyOneOrNone returns the single value for a seq with one element" {
            let source = seq { yield { Value = 1 } }
            let actual =
                query {
                    for x in source do
                    exactlyOneOrNone
                }
            Expect.equal actual (Seq.tryHead source) "Expected { Value = 1 }"
        }

        test "query exactlyOneOrNone returns None for an empty seq" {
            let source = Seq.empty<TestRec>
            let actual =
                query {
                    for x in source do
                    exactlyOneOrNone
                }
            Expect.equal actual None "Expected None"
        }

        test "query exactlyOneOrNone returns None for a seq with more than one element" {
            let source = seq { yield { Value = 1 }; yield { Value = 2 } }
            let actual =
                query {
                    for x in source do
                    exactlyOneOrNone
                }
            Expect.equal actual None "Expected None"
        }
        
        test "rxquery can select values from a source" {
            let expected = [|1..10|]
            let actual = Array.zeroCreate<int> 10
            let source = Observable.Range(1, 10)
            use disp =
                rxquery {
                    for x in source do
                    select x
                }
                |> Observable.subscribe (fun i -> actual.[i - 1] <- i)
            Expect.equal actual expected "Expected observable to populate empty array with selected values"
        }
        
        test "rxquery can return the first value from a source" {
            let mutable actual : int = -1
            let source = Observable.Range(1, 10)
            use disp =
                rxquery {
                    for x in source do
                    head
                }
                |> Observable.subscribe (fun i -> actual <- i)
            Expect.equal actual 1 "Expected head to return 1"
        }

        test "rxquery can return the single value from a source of one element" {
            let mutable actual : int = -1
            let source = Observable.Return(1)
            use disp =
                rxquery {
                    for x in source do
                    exactlyOne
                }
                |> Observable.subscribe (fun i -> actual <- i)
            Expect.equal actual 1 "Expected exactlyOne to return 1"
        }
        
        test "rxquery can filter an observable with where" {
            let expected = [|1..5|]
            let actual = Array.zeroCreate<int> 5
            let source = Observable.Range(1, 10)
            use disp =
                rxquery {
                    for x in source do
                    where (x <= 5)
                    select x
                }
                |> Observable.subscribe (fun i -> actual.[i - 1] <- i)
            Expect.equal actual expected "Expected where to filter input"
        }
        
        test "rxquery can group an observable" {
            let expected = [|"a";"b"|]
            let actual = ResizeArray<string>()
            let source =
                Observable.Generate(1,
                    (fun x -> x < 10),
                    (fun x -> x + 1),
                    (fun x ->
                        if x < 2 then "a", x
                        else "b", x))
            use disp =
                rxquery {
                    for (k, _) in source do
                    groupBy k into g
                    select g.Key
                }
                |> Observable.subscribe actual.Add
            Expect.equal (actual.ToArray()) expected "Expected where to filter input"
        }
    ]