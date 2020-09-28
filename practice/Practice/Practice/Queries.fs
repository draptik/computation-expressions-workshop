module Queries

//open System.Linq
//open System.Reactive.Linq
//open System.Reactive.Concurrency
open Microsoft.FSharp.Linq
open Expecto

type Microsoft.FSharp.Linq.QueryBuilder with

    [<CustomOperation("headOrNone")>]
    member __.HeadOrNone(source: QuerySource<'T, 'Q>) =
        Seq.tryHead source.Source

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
    ]