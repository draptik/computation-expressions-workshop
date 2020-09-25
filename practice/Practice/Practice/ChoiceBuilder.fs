module Choose

open Expecto
open Options

type ChoiceBuilderVerbose() =
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

type ChoiceBuilder() =
    inherit OptionBuilder()
    
    member __.Combine(m: 'a option, f: unit -> 'a option) =
        match m with
        | Some _ -> m
        | None -> f ()
        
    member __.Delay(f: unit -> 'a option) = f
        
    member __.Run(f: unit -> 'a option) = f ()

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
        
        test "choose returns second value if first is None" {
            let actual = choose {
                return! None
                printfn "returning second value?"
                return! Some 2
            }
            Expect.equal actual (Some 2) "Expected the second value to be returned."
        }
        
        test "choose returns the last value if all previous are None" {
            let actual = choose {
                return! None
                return! None
                return! None
                return! None
                return! None
                return! None
                return! Some 7
            }
            Expect.equal actual (Some 7) "Expected the seventh value to be returned."
        }
        
        test "ChoiceBuilder can chain a computation onto another returning None, where None indicates success" {
            let dirExists path =
                let fileInfo = System.IO.FileInfo(path)
                let fileName = fileInfo.Name
                let pathDir = fileInfo.Directory.FullName.TrimEnd('~')
                if System.IO.Directory.Exists(pathDir) then
                    Some (System.IO.Path.Combine(pathDir, fileName))
                else
                    None
                
            let choosePath = Some "~/test.txt"
            
            let writeFile =
                choose {
                    let! path = choosePath
                    let! fullPath = dirExists path
                    System.IO.File.WriteAllText(fullPath, "Test succeeded")
                }
                
            let actual =
                choose {
                    return! writeFile
                    return "Successfully wrote file"
                }
                
            Expect.equal actual (Some "Successfully wrote file") "Actual should indicate success"
        }
    ]
    