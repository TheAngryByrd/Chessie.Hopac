module Tests


open Expecto
open Hopac
open Chessie.ErrorHandling
open Chessie.Hopac
open System.Threading.Tasks

let getResult (jr : JobResult<'a,'b>) =
  jr
  |> JobTrial.toJobOfResult
  |> Hopac.run
let getOkValue (jr : JobResult<'a,'b>) =
  jr
  |> getResult
  |> Trial.returnOrFail
let makeDisposable () =
    { new System.IDisposable
        with member this.Dispose() = () }

type DisposableRecord =
  { Id : string }
    interface System.IDisposable with
      member x.Dispose () = ()

[<Tests>]
let tests =
  testList "tests" [
    testCase "Computation Expreession Returns Value" <| fun () ->
        let return42 = jobTrial { return 42 }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Returns Job" <| fun () ->
        let return42 = jobTrial { return! job { return 42 }  }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Returns Async" <| fun () ->
        let return42 = jobTrial { return! async { return 42 }  }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Returns Task" <| fun () ->
        let return42 = jobTrial { return! Task.FromResult(42)  }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Returns Result" <| fun () ->
        let return42 = jobTrial { return! trial { return 42}  }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Returns AsyncResult" <| fun () ->
        let return42 = jobTrial { return! asyncTrial { return 42 }  }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Returns JobResult" <| fun () ->
        let return42 = jobTrial { return! jobTrial { return 42 }  }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Returns Choice" <| fun () ->
        let return42 = jobTrial { return! Choice1Of2 42 }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Returns Choice Failure" <| fun () ->
        let return42 = jobTrial { return! Choice2Of2 42 }
        let result = return42 |> getResult
        Expect.equal result (fail 42) "should be fail 42"
    testCase "Computation Expreession Binds JobResult" <| fun () ->
        let return42 = jobTrial{
            let! result = jobTrial { return 42 }
            return result
        }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Binds Choice" <| fun () ->
        let return42 = jobTrial {
            let! result = Choice1Of2 42
            return result
        }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Binds Cold Task" <| fun () ->
        let return42 = jobTrial{
            let! result =  fun () -> Task.FromResult 42
            return result
        }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Binds Job" <| fun () ->
        let return42 = jobTrial{
            let! result = job { return 42 }
            return result
        }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Binds Async" <| fun () ->
        let return42 = jobTrial{
            let! result = async { return 42 }
            return result
        }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Binds Task" <| fun () ->
        let return42 = jobTrial{
            let! result = Task.FromResult(42)
            return result
        }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Binds Result" <| fun () ->
        let return42 = jobTrial{
            let! result = trial { return 42 }
            return result
        }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Binds AsyncResult" <| fun () ->
        let return42 = jobTrial{
            let! result = asyncTrial { return 42 }
            return result
        }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Zero/Combine/Delay/Run" <| fun () ->
        let return42 = jobTrial {
            let result = 42
            if true then ()
            return result
          }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession TryWith" <| fun () ->
        let return42 = jobTrial {
            let result =42
            try
              ()
            with e -> ()
            return result
          }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession TryFinally" <| fun () ->
        let return42 = jobTrial {
            let result = 42
            try
              ()
            finally
              ()
            return result
          }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Using" <| fun () ->
        let return42 = jobTrial {
            use d = makeDisposable()
            let result = 42
            return result
          }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Using Record" <| fun () ->
        let return42 = jobTrial {
            use d = {DisposableRecord.Id = "things"}
            let result = 42
            return result
          }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
    testCase "Computation Expreession Using NullRecord" <| fun () ->
        let return42 = jobTrial {
            use d : System.IDisposable = null
            let result = 42
            return result
          }
        let result = return42 |> getOkValue
        Expect.equal result 42 "should be 42"
  ]
