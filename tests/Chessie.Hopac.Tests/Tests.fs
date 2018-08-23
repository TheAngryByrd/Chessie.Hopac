namespace Chessie.Hopac.Tests 

open Chessie.Hopac
open Xunit
open Hopac
open Chessie.ErrorHandling
open System.Threading.Tasks


module ComputationExpression =
    let getResult (jr : JobResult<'a,'b>) =
      jr 
      |> JobTrial.toJobOfResult
      |> Hopac.run
    let getOkValue (jr : JobResult<'a,'b>) =
      jr 
      |> getResult
      |> Trial.returnOrFail




    [<Fact>]
    let ``Computation Expreession Returns value`` () =
      let return42 = jobTrial { return 42 }
      let result = return42 |> getOkValue
      Assert.Equal(42,result)


    [<Fact>]
    let ``Computation Expreession Returns job`` () =

      let returnsReturn = jobTrial{
        return! job { return 42 } 
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)
    [<Fact>]
    let ``Computation Expreession Returns JobResult`` () =
      let returnsReturn = jobTrial{
        return!  jobTrial { return 42 }
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)

    [<Fact>]
    let ``Computation Expreession Returns Result`` () =
      let returnsReturn = jobTrial{
        return! ok 42 
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)

    [<Fact>]
    let ``Computation Expreession Returns AsyncResult`` () =
      let returnsReturn = jobTrial{
        return! asyncTrial { return 42 }  
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)
    [<Fact>]
    let ``Computation Expreession Returns async`` () =
      let returnsReturn = jobTrial{
        return! async { return  42 }  
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)
    [<Fact>]
    let ``Computation Expreession Returns TaskOfResult`` () =
      let returnsReturn = jobTrial{
        return! Task.FromResult(42)
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)

    [<Fact>]
    let ``Computation Expreession Binds JobResult`` () =
      let returnsReturn = jobTrial{
        let! result = jobTrial { return 42 }
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)

    [<Fact>]
    let ``Computation Expreession Binds JobOfResult`` () =
      let returnsReturn = jobTrial{
        let! result = job { return 42 }
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)
    [<Fact>]
    let ``Computation Expreession Binds Result`` () =
      let returnsReturn = jobTrial{
        let! result = trial { return 42 }
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)
    [<Fact>]
    let ``Computation Expreession Binds AsyncResult`` () =
      let returnsReturn = jobTrial{
        let! result = asyncTrial { return 42 }
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)
    [<Fact>]
    let ``Computation Expreession Binds Async`` () =
      let returnsReturn = jobTrial{
        let! result = async { return  42 }
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)
    [<Fact>]
    let ``Computation Expreession Binds Task`` () =
      let returnsReturn = jobTrial{
        let! result = Task.FromResult(42)
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)

    [<Fact>]
    let ``Computation Expreession Zero/Combine/Delay/Run`` () =
      let returnsReturn = jobTrial {
        let result =42
        if true then ()
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)

    [<Fact>]
    let ``Computation Expreession TryWith`` () =
      let returnsReturn = jobTrial {
        let result =42
        try 
          () 
        with e -> ()
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)

    [<Fact>]
    let ``Computation Expreession TryFinally`` () =
      let returnsReturn = jobTrial {
        let result = 42
        try 
          () 
        finally
          ()
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)
   
    let makeDisposable () = 
      { new System.IDisposable 
        with member this.Dispose() = () }
    [<Fact>]
    let ``Computation Expreession Using`` () =
      let returnsReturn = jobTrial {
        use d = makeDisposable()
        let result = 42
        try 
          () 
        finally
          ()
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)

    type DisposableRecord = 
      { Id : string } 
        interface System.IDisposable with 
          member x.Dispose () = ()

    [<Fact>]
    let ``Computation Expreession Using Record`` () =
      let returnsReturn = jobTrial {
        use d = {DisposableRecord.Id = "things"}
        let result = 42
        try 
          () 
        finally
          ()
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)
    [<Fact>]
    let ``Computation Expreession Using NullRecord`` () =
      let returnsReturn = jobTrial {
        use d : System.IDisposable = null
        let result = 42
        try 
          () 
        finally
          ()
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)

    [<Fact>]
    let ``Computation Expreession bind choice1of2`` () =
      let returnsReturn = jobTrial {

        let! result = Choice1Of2 42

        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.Equal(42,result)

    [<Fact>]
    let ``Computation Expreession bind choice2of2`` () =
      let returnsReturn = jobTrial {

        let! result = Choice2Of2 42

        return result
      }
      let result = returnsReturn |> getResult
      
      Assert.Equal(fail 42,result)

    [<Fact>]
    let ``Computation Expreession cold task`` () =
      
      let returnsReturn = jobTrial {

        let! result = fun () -> Task.FromResult 42

        return result
      }
      let result = returnsReturn |> getResult
      
      Assert.Equal(ok 42,result)