namespace Chessie.Hopac.Tests 

open Chessie.Hopac
open NUnit.Framework
open Hopac
open Chessie.ErrorHandling
open System.Threading.Tasks


module ComputationExpression =

    let getOkValue (jr : JobResult<'a,'b>) =
      jr 
      |> JobTrial.ofJobResult
      |> Hopac.run
      |> Trial.returnOrFail


    [<Test>]
    let ``Computation Expreession Returns value`` () =
      let return42 = jobTrial { return 42 }
      let result = return42 |> getOkValue
      Assert.AreEqual(42,result)


    [<Test>]
    let ``Computation Expreession Returns job`` () =

      let returnsReturn = jobTrial{
        return! job { return 42 } 
      }
      let result = returnsReturn |> getOkValue
      Assert.AreEqual(42,result)
    [<Test>]
    let ``Computation Expreession Returns JobResult`` () =
      let returnsReturn = jobTrial{
        return!  jobTrial { return 42 }
      }
      let result = returnsReturn |> getOkValue
      Assert.AreEqual(42,result)

    [<Test>]
    let ``Computation Expreession Returns Result`` () =
      let returnsReturn = jobTrial{
        return! ok 42 
      }
      let result = returnsReturn |> getOkValue
      Assert.AreEqual(42,result)

    [<Test>]
    let ``Computation Expreession Returns AsyncResult`` () =
      let returnsReturn = jobTrial{
        return! asyncTrial { return 42 }  
      }
      let result = returnsReturn |> getOkValue
      Assert.AreEqual(42,result)
    [<Test>]
    let ``Computation Expreession Returns async`` () =
      let returnsReturn = jobTrial{
        return! async { return  42 }  
      }
      let result = returnsReturn |> getOkValue
      Assert.AreEqual(42,result)
    [<Test>]
    let ``Computation Expreession Returns TaskOfResult`` () =
      let returnsReturn = jobTrial{
        return! Task.FromResult(42)
      }
      let result = returnsReturn |> getOkValue
      Assert.AreEqual(42,result)

    [<Test>]
    let ``Computation Expreession Binds JobResult`` () =
      let returnsReturn = jobTrial{
        let! result = jobTrial { return 42 }
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.AreEqual(42,result)

    [<Test>]
    let ``Computation Expreession Binds JobOfResult`` () =
      let returnsReturn = jobTrial{
        let! result = job { return 42 }
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.AreEqual(42,result)
    [<Test>]
    let ``Computation Expreession Binds Result`` () =
      let returnsReturn = jobTrial{
        let! result = trial { return 42 }
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.AreEqual(42,result)
    [<Test>]
    let ``Computation Expreession Binds AsyncResult`` () =
      let returnsReturn = jobTrial{
        let! result = asyncTrial { return 42 }
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.AreEqual(42,result)
    [<Test>]
    let ``Computation Expreession Binds Async`` () =
      let returnsReturn = jobTrial{
        let! result = async { return  42 }
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.AreEqual(42,result)
    [<Test>]
    let ``Computation Expreession Binds Task`` () =
      let returnsReturn = jobTrial{
        let! result = Task.FromResult(42)
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.AreEqual(42,result)

    [<Test>]
    let ``Computation Expreession Zero/Combine/Delay/Run`` () =
      let returnsReturn = jobTrial {
        let result =42
        if true then ()
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.AreEqual(42,result)

    [<Test>]
    let ``Computation Expreession TryWith`` () =
      let returnsReturn = jobTrial {
        let result =42
        try 
          () 
        with e -> ()
        return result
      }
      let result = returnsReturn |> getOkValue
      Assert.AreEqual(42,result)

    [<Test>]
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
      Assert.AreEqual(42,result)
