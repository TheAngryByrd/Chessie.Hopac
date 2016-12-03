module Chessie.Hopac.Tests

open Chessie.Hopac
open NUnit.Framework
open Hopac
open Chessie.ErrorHandling
open System.Threading.Tasks

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

let ``Computation Expreession Returns JobResult`` () =
  let returnsReturn = jobTrial{
    return!  jobTrial { return 42 }
  }
  let result = returnsReturn |> getOkValue
  Assert.AreEqual(42,result)

let ``Computation Expreession Returns JobOfResult`` () =

  let returnsReturn = jobTrial{
    return! job { return ok 42 } 
  }
  let result = returnsReturn |> getOkValue
  Assert.AreEqual(42,result)

let ``Computation Expreession Returns Result`` () =
  let returnsReturn = jobTrial{
    return! ok 42 
  }
  let result = returnsReturn |> getOkValue
  Assert.AreEqual(42,result)


let ``Computation Expreession Returns AsyncResult`` () =
  let returnsReturn = jobTrial{
    return! asyncTrial { return 42 }  
  }
  let result = returnsReturn |> getOkValue
  Assert.AreEqual(42,result)

let ``Computation Expreession Returns AsyncOfResult`` () =
  let returnsReturn = jobTrial{
    return! async { return ok 42 }  
  }
  let result = returnsReturn |> getOkValue
  Assert.AreEqual(42,result)

let ``Computation Expreession Returns TaskOfResult`` () =
  let returnsReturn = jobTrial{
    return! Task.FromResult(ok 42)
  }
  let result = returnsReturn |> getOkValue
  Assert.AreEqual(42,result)