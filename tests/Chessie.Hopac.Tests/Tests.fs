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
[<Test>]
let ``Computation Expreession Returns JobResult`` () =
  let returnsReturn = jobTrial{
    return!  jobTrial { return 42 }
  }
  let result = returnsReturn |> getOkValue
  Assert.AreEqual(42,result)
[<Test>]
let ``Computation Expreession Returns JobOfResult`` () =

  let returnsReturn = jobTrial{
    return! job { return ok 42 } 
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
let ``Computation Expreession Returns AsyncOfResult`` () =
  let returnsReturn = jobTrial{
    return! async { return ok 42 }  
  }
  let result = returnsReturn |> getOkValue
  Assert.AreEqual(42,result)
[<Test>]
let ``Computation Expreession Returns TaskOfResult`` () =
  let returnsReturn = jobTrial{
    return! Task.FromResult(ok 42)
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
    let! result = job { return ok 42 }
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
let ``Computation Expreession Binds AsyncOfResult`` () =
  let returnsReturn = jobTrial{
    let! result = async { return ok 42 }
    return result
  }
  let result = returnsReturn |> getOkValue
  Assert.AreEqual(42,result)
[<Test>]
let ``Computation Expreession Binds TaskOfResult`` () =
  let returnsReturn = jobTrial{
    let! result = Task.FromResult(ok 42)
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
