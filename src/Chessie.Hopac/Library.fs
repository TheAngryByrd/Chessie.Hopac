namespace Chessie.Hopac

open Chessie.ErrorHandling
open Hopac
open System.Threading.Tasks

[<NoComparison;NoEquality>]
type JobResult<'a, 'b> = 
    | JobResult of Job<Result<'a, 'b>>


[<AutoOpen>]
module JobTrial =

    let inline ofValue value =
        value
        |> ok 
        |> Job.result
        |> JobResult

    let inline ofJobOfResult (value : Job<Result<'a,'b>>) =
        value
        |> JobResult

    let inline ofResult (value : Result<'a,'b>) = 
        value
        |> Job.result
        |> ofJobOfResult

    let inline ofAsyncResult (value : AsyncResult<'a,'b>) =
        value
        |> Async.ofAsyncResult
        |> Job.fromAsync
        |> ofJobOfResult
    let inline ofAsyncOfResult (value : Async<Result<'a,'b>>) =
        value
        |> Job.fromAsync
        |> ofJobOfResult

    let inline ofTaskOfResult (value : Task<Result<'a,'b>>) =
        value
        |> Job.awaitTask
        |> ofJobOfResult

    let inline ofJobResult (JobResult jr) = jr

    type JobTrialBuilder () =
      member __.Return value : JobResult<'a,'b> =
       value
       |> ofValue 
      member __.ReturnFrom (value : JobResult<'a,'b>) = value
      member __.ReturnFrom (value : Job<Result<'a,'b>>) = 
        value
        |> ofJobOfResult
      member __.ReturnFrom (value : Result<'a,'b>) = 
        value
        |> ofResult

      member __.ReturnFrom (value : AsyncResult<'a,'b>) = 
        value
        |> ofAsyncResult
      member __.ReturnFrom (value : Async<Result<'a,'b>>) = 
        value
        |> ofAsyncOfResult

      member __.ReturnFrom (value : Task<Result<'a,'b>>) = 
        value
        |> ofTaskOfResult
  
    let jobTrial = JobTrialBuilder()
    
  