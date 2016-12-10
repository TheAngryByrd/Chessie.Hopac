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

    let inline ofJob value =
      value
      |> Job.map ok
      |> JobResult

    let inline ofJobOfResult (value : Job<Result<'a,'b>>) =
        value
        |> JobResult

    let inline ofResult (value : Result<'a,'b>) = 
        value
        |> Job.result
        |> ofJobOfResult

    let inline ofAsync value =
      value
      |> Job.fromAsync
      |> ofJob

    let inline ofAsyncResult (value : AsyncResult<'a,'b>) =
        value
        |> Async.ofAsyncResult
        |> Job.fromAsync
        |> ofJobOfResult

    let inline ofAsyncOfResult (value : Async<Result<'a,'b>>) =
        value
        |> Job.fromAsync
        |> ofJobOfResult

    let inline ofTask (value : Task<_>) =
        value
        |> Job.awaitTask
        |> ofJob

    let inline ofTaskOfResult (value : Task<Result<'a,'b>>) =
        value
        |> Job.awaitTask
        |> ofJobOfResult

    let inline ofJobResult (JobResult jr) = jr
    let inline bindJobResult (next : 'a -> JobResult<'b, 'c>) (jr : JobResult<'a,'c>) =
      let fSuccess (value, msgs) = 
          value |> (next
                    >> ofJobResult
                    >> Job.map (mergeMessages msgs))
            
      let fFailure errs = 
          errs
          |> Bad
          |> Job.result
      jr
      |> ofJobResult
      |> Job.bind (either fSuccess fFailure)
      |> JobResult

    let inline bindJobOfResult (next : 'a -> JobResult<'b, 'c>) (r : Job<Result<'a,'c>>) =
      r 
      |> ofJobOfResult
      |> bindJobResult next

    let inline bindResult (next : 'a -> JobResult<'b, 'c>) (r : Result<'a,'c>) =
      r 
      |> ofResult
      |> bindJobResult next

    let inline bindAsyncResult (next : 'a -> JobResult<'b, 'c>) (r : AsyncResult<'a,'c>) =
      r 
      |> ofAsyncResult
      |> bindJobResult next
    let inline bindAsyncOfResult (next : 'a -> JobResult<'b, 'c>) (r : Async<Result<'a,'c>>) =
      r 
      |> ofAsyncOfResult
      |> bindJobResult next

    let inline bindTaskOfResult (next : 'a -> JobResult<'b, 'c>) (r : Task<Result<'a,'c>>) =
      r 
      |> ofTaskOfResult
      |> bindJobResult next

    let inline mapFun f x =
      x |> bindJobResult (f >> ofValue)

    let inline mapJob f x =
      x |> bindJobResult (f >> ofJob)

    let inline mapResult f x =
      x |> bindJobResult (f >> ofResult)

    let inline mapFailure f (result : JobResult<_,_>) =  
      result
      |> ofJobResult
      |> Job.map(Trial.mapFailure f) 
      |> JobResult

    type JobTrialBuilder () =

      member __.Bind(jobResult : JobResult<'a, 'c>, binder : 'a -> JobResult<'b, 'c>) : JobResult<'b, 'c> = 
        bindJobResult binder jobResult

      member __.Bind(jobResult : Job<_>, binder : 'a -> JobResult<'b, 'c>) : JobResult<'b, 'c> = 
        jobResult |> ofJob |> bindJobResult binder 

      member __.Bind(result : Result<'a, 'c>, binder : 'a -> JobResult<'b, 'c>) : JobResult<'b, 'c> = 
        bindResult binder result

      member __.Bind(result : AsyncResult<'a, 'c>, binder : 'a -> JobResult<'b, 'c>) : JobResult<'b, 'c> = 
        bindAsyncResult binder result

      member __.Bind(result : Async<_>, binder : 'a -> JobResult<'b, 'c>) : JobResult<'b, 'c> = 
        result |> ofAsync |> bindJobResult binder 
      
      member __.Bind(result : Task<_>, binder : 'a -> JobResult<'b, 'c>) : JobResult<'b, 'c> = 
        result |> ofTask |> bindJobResult binder 
     
      member __.Return value : JobResult<'a,'b> =
        value
        |> ofValue 
      member __.ReturnFrom (value : JobResult<'a,'b>) =
        value

      member __.ReturnFrom (value : Job<_>) = 
        value 
        |> ofJob
      
      member __.ReturnFrom (value : Result<'a,'b>) = 
        value
        |> ofResult
      
      member __.ReturnFrom (value : AsyncResult<'a,'b>) = 
        value
        |> ofAsyncResult
      
      member __.ReturnFrom (value : Async<_>) = 
        value
        |> ofAsync
     
      member __.ReturnFrom (value : Task<_>) = 
        value
        |> ofTask

      member __.Zero () = __.Return ()

      member __.Combine (a : JobResult<unit,'a>,b) = bindJobResult b a

      member __.Delay(f : unit -> JobResult<'a,'b>) = f 

      member __.Run (f) = f ()

      member __.TryWith(jobResult, catchHandler : exn -> JobResult<'a, 'b>) : JobResult<'a, 'b> = 
          job.TryWith( jobResult >> ofJobResult, (catchHandler >> ofJobResult)) |> JobResult
      
      member __.TryFinally(jobResult , compensation : unit -> unit) : JobResult<'a, 'b> = 
          job.TryFinally( jobResult >> ofJobResult, compensation) |> JobResult
  
    let jobTrial = JobTrialBuilder()
    
  