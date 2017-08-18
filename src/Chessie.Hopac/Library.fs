namespace Chessie.Hopac

open System
open Chessie.ErrorHandling
open Hopac
open System.Threading.Tasks


[<NoComparison;NoEquality>]
type JobResult<'a, 'b> = 
    | JobResult of Job<Result<'a, 'b>>


module Job =    

    let inline tee (x2yJ: _ -> Job<unit>) x = 
      x 
      |> x2yJ
      |> Job.map(fun _ -> x)
    let inline noop _ = Job.unit()

[<AutoOpen>]
module JobTrial =
    let inline ofValue value =
        value
        |> ok 
        |> Job.result
        |> JobResult

    let inline ofFailure value =
        value
        |> fail
        |> Job.result
        |> JobResult

    let inline ofOption failCase value =
      match value with
      | Some v -> v |> ofValue
      | None -> failCase ()

    let inline ofChoice failCase value =
      match value with
      | Choice1Of2 v -> v |> ofValue
      | Choice2Of2 e -> e |> failCase 

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

    let inline ofError value =
      value 
      |> fail
      |> ofResult

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

    let inline bindJob next r = 
      r |> ofJob |> bindJobResult next

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

    let inline mapAsync f x =
      x |> bindJobResult (f >> ofAsync)
    
    let inline mapTask f x =
      x |> bindJobResult (f >> ofTask)

    let inline mapFailure f (result : JobResult<_,_>) =  
      result
      |> ofJobResult
      |> Job.map(Trial.mapFailure f) 
      |> JobResult

    let inline ofJobOption failCase value =
        value
        |> bindJob (ofOption failCase)

    let inline ofAsyncOption failCase value =
        value
        |> Job.fromAsync
        |> ofJobOption failCase

    let inline ofTaskOption failCase value =
        value
        |> Job.awaitTask
        |> ofJobOption failCase

    let inline ofJobOfChoice failCase value=
        value
        |> bindJob (ofChoice failCase)


    let inline ofAsyncOfChoice failCase value=
        value
        |> Job.fromAsync
        |> ofJobOfChoice failCase

    let inline ofTaskOfChoice failCase value=
        value
        |> Job.awaitTask
        |> ofJobOfChoice failCase



    let inline eitherJob fSuccess fFailure   (trialResult : JobResult<_,_>) =
        trialResult
        |> ofJobResult
        |> Job.bind (Trial.either fSuccess fFailure)
    let inline eitherFun fSuccess fFailure (trialResult : JobResult<_,_>) =         
      eitherJob (fSuccess >> Job.result) (fFailure >> Job.result) trialResult
    let inline eitherAsync fSuccess fFailure (trialResult : JobResult<_,_>) =  
      eitherJob (fSuccess >> Job.fromAsync) (fFailure >> Job.fromAsync) trialResult
    let inline eitherTask fSuccess fFailure (trialResult : JobResult<_,_>) = 
      eitherJob (fSuccess >> Job.awaitTask) (fFailure >> Job.awaitTask) trialResult

    

    let inline eitherTee fSuccess fFailure =
        ofJobResult
        >> Job.map(fun x -> Trial.eitherTee fSuccess fFailure x)
        >> ofJobOfResult

    let inline successTee fSuccess result=
      eitherTee fSuccess ignore result
    let inline failureTee fFailure result=
      eitherTee ignore fFailure result
    let inline eitherTeeJob fSuccess fFailure  =
        ofJobResult
        >> Job.bind(Job.tee(Trial.either fSuccess fFailure))
        >> ofJobOfResult

    let inline successTeeJob fSuccess result=
      eitherTeeJob fSuccess Job.noop result
    let inline failureTeeJob fFailure result=
      eitherTeeJob Job.noop fFailure result



    type JobTrialBuilder () =

      member __.Bind(jobResult : JobResult<'a, 'c>, binder : 'a -> JobResult<'b, 'c>) : JobResult<'b, 'c> = 
        bindJobResult binder jobResult

      member __.Bind(jobResult : Job<_>, binder : 'a -> JobResult<'b, 'c>) : JobResult<'b, 'c> = 
        bindJob binder jobResult

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

      member __.Using(d:#IDisposable, body) =
            let result = fun () -> body d
            __.TryFinally (result, fun () ->
                match box d with
                | null -> ()
                | _ -> d.Dispose())
      member __.While (guard, body) =
            if not <| guard () then
                __.Zero()
            else
                bindJobResult (fun () -> __.While(guard, body)) (body())

      member __.TryWith(jobResult, catchHandler : exn -> JobResult<'a, 'b>) : JobResult<'a, 'b> = 
          job.TryWith( jobResult >> ofJobResult, (catchHandler >> ofJobResult)) |> JobResult
      
      member __.TryFinally(jobResult , compensation : unit -> unit) : JobResult<'a, 'b> = 
          job.TryFinally( jobResult >> ofJobResult, compensation) |> JobResult
  
    let jobTrial = JobTrialBuilder()

    let catch (failCase : exn -> 'b) (v : JobResult<'a,'b>) = jobTrial {
      try
        return! v
      with e -> 
        return! failCase e |> ofFailure
    }

