namespace Chessie.Hopac

open System
open Chessie.ErrorHandling
open Hopac
open System.Threading.Tasks


[<NoComparison;NoEquality>]
type JobResult<'a, 'b> =
    | JobResult of Job<Result<'a, 'b>>


module Job =


    ///**Description**
    /// Intercepts a value within a pipeline. The value is applied to a given function, then returned so it can continue through the pipeline. This function is primarily useful for debugging or other side effects.
    ///**Parameters**
    ///  * `x2yJ` - parameter of type `'a -> Job<unit>`
    ///  * `x` - parameter of type `'a`
    ///
    ///**Output Type**
    ///  * `Job<'a>`
    ///
    ///**Exceptions**
    ///
    let inline tee (x2yJ: _ -> Job<unit>) x =
      x
      |> x2yJ
      |> Job.map(fun _ -> x)


    ///**Description**
    /// Takes any value and returns a job that does nothing
    ///**Parameters**
    ///
    ///
    ///**Output Type**
    ///  * `Job<unit>`
    ///
    ///**Exceptions**
    ///
    let inline noop _ = Job.unit()


module Trial =
    let inline ofOption failCase opt =
      match opt with
      | Some v -> ok v
      | None -> failCase () |> fail

[<AutoOpen>]
module JobTrial =

    ///**Description**
    /// Takes a JobResult<_,_> and coverts it to a Job<Result<_,_>>.  Useful for "exiting" the monad for IO boundaries. Same as `ofJobResult`
    ///**Parameters**
    ///
    ///
    ///**Output Type**
    ///  * `Job<Result<'a,'b>>`
    ///
    ///**Exceptions**
    ///
    let inline toJobOfResult (JobResult jr) = jr

    ///**Description**
    /// Takes a JobResult<_,_> and coverts it to a Job<Result<_,_>>.  Useful for "exiting" the monad for IO boundaries. Same as `toJobOfResult`
    ///**Parameters**
    ///
    ///
    ///**Output Type**
    ///  * `Job<Result<'a,'b>>`
    ///
    ///**Exceptions**
    ///
    let inline ofJobResult (JobResult jr) = jr

    ///**Description**
    /// Takes a value and returns a sucessful JobResult with that value.
    ///**Parameters**
    ///  * `value` - parameter of type `'a`
    ///
    ///**Output Type**
    ///  * `JobResult<'a,'b>`
    ///
    ///**Exceptions**
    ///
    let inline ofValue value =
        value
        |> ok
        |> Job.result
        |> JobResult


    ///**Description**
    /// Takes a value and returns a failed JobResult with that value.
    ///**Parameters**
    ///  * `value` - parameter of type `'a`
    ///
    ///**Output Type**
    ///  * `JobResult<'b,'a>`
    ///
    ///**Exceptions**
    ///
    let inline ofFailure value =
        value
        |> fail
        |> Job.result
        |> JobResult


    ///**Description**
    /// Takes an Option<_> and returns a successful JobResult if Some.  Otherwise calls the failCase function provided and returns a failed JobResult from that function.
    ///**Parameters**
    ///  * `failCase` - parameter of type `unit -> 'a`
    ///  * `opt` - parameter of type `'b option`
    ///
    ///**Output Type**
    ///  * `JobResult<'b,'a>`
    ///
    ///**Exceptions**
    ///
    let inline ofOption failCase opt =
      match opt with
      | Some v -> v |> ofValue
      | None -> failCase () |>  ofFailure


    ///**Description**
    /// Takes a Job<_> and returns a successful JobResult
    ///**Parameters**
    ///  * `job` - parameter of type `Job<'a>`
    ///
    ///**Output Type**
    ///  * `JobResult<'a,'b>`
    ///
    ///**Exceptions**
    ///
    let inline ofJob job =
      job
      |> Job.map ok
      |> JobResult


    ///**Description**
    /// Takes a Job<Result<_,_>> and returns a JobResult
    ///**Parameters**
    ///  * `jobOfResult` - parameter of type `Job<Result<'a,'b>>`
    ///
    ///**Output Type**
    ///  * `JobResult<'a,'b>`
    ///
    ///**Exceptions**
    ///
    let inline ofJobOfResult (jobOfResult : Job<Result<'a,'b>>) =
        jobOfResult
        |> JobResult



    ///**Description**
    /// Takes a Job<Choice<_,_>> and coverts a JobResult
    ///**Parameters**
    ///  * `jobOfChoice` - parameter of type `Job<Choice<'a,'b>>`
    ///
    ///**Output Type**
    ///  * `JobResult<'a,'b>`
    ///
    ///**Exceptions**
    ///
    let inline ofJobOfChoice (jobOfChoice : Job<Choice<'a,'b>>) =
      jobOfChoice
      |> Job.map Trial.ofChoice
      |> ofJobOfResult


    ///**Description**
    /// Takes a Job<Option<_>> and returns a successful JobResult if Some.
    /// Otherwise calls the failCase function provided and returns a failed JobResult from that function.
    ///**Parameters**
    ///  * `failCase` - parameter of type `unit -> 'a`
    ///  * `jobOfOption` - parameter of type `Job<'b option>`
    ///
    ///**Output Type**
    ///  * `JobResult<'b,'a>`
    ///
    ///**Exceptions**
    ///
    let inline ofJobOfOption failCase jobOfOption =
        jobOfOption
        |> Job.map (Trial.ofOption failCase)
        |> ofJobOfResult

    ///**Description**
    /// Takes a Job<_>, wraps it in a catch and returns a JobResult based on the result of the catch.
    ///**Parameters**
    ///  * `job` - parameter of type `Job<'a>`
    ///
    ///**Output Type**
    ///  * `JobResult<'a,exn>`
    ///
    ///**Exceptions**
    ///
    let inline ofJobCatch job =
      job
      |> Job.catch
      |> ofJobOfChoice

    ///**Description**
    /// Takes a Result<_,_> and converts it to a JobResult<_,_>
    ///**Parameters**
    ///  * `result` - parameter of type `Result<'a,'b>`
    ///
    ///**Output Type**
    ///  * `JobResult<'a,'b>`
    ///
    ///**Exceptions**
    ///
    let inline ofResult (result : Result<'a,'b>) =
        result
        |> Job.result
        |> ofJobOfResult

    ///**Description**
    /// Takes a Choice<_,_> and returns a successful JobResult for Choice1Of2 and a failed JobResult for Choice2Of2.
    ///**Parameters**
    ///  * `choice` - parameter of type `Choice<'b,'a>`
    ///
    ///**Output Type**
    ///  * `JobResult<'b,'c>`
    ///
    ///**Exceptions**
    ///
    let inline ofChoice choice =
      choice
      |> Trial.ofChoice
      |> ofResult

    ///**Description**
    /// Takes a Async<_> and returns a successful JobResult
    ///**Parameters**
    ///  * `async` - parameter of type `Async<'a>`
    ///
    ///**Output Type**
    ///  * `JobResult<'a,'b>`
    ///
    ///**Exceptions**
    ///
    let inline ofAsync async =
      async
      |> Job.fromAsync
      |> ofJob


    ///**Description**
    /// Converts an AsyncResult<_,_> to a JobResult<_,_>
    ///**Parameters**
    ///  * `asyncResult` - parameter of type `AsyncResult<'a,'b>`
    ///
    ///**Output Type**
    ///  * `JobResult<'a,'b>`
    ///
    ///**Exceptions**
    ///
    let inline ofAsyncResult (asyncResult : AsyncResult<'a,'b>) =
        asyncResult
        |> Async.ofAsyncResult
        |> Job.fromAsync
        |> ofJobOfResult



    ///**Description**
    /// Converts an Async<Result<_,_>> to a JobResult<_,_>
    ///**Parameters**
    ///  * `asyncOfResult` - parameter of type `Async<Result<'a,'b>>`
    ///
    ///**Output Type**
    ///  * `JobResult<'a,'b>`
    ///
    ///**Exceptions**
    ///
    let inline ofAsyncOfResult (asyncOfResult : Async<Result<'a,'b>>) =
        asyncOfResult
        |> Job.fromAsync
        |> ofJobOfResult


    ///**Description**
    ///Takes a Async<Choice<_,_>> and returns a JobResult
    ///**Parameters**
    ///  * `asyncOfChoice` - parameter of type `Async<Choice<'a,'b>>`
    ///
    ///**Output Type**
    ///  * `JobResult<'a,'b>`
    ///
    ///**Exceptions**
    ///
    let inline ofAsyncOfChoice (asyncOfChoice : Async<Choice<'a,'b>>) =
        asyncOfChoice
        |> Job.fromAsync
        |> ofJobOfChoice



    ///**Description**
    /// Takes an Async<Option<_>> and returns a successful JobResult if Some.
    /// Otherwise calls the failCase function provided and returns a failed JobResult from that function.
    ///**Parameters**
    ///  * `failCase` - parameter of type `unit -> 'a`
    ///  * `asyncOfOption` - parameter of type `Async<Option<'a>>`
    ///
    ///**Output Type**
    ///  * `JobResult<'a,'a0>`
    ///
    ///**Exceptions**
    ///
    let inline ofAsyncOfOption failCase (asyncOfOption : Async<Option<'a>>) =
        asyncOfOption
        |> Job.fromAsync
        |> ofJobOfOption failCase


    ///**Description**
    ///Takes an Async<_>, wraps it in a catch and returns a JobResult based on the result of the catch.
    ///**Parameters**
    ///  * `async` - parameter of type `Async<'a>`
    ///
    ///**Output Type**
    ///  * `JobResult<'a,exn>`
    ///
    ///**Exceptions**
    ///
    let inline ofAsyncCatch async =
      async
      |> Async.Catch
      |> ofAsyncOfChoice


    ///**Description**
    /// Takes a Task<_> and returns a successful JobResult
    ///**Parameters**
    ///  * `task` - parameter of type `Task<'a>`
    ///
    ///**Output Type**
    ///  * `JobResult<'a,'b>`
    ///
    ///**Exceptions**
    ///
    let inline ofTask (task : Task<_>) =
        task
        |> Job.awaitTask
        |> ofJob



    ///**Description**
    /// Takes a Task<Result<_,_>> and converts to a JobResult
    ///**Parameters**
    ///  * `taskOfResult` - parameter of type `Task<Result<'a,'b>>`
    ///
    ///**Output Type**
    ///  * `JobResult<'a,'b>`
    ///
    ///**Exceptions**
    ///
    let inline ofTaskOfResult (taskOfResult : Task<Result<'a,'b>>) =
        taskOfResult
        |> Job.awaitTask
        |> ofJobOfResult



    ///**Description**
    /// Takes a Task<Choice<_,_>> and converts to a JobResult
    ///**Parameters**
    ///  * `taskOfChoice` - parameter of type `Task<Choice<'a,'b>>`
    ///
    ///**Output Type**
    ///  * `JobResult<'a,'b>`
    ///
    ///**Exceptions**
    ///
    let inline ofTaskOfChoice (taskOfChoice : Task<Choice<'a,'b>>) =
        taskOfChoice
        |> Job.awaitTask
        |> ofJobOfChoice

    ///**Description**
    /// Takes an Task<Option<_>> and returns a successful JobResult if Some.
    /// Otherwise calls the failCase function provided and returns a failed JobResult from that function.
    ///**Parameters**
    ///  * `failCase` - parameter of type `unit -> 'a`
    ///  * `taskOfOption` - parameter of type `Task<Option<'a>>`
    ///
    ///**Output Type**
    ///  * `JobResult<'a,'a0>`
    ///
    ///**Exceptions**
    ///
    let inline ofTaskOfOption failCase (taskOfOption : Task<Option<'a>>) =
        taskOfOption
        |> Job.awaitTask
        |> ofJobOfOption failCase

    ///**Description**
    /// If the result is a Success it executes the given function on the value.
    /// Otherwise the exisiting failure is propagated.
    ///**Parameters**
    ///  * `next` - parameter of type `'a -> JobResult<'b,'c>`
    ///  * `jobResult` - parameter of type `JobResult<'a,'c>`
    ///
    ///**Output Type**
    ///  * `JobResult<'b,'c>`
    ///
    ///**Exceptions**
    ///
    let inline bindJobResult (next : 'a -> JobResult<'b, 'c>) (jobResult : JobResult<'a,'c>) =
      let fSuccess (value, msgs) =
          value |> (next
                    >> toJobOfResult
                    >> Job.map (mergeMessages msgs))

      let fFailure errs =
          errs
          |> Bad
          |> Job.result
      jobResult
      |> toJobOfResult
      |> Job.bind (either fSuccess fFailure)
      |> JobResult


    ///**Description**
    /// If the result is a Success it executes the given function on the value.
    /// Otherwise the exisiting failure is propagated.
    ///**Parameters**
    ///  * `next` - parameter of type `'a -> JobResult<'b,'c>`
    ///  * `jobOfResult` - parameter of type `JobResult<'a,'c>`
    ///
    ///**Output Type**
    ///  * `JobResult<'b,'c>`
    ///
    ///**Exceptions**
    ///
    let inline bindJobOfResult (next : 'a -> JobResult<'b, 'c>) (jobOfResult : Job<Result<'a,'c>>) =
      jobOfResult
      |> ofJobOfResult
      |> bindJobResult next

    ///**Description**
    /// If the result is a Success it executes the given function on the value.
    /// Otherwise the exisiting failure is propagated.
    ///**Parameters**
    ///  * `next` - parameter of type `'a -> JobResult<'b,'c>`
    ///  * `job` - parameter of type `Job<'a>`
    ///
    ///**Output Type**
    ///  * `JobResult<'b,'c>`
    ///
    ///**Exceptions**
    ///
    let inline bindJob (next : 'a -> JobResult<'b,'c>) (job : Job<'a>) =
       job |> ofJob |> bindJobResult next

    ///**Description**
    /// If the result is a Success it executes the given function on the value.
    /// Otherwise the exisiting failure is propagated.
    ///**Parameters**
    ///  * `next` - parameter of type `'a -> JobResult<'b,'c>`
    ///  * `result` - parameter of type `Result<'a,'c>`
    ///
    ///**Output Type**
    ///  * `JobResult<'b,'c>`
    ///
    ///**Exceptions**
    ///
    let inline bindResult (next : 'a -> JobResult<'b, 'c>) (result : Result<'a,'c>) =
      result
      |> ofResult
      |> bindJobResult next

    ///**Description**
    /// If the result is a Success it executes the given function on the value.
    /// Otherwise the exisiting failure is propagated.
    ///**Parameters**
    ///  * `next` - parameter of type `'a -> JobResult<'b,'c>`
    ///  * `choice` - parameter of type `Choice<'a,'c>`
    ///
    ///**Output Type**
    ///  * `JobResult<'b,'c>`
    ///
    ///**Exceptions**
    ///
    let inline bindChoice (next : 'a -> JobResult<'b, 'c>)  (choice : Choice<'a,'c>) =
      choice
      |> ofChoice
      |> bindJobResult next

    ///**Description**
    /// If the result is a Success it executes the given function on the value.
    /// Otherwise the exisiting failure is propagated.
    ///**Parameters**
    ///  * `next` - parameter of type `'a -> JobResult<'b,'c>`
    ///  * `asyncResult` - parameter of type `AsyncResult<'a,'c>`
    ///
    ///**Output Type**
    ///  * `JobResult<'b,'c>`
    ///
    ///**Exceptions**
    ///
    let inline bindAsyncResult (next : 'a -> JobResult<'b, 'c>) (asyncResult : AsyncResult<'a,'c>) =
      asyncResult
      |> ofAsyncResult
      |> bindJobResult next

    ///**Description**
    /// If the result is a Success it executes the given function on the value.
    /// Otherwise the exisiting failure is propagated.
    ///**Parameters**
    ///  * `next` - parameter of type `'a -> JobResult<'b,'c>`
    ///  * `asyncOfResult` - parameter of type `Async<Result<'a,'c>>`
    ///
    ///**Output Type**
    ///  * `JobResult<'b,'c>`
    ///
    ///**Exceptions**
    ///
    let inline bindAsyncOfResult (next : 'a -> JobResult<'b, 'c>) (asyncOfResult : Async<Result<'a,'c>>) =
      asyncOfResult
      |> ofAsyncOfResult
      |> bindJobResult next

    ///**Description**
    /// If the result is a Success it executes the given function on the value.
    /// Otherwise the exisiting failure is propagated.
    ///**Parameters**
    ///  * `next` - parameter of type `'a -> JobResult<'b,'c>`
    ///  * `taskOfResult` - parameter of type `Task<Result<<'a,'c>>`
    ///
    ///**Output Type**
    ///  * `JobResult<'b,'c>`
    ///
    ///**Exceptions**
    ///
    let inline bindTaskOfResult (next : 'a -> JobResult<'b, 'c>) (taskOfResult : Task<Result<'a,'c>>) =
      taskOfResult
      |> ofTaskOfResult
      |> bindJobResult next


    ///**Description**
    /// Lifts a function into a Result container and applies it on the given result.
    ///**Parameters**
    ///  * `f` - parameter of type `'a -> 'b`
    ///  * `jobResult` - parameter of type `JobResult<'a,'c>`
    ///
    ///**Output Type**
    ///  * `JobResult<'b,'c>`
    ///
    ///**Exceptions**
    ///
    let inline mapFun f (jobResult) =
      jobResult |> bindJobResult (f >> ofValue)


    ///**Description**
    /// Lifts a Job function into a Result container and applies it on the given result.
    ///**Parameters**
    ///  * `f` - parameter of type `'a -> Job<'b>`
    ///  * `jobResult` - parameter of type `JobResult<'a,'c>`
    ///
    ///**Output Type**
    ///  * `JobResult<'b,'c>`
    ///
    ///**Exceptions**
    ///
    let inline mapJob (f : _ -> Job<_>) jobResult =
      jobResult |> bindJobResult (f >> ofJob)


    ///**Description**
    /// Lifts a result function into a Result container and applies it on the given result.
    ///**Parameters**
    ///  * `f` - parameter of type `'a -> Result<'b,'c>`
    ///  * `jobResult` - parameter of type `JobResult<'a,'c>`
    ///
    ///**Output Type**
    ///  * `JobResult<'b,'c>`
    ///
    ///**Exceptions**
    ///
    let inline mapResult f jobResult =
      jobResult |> bindJobResult (f >> ofResult)


    ///**Description**
    /// Lifts an async function into a Result container and applies it on the given result.
    ///**Parameters**
    ///  * `f` - parameter of type `'a -> Async<'b>`
    ///  * `jobResult` - parameter of type `JobResult<'a,'c>`
    ///
    ///**Output Type**
    ///  * `JobResult<'b,'c>`
    ///
    ///**Exceptions**
    ///
    let inline mapAsync f jobResult =
      jobResult |> bindJobResult (f >> ofAsync)


    ///**Description**
    /// Lifts a task function into a Result container and applies it on the given result.
    ///**Parameters**
    ///  * `f` - parameter of type `'a -> 'Task<b>`
    ///  * `jobResult` - parameter of type `JobResult<'a,'d>`
    ///
    ///**Output Type**
    ///  * `JobResult<'c,'d>`
    ///
    ///**Exceptions**
    ///
    let inline mapTask (f : _ -> Task<_>) jobResult =
      jobResult |> bindJobResult (f >> ofTask)


    ///**Description**
    /// Maps a function over the existing error messages in case of failure. In case of success, the message type will be changed and warnings will be discarded.
    ///**Parameters**
    ///  * `f` - parameter of type `'a list -> 'b list`
    ///  * `jobResult` - parameter of type `JobResult<'c,'a>`
    ///
    ///**Output Type**
    ///  * `JobResult<'c,'b>`
    ///
    ///**Exceptions**
    ///
    let inline mapFailure f (jobResult : JobResult<_,_>) =
      jobResult
      |> toJobOfResult
      |> Job.map(Trial.mapFailure f)
      |> JobResult


    /// **Description**
    /// Maps a function over the existing error messages in case of failure. In case of success, the message type will be changed and warnings will be discarded.
    /// This is the same as `result |> mapFailure (List.map f)`
    ///
    /// **Parameters**
    ///   * `f` - parameter of type `'a -> 'b`
    ///   * `jobResult` - parameter of type `JobResult<'c,'a>`
    ///
    /// **Output Type**
    ///   * `JobResult<'c,'b>`
    ///
    /// **Exceptions**
    ///
    let inline mapFailures f (jobResult : JobResult<_,_>) =
      jobResult |> mapFailure (List.map f)



    /// **Description**
    /// Takes a JobResult and maps it with fSuccess if it is a Success otherwise it maps it with fFailure.
    /// **Parameters**
    ///   * `fSuccess` - parameter of type `'a * 'b list -> Job<'c>`
    ///   * `fFailure` - parameter of type `'b list -> Job<'c>`
    ///   * `jobResult` - parameter of type `JobResult<'a,'b>`
    ///
    /// **Output Type**
    ///   * `Job<'d>`
    ///
    /// **Exceptions**
    ///
    let inline eitherJob fSuccess fFailure   (jobResult : JobResult<_,_>) =
        jobResult
        |> toJobOfResult
        |> Job.bind (Trial.either fSuccess fFailure)



    /// **Description**
    /// Takes a JobResult and maps it with fSuccess if it is a Success otherwise it maps it with fFailure.
    /// **Parameters**
    ///   * `fSuccess` - parameter of type `'a * 'b list -> 'c`
    ///   * `fFailure` - parameter of type `'b list -> 'c`
    ///   * `jobResult` - parameter of type `JobResult<'a,'b>`
    ///
    /// **Output Type**
    ///   * `Job<'c>`
    ///
    /// **Exceptions**
    ///
    let inline eitherFun fSuccess fFailure (jobResult : JobResult<_,_>) =
      eitherJob (fSuccess >> Job.result) (fFailure >> Job.result) jobResult

    /// **Description**
    /// Takes a JobResult and maps it with fSuccess if it is a Success otherwise it maps it with fFailure.
    /// **Parameters**
    ///   * `fSuccess` - parameter of type `'a * 'b list -> Async<'c>`
    ///   * `fFailure` - parameter of type `'b list -> Async<'c>`
    ///   * `jobResult` - parameter of type `JobResult<'a,'b>`
    ///
    /// **Output Type**
    ///   * `Job<'c>`
    ///
    /// **Exceptions**
    ///
    let inline eitherAsync fSuccess fFailure (jobResult : JobResult<_,_>) =
      eitherJob (fSuccess >> Job.fromAsync) (fFailure >> Job.fromAsync) jobResult


    /// **Description**
    /// Takes a JobResult and maps it with fSuccess if it is a Success otherwise it maps it with fFailure.
    /// **Parameters**
    ///   * `fSuccess` - parameter of type `'a * 'b list -> Task<'c>`
    ///   * `fFailure` - parameter of type `'b list -> <Task<'e>`
    ///   * `jobResult` - parameter of type `JobResult<'a,'b>`
    ///
    /// **Output Type**
    ///   * `Job<'d>`
    ///
    /// **Exceptions**
    ///
    let inline eitherTask fSuccess fFailure (jobResult : JobResult<_,_>) =
      eitherJob (fSuccess >> Job.awaitTask) (fFailure >> Job.awaitTask) jobResult




    /// **Description**
    /// If the result is a Success it executes the given success function on the value and the messages.
    /// If the result is a Failure it executes the given failure function on the messages.
    /// Result is propagated unchanged.
    /// **Parameters**
    ///   * `fSuccess` - parameter of type `'a * 'b list -> unit`
    ///   * `fFailure` - parameter of type `'b list -> unit`
    ///   * `jobResult` - parameter of type `JobResult<'a,'b>`
    ///
    /// **Output Type**
    ///   * `JobResult<'a,'b> -> JobResult<'a,'b>`
    ///
    /// **Exceptions**
    ///
    let inline eitherTee fSuccess fFailure jobResult =
        jobResult
        |> toJobOfResult
        |> Job.map(fun x -> Trial.eitherTee fSuccess fFailure x)
        |> ofJobOfResult


    /// **Description**
    /// If the result is a Success it executes the given function on the value and the messages.
    /// Result is propagated unchanged.
    /// **Parameters**
    ///   * `fSuccess` - parameter of type `'a * 'b list -> unit`
    ///   * `jobResult` - parameter of type `JobResult<'a,'b>`
    ///
    /// **Output Type**
    ///   * `JobResult<'a,'b>`
    ///
    /// **Exceptions**
    ///
    let inline successTee fSuccess jobResult=
      eitherTee fSuccess ignore jobResult

    /// **Description**
    /// If the result is a Failure it executes the given function on the messages.
    /// Result is propagated unchanged.
    /// **Parameters**
    ///   * `fFailure` - parameter of type `'a list -> unit`
    ///   * `jobResult` - parameter of type `JobResult<'b,'a>`
    ///
    /// **Output Type**
    ///   * `JobResult<'b,'a>`
    ///
    /// **Exceptions**
    ///
    let inline failureTee fFailure jobResult=
      eitherTee ignore fFailure jobResult



    /// **Description**
    /// If the result is a Success it executes the given success function on the value and the messages.
    /// If the result is a Failure it executes the given failure function on the messages.
    /// Result is propagated unchanged.
    /// **Parameters**
    ///   * `fSuccess` - parameter of type `'a * 'b list -> Job<unit>`
    ///   * `fFailure` - parameter of type `'b list -> Job<unit>`
    ///   * `jobResult` - parameter of type `JobResult<'a,'b>`
    ///
    /// **Output Type**
    ///   * `JobResult<'a,'b> -> JobResult<'a,'b>`
    ///
    /// **Exceptions**
    ///
    let inline eitherTeeJob fSuccess fFailure jobResult =
        jobResult
        |> toJobOfResult
        |> Job.bind(Job.tee(Trial.either fSuccess fFailure))
        |> ofJobOfResult


    /// **Description**
    /// If the result is a Success it executes the given function on the value and the messages.
    /// Result is propagated unchanged.
    /// **Parameters**
    ///   * `fSuccess` - parameter of type `'a * 'b list -> Job<unit>`
    ///   * `jobResult` - parameter of type `JobResult<'a,'b>`
    ///
    /// **Output Type**
    ///   * `JobResult<'a,'b>`
    ///
    /// **Exceptions**
    ///
    let inline successTeeJob fSuccess jobResult=
      eitherTeeJob fSuccess Job.noop jobResult

    /// **Description**
    /// If the result is a Failure it executes the given function on the messages.
    /// Result is propagated unchanged.
    /// **Parameters**
    ///   * `fFailure` - parameter of type `'a list -> Job<unit>`
    ///   * `jobResult` - parameter of type `JobResult<'b,'a>`
    ///
    /// **Output Type**
    ///   * `JobResult<'b,'a>`
    ///
    /// **Exceptions**
    ///
    let inline failureTeeJob fFailure jobResult=
      eitherTeeJob Job.noop fFailure jobResult



    type JobTrialBuilder () =

      member inline __.Bind(jobResult : JobResult<'a, 'c>, binder : 'a -> JobResult<'b, 'c>) : JobResult<'b, 'c> =
        bindJobResult binder jobResult

      member inline __.Bind(jobResult : Job<_>, binder : 'a -> JobResult<'b, 'c>) : JobResult<'b, 'c> =
        bindJob binder jobResult

      member inline __.Bind(result : Result<'a, 'c>, binder : 'a -> JobResult<'b, 'c>) : JobResult<'b, 'c> =
        bindResult binder result

      member inline __.Bind(result : AsyncResult<'a, 'c>, binder : 'a -> JobResult<'b, 'c>) : JobResult<'b, 'c> =
        bindAsyncResult binder result

      member inline __.Bind(result : Async<_>, binder : 'a -> JobResult<'b, 'c>) : JobResult<'b, 'c> =
        result |> ofAsync |> bindJobResult binder

      member inline __.Bind(result : Task<_>, binder : 'a -> JobResult<'b, 'c>) : JobResult<'b, 'c> =
        result |> ofTask |> bindJobResult binder

      member inline __.Bind(result : unit -> Task<_>, binder : 'a -> JobResult<'b, 'c>) : JobResult<'b, 'c> =
        result |> Job.fromTask |> ofJob |> bindJobResult binder

      member inline __.Bind(result : Choice<_,_>, binder : 'a -> JobResult<'b, 'c>) : JobResult<'b, 'c> =
        result |> bindChoice binder

      member inline __.Return value : JobResult<'a,'b> =
        value
        |> ofValue
      member inline __.ReturnFrom (value : JobResult<'a,'b>) =
        value

      member inline __.ReturnFrom (value : Choice<'a,'b>) =
        value
        |> ofChoice

      member inline __.ReturnFrom (value : Job<_>) =
        value
        |> ofJob

      member inline __.ReturnFrom (value : Result<'a,'b>) =
        value
        |> ofResult

      member inline __.ReturnFrom (value : AsyncResult<'a,'b>) =
        value
        |> ofAsyncResult

      member inline __.ReturnFrom (value : Async<_>) =
        value
        |> ofAsync

      member inline __.ReturnFrom (value : Task<_>) =
        value
        |> ofTask

      member inline __.ReturnFrom(value : unit -> Task<_>) =
        value
        |> Job.fromTask
        |> ofJob


      member inline __.Zero () = __.Return ()

      member inline __.Combine (a : JobResult<unit,'a>,b) = bindJobResult b a

      member inline __.Delay(f : unit -> JobResult<'a,'b>) = f

      member inline __.Run (f) = f ()

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

      member inline __.TryWith(jobResult, catchHandler : exn -> JobResult<'a, 'b>) : JobResult<'a, 'b> =
          job.TryWith( jobResult >> toJobOfResult, (catchHandler >> toJobOfResult)) |> JobResult

      member __.TryFinally(jobResult , compensation : unit -> unit) : JobResult<'a, 'b> =
          job.TryFinally( jobResult >> toJobOfResult, compensation) |> JobResult

    let jobTrial = JobTrialBuilder()



    /// **Description**
    /// Executes the given function on a given success or captures the exception in a failure
    /// **Parameters**
    ///   * `failCase` - parameter of type `exn -> 'b`
    ///   * `jobResult` - parameter of type `JobResult<'a,'b>`
    ///
    /// **Output Type**
    ///   * `JobResult<'a,'b>`
    ///
    /// **Exceptions**
    ///
    let inline catch (failCase : exn -> 'b) (jobResult : JobResult<'a,'b>) = jobTrial {
      try
        return! jobResult
      with e ->
        return! failCase e |> ofFailure
    }


    /// **Description**
    /// Flattens a `JobResult` to a value
    /// **Parameters**
    ///   * `errorHandler` - parameter of type `'b list -> Job<'a>`
    ///   * `jobResult` - parameter of type `JobResult<'a,'b>`
    ///
    /// **Output Type**
    ///   * `Job<'a>`
    ///
    /// **Exceptions**
    ///
    let inline recoverJob (errorHandler : 'b list -> Job<'a>) (jobResult : JobResult<'a,'b>) : Job<'a> =
      eitherJob (fst >> Job.result) errorHandler jobResult


    /// **Description**
    /// Flattens a `JobResult` to a value
    /// **Parameters**
    ///   * `errorHandler` - parameter of type `'b list -> 'a`
    ///   * `jobResult` - parameter of type `JobResult<'a,'b>`
    ///
    /// **Output Type**
    ///   * `Job<'a>`
    ///
    /// **Exceptions**
    ///
    let inline recoverFun (errorHandler : 'b list -> 'a) (jobResult : JobResult<'a,'b>) : Job<'a> =
      recoverJob (errorHandler >> Job.result) jobResult

