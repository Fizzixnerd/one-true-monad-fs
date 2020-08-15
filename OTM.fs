namespace OTM

// Partially stolen from https://github.com/swlaschin/DomainModelingMadeFunctional/blob/master/src/OrderTaking/Result.fs from AsyncResult

open System.Diagnostics
open Serilog

/// An `error` along with a `StackTrace`
type Traced<'err> =
    { error: 'err
      trace: StackTrace }

module Traced =
    /// `map` over the `'err`
    let map (f: 'err1 -> 'err2) (x: Traced<'err1>) =
        { error = f x.error
          trace = x.trace }

    /// Create a `StackTrace` for the given `err`
    let trace err =
        { error = err
          trace = StackTrace(true) }

type Reader<'a, 'r> = 'r -> 'a

module Reader =
    let map f (x: Reader<_, _>) : Reader<_, _> = x >> f
    let withReader f (x: Reader<_, _>) : Reader<_, _> = f >> x

    let retn x : Reader<_, _> = fun _ -> x
    let join (x: Reader<Reader<_, _>, _>) : Reader<_, _> =
        fun r -> (x r) r
    let bind (f: _ -> Reader<_, _>) (x: Reader<_, _>) : Reader<_, _> = x |> map f |> join
    let ask: Reader<_, _> = id
    let asks (f: 'r -> 'b) : Reader<_, _> = f
    let local (f: 'r -> 'r) x = withReader f x

/// The One True Monad; encapsulates the following effects:
/// - `Reader`
/// - `Async`
/// - `Result` with a `Traced` error component
type OTM<'a, 'r, 'err> = Reader<Async<Result<'a, Traced<'err>>>, 'r>

[<RequireQualifiedAccess>]
module OTM =

    /// Functor `map`
    let map (f: 'a -> 'b) (x: OTM<'a, 'r, 'err>) : OTM<'b, 'r, 'err> =
        x |> Reader.map (AsyncResult.map f)

    /// `map` for the `'err` type
    let mapError f (x: OTM<'a, 'r, 'err1>) : OTM<'a, 'r, 'err2> =
        x |> Reader.map (AsyncResult.mapError (Traced.map f))

    /// `map` over the `Ok` and `Error` cases respectively
    let bimap onSuccess onError x =
        x
        |> map onSuccess
        |> mapError onError

    // various Reader functions

    /// Reader `run` lifted to `OTM`
    let run r (x: OTM<_, _, _>) = x r

    /// Reader `ask` lifted to `OTM`
    let ask : OTM<'r, 'r, 'err> = AsyncResult.retn

    /// Reader `asks` lifted to `OTM`
    let asks (f: 'r -> 'b) : OTM<_, _, 'err> = f >> AsyncResult.retn

    /// contravariant `map` of the `'r` type, lifted from `Reader`
    let withReader f (x: OTM<'a, 'r2, 'err>) : OTM<'a, 'r1, 'err> =
        x |> Reader.withReader f

    /// Reader `local` lifted to `OTM`
    let local f (x: OTM<_, 'r, 'err>) : OTM<_, 'r, 'err> = Reader.local f x

    /// Monadic `return`
    let retn (x: 'a) : OTM<'a, 'r, 'err> = x |> AsyncResult.retn |> Reader.retn

    /// Monadic `bind`
    let bind (f: 'a -> OTM<'b, 'r, 'err>) (x: OTM<'a, 'r, 'err>) : OTM<'b, 'r, 'err> =
        fun r ->
            async {
                match! x r with
                | Ok x -> return! (f x) r
                | Error e -> return Error e
            }

    /// `bind`, but for the `'err` component; see also `OTM.handle`
    let bindError (f: 'err1 -> OTM<'a, 'r, 'err2>) (x: OTM<'a, 'r, 'err1>) : OTM<'a, 'r, 'err2> =
        fun r ->
            async {
                match! x r with
                | Ok x' -> return Ok x'
                | Error tracedError -> return! (f tracedError.error) r
            }

    /// Applicative `apply`
    let apply f x : OTM<_, 'r, 'err> = f |> bind (fun f' -> x |> bind (fun x' -> f' x' |> retn))

    /// Applicative `lift2`
    let lift2 f x y : OTM<_, 'r, 'err> =
        let (<*>) = apply
        let (<!>) = map
        f <!> x <*> y

    /// Applicative `lift3`
    let lift3 f x y z : OTM<_, 'r, 'err> =
        let (<*>) = apply
        let (<!>) = map
        f <!> x <*> y <*> z

    /// Applicative `lift3`
    let lift4 f x y z w : OTM<_, 'r, 'err> =
        let (<*>) = apply
        let (<!>) = map
        f <!> x <*> y <*> z <*> w

    /// Applicative `traverse`, specialized to lists
    let traverse f xs =
        let (<*>) = apply
        let (<!>) = map
        let cons head tail = head :: tail
        let consM head tailM = cons <!> f head <*> tailM
        let x0 = retn []
        List.foldBack consM xs x0

    /// Applicative `sequence`, specialized to lists
    let sequence xs : OTM<_, 'r, 'err> = traverse id xs

    /// `ignore` lifted to `OTM`
    let ignore (x: OTM<_, 'r, 'err>) : OTM<_, 'r, 'err> = x |> map ignore

    /// Catch an exception if raised in an `OTM` and map it to an `'err`
    let catch f (x: OTM<_, 'r, 'err>) : OTM<_, 'r, 'err> =
        fun r -> AsyncResult.catch (f >> Traced.trace) (x r)

    // various lifting functions

    /// Alias for `OTM.retn`
    let ofSuccess x : OTM<_, 'r, 'err> = retn x

    /// Lift an `'err` into an `OTM`, creating a `StackTrace` for it at the call site
    /// of this function
    let throw x : OTM<_, 'r, 'err> = x |> Traced.trace |> Result.Error |> Async.retn |> Reader.retn

    /// Lift an already `Traced<'err>` into an `OTM`
    let rethrow x : OTM<_, 'r, 'err> = x |> Result.Error |> Async.retn |> Reader.retn

    /// Lift an `Async` computation into an `OTM`
    let ofAsync x : OTM<_, 'r, 'err> = fun _ -> x |> Async.map Result.Ok

    /// Obtain an OTM containing the Async portion of the computation
    let asAsync (x: OTM<_, 'r, 'err>) : OTM<AsyncResult<'a, Traced<'err>>, 'r, 'err> =
        fun r ->
            r |> retn (x r)

    /// Obtain an OTM containing the Result portion of the computation. Useful
    /// for `match!`-ing inside an `otm` block:
    ///
    /// ```
    /// otm {
    ///     match! x |> OTM.asResult with
    ///     | Ok x' -> return f x'
    ///     | Error e -> return! g e
    /// }
    /// ```
    let asResult (x: OTM<_, 'r, 'err>) : OTM<Result<'a, Traced<'err>>, 'r, 'err> =
        fun r ->
            async {
                let! res = x r
                return! (retn res) r
            }

    /// `Async.Sleep : int -> Async<unit>` lifted into an `OTM`
    let sleep (ms: int) : OTM<_, 'r, 'err> = Async.Sleep ms |> ofAsync

    /// Alias for `OTM.bindError`; handle an error in the same manner of a `try ...
    /// with` block:
    ///
    /// ```
    /// try
    ///     <expression>
    /// with
    ///     | ExceptionType1 _ as e1 -> f e1
    ///     | ExceptionType2 _ as e2 -> g e2
    ///     | e -> h e
    /// ```
    ///
    /// becomes
    ///
    /// ```
    /// <expression>
    /// |> OTM.handle
    ///     (function
    ///      | ErrorType1 _ as e1 -> f e1
    ///      | ErrorType2 _ as e2 -> g e2
    ///      | e -> h e)
    /// ```
    let handle f (x: OTM<_, 'r, 'err1>) : OTM<_, 'r, 'err2> = bindError f x

    /// Like `OTM.handle`, but with access to the `StackTrace` of the `'err`
    let handleTraced (f: Traced<'err1> -> OTM<_, _, _>) (x: OTM<_, 'r, 'err1>) : OTM<_, 'r, 'err2> =
        fun r ->
            async {
                match! x r with
                | Ok x' -> return Ok x'
                | Error tracedError -> return! f tracedError r
            }

    module Operators =
        /// bind
        let (>>=) x f : OTM<_, 'r, 'err> = x |> bind f
        /// flip bind
        let (=<<) f x : OTM<_, 'r, 'err> = x |> bind f
        /// Kliesli composition
        let (>=>) f g : _ -> OTM<_, 'r, 'err> = fun x -> x >>= f >>= g
        /// flip Kliesli
        let (<=<) g f : _ -> OTM<_, 'r, 'err> = fun x -> x >>= f >>= g
        /// map
        let (<!>) f x : OTM<_, 'r, 'err> = x |> map f
        /// apply
        let (<*>) f x : OTM<_, 'r, 'err> = x |> apply f

[<AutoOpen>]
module OTMComputationExpression =

    type OTMBuilder() =
        member __.Return(x) = OTM.retn x
        member __.Bind(x, f) = x |> OTM.bind f
        member __.ReturnFrom(x) = x

        member this.Zero() = this.Return ()

        member __.Delay(f) = f
        member __.Run(f) = f ()

        member this.While(guard, body) =
            if not (guard())
            then this.Zero()
            else this.Bind( body(), fun () ->
                this.While(guard, body))

        member this.TryWith(body, handler) =
            async {
                try return! this.ReturnFrom(body())
                with e -> return! handler e
            }

        member this.TryFinally(body, compensation) =
            async {
                try return! this.ReturnFrom(body())
                finally compensation()
            }

        member this.Using(disposable: #System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () ->
                match disposable with
                    | null -> ()
                    | disp -> disp.Dispose())

        member this.For(sequence:seq<_>, body: 'e -> OTM<_, 'r, 'err>) =
                this.Using(sequence.GetEnumerator(),fun enum ->
                    async {
                        return
                            this.While(enum.MoveNext,
                                this.Delay(fun () -> body enum.Current))
                    })

        member this.Combine (a,b) =
            this.Bind(a, fun () -> b())

    let otm = OTMBuilder()

module Example =
    type Error = | TooLowBadness of int

    type Config =
        { lowest: int }
        static member lowest_ config = config.lowest

    type ReaderState =
        { logger: ILogger
          config: Config }
        static member logger_ r = r.logger
        static member config_ r = r.config

    let ofInt x = otm {
        let! lowest = OTM.asks (ReaderState.config_ >> Config.lowest_)
        return! if x < lowest then OTM.throw (TooLowBadness lowest) else OTM.retn x
    }

    let incr x = x |> OTM.map (fun y -> y + 1)

    let decr x =
        otm {
            let! x' = x |> OTM.map (fun y -> y - 1)
            return! ofInt x'
        }

    let zero = 0 |> OTM.ofSuccess

    let three = zero |> incr |> incr |> incr

    let negativeOne = zero |> decr

    let logError (x: OTM<_, ReaderState, 'err>) =
        otm {
            let! logger = OTM.asks ReaderState.logger_
            return!
                x |> OTM.handleTraced (fun e ->
                    logger.Error("An error has occurred: {Error}\n    with StackTrace:\n{StackTrace}", e.error, e.trace)
                    OTM.rethrow e)
        }

    let logSuccess (x: OTM<int, ReaderState, 'err>): OTM<_, _, _> =
        otm {
            let! x' = x
            let! logger = OTM.asks ReaderState.logger_
            logger.Information("Success! The value is: {Value}", x')
            return x'
        }

    let main state =
        otm {
            let! x = three
            let! y = negativeOne
            return x + y
        }
        |> logError
        |> logSuccess
        |> OTM.run state
        |> Async.RunSynchronously

module Main =
    open Example
    open Microsoft.Extensions.Configuration
    open Microsoft.Extensions.Configuration.Json

    [<EntryPoint>]
    let main _ =
        let configRoot =
            ConfigurationBuilder()
                .AddJsonFile("appsettings.json")
                .Build()

        let config = { lowest = configRoot.["Lowest"] |> int }

        let logger = LoggerConfiguration().WriteTo.Console().CreateLogger()

        let state =
            { logger = logger
              config = config }

        do Example.main state |> ignore
        0
