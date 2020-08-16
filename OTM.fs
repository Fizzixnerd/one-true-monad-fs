namespace OTM

// Partially stolen from https://github.com/swlaschin/DomainModelingMadeFunctional/blob/master/src/OrderTaking/Result.fs from AsyncResult

open System.Diagnostics

[<RequireQualifiedAccess>]
module Async =

    let retn x = async.Return(x)
    let bind f x = async.Bind(x, f)
    let map f x = x |> bind (f >> retn)

/// An `error` along with a `StackTrace`
type Traced<'err> =
    { error: 'err
      trace: StackTrace }

[<RequireQualifiedAccess>]
module Traced =

    /// `map` over the `'err`
    let map (f: 'err1 -> 'err2) (x: Traced<'err1>) =
        { error = f x.error
          trace = x.trace }

    /// Create a `StackTrace` for the given `err`
    let trace err =
        let withSourceInfo =
            #if DEBUG
            true
            #else
            false
            #endif

        { error = err
          trace = StackTrace(withSourceInfo) }

type Reader<'a, 'r> = 'r -> 'a

[<RequireQualifiedAccess>]
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

    let run r (x: Reader<_, _>) = x r

/// The One True Monad; encapsulates the following effects:
/// - `Reader`
/// - `Async`
/// - `Result` with a `Traced` error component
type OTM<'a, 'r, 'err> = Reader<Async<Result<'a, Traced<'err>>>, 'r>

[<RequireQualifiedAccess>]
module OTM =

    /// Functor `map`
    let map (f: 'a -> 'b) (x: OTM<'a, 'r, 'err>) : OTM<'b, 'r, 'err> =
        x |> Reader.map (Async.map (Result.map f))

    /// `map` for the `'err` type
    let mapError f (x: OTM<'a, 'r, 'err1>) : OTM<'a, 'r, 'err2> =
        x |> Reader.map (Async.map (Result.mapError (Traced.map f)))

    /// `map` over the `Ok` and `Error` cases respectively
    let bimap onSuccess onError x =
        x
        |> map onSuccess
        |> mapError onError

    // various Reader functions

    /// Reader `run` lifted to `OTM`
    let run r (x: OTM<_, _, _>) = Reader.run r x

    /// Reader `run >> Async.RunSynchronously` lifted to `OTM`
    let runSynchronously r x = run r x |> Async.RunSynchronously

    /// Reader `ask` lifted to `OTM`
    let ask : OTM<'r, 'r, 'err> = fun x -> x |> Result.Ok |> Async.retn

    /// Reader `asks` lifted to `OTM`
    let asks (f: 'r -> 'b) : OTM<_, _, 'err> = f >> Result.Ok >> Async.retn

    /// contravariant `map` of the `'r` type, lifted from `Reader` into `OTM`
    let withReader f (x: OTM<'a, 'r2, 'err>) : OTM<'a, 'r1, 'err> =
        x |> Reader.withReader f

    /// Reader `local` lifted to `OTM`
    let local f (x: OTM<_, 'r, 'err>) : OTM<_, 'r, 'err> = Reader.local f x

    /// Monadic `return`
    let retn (x: 'a) : OTM<'a, 'r, 'err> = x |> Result.Ok |> Async.retn |> Reader.retn

    /// Monadic `bind`
    let bind (f: 'a -> OTM<'b, 'r, 'err>) (x: OTM<'a, 'r, 'err>) : OTM<'b, 'r, 'err> =
        fun r ->
            async {
                match! x r with
                | Ok x -> return! (f x) r
                | Error e -> return Error e
            }

    /// Monadic `join`
    let join (x: OTM<_, 'r, 'err>) = x |> bind id

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

    /// Catch an exception if raised in an `OTM` and map it to an `'err`, preserving the `StackTrace`
    let catch f (x: OTM<_, 'r, 'err>) : OTM<_, 'r, 'err> =
        fun r ->
            x r
            |> Async.Catch
            |> Async.map
                (function
                 | Choice1Of2 x -> Ok x
                 | Choice2Of2 e ->
                     Error
                         { error = f e
                           trace = StackTrace(e) })

    // various lifting functions

    /// Lift an `'err` into an `OTM`, creating a `StackTrace` for it at the call site
    /// of this function
    let throw x : OTM<_, 'r, 'err> = x |> Traced.trace |> Result.Error |> Async.retn |> Reader.retn

    /// Lift an already `Traced<'err>` into an `OTM`, leaving the `StackTrace` as-is
    let rethrow x : OTM<_, 'r, 'err> = x |> Result.Error |> Async.retn |> Reader.retn

    /// Lift an `Async` computation into an `OTM`
    let ofAsync x : OTM<_, 'r, 'err> = fun _ -> x |> Async.map Result.Ok

    /// Obtain an OTM containing the Async portion of the computation
    let asAsync (x: OTM<_, 'r, 'err>) : OTM<Async<Result<'a, Traced<'err>>>, 'r, 'err> =
        fun r ->
            r |> retn (x r)

    /// Lift a `Result<'a, 'err>` into an `OTM`, creating a stack trace if it is
    /// a `Result.Error`
    let ofResult (x: Result<_, 'err>) : OTM<_, 'r, 'err> =
        fun _ -> x |> Result.mapError Traced.trace |> Async.retn

    let asResult (x: OTM<_, 'r, 'err>) : OTM<Result<'a, 'err>, 'r, 'err> =
        fun r ->
            async {
                let! res = x r
                return! (retn (res |> Result.mapError (fun x -> x.error))) r
            }

    /// Lift a `Result<'a, Traced<'err>` into an `OTM`, leaving the `StackTrace`
    /// as-is
    let ofTracedResult (x: Result<_, Traced<'err>>) : OTM<_, 'r, 'err> =
        fun _ -> x |> Async.retn

    /// Obtain an `OTM` containing the `Result` portion of the computation,
    /// including the `Traced` portion of the `'err`. Useful for `match!`-ing
    /// inside an `otm` block:
    ///
    /// ```
    /// otm {
    ///     match! x |> OTM.asTracedResult with
    ///     | Ok x' -> return f x'
    ///     | Error e -> return! g e
    /// }
    /// ```
    let asTracedResult (x: OTM<_, 'r, 'err>) : OTM<Result<'a, Traced<'err>>, 'r, 'err> =
        fun r ->
            async {
                let! res = x r
                return! (retn res) r
            }

    /// `Async.Sleep : int -> Async<unit>` lifted into an `OTM`
    let sleep (ms: int) : OTM<_, 'r, 'err> = Async.Sleep ms |> ofAsync

    /// Handle an error in the same manner of a `try ...
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
    let handle f (x: OTM<_, 'r, 'err1>) : OTM<_, 'r, 'err2> =
        fun r ->
            async {
                match! x r with
                | Ok x' -> return Ok x'
                | Error tracedError -> return! (f tracedError.error) r
            }

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

        // TODO: Is this even correct?
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

    open Serilog

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

    let zero = 0 |> ofInt

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
            let! logger = OTM.asks ReaderState.logger_
            let! x' = x
            logger.Information("Success! The value is: {Value}", x')
            return x'
        }

    let main =
        otm {
            let! x = three
            let! y = negativeOne
            return x + y
        }
        |> logError
        |> logSuccess

module Main =

    open Example
    open Serilog
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

        Example.main
        |> OTM.runSynchronously state
        |> ignore
        0
