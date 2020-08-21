namespace Otm

open System.Diagnostics
open System.Runtime.CompilerServices

[<RequireQualifiedAccess>]
module Async =

    let inline retn x = async.Return(x)
    let inline bind f x = async.Bind(x, f)
    let inline map f x = x |> bind (f >> retn)

module Result =
    /// Analogous to Applicative `traverse`, but taking an `Array` and returning
    /// a `List`
    let traverseAtoL f (xs: _ array) =
        let (<!>) = Result.map
        let (<*>) f x =
            match f, x with
            | Ok f', Ok x' -> Ok (f' x')
            | Error e, Ok _ -> Error e
            | Ok _, Error e -> Error e
            | Error e, Error _ -> Error e
        let cons y ys = y :: ys
        let consM y ys = cons <!> f y <*> ys
        let x0 = Ok []
        Array.foldBack consM xs x0

    /// Analogous to Applicative `sequence`, but taking an `Array` and returning
    /// a `List`
    let sequenceAtoL xs = traverseAtoL id xs

type Frame = string
type SymbolicStackTrace = SymbolicStackTrace of Frame list

module SymbolicStackTrace =
    let cons frame (SymbolicStackTrace sst) = SymbolicStackTrace (frame :: sst)
    let rev (SymbolicStackTrace sst) = SymbolicStackTrace (List.rev sst)

/// An `error` along with a `StackTrace`
type Traced<'err> =
    { error: 'err
      trace: StackTrace
      strace: SymbolicStackTrace }

[<RequireQualifiedAccess>]
module Traced =

    /// A smart variable that uses the current build settings to determine if
    /// StackTraces should be made with full source information
    let withSourceInfo =
        #if DEBUG
        true
        #else
        false
        #endif

    /// `map` over the `'err`
    let map (f: 'err1 -> 'err2) (x: Traced<'err1>) =
        { error = f x.error
          trace = x.trace
          strace = x.strace }

    /// Create a `StackTrace` for the given `err` and return it in a
    /// `Traced<'err>`
    let trace err : Traced<'err> =
        { error = err
          trace = StackTrace(1, withSourceInfo)
          strace = SymbolicStackTrace [] }

    /// Obtain the `'err` component of a Traced<'err>
    let untrace err : 'err = err.error

    let addFrame (f: Frame) (t: Traced<'a>) =
        { t with
             strace = SymbolicStackTrace.cons f t.strace }

/// The Reader monad
type Reader<'a, 'r> = ('r -> 'a)

[<RequireQualifiedAccess>]
module Reader =

    let inline run r (x: Reader<'a, 'r>) = x r

    let inline map f x : Reader<_, 'r> = x >> f
    let inline withReader f x : Reader<_, _> = f >> x

    let inline retn x : Reader<_, _> = fun _ -> x
    let inline join (x: Reader<Reader<_, 'r>, 'r>) : Reader<'a, 'r> =
        fun r -> run r (run r x)
    let inline bind (f: _ -> Reader<_, 'r>) (x: Reader<_, _>) : Reader<_, _> = x |> map f |> join

    let ask : Reader<'r, 'r> = id
    let inline asks (f: 'r -> 'b) : Reader<_, _> = f
    let inline local (f: 'r -> 'r) x = withReader f x


/// The One True Monad; encapsulates the following effects:
/// - `Reader`
/// - `Async`
/// - `Result` with a `Traced` error component
type Otm<'a, 'r, 'err> = Reader<Async<Result<'a, Traced<'err>>>, 'r>

[<RequireQualifiedAccess>]
module Otm =

    /// Functor `map`
    let inline map (f: 'a -> 'b) (x: Otm<'a, 'r, 'err>) : Otm<'b, 'r, 'err> =
        x |> Reader.map (Async.map (Result.map f))

    /// `map` for the `'err` type
    let inline mapError f (x: Otm<'a, 'r, 'err1>) : Otm<'a, 'r, 'err2> =
        x |> Reader.map (Async.map (Result.mapError (Traced.map f)))

    /// `map` over the `Ok` and `Error` cases respectively
    let bimap onSuccess onError x =
        x
        |> map onSuccess
        |> mapError onError

    // various Reader functions

    /// Reader `run` lifted to `Otm`
    let inline run r (x: Otm<_, _, _>) = Reader.run r x

    let addFrame (f: Frame) (x: Otm<_, 'r, 'err>) : Otm<_, 'r, 'err> =
        fun r ->
            async {
                match! run r x with
                | Ok x -> return Ok x
                | Error e -> return Error <| Traced.addFrame f e
            }

    /// Functor `map`

    /// Reader `run >> Async.RunSynchronously` lifted to `Otm`
    let runSynchronously r x = run r x |> Async.RunSynchronously

    /// `Reader.map` for `Otm`s
    let inline readerMap f (x: Otm<_, 'r, 'err>) = Reader.map f x

    /// Reader `ask` lifted to `Otm`
    let ask: Otm<'r, 'r, 'err> = fun x -> x |> Result.Ok |> Async.retn

    /// Reader `asks` lifted to `Otm`
    let inline asks (f: 'r -> 'b) : Otm<_, _, 'err> = f >> Result.Ok >> Async.retn

    /// contravariant `map` of the `'r` type, lifted from `Reader` into `Otm`
    let withReader f (x: Otm<'a, 'r2, 'err>) : Otm<'a, 'r1, 'err> =
        x |> Reader.withReader f

    /// Reader `local` lifted to `Otm`
    let local f (x: Otm<_, 'r, 'err>) : Otm<_, 'r, 'err> = Reader.local f x

    /// Monadic `return`
    let inline retn (x: 'a) : Otm<'a, 'r, 'err> =
        async {
            return x |> Result.Ok
        }
        |> Reader.retn

    /// Monadic `bind`
    let inline bind (f: 'a -> Otm<'b, 'r, 'err>) (x: Otm<'a, 'r, 'err>) : Otm<'b, 'r, 'err> =
        fun r ->
            run r x
            |> Async.bind (function
                | Ok a -> run r (f a)
                | Error e -> Async.retn <| Error e)

    /// Monadic `join`
    let inline join (x: Otm<_, 'r, 'err>) = x |> bind id

    /// Applicative `apply`
    let inline apply f x : Otm<_, 'r, 'err> = f |> bind (fun f' -> x |> bind (fun x' -> f' x' |> retn))

    /// Applicative `lift2`
    let lift2 f x y : Otm<_, 'r, 'err> =
        let (<*>) = apply
        let (<!>) = map
        f <!> x <*> y

    /// Applicative `lift3`
    let lift3 f x y z : Otm<_, 'r, 'err> =
        let (<*>) = apply
        let (<!>) = map
        f <!> x <*> y <*> z

    /// Applicative `lift3`
    let lift4 f x y z w : Otm<_, 'r, 'err> =
        let (<*>) = apply
        let (<!>) = map
        f <!> x <*> y <*> z <*> w

    /// Applicative `traverse`, specialized to lists
    let traverse (f: 'a -> Otm<'b, 'r, 'err>) xs =
        let (<*>) = apply
        let (<!>) = map
        let cons head tail = head :: tail
        let consM head tailM = cons <!> f head <*> tailM
        let x0 = retn []
        List.foldBack consM xs x0

    /// Applicative `sequence`, specialized to lists
    let sequence xs : Otm<_, 'r, 'err> = traverse id xs

    /// Similar to `Async.Parallel`, but for `Otm`; note the use of `array` and
    /// `list` in the signature
    ///
    /// Always O(n) because of the need to `sequence` the `Result`s, but
    /// performs the actual `Otm` computations in parallel
    let parallel' (xs: Otm<_, 'r, 'err> array) : Otm<_ list, 'r, 'err> =
        fun r ->
            xs
            |> Array.Parallel.map (run r)
            |> Async.Parallel
            |> Async.map Result.sequenceAtoL

    /// `ignore` lifted to `Otm`
    let ignore (x: Otm<_, 'r, 'err>) : Otm<_, 'r, 'err> = x |> map ignore

    /// Catch an exception if raised in an `Otm` and map it to an `'err`, preserving the `StackTrace`
    let catch f (x: Otm<_, 'r, 'err>) : Otm<_, 'r, 'err> =
        fun r ->
            run r x
            |> Async.Catch
            |> Async.map
                (function
                 | Choice1Of2 x -> Ok x
                 | Choice2Of2 e ->
                     Error
                         { error = f e
                           trace = StackTrace(e, Traced.withSourceInfo)
                           strace = SymbolicStackTrace [] })

    // various lifting functions

    /// Lift an `'err` into an `Otm`, creating a `StackTrace` for it at the call site
    /// of this function
    let throw x : Otm<_, 'r, 'err> = x |> Traced.trace |> Result.Error |> Async.retn |> Reader.retn

    /// Lift an already `Traced<'err>` into an `Otm`, leaving the `StackTrace` as-is
    let rethrow x : Otm<_, 'r, 'err> = x |> Result.Error |> Async.retn |> Reader.retn

    /// Lift an `Async` computation into an `Otm`
    let ofAsync x : Otm<_, 'r, 'err> = fun _ -> x |> Async.map Result.Ok

    /// Obtain an Otm containing the Async portion of the computation
    let asAsync (x: Otm<_, 'r, 'err>) : Otm<Async<Result<'a, Traced<'err>>>, 'r, 'err> =
        fun r -> run r (retn (run r x))

    /// Lift a `Result<'a, 'err>` into an `Otm`, creating a stack trace if it is
    /// a `Result.Error`
    let ofResult (x: Result<_, 'err>) : Otm<_, 'r, 'err> =
        fun _ -> x |> Result.mapError Traced.trace |> Async.retn

    /// Obtain an `Otm` containing the `Result` portion of the computation, sans
    /// the `'err`'s `StackTrace`
    let asResult (x: Otm<_, 'r, 'err>) : Otm<Result<'a, 'err>, 'r, 'err> =
        fun r ->
            async {
                let! res = run r x
                return! run r (retn (res |> Result.mapError (fun x -> x.error)))
            }

    /// Lift an untraced `Async<Result<'a, 'err>>` into an `Otm`, forming a
    /// StackTrace at the callsite of this function if in the Error case.
    let ofAsyncResult (x: Async<Result<_, 'err>>) : Otm<_, 'r, 'err> =
        fun _ -> x |> Async.map (Result.mapError Traced.trace)

    /// Obtain an `Otm` containing the `Async Result` portion of the
    /// computation, without the StackTrace.
    let asAsyncResult (x : Otm<_, 'r, 'err>) =
        fun r ->
            x
            |> run r
            |> Async.map (Result.mapError (fun te -> te.error))
            |> retn
            |> run r

    /// Lift a `Result<'a, Traced<'err>` into an `Otm`, leaving the `StackTrace`
    /// as-is
    let ofTracedResult (x: Result<_, Traced<'err>>) : Otm<_, 'r, 'err> =
        fun _ -> x |> Async.retn

    /// Obtain an `Otm` containing the `Result` portion of the computation,
    /// including the `Traced` portion of the `'err`. Useful for `match!`-ing
    /// inside an `otm` block:
    ///
    /// ```
    /// otm {
    ///     match! x |> Otm.asTracedResult with
    ///     | Ok x' -> return f x'
    ///     | Error e -> printf "Error: %O with StackTrace %O" e.error e.trace
    /// }
    /// ```
    let asTracedResult (x: Otm<_, 'r, 'err>) : Otm<Result<'a, Traced<'err>>, 'r, 'err> =
        fun r ->
            async {
                let! res = run r x
                return! run r (retn res)
            }

    /// `Async.Sleep : int -> Async<unit>` lifted into an `Otm`
    let sleep (ms: int) : Otm<_, 'r, 'err> = Async.Sleep ms |> ofAsync

    /// Handle an error in the same manner of a `try ... with` block:
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
    /// |> Otm.handle
    ///     (function
    ///      | ErrorType1 _ as e1 -> f e1
    ///      | ErrorType2 _ as e2 -> g e2
    ///      | e -> h e)
    /// ```
    let bindError f (x: Otm<_, 'r, 'err1>) : Otm<_, 'r, 'err2> =
        fun r ->
            async {
                match! run r x with
                | Ok x' -> return Ok x'
                | Error tracedError -> return! run r (f tracedError.error)
            }

    /// Like `Otm.bindError`, but with access to the `StackTrace` of the `'err`
    let bindTracedError (f: Traced<'err1> -> Otm<_, _, _>) (x: Otm<_, 'r, 'err1>) : Otm<_, 'r, 'err2> =
        fun r ->
            async {
                match! run r x with
                | Ok x' -> return Ok x'
                | Error tracedError -> return! run r (f tracedError)
            }

    module Operators =

        /// bind
        let inline (>>=) x f : Otm<_, 'r, 'err> = x |> bind f
        /// flip bind
        let inline (=<<) f x : Otm<_, 'r, 'err> = x |> bind f
        /// Kliesli composition
        let inline (>=>) f g : _ -> Otm<_, 'r, 'err> = fun x -> f x >>= g
        /// flip Kliesli
        let inline (<=<) g f : _ -> Otm<_, 'r, 'err> = fun x -> f x >>= g
        /// map
        let inline (<!>) f x : Otm<_, 'r, 'err> = x |> map f
        /// apply
        let inline (<*>) f x : Otm<_, 'r, 'err> = x |> apply f

module Utils =
    let inline orUnknown x = x |> Option.defaultValue "<unknown>"

[<AutoOpen>]
module OtmComputationExpression =

    type OtmBuilder() =
        member inline __.Return
            (x,
            [<CallerMemberName>]?callerName: string,
            [<CallerFilePath>]?callerFilePath: string,
            [<CallerLineNumber>]?callerLineNumber: int) =
            x
            |> Otm.retn
            // |> Otm.addFrame (sprintf "%s in %s at %s" (callerName |> Utils.orUnknown) (callerFilePath |> Utils.orUnknown) (callerLineNumber |> Option.map string |> Utils.orUnknown))

        member inline __.Bind
            (x: Otm<_, 'r, 'err>,
             f: _ -> Otm<_, 'r, 'err>,
             [<CallerMemberName>]?callerName: string,
             [<CallerFilePath>]?callerFilePath: string,
             [<CallerLineNumber>]?callerLineNumber: int) =
            x
            |> Otm.bind f
            |> Otm.addFrame (sprintf "%s in %s at %s" (callerName |> Utils.orUnknown) (callerFilePath |> Utils.orUnknown) (callerLineNumber |> Option.map string |> Utils.orUnknown))

        member inline __.ReturnFrom
            (x: Otm<_, 'r, 'err>,
             [<CallerMemberName>]?callerName: string,
             [<CallerFilePath>]?callerFilePath: string,
             [<CallerLineNumber>]?callerLineNumber: int) =
            x
            |> Otm.addFrame (sprintf "%s in %s at %s" (callerName |> Utils.orUnknown) (callerFilePath |> Utils.orUnknown) (callerLineNumber |> Option.map string |> Utils.orUnknown))

        member this.Zero() = this.Return(())

        member __.Delay(f) = fun r -> Otm.run r (f ())

        member this.While(guard, body) =
            if not (guard())
            then this.Zero()
            else this.Bind( body(), fun () ->
                this.While(guard, body))

        member this.TryWith(body, handler) =
            fun r ->
                async {
                    try return! this.ReturnFrom(body()) |> Otm.run r
                    with e -> return! handler e |> Otm.run r
                }

        member this.TryFinally(body, compensation) : Otm<_, 'r, 'err> =
            fun r ->
                async {
                    try return! this.ReturnFrom(body()) |> Otm.run r
                    finally compensation()
                }

        member this.Using(disposable: #System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () ->
                match disposable with
                | null -> ()
                | disp -> disp.Dispose())

        member this.For(sequence:seq<_>, body: 'e -> Otm<_, 'r, 'err>) =
            this.Using(sequence.GetEnumerator(),fun enum ->
                this.While(enum.MoveNext,
                    fun () -> this.Delay(fun () -> body enum.Current)))

        member this.Combine(a,b) =
            this.Bind(a, fun () -> b)

    /// Builds a Otm using computation expression syntax
    let otm = OtmBuilder()
