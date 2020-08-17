namespace OTM

// Partially stolen from https://github.com/swlaschin/DomainModelingMadeFunctional/blob/master/src/OrderTaking/Result.fs from AsyncResult

open System.Diagnostics

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

/// An `error` along with a `StackTrace`
type Traced<'err> =
    { error: 'err
      trace: StackTrace }

[<RequireQualifiedAccess>]
module Traced =

    let withSourceInfo =
        #if DEBUG
        true
        #else
        false
        #endif

    /// `map` over the `'err`
    let map (f: 'err1 -> 'err2) (x: Traced<'err1>) =
        { error = f x.error
          trace = x.trace }

    /// Create a `StackTrace` for the given `err` and return it in a
    /// `Traced<'err>`
    let trace err : Traced<'err> =
        { error = err
          trace = StackTrace(withSourceInfo) }

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
type OTM<'a, 'r, 'err> = Reader<Async<Result<'a, Traced<'err>>>, 'r>

[<RequireQualifiedAccess>]
module OTM =

    /// Functor `map`
    let inline map (f: 'a -> 'b) (x: OTM<'a, 'r, 'err>) : OTM<'b, 'r, 'err> =
        x |> Reader.map (Async.map (Result.map f))

    /// `map` for the `'err` type
    let inline mapError f (x: OTM<'a, 'r, 'err1>) : OTM<'a, 'r, 'err2> =
        x |> Reader.map (Async.map (Result.mapError (Traced.map f)))

    /// `map` over the `Ok` and `Error` cases respectively
    let bimap onSuccess onError x =
        x
        |> map onSuccess
        |> mapError onError

    // various Reader functions

    /// Reader `run` lifted to `OTM`
    let inline run r (x: OTM<_, _, _>) = Reader.run r x

    /// Reader `run >> Async.RunSynchronously` lifted to `OTM`
    let runSynchronously r x = run r x |> Async.RunSynchronously

    /// Reader `ask` lifted to `OTM`
    let ask: OTM<'r, 'r, 'err> = fun x -> x |> Result.Ok |> Async.retn

    /// Reader `asks` lifted to `OTM`
    let inline asks (f: 'r -> 'b) : OTM<_, _, 'err> = f >> Result.Ok >> Async.retn

    /// contravariant `map` of the `'r` type, lifted from `Reader` into `OTM`
    let withReader f (x: OTM<'a, 'r2, 'err>) : OTM<'a, 'r1, 'err> =
        x |> Reader.withReader f

    /// Reader `local` lifted to `OTM`
    let local f (x: OTM<_, 'r, 'err>) : OTM<_, 'r, 'err> = Reader.local f x

    /// Monadic `return`
    let inline retn (x: 'a) : OTM<'a, 'r, 'err> = x |> Result.Ok |> Async.retn |> Reader.retn

    /// Monadic `bind`
    let inline bind (f: 'a -> OTM<'b, 'r, 'err>) (x: OTM<'a, 'r, 'err>) : OTM<'b, 'r, 'err> =
        fun r ->
            async {
                match! run r x with
                | Ok x -> return! run r (f x)
                | Error e -> return Error e
            }

    /// Monadic `join`
    let inline join (x: OTM<_, 'r, 'err>) = x |> bind id

    /// Applicative `apply`
    let inline apply f x : OTM<_, 'r, 'err> = f |> bind (fun f' -> x |> bind (fun x' -> f' x' |> retn))

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

    /// Similar to `Async.Parallel`, but for `OTM`; note the use of `array` and
    /// `list` in the signature
    ///
    /// Always O(n) because of the need to `sequence` the `Result`s, but
    /// performs the actual `OTM` computations in parallel
    let parallel' (xs: OTM<_, 'r, 'err> array) : OTM<_ list, 'r, 'err> =
        fun r ->
            xs
            |> Array.Parallel.map (fun x -> run r x)
            |> Async.Parallel
            |> Async.map Result.sequenceAtoL

    /// `ignore` lifted to `OTM`
    let ignore (x: OTM<_, 'r, 'err>) : OTM<_, 'r, 'err> = x |> map ignore

    /// Catch an exception if raised in an `OTM` and map it to an `'err`, preserving the `StackTrace`
    let catch f (x: OTM<_, 'r, 'err>) : OTM<_, 'r, 'err> =
        fun r ->
            run r x
            |> Async.Catch
            |> Async.map
                (function
                 | Choice1Of2 x -> Ok x
                 | Choice2Of2 e ->
                     Error
                         { error = f e
                           trace = StackTrace(e, Traced.withSourceInfo) })

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
        fun r -> run r (retn (run r x))

    /// Lift a `Result<'a, 'err>` into an `OTM`, creating a stack trace if it is
    /// a `Result.Error`
    let ofResult (x: Result<_, 'err>) : OTM<_, 'r, 'err> =
        fun _ -> x |> Result.mapError Traced.trace |> Async.retn

    /// Obtain an `OTM` containing the `Result` portion of the computation, sans
    /// the `'err`'s `StackTrace`
    let asResult (x: OTM<_, 'r, 'err>) : OTM<Result<'a, 'err>, 'r, 'err> =
        fun r ->
            async {
                let! res = run r x
                return! run r (retn (res |> Result.mapError (fun x -> x.error)))
            }

    /// Lift an untraced `Async<Result<'a, 'err>>` into an OTM, forming a
    /// StackTrace at the callsite of this function if in the Error case.
    let ofAsyncResult (x: Async<Result<_, 'err>>) : OTM<_, 'r, 'err> =
        fun _ -> x |> Async.map (Result.mapError Traced.trace)

    /// Obtain an `OTM` containing the `Async Result` portion of the
    /// computation, without the StackTrace.
    let asAsyncResult (x : OTM<_, 'r, 'err>) =
        fun r ->
            x
            |> run r
            |> Async.map (Result.mapError (fun te -> te.error))
            |> retn
            |> run r

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
    ///     | Error e -> printf "Error: %O with StackTrace %O" e.error e.trace
    /// }
    /// ```
    let asTracedResult (x: OTM<_, 'r, 'err>) : OTM<Result<'a, Traced<'err>>, 'r, 'err> =
        fun r ->
            async {
                let! res = run r x
                return! run r (retn res)
            }

    /// `Async.Sleep : int -> Async<unit>` lifted into an `OTM`
    let sleep (ms: int) : OTM<_, 'r, 'err> = Async.Sleep ms |> ofAsync

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
    /// |> OTM.handle
    ///     (function
    ///      | ErrorType1 _ as e1 -> f e1
    ///      | ErrorType2 _ as e2 -> g e2
    ///      | e -> h e)
    /// ```
    let handle f (x: OTM<_, 'r, 'err1>) : OTM<_, 'r, 'err2> =
        fun r ->
            async {
                match! run r x with
                | Ok x' -> return Ok x'
                | Error tracedError -> return! run r (f tracedError.error)
            }

    /// Like `OTM.handle`, but with access to the `StackTrace` of the `'err`
    let handleTraced (f: Traced<'err1> -> OTM<_, _, _>) (x: OTM<_, 'r, 'err1>) : OTM<_, 'r, 'err2> =
        fun r ->
            async {
                match! run r x with
                | Ok x' -> return Ok x'
                | Error tracedError -> return! run r (f tracedError)
            }

    module Operators =

        /// bind
        let inline (>>=) x f : OTM<_, 'r, 'err> = x |> bind f
        /// flip bind
        let inline (=<<) f x : OTM<_, 'r, 'err> = x |> bind f
        /// Kliesli composition
        let inline (>=>) f g : _ -> OTM<_, 'r, 'err> = fun x -> x >>= f >>= g
        /// flip Kliesli
        let inline (<=<) g f : _ -> OTM<_, 'r, 'err> = fun x -> x >>= f >>= g
        /// map
        let inline (<!>) f x : OTM<_, 'r, 'err> = x |> map f
        /// apply
        let inline (<*>) f x : OTM<_, 'r, 'err> = x |> apply f

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
            fun r ->
                async {
                    try return! this.ReturnFrom(body()) |> OTM.run r
                    with e -> return! handler e |> OTM.run r
                }

        member this.TryFinally(body, compensation) : OTM<_, 'r, 'err> =
            fun r ->
                async {
                    try return! this.ReturnFrom(body()) |> OTM.run r
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
                    this.While(enum.MoveNext,
                        this.Delay(fun () -> body enum.Current)))

        member this.Combine (a,b) =
            this.Bind(a, fun () -> b())

    /// Builds a OTM using computation expression syntax
    let otm = OTMBuilder()

