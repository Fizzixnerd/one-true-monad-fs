open Otm

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

    let ofInt x =
        Otm.asks (ReaderState.config_ >> Config.lowest_)
        |> Otm.bind (fun lowest ->
            if x < lowest then Otm.throw (TooLowBadness lowest) else Otm.retn x)

    let incr x = x |> Otm.map (fun y -> y + 1)

    let decr x =
        otm {
            let! x' = x |> Otm.map (fun y -> y - 1)
            return! ofInt x'
        }

    let zero = 0 |> ofInt

    let three = zero |> incr |> incr |> incr

    let negativeOne = zero |> decr

    let logError (x: Otm<_, ReaderState, 'err>) =
        otm {
            let! logger = Otm.asks ReaderState.logger_
            return!
                x |> Otm.bindTracedError (fun e ->
                    logger.Error("An error has occurred: {Error} with {SymbolicStackTrace}", e.error, e.strace)
                    Otm.rethrow e)
        }

    let logSuccess (x: Otm<int, ReaderState, 'err>): Otm<_, _, _> =
        otm {
            let! logger = Otm.asks ReaderState.logger_
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

        // Shouldn't print anything
        otm {
            printf "Rawr"
        }
        |> ignore

        // Shouldn't print anything
        async {
            printfn "Moo"
        }
        |> ignore

        Example.main
        |> Otm.runSynchronously state
        |> ignore
        0
