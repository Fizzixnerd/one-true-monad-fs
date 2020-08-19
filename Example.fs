open OTM

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
                x |> OTM.bindTracedError (fun e ->
                    logger.Error("An error has occurred: {Error}\n    with StackTrace:\n{StackTrace}\n    with SymbolicStackTrace:\n{SymbolicStackTrace}", e.error, e.trace, e.strace |> SymbolicStackTrace.rev)
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
        |> OTM.runSynchronously state
        |> ignore
        0
