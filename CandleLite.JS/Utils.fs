namespace CandleLite.JS.JSON

open Fable.Core

open CandleLite.Core.Types

module Utils =
    // from  https://github.com/fable-compiler/Fable/blob/da59e287b5d00668f0c260a0649ef587e3b1e214/tests/Js/Main/Util/Thoth.Json.Decode.fs#L19
    // [<Emit("Object.getPrototypeOf($0 || false) === Object.prototype")>]

    [<Emit("typeof($0 || false) === 'object'")>]
    let isObject (_ : obj) : bool = jsNative

    [<Emit("typeof($0 || false) === 'string'")>]
    let isString (_ : obj) : bool = jsNative

    let isArray (x: obj) = JS.Constructors.Array.isArray x

    let raiseOO httpStatus code msg =
        let oo = operationOutcome Error code msg
        raise <| OperationOutcomeException(httpStatus, oo)
