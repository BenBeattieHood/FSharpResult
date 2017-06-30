module Result

open Microsoft.FSharp.Core

/// If Result is Result.Ok, return its value.
/// Otherwise throw ArgumentException.
let get =
    function
    | Result.Ok value -> value
    | Result.Error error -> invalidArg "result" (sprintf "The result was Result.Error: '%A'" error) |> raise
    
/// Wraps a function, encapsulates any exception thrown within to a Choice
let tryWith 
    (f: unit -> 'TOk) 
    (onError: System.Exception -> 'TError) 
    : Result<'TOk, 'TError> =
    try
        f() |> Result.Ok
    with
        ex -> onError ex |> Result.Error

/// Attempts to cast an object.
/// Stores the cast value in Result.Ok if successful, otherwise stores the exception in Result.Error
let cast (o: obj) = tryWith (unbox o) (fun ex -> ex.Message)

/// Sequential application
let ap x f =
    match f,x with
    | Result.Ok f, Result.Ok x -> Result.Ok (f x)
    | Result.Error e, _            -> Result.Error e
    | _           , Result.Error e -> Result.Error e
    
/// Sequential application
let inline (<*>) f x = ap x f

/// Maps both parts of a Result.
/// Applies the first function if Result is Result.Ok.
/// Otherwise applies the second function
let inline bimap f1 f2 = 
    function
    | Result.Ok x -> Result.Ok (f1 x)
    | Result.Error x -> Result.Error (f2 x)

/// Transforms a Result's success value by using a specified mapping function.
let map f =
    bimap id f
    
/// Transforms a Results's error by using a specified mapping function.
let inline mapFailure f = bimap id f

/// Infix map
let inline (<!>) f x = map f x

/// Promote a function to a monad/applicative, scanning the monadic/applicative arguments from left to right.
let inline lift2 f a b = f <!> a <*> b

/// Sequence actions, discarding the value of the first argument.
let inline ( *>) a b = lift2 (fun _ z -> z) a b

/// Sequence actions, discarding the value of the second argument.
let inline ( <*) a b = lift2 (fun z _ -> z) a b

/// Monadic bind
let bind f = 
    function
    | Result.Ok x -> f x
    | Result.Error x -> Result.Error x
    
/// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
let inline (>>=) m f = bind f m

/// Flipped >>=
let inline (=<<) f m = bind f m

/// Sequentially compose two either actions, discarding any value produced by the first
let inline (>>.) m1 m2 = m1 >>= (fun _ -> m2)

/// Left-to-right Kleisli composition
let inline (>=>) f g = fun x -> f x >>= g

/// Right-to-left Kleisli composition
let inline (<=<) x = CoreExt.Core.flip (>=>) x

type ResultBuilder() = 
    member this.Return a = Result.Ok a
    member this.Bind (m, f) = bind f m
    member this.ReturnFrom m = m

let result = ResultBuilder()


let ofOptionF 
    (noneF: unit -> 'TNoValue) 
    (maybeValue:'TValue option)
    : Result<'TValue, 'TNoValue> =

    match maybeValue with
    | Some value -> value |> Result.Ok
    | None -> noneF() |> Result.Error


let ofOption none =
    ofOptionF (fun _ -> none)


let ofSeq
    (source: Result<'TOk, 'TError> seq)
    : Result<'TOk seq, 'TError seq> =

    let failures =
        source
        |> Seq.choose
            (function
                | Result.Error error -> Some error
                | _ -> None
            )
    
    if failures |> Seq.isEmpty then
        source
        |> Seq.choose
            (function
                | Result.Ok value -> Some value
                | Result.Error error -> None
            )
        |> Result.Ok
    else
        failures
        |> Result.Error
    