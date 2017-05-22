module Validation

type Result<'TSuccess, 'TFailure> = 
| Success of 'TSuccess
| Failure of 'TFailure

let (|Success|Failure|) =
    function
    | Success value -> Success value
    | Failure error -> Failure error

/// If Result is Success, return its value.
/// Otherwise throw ArgumentException.
let get =
    function
    | Success value -> value
    | Failure error -> invalidArg "result" (sprintf "The result was Failure: '%A'" error) |> raise
    
/// Wraps a function, encapsulates any exception thrown within to a Choice
let tryWith 
    (f: unit -> 'TSuccess) 
    (onError: System.Exception -> 'TFailure) 
    : Result<'TSuccess, 'TFailure> =
    try
        f() |> Success
    with
        ex -> onError ex |> Failure

/// Attempts to cast an object.
/// Stores the cast value in Success if successful, otherwise stores the exception in Failure
let cast (o: obj) = tryWith (unbox o) (fun ex -> ex.Message)

/// Sequential application
let ap x f =
    match f,x with
    | Success f, Success x -> Success (f x)
    | Failure e, _            -> Failure e
    | _           , Failure e -> Failure e
    
/// Sequential application
let inline (<*>) f x = ap x f

/// Maps both parts of a Result.
/// Applies the first function if Result is Success.
/// Otherwise applies the second function
let inline bimap f1 f2 = 
    function
    | Success x -> Success (f1 x)
    | Failure x -> Failure (f2 x)

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
    | Success x -> f x
    | Failure x -> Failure x
    
/// Sequentially compose two actions, passing any value produced by the first as an argument to the second.
let inline (>>=) m f = bind f m

/// Flipped >>=
let inline (=<<) f m = bind f m

/// Sequentially compose two either actions, discarding any value produced by the first
let inline (>>.) m1 m2 = m1 >>= (fun _ -> m2)

/// Left-to-right Kleisli composition
let inline (>=>) f g = fun x -> f x >>= g

/// Right-to-left Kleisli composition
let inline (<=<) x = FSharpx.Functional.Prelude.flip (>=>) x

type ValidatedBuilder() = 
    member this.Return a = Success a
    member this.Bind (m, f) = bind f m
    member this.ReturnFrom m = m

let validated = ValidatedBuilder()


let ofOptionM 
    (noneM: unit -> 'TNoValue) 
    (maybeValue:'TValue option)
    : Result<'TValue, 'TNoValue> =

    match maybeValue with
    | Some value -> value |> Success
    | None -> noneM() |> Failure


let ofOption none =
    ofOptionM (fun _ -> none)


let ofSeq
    (source: Result<'TSuccess, 'TFailure> seq)
    : Result<'TSuccess seq, 'TFailure seq> =

    let failures =
        source
        |> Seq.choose
            (function
                | Result.Failure error -> Some error
                | _ -> None
            )
    
    if failures |> Seq.isEmpty then
        source
        |> Seq.choose
            (function
                | Result.Success value -> Some value
                | Result.Failure error -> None
            )
        |> Result.Success
    else
        failures
        |> Result.Failure
    