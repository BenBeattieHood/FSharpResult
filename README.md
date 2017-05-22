# FSharpValidation

General validation functions and monad

Usage:
```
validated {
    let! unpackedGood = 
        42
        |> Success
        
    let! unpackedBad = 
        None
        |> Validation.ofOption "Yikes"

    return ()
}
|> function
    | Success value ->
        sprintf "Success value: %i" value
        |> System.Console.WriteLine 
    | Failure error ->
        sprintf "Success value: %s" error
        |> System.Console.WriteLine 
```
