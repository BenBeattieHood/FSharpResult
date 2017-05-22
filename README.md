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

Huge thanks to the FSharpx validated code, which this is really just a rehash of, minus some bits I didn't agree with (Choice), plus some helpful things around options and sequences.
