# FSharpValidation

General validation functions and monad

Usage:
```
open Validation

let getUser id =
    DB.getUser id
    |> Validation.ofOption "Cannot load user"

    
let displayNameResult =
    validated {
        let! user = getUser 32

        return sprintf "%s %s" user.firstName user.lastName
    }


match displayNameResult with
| Success displayName ->
    displayName
    |> sprintf "Hi %s!"

| Failure error ->
    error
    |> sprintf "Error: %s"

|> System.Console.WriteLine
```

Huge thanks to the FSharpx validated code, which this is really just a rehash of, minus some bits I didn't agree with (Choice), plus some helpful things around options and sequences.
