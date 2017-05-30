module ResultTests

open System
open Xunit
open Swensen.Unquote

open Result

type RoundTripValidationMonadData = {
    value: Result<int, string>
}

let roundTripValidationMonad
    (data: RoundTripValidationMonadData)
    : unit =
    let actual =
        result {
            let! value = data.value
            return value
        }
        
    test <@ actual = data.value @>
    
[<Fact>]
let ``validation`` () =
    [
        { value= 1 |> Result.Ok }
        { value= "Uh oh!" |> Result.Error }
    ]
    |> Seq.iter roundTripValidationMonad
