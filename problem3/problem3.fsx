let getMaxPrimeFactor number =
    let rec getPrimeFactors num factor =
        [ if num >= factor then
              match num % factor with
              | 0L ->
                  yield factor
                  yield! getPrimeFactors (num / factor) (factor + 1L)
              | _ -> yield! getPrimeFactors num (factor + 1L) ]

    getPrimeFactors number (int64 2) |> List.max

let num = 600851475143L
printfn $"{getMaxPrimeFactor num}"
