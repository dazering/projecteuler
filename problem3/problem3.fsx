// The prime factors of 13195 are 5, 7, 13 and 29.

// What is the largest prime factor of the number 600851475143 ?

let getMaxPrimeFactor number =
    let rec getPrimeFactors num factor =
        [ if num >= factor then
              match num % factor with
              | 0L ->
                  yield factor
                  yield! getPrimeFactors (num / factor) (factor + 1L)
              | _ -> yield! getPrimeFactors num (factor + 1L) ]

    getPrimeFactors number (2L) |> List.max

let num = 600851475143L
printfn $"{getMaxPrimeFactor num}"
