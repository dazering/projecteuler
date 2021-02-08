// A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

// Find the largest palindrome made from the product of two 3-digit numbers.
open System.Linq

let GetMaxPalindrome min max =
    let isNumberPalindrome (number: int) =
        let num = number.ToString()
        let numRev = System.String.Join("", num.Reverse())
        numRev = num

    let mutable maxVal = 0

    let rec getPalindrome num factor maxP =
        match num > min && factor > min with
        | true ->
            let mul = factor * num
            if (mul > maxP) && isNumberPalindrome mul then
                getPalindrome num (factor - 1) mul
            else getPalindrome num (factor - 1) maxP
        | false ->
            if num > min then
                getPalindrome (num - 1) max maxP
            else maxP
    let value = getPalindrome max max 0
    value


printfn $"{GetMaxPalindrome 99 999}"
