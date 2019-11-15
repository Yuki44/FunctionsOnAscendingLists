// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System


// Check if the list is weakly ascending:
let checkWeakAscent weakList =
    let rec checkWeakAscent' weakList current =
        match weakList with
        | [] -> true
        | x :: [] -> current <= x
        | x1 :: xs -> (current <= x1) && checkWeakAscent' xs x1
    checkWeakAscent' weakList Int32.MinValue
