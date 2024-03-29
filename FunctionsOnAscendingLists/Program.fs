﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open System.Linq



// Function: Count
let rec count weakList item =
    match weakList with
    | [] -> 0
    | x :: [] ->
        if x = item then 1
        else 0
    | x :: xs ->
        (if x = item then 1
         else 0)
        + count xs item

// Function: Insert
let rec insert weakList item =
    match weakList with
    | [] -> [ item ]
    | x :: [] when item <= x -> [ item ] @ [ x ]
    | x :: [] when item > x -> [ x ] @ [ item ]
    | x :: xs when item <= x -> item :: [ x ] @ xs
    | x :: xs when item > x -> x :: (insert xs item)
    | _ -> failwith "Incomplete match on %A"


// Function: Intersect
let intersect (list1, list2) = List.filter (fun elm -> List.contains elm list1) list2


// Function: Plus
let plus (list1, list2) =
    let rec plus' list resultlist =
        match list with
        | [] -> resultlist
        | x :: xs -> insert resultlist x |> plus' xs
    plus' list2 list1

// Function: Minus
let minus (minuendList, subtrahendList) =
    let rec removeItem list itemToRemove resultList =
        match list with
        | [] -> resultList
        | x :: [] when x = itemToRemove -> resultList
        | x :: xs when x = itemToRemove -> resultList @ xs
        | x :: xs -> resultList @ [ x ] @ removeItem xs itemToRemove resultList

    let rec removeList listToRemove resultlist =
        match listToRemove with
        | [] -> resultlist
        | x :: xs -> removeList xs (removeItem resultlist x [])

    removeList subtrahendList minuendList