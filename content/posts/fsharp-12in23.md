---
title: "F# 12 in 23"
date: 2023-01-08T14:00:00Z
draft: false
tags: ["fsharp", "F#", "12in23"]
showtoc: false
tocopen: false


---

# FSharp exercism solutions for 12in23

Exercism is running an event called "12in23" this year which is a challenge to use 12 different programming languages and solve 5 exercises in each one to get a badge, these are my solutions and commentary on each exercise as I solve them in F# for the first language.

## Allergies

```fsharp
module Allergies

open System

type Allergen =
    | Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats


let maxAllergen value = 
    match value with
    | x when x >= 128 -> (Cats, value - 128)
    | x when x >= 64  -> (Pollen, value - 64)
    | x when x >= 32  -> (Chocolate, value - 32)
    | x when x >= 16  -> (Tomatoes, value - 16)
    | x when x >= 8   -> (Strawberries, value - 8)
    | x when x >= 4   -> (Shellfish, value - 4)
    | x when x >= 2   -> (Peanuts, value - 2)
    | x when x >= 1   -> (Eggs, value - 1)
    | _ -> failwith "invalid value"

let allergyValue (allergen: Allergen) = 
    match allergen with
    | Eggs -> 1
    | Peanuts -> 2
    | Shellfish -> 4
    | Strawberries -> 8
    | Tomatoes -> 16
    | Chocolate -> 32
    | Pollen -> 64
    | Cats -> 128

let sortAllergiesList (allergies: Allergen list) = 
    List.sortBy allergyValue allergies

let rec listAcc (value: int) (acc: Allergen Set) =
    match value with
    | 0 -> sortAllergiesList (Set.toList acc)
    | v ->
        let (highest, newVal) = maxAllergen v
        printfn $"curr value: %d{v}, next: %d{newVal}, added: %A{highest}"
        listAcc newVal (Set.add highest acc)

let list (codedAllergies: int) = listAcc codedAllergies Set.empty

let allergicTo (codedAllergies: int) (allergen: Allergen) = 
    codedAllergies &&& allergyValue allergen <> 0 
```

This wasn't too bad, from first look I saw that it was basically a reducer problem, I implemented that using a recursive function `listAcc`, I could have massaged that into an actual reducer but it was a bit easier for me to understand when written this way.