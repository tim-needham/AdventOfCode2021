module Day8

open System;
open System.Diagnostics;
open System.IO;

let parse (input : string) : string list * string list =
    input.Split([| " | " |], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.toList
    |> List.map (fun x -> x.Split([| " " |], StringSplitOptions.RemoveEmptyEntries) |> Seq.toList)
    |> (fun x -> List.head x, x |> List.skip 1 |> List.head);

// numbers with unique numbers of segments
// 1 2
// 4 4
// 7 3
// 8 7
let uniqueDigits (input : string list) : int =
    input
    |> List.filter (fun x -> x.Length = 2 || x.Length = 3 || x.Length = 4 || x.Length = 7)
    |> List.length;

let overlaps (tokens : char list) (input : string) : bool =
    tokens
    |> List.fold (fun a c -> a && (input.IndexOf(c) >= 0)) true;

// going to assume I will always find what I'm looking for so no option pattern
let findPattern (length : int) (tokens : char list) (input : string list) : char list =
    match tokens with
    | [] -> input
            |> List.find (fun x -> x.Length = length)
            |> Seq.toList;
    | _ ->  input
            |> List.filter (fun x -> x.Length = length)
            |> List.find (fun x -> overlaps tokens x)
            |> Seq.toList;

let removeFrom (tokens : char list) (input : char list) : char list =
    input
    |> List.filter (fun x -> List.tryFind (fun t -> t = x) tokens = None);

let removeFrom1 (tokens : char list) (input : char list) : char =
    input
    |> removeFrom tokens
    |> List.head;

let merge (tokensA : char list) (tokensB : char list) : char list =
    tokensA @ tokensB
    |> List.sort
    |> List.distinct;

// nothing fancy, just logic
let solve ((inputs, outputs) : string list * string list) : int =
    // sort the inputs and outputs alphabetically because the puzzle inputs aren't consistantly ordered
    let sortedInputs =  inputs
                        |> List.map (Seq.sort >> String.Concat);
    let sortedOutputs = outputs
                        |> List.map (Seq.sort >> String.Concat);

    // find the input with 2 segments, this = 1 and the segments map to c f
    let d1 = findPattern 2 [] sortedInputs;
    // find the input with 4 segments, this = 4
    let d4 = findPattern 4 [] sortedInputs;
    // find the input with 3 segments, this = 7 and the extra segment over 1 maps to a
    let d7 = findPattern 3 [] sortedInputs;
    let a = removeFrom1 d1 d7;
    // find the input with 7 segments, this = 8
    let d8 = findPattern 7 [] sortedInputs;
    // find the input with 6 segments that overlaps 4 & 7, this is 9 and the extra segment maps to g
    let d9 = findPattern 6 (merge d4 d7) sortedInputs;
    let g = removeFrom1 (merge d4 d7) d9;
    // the difference between 8 and 9 maps to e
    let e = removeFrom1 d9 d8;
    // find the input with 5 segments containing 7, this is 3 and the extra segment over 7 and g maps to d
    let d3 = findPattern 5 d7 sortedInputs;
    let d = removeFrom1 (merge [g] d7) d3;
    // the extra segment in 4 over 1 and d maps to b
    let b = removeFrom1 (merge [d] d1) d4;
    // find the input with 5 segments containing abdg, this is 5 and the extra segment maps f
    let d5 = findPattern 5 [a; b; d; g] sortedInputs;
    let f = removeFrom1 [a; b; d; g] d5;
    // c is the extra segment in 1
    let c = removeFrom1 [f] d1;
    // now we have identified all the mappings, we can quickly find 0, 2 and 6
    let d0 = findPattern 6 [a; b; c; e; f; g] sortedInputs;
    let d2 = findPattern 5 [a; c; d; e; g] sortedInputs;
    let d6 = findPattern 6 [a; b; d; e; f; g] sortedInputs;

    let ds =    [ d0; d1; d2; d3; d4; d5; d6; d7; d8; d9 ]
                |> List.map String.Concat;

    sortedOutputs
    |> List.fold (fun a c -> (10 * a) + List.findIndex (fun y -> y = c) ds) 0;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = File.ReadAllLines(file)
                |> Seq.toList
                |> List.map parse;

    input
    |> List.sumBy (snd >> uniqueDigits)
    |> printfn "Day 8, part 1: %d";

    input
    |> List.sumBy (fun x -> solve x)
    |> printfn "Day 8, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
