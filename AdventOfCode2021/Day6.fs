module Day6

open Memo;
open System;
open System.Diagnostics;
open System.IO;

let parse (input : string) : int list =
    input.Split([| "," |], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.toList
    |> List.map Int32.Parse;

// generate fish using memoization
let rec generate (clock : int) (fish : int) : int64 =
    match clock, fish with
    | 0, _ -> 1L;
    | c, 0 -> (generateMemo (c - 1) 8) + (generateMemo (c - 1) 6);
    | c, n -> generateMemo (c - 1) (n - 1);
and generateMemo = memoize2 generate;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = File.ReadAllLines(file)
                |> Seq.toList
                |> List.collect parse;

    input
    |> List.fold (fun a x -> a + (generateMemo 80 x)) 0L
    |> printfn "Day 6, part 1: %d";

    input
    |> List.fold (fun a x -> a + (generateMemo 256 x)) 0L
    |> printfn "Day 6, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
