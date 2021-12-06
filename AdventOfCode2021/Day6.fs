module Day6

open System;
open System.Diagnostics;
open System.IO;

let parse (input : string) : int list =
    input.Split([| "," |], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.toList
    |> List.map Int32.Parse;

// memoize a given function
let memoize (f : 'TArg -> 'TResult) : ('TArg -> 'TResult) =
    let cache = new System.Collections.Generic.Dictionary<_, _>();

    (fun x ->
        match cache.TryGetValue x with
        | true, v -> v;
        | false, _ ->   let v = f x;
                        cache.Add(x, v);
                        v);

// memoize only works on functions with one parameter so use this overload for 2 parameters
let memoize2 (f': 'TArg1 -> 'TArg2 -> 'TResult) : ('TArg1 -> 'TArg2 -> 'TResult) =
    let f = memoize (fun (a, b) -> f' a b);
    (fun a b -> f(a, b));

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
