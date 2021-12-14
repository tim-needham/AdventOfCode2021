module Day14

open Memo;
open System;
open System.Diagnostics;
open System.IO;

let parseRule (input : string) : (char * char * char) =
    match input.Split([| " -> " |], StringSplitOptions.RemoveEmptyEntries) with
    | [| a ; b |] ->    let x = a |> Seq.toList;
                        (x.[0], x.[1], b.[0]);
    | _ -> failwithf "Invalid rule format %s" input;

let parse (input : string list) : (char list * ((char * char * char) list)) =
    let template = input |> List.head |> Seq.toList;
    let rules = input |> List.skip 2 |> List.map parseRule;
    (template, rules);

let findRule (rules : (char * char * char) list) (a : char) (b : char) : char =
    let (_, _, c) = List.find (fun (x, y, _) -> a = x && b = y) rules;
    c;

let combineCounts (pairsA : (char * int64) list) (pairsB : (char * int64) list) : (char * int64) list =
    pairsB
    |> List.append pairsA
    |> List.groupBy (fun (k, _) -> k)
    |> List.map (fun (k, vs) -> (k, List.sumBy snd vs));

// treat this like the lantern fish and keep generating pairs...
let rec generate (rules : (char * char * char) list) (clock : int) (pair : char * char) : (char * int64) list =
    match clock with
    | 0 -> [ (fst pair, 1L); (snd pair, 1L) ];
    | c ->  let r = findRule rules (fst pair) (snd pair);
            combineCounts (generateMemo rules (c - 1) (fst pair, r)) (generateMemo rules (c - 1) (r, snd pair))
and generateMemo = memoize3 generate;

let polymerize (step : int) (rules : (char * char * char) list) (polymer : char list) =
    //this approach double counts everything except the first and last elements so store for use as the seed in the fold below
    let a = List.head polymer;
    let b = polymer |> List.rev |> List.head;

    let p = polymer
            |> List.pairwise
            |> List.map (fun p -> generateMemo rules step p)
            |> List.fold (fun a c -> combineCounts a c) [ (a, 1L); (b, 1L) ]
            |> List.sortByDescending (fun (_, c) -> c);

    let mx = p |> List.head |> snd;
    let mn = p |> List.rev |> List.head |> snd;
    (mx - mn) / 2L;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let template, rules = File.ReadAllLines(file)
                            |> Seq.toList
                            |> parse;

    template
    |> polymerize 10 rules
    |> printfn "Day 14, part 1: %d";

    template
    |> polymerize 40 rules
    |> printfn "Day 14, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
