module Day13

open System;
open System.Diagnostics;
open System.IO;

type Fold =
    | X of int
    | Y of int
;;

let parseDot (input : string) : int * int =
    let ps =    input.Split([| "," |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.map Int32.Parse;
    (ps.[0], ps.[1]);

let parseFold (input : string) : Fold =
    match input.Split([| "=" |], StringSplitOptions.RemoveEmptyEntries) with
    | [| "fold along x"; n |] -> n |> Int32.Parse |> X;
    | [| "fold along y"; n |] -> n |> Int32.Parse |> Y;
    | _ -> failwithf "Invalid parse input %s" input;

let parse (input : string list) : ((int * int) list) * Fold list =
    let dots = input 
                |> List.takeWhile (fun i -> i <> "") 
                |> List.map parseDot;
    let folds = input
                |> List.skipWhile (fun i -> i <> "")
                |> List.skip 1
                |> List.map parseFold;
    (dots, folds);

let doFold (dots : (int * int) list) (fold : Fold) : (int * int) list =
    match fold with
    | X n ->    let left, right = dots |> List.partition (fun (x, _) -> x <= n);
                let right' = right |> List.map (fun (x, y) -> ((2 * n) - x, y));
                (left @ right') |> List.distinct;
    | Y n ->    let upper, lower = dots |> List.partition (fun (_, y) -> y <= n);
                let lower' = lower |> List.map (fun (x, y) -> (x, (2 * n) - y));
                (upper @ lower') |> List.distinct;

let rec doFolds (dots : (int * int) list) (folds : Fold list) : (int * int) list =
    match folds with
    | [] -> dots;
    | f::fs -> doFolds (doFold dots f) fs;

let prettyPrint (dots : (int * int) list) : unit =
    let mx = dots |> List.maxBy (fun (x, _) -> x) |> fst;
    let my = dots |> List.maxBy (fun (_, y) -> y) |> snd;

    for j in 0..my do
        for i in 0..mx do
            if List.contains (i, j) dots then
                printf "#";
            else
                printf " ";
        printfn "";
    printfn "";

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let (dots, folds) = File.ReadAllLines(file)
                        |> Seq.toList
                        |> parse;

    folds.[0]
    |> doFold dots
    |> List.length
    |> printfn "Day 13, part 1: %d";

    printfn "Day 13, part 2:";
    
    folds
    |> doFolds dots
    |> prettyPrint;

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
