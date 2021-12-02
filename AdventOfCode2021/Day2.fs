module Day2

open System;
open System.Diagnostics;
open System.IO;

type Move = 
    | Forward of n : int
    | Down of n : int
    | Up of n : int
;;

let parse (input : string) : Move =
    match (input.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)) with
    | [| "forward"; n |] -> Forward (Int32.Parse n);
    | [| "up"; n |] -> Up (Int32.Parse n);
    | [| "down"; n |] -> Down (Int32.Parse n);
    | _ -> failwithf "Invalid parse input %s" input;

let rec moveSub (depth : int) (position: int) (moves : list<Move>) : (int * int) =
    match moves with
    | [] -> depth, position;
    | Forward n::ms -> moveSub depth (position + n) ms;
    | Down n::ms -> moveSub (depth + n) position ms;
    | Up n::ms -> moveSub (depth - n) position ms;

let rec moveSubAim (depth : int) (position : int) (aim : int) (moves : list<Move>) : (int * int * int) =
    match moves with
    | [] -> depth, position, aim;
    | Forward n::ms -> moveSubAim (depth + (aim * n)) (position + n) aim ms;
    | Down n::ms -> moveSubAim depth position (aim + n) ms;
    | Up n::ms -> moveSubAim depth position (aim - n) ms;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let moves = File.ReadAllLines(file)
                |> Seq.map (parse)
                |> Seq.toList;

    moves
    |> moveSub 0 0
    |> (fun (d, p) -> d * p)
    |> printfn "Day 2, part 1: %d";

    moves
    |> moveSubAim 0 0 0
    |> (fun (d, p, _) -> d * p)
    |> printfn "Day 2, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
