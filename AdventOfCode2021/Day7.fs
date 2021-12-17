module Day7

open Math2;
open System;
open System.Diagnostics;
open System.IO;

let parse (input : string) : int list =
    input.Split([| "," |], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.toList
    |> List.map Int32.Parse;

let fuelCostConstant (position : int) (crabs : int list) : int =
    crabs
    |> List.sumBy (fun x -> Math.Abs(position - x));

let fuelCostTriangular (position : int) (crabs : int list) : int =
    crabs
    |> List.sumBy (fun x -> Math.Abs(position - x) |> triangularMemo);

// moving across the position space in order, results will decrease then increase so stop when we start increasing
let rec bestFuel (fuelCost : int option) (crabs : int list) (cost : int -> int list -> int) (positions : int list) : int =
    match positions, fuelCost with
    | [], None -> failwith "Unable to calculate best fuel!";
    | [], Some f -> f;
    | x::xs, None -> bestFuel (Some (cost x crabs)) crabs cost xs;
    | x::xs, Some f ->  let f' = cost x crabs;
                        if f' > f then
                            f;
                        else
                            bestFuel (Some f') crabs cost xs;

let solve (cost : int -> int list -> int) (crabs : int list) : int =
    [List.min crabs..List.max crabs]
    |> bestFuel None crabs cost;
    
let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = File.ReadAllLines(file)
                |> Seq.toList
                |> List.collect parse;

    // seems like you can solve part 1 using the median value of the list of crabs... obviously doesn't work for part 2 though
    input
    |> solve fuelCostConstant
    |> printfn "Day 7, part 1: %d";

    input
    |> solve fuelCostTriangular
    |> printfn "Day 7, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
