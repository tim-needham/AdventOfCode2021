module Day9

open System;
open System.Diagnostics;
open System.IO;

let parse (input : string) : int list =
    input
    |> Seq.toList
    |> List.map (fun x -> Int32.Parse(x.ToString()));

let neighbours (x : int) (y : int) (grid : int list list) : seq<int * int * int> =
    seq {
        if x > 0 then
            yield (x - 1, y, grid.[y].[x - 1]);
        if y > 0 then
            yield (x, y - 1, grid.[y - 1].[x]);
        if x < grid.[y].Length - 1 then
            yield (x + 1, y, grid.[y].[x + 1]);
        if y < grid.Length - 1 then
            yield (x, y + 1, grid.[y + 1].[x]);
    };

let lowPoints (input : int list list) : seq<int * int * int> =
    seq {
        for j in 0..input.Length - 1 do
            for i in 0..input.[j].Length - 1 do
                if (neighbours i j input |> Seq.fold (fun a (_, _, c) -> a && (input.[j].[i] < c)) true) then
                    yield (i, j, input.[j].[i]);
    };

let risk ((x, y, h) : int * int * int) : int * int * int =
    (x, y, h + 1);

// we don't really need to keep a list of grid points that have already been traversed as we can't go back
// on ourselves due to the ascending nature of the basin height values so our only duplicates will be when
// two diagonal points return the same neighbour - otherwise this would need to be a fold rather than a recursion
let rec basin (grid : int list list) ((x, y, h) : int * int * int) : seq<int * int> =
    seq {
        yield (x, y);        
        yield!  neighbours x y grid
                |> Seq.filter (fun (_, _, c) -> c > h && c < 9)
                |> Seq.collect (fun n -> basin grid n);
    };

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = File.ReadAllLines(file)
                |> Seq.toList
                |> List.map parse;

    input
    |> lowPoints
    |> Seq.map risk
    |> Seq.fold (fun a (_, _, r) -> a + r) 0
    |> printfn "Day 9, part 1: %d";

    input
    |> lowPoints
    |> Seq.map (fun l -> basin input l)
    |> Seq.map (Seq.toList >> List.distinct)
    |> Seq.sortByDescending List.length
    |> Seq.take 3
    |> Seq.fold (fun a c -> a * (List.length c)) 1
    |> printfn "Day 9, part 2: %d";
    
    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
