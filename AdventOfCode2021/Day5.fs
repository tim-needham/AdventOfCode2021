module Day5

open System;
open System.Diagnostics;
open System.IO;

type Point = {
    X : int;
    Y : int;
};;

type Vent = {
    Origin : Point;
    Destination : Point;
};;

let parsePoint (input : string) : Point =
    match input.Split([| "," |], StringSplitOptions.RemoveEmptyEntries) with
    | [| x; y |] -> { X = Int32.Parse(x); Y = Int32.Parse(y); };
    | _ -> failwithf "Invalid point format %s!" input;
    
let parse (input : string) : Vent =
    match input.Split([| " -> " |], StringSplitOptions.RemoveEmptyEntries) with
    | [| o; d |] -> { Origin = parsePoint o; Destination = parsePoint d; };
    | _ -> failwithf "Invalid vent format %s!" input;

let expand (diagonals : bool) (vent : Vent) : Point list =
    let dx = vent.Destination.X - vent.Origin.X
    let dy = vent.Destination.Y - vent.Origin.Y;
    let ix = if dx >= 0 then 1 else -1;
    let iy = if dy >= 0 then 1 else -1;

    if vent.Origin.X = vent.Destination.X && vent.Origin.Y = vent.Destination.Y then
        [
            { X = vent.Origin.X; Y = vent.Origin.Y }
        ];
    else if vent.Origin.X = vent.Destination.X then
        [
            for i in vent.Origin.Y..iy..vent.Destination.Y -> { X = vent.Origin.X; Y = i }
        ];
    else if vent.Origin.Y = vent.Destination.Y then
        [
            for i in vent.Origin.X..ix..vent.Destination.X -> { X = i; Y = vent.Origin.Y }
        ];
    else if diagonals then
        [
            for i in 0..(dx/ix) -> { X = vent.Origin.X + (i * ix); Y = vent.Origin.Y + (i * iy) }
        ];
    else
        [];

let overlaps (diagonals : bool) (vents : Vent list) : int =
    vents
    |> List.map (expand diagonals)
    |> List.concat
    |> List.groupBy (fun p -> p)
    |> List.filter (fun g -> (snd g).Length > 1)
    |> List.length;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = File.ReadAllLines(file)
                |> Seq.toList
                |> List.map parse;

    input
    |> overlaps false
    |> printfn "Day 5, part 1: %d";

    input
    |> overlaps true
    |> printfn "Day 5, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
