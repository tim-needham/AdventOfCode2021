module Day17

open Math2;
open System;
open System.Diagnostics;
open System.IO;

let parse (input : string) : int * int * int * int =
    input
        .Substring(14)
        .Split([| '.'; ','; ' '; '='; 'x'; 'y' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.map Int32.Parse
        |> (fun a -> a.[0], a.[1], a.[2], a.[3]);

let accelerate ((px, py) : int * int) ((vx, vy) : int * int) : (int * int) * (int * int) =
    (px + vx, py + vy), (vx - sign vx, vy - 1);

let rec projectile (targetArea : int * int * int * int) (maxHeight : int) (position : int * int) (velocity : int * int) =
    let beyond ((_, tMaxX, tMinY, _) : int * int * int * int) ((px, py) : int * int) =
        px > tMaxX || py < tMinY;

    let within ((tMinX, tMaxX, tMinY, tMaxY) : int * int * int * int) ((px, py) : int * int) =
        px >= tMinX && px <= tMaxX && py >= tMinY && py <= tMaxY;

    match accelerate position velocity with
    | pos, _ when beyond targetArea pos -> None;
    | pos, _ when within targetArea pos -> Some maxHeight;
    | (px, py), vel -> projectile targetArea (Math.Max(maxHeight, py)) (px, py) vel;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let minX, maxX, minY, maxY = File.ReadAllLines(file)
                                    |> Seq.head
                                    |> parse;

    minY
    |> Math.Abs
    |> (fun x -> x - 1)
    |> triangular    
    |> printfn "Day 17, part 1: %d";

    ([ 1 .. maxX ], [ minY .. abs minY ])
    ||> List.allPairs
    |> List.map (fun v -> projectile (minX, maxX, minY, maxY) 0 (0, 0) v)
    |> List.filter Option.isSome
    |> List.length
    |> printfn "Day 17, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
