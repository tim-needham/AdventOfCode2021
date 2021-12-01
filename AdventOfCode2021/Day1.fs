module Day1

open System;
open System.Diagnostics;
open System.IO;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let depths =    File.ReadAllLines(file)
                    |> Seq.map (Int32.Parse)

    depths
    |> Seq.pairwise
    |> Seq.map (fun (a, b) -> if b > a then 1 else 0)
    |> Seq.sum
    |> printfn "Day 1, part 1: %d";

    depths
    |> Seq.windowed 3
    |> Seq.map (Array.sum)
    |> Seq.pairwise
    |> Seq.map (fun (a, b) -> if b > a then 1 else 0)
    |> Seq.sum
    |> printfn "Day 1, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
