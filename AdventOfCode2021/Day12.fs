module Day12

open System;
open System.Diagnostics;
open System.IO;

let parse (input : string) : (string * string) list =
    let p = input.Split([| "-" |], StringSplitOptions.RemoveEmptyEntries);
    // connections work both ways
    [ (p.[0], p.[1]); (p.[1], p.[0]) ];

let bigCave (input : string) : bool =
    List.contains input.[0] ['A'..'Z'];

let rec traverse (target : string) (path : string list) (twice : bool) (connections : (string * string) list) : seq<string list> =
    seq {
        match List.head path with
        | x when x = target -> yield path;
        | x ->  let caves = connections
                            |> List.filter (fun (a, b) -> b = x && (not (List.contains a path) || not twice || bigCave a));
                
                if List.length caves > 0 then
                    yield!  caves
                            |> Seq.collect (fun (a, _) ->   match bigCave a with
                                                            | true -> traverse target (a::path) twice connections;
                                                            | false -> traverse target (a::path) (twice || List.contains a path) connections;
                                            );
    };

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = File.ReadAllLines(file)
                |> Seq.toList
                |> List.collect parse
                // we can make part 2 easier by ensuring the start and end connections are 1 way only...
                |> List.filter (fun (a, b) -> b <> "start" && a <> "end");

    input
    |> traverse "start" ["end"] true
    |> Seq.length
    |> printfn "Day 12, part 1: %d";

    input
    |> traverse "start" ["end"] false
    |> Seq.length
    |> printfn "Day 12, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
