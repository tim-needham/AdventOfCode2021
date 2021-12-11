module Day11

open System;
open System.Diagnostics;
open System.IO;

let parse (input : string list) : int list list =
    input
    |> List.map Seq.toList
    |> List.map (List.map (fun c -> Int32.Parse (c.ToString())));

let prettyPrint (input : int list list) : unit =
    let fc = Console.ForegroundColor;
    for j in 0..input.Length - 1 do
        for i in 0..input.[j].Length - 1 do
            if input.[j].[i] = 0 then
                Console.ForegroundColor <- ConsoleColor.Cyan;

            printf "%d" input.[j].[i];

            Console.ForegroundColor <- fc;
        printfn "";
    printfn "";

let increment (input : int list list) : int list list =
    [ for j in 0..input.Length - 1 ->
        [ for i in 0..input.[j].Length - 1 ->
            input.[j].[i] + 1
        ]
    ];

let flashers (input : int list list) : (int * int) list =
    seq {
        for j in 0..input.Length - 1 do
            for i in 0..input.[j].Length - 1 do
                if input.[j].[i] > 9 then
                    yield (i, j);
        
    } |> Seq.toList;

let impacted (input : int list list) (flashed : (int * int) list) ((fx, fy) : int * int) : (int * int) list =
    seq {
        for j in Math.Max(0, fy - 1)..Math.Min(input.Length - 1, fy + 1) do
            for i in Math.Max(0, fx - 1)..Math.Min(input.[j].Length - 1, fx + 1) do
                if not (List.contains (i, j) flashed) && not (i = fx && j = fy) then
                    yield (i, j);
    } |> Seq.toList;

let rec flash (flashed : (int * int) list) (input : int list list) : (int list list * int) =
    let fsh = flashers input;
    match fsh with
    | [] -> (input, List.length flashed);
    | _ ->  let fsd = flashed @ fsh;
            let imp = fsh
                        |> List.collect (fun f -> impacted input fsd f)
                        |> List.groupBy (fun f -> f)
                        |> List.map (fun (k, v) -> (k, List.length v));

            let out =   [ for j in 0..input.Length - 1 ->
                            [ for i in 0..input.[j].Length - 1 ->
                                if input.[j].[i] > 9 then
                                    0;
                                else
                                    match List.tryFind (fun ((x, y), _) -> x = i && y = j) imp with
                                    | None -> input.[j].[i];
                                    | Some (_, v) -> input.[j].[i] + v;
                            ]
                         ];

            flash fsd out;

let rec step (steps : int) (flashed : int) (input : int list list) : int =
    match steps with
    | 0 -> flashed;
    | n ->  let output, f = input
                            |> increment
                            |> flash [];

            step (n - 1) (flashed + f) output;

let rec firstSync (steps : int) (input : int list list) : int =
    let s' = steps + 1;
    let output, f = input
                    |> increment
                    |> flash [];

    match f with
    | 100 -> s';
    | _ -> firstSync s' output;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = File.ReadAllLines(file)
                |> Seq.toList
                |> parse;

    input
    |> step 100 0
    |> printfn "Day 11, part 1: %d";

    input
    |> firstSync 0
    |> printfn "Day 11, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
