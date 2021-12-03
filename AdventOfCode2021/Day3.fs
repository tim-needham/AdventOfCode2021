module Day3

open System;
open System.Diagnostics;
open System.IO;

let parse (input : string) : int list =
    input
    |> Seq.toList
    |> List.map (fun x -> Int32.Parse (x.ToString()));

let gammaCheck (input : int list) : int =
    input
    |> List.groupBy (fun x -> x)
    |> List.sortByDescending (fun (k, y) -> y.Length, k)
    |> List.head
    |> (fun (k, _) -> k);

let epsilonCheck (input : int list) : int =
    input
    |> List.groupBy (fun x -> x)
    |> List.sortBy (fun (k, y) -> y.Length, k)
    |> List.head
    |> (fun (k, _) -> k);

let rec applyCheck (pivoted : int list list) (check : int list -> int) (bytes : int list list) : int list =
    match List.head bytes with
    | [] -> pivoted
            |> List.map (fun x -> check x);
    | _ ->  let p', b' =    bytes
                            |> List.fold (fun (a, b) x -> (a @ [List.head x], b @ [List.tail x])) ([], [])
            applyCheck (pivoted @ [p']) check b';

let binToDec (input : int list) : int =
    let rec binToDecInner (total : int) (power : int) (input : int list) : int =
        match input with
        | [] -> total;
        | x::xs -> binToDecInner (total + (power * x)) (power * 2) xs;

    binToDecInner 0 1 (List.rev input);

let rec filter (index : int) (check : int list -> int) (domain : int list list) : int list =
    match domain with
    | [] -> failwith "Run out of domain input to filter!";
    | [x] -> x;
    | _ ->  let g = domain |> List.map (List.skip index >> List.head) |> check;
            let domain' = List.filter (fun x -> x |> List.skip index |> List.head = g) domain;
            filter (index + 1) check domain';

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = File.ReadAllLines(file)
                |> Seq.map parse
                |> Seq.toList

    (applyCheck [] gammaCheck input, applyCheck [] epsilonCheck input)
    |> (fun (x, y) -> binToDec x, binToDec y)
    |> (fun (x, y) -> x * y)
    |> printfn "Day 3, part 1: %d";

    (filter 0 gammaCheck input, filter 0 epsilonCheck input)
    |> (fun (x, y) -> binToDec x, binToDec y)
    |> (fun (x, y) -> x * y)
    |> printfn "Day 3, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
