module Day4

open System;
open System.Diagnostics;
open System.IO;

let parseCalls (input : string) : int list =
    input.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.toList
    |> List.map Int32.Parse;

let rec parseBoards (boards : int list list list) (board : int list list) (input : string list) : int list list list =
    match input with
    | [] -> boards @ [board];
    | ""::xs ->  parseBoards (boards @ [board]) [] xs;
    | x::xs ->  let line =  x.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
                            |> Seq.toList
                            |> List.map Int32.Parse;
                parseBoards boards (board @ [line]) xs;

let parse (input : string list) : int list * int list list list =
    let calls = input
                |> List.head
                |> parseCalls;
    let boards =    input
                    |> List.skip 2
                    |> parseBoards [] []
    (calls, boards)

let winningRow (board : int list list) : bool =
    board
    |> List.map (fun r -> List.fold (fun a c -> a && (c = -1)) true r)
    |> List.exists (fun r -> r = true);

let rec winningColumn (board : int list list) : bool =
    match List.head board with
    | [] -> false;
    | _ ->  let heads = board |> List.map List.head;
            if List.fold (fun a c -> a && (c = -1)) true heads then
                true;
            else
                winningColumn (board |> List.map List.tail);

let winner (board : int list list) : bool =
    if winningRow board then
        true;
    else
        winningColumn board; 

let findWinners (boards : int list list list) : int list list list * int list list list =
    boards
    |> List.partition (fun b -> winner b);

let findWinner (boards : int list list list) : int list list option * int list list list =
    match findWinners boards with
    | [], _ -> None, boards;
    | ws, bs -> Some (List.head ws), bs;

let applyBall (ball : int) (board : int list list) : int list list =
    [ for j in 0..board.Length - 1 ->
        [ for i in 0..board.[j].Length - 1 ->
            if ball = board.[j].[i] then -1 else board.[j].[i]
        ]
    ];

let rec callBalls (boards : int list list list) (calls : int list) : int * int list list * int list list list =
    match calls with
    | [] -> failwith "No more calls, no winning boards!";
    | x::xs ->  let boards' = boards |> List.map (applyBall x);
                match findWinner boards' with
                | Some b, bs -> x, b, bs;
                | None, _ -> callBalls boards' xs;

let rec findLastWinner (call : int) (winner : int list list) (boards : int list list list) (calls : int list) : int * int list list =
    match calls with
    | [] -> call, winner;
    | x::xs ->  let boards' = boards |> List.map (applyBall x);
                match findWinners boards' with
                | [], _ -> findLastWinner call winner boards' xs;
                | ws, bs -> findLastWinner x (List.head ws) bs xs;

let score ((call, board) : int * int list list) : int =
   board
   |> List.map (List.filter (fun x -> x <> -1))
   |> List.map (List.fold (fun a c -> a + c) 0)
   |> List.fold (fun a c -> a + c) 0
   |> (fun x -> x * call);

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let (calls, boards) =   File.ReadAllLines(file)
                            |> Seq.toList
                            |> parse;

    calls
    |> callBalls boards
    |> (fun (x, y, _) -> x, y)
    |> score
    |> printfn "Day 4, part 1: %d"

    calls
    |> findLastWinner -1 [] boards
    |> score
    |> printfn "Day 4, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
