module Day10

open System.Diagnostics;
open System.IO;

type ParseResult =
    | Valid
    | Incomplete of char list
    | Invalid of char
;;

let rec parse (openers : char list) (input : char list) : ParseResult =
    match openers, input with
    | [], [] -> Valid;
    | [], y::ys -> parse [y] ys;
    | '('::xs, ')'::ys -> parse xs ys;
    | '['::xs, ']'::ys -> parse xs ys;
    | '{'::xs, '}'::ys -> parse xs ys;
    | '<'::xs, '>'::ys -> parse xs ys;
    | xs, '('::ys -> parse ('('::xs) ys;
    | xs, '['::ys -> parse ('['::xs) ys;
    | xs, '{'::ys -> parse ('{'::xs) ys;
    | xs, '<'::ys -> parse ('<'::xs) ys;
    | _, [] -> Incomplete openers;
    | _, ys -> Invalid (List.head ys);

let totalError (result : ParseResult) : int =
    match result with
    | Valid -> 0;
    | Incomplete _ -> 0;
    | Invalid ')' -> 3;
    | Invalid ']' -> 57;
    | Invalid '}' -> 1197;
    | Invalid '>' -> 25137;
    | Invalid n -> failwithf "Invalid syntax error '%c'!" n;

let rec completionScore (total : int64) (tokens : char list) : int64 =
    let score (token : char) : int64 =
        match token with
        | ')' -> 1L;
        | ']' -> 2L;
        | '}' -> 3L;
        | '>' -> 4L;
        | _ -> failwithf "Invalid completion token '%c'" token;

    match tokens with
    | [] -> total;
    | t::ts -> completionScore ((total * 5L) + score t) ts;

let complete (result : ParseResult) : char list =
    let rec comp (tokens : char list) : char list =
        match tokens with
        | [] -> [];
        | '('::ts -> ')' :: (comp ts);
        | '['::ts -> ']' :: (comp ts);
        | '{'::ts -> '}' :: (comp ts);
        | '<'::ts -> '>' :: (comp ts);
        | x::_ -> failwithf "invalid token '%c'!" x;

    match result with
    | Incomplete cs -> comp cs;
    | _ -> failwithf "Invalid result for completion %A!" result;

let isIncomplete (result : ParseResult) : bool =
    match result with
    | Incomplete _ -> true;
    | _ -> false;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = File.ReadAllLines(file)
                |> Seq.toList
                |> List.map Seq.toList
                |> List.map (parse []);

    input
    |> List.sumBy totalError
    |> printfn "Day 10, part 1: %d";

    let scores = input
                    |> List.filter isIncomplete
                    |> List.map complete
                    |> List.map (completionScore 0L)
                    |> List.sort;
    
    scores
    |> List.skip (List.length scores / 2)
    |> List.head
    |> printfn "Day 10, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
