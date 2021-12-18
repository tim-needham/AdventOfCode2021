module Day18

open System;
open System.Diagnostics;
open System.IO;

type Node = 
    | Num of int
    | Parent of Node * Node
;;

let rec prettyPrint (node : Node) : string =
    match node with
    | Num n -> string n;
    | Parent (x, y) ->  "[" + (prettyPrint x) + "," + (prettyPrint y) + "]";

let rec parse (parts : Node list) (input : char list) : Node =
    match input with
    | [] -> parts |> List.head;
    | c::cs when Char.IsDigit c ->  parse ((c |> Char.ToString |> Int32.Parse |> Num)::parts) cs;
    | '['::cs -> parse parts cs;
    | ']'::cs ->    let es, ps = List.splitAt 2 parts;
                    parse (Parent (es.[1], es.[0])::ps) cs;
    | ','::cs -> parse parts cs;  
    | c::_ -> failwithf "Unexpected token in parse input '%c'" c;
    
let rec reduce (node : Node) : Node =
    let rec descendLeft (value : int) (node : Node) : Node =
        match node with
        | Num n -> Num (n + value);
        | Parent (x, y) -> Parent (descendLeft value x, y);

    let rec descendRight (value : int) (node : Node) : Node =
        match node with
        | Num n -> Num (n + value);
        | Parent (x, y) -> Parent (x, descendRight value y);

    let rec explode (depth : int) (currentDepth : int) (node : Node) : Node * bool * int * int =
        match (depth = currentDepth), node with
        | false, Num _ -> node, false, 0, 0;
        | false, Parent (x, y) ->   match explode depth (currentDepth + 1) x with
                                    | n, true, a, b -> Parent (n, descendLeft b y), true, a, 0;
                                    | n, false, _, _ -> match explode depth (currentDepth + 1) y with
                                                        | o, true, a, b -> Parent (descendRight a n, o), true, 0, b;
                                                        | o, false, _, _ -> Parent (n, o), false, 0, 0;
        | true, Num _ -> node, false, 0, 0;
        // guaranteed to be a Parent (Num, Num) apparently...
        | true, Parent (Num a, Num b) -> Num 0, true, a, b;
        | true, _ -> failwithf "Invalid snailfish number!";

    let rec split (node : Node) : Node * bool =
        match node with
        | Num n when n > 9 -> Parent (Num (n/2), Num (int (Math.Ceiling((decimal n)/2m)))), true;
        | Num _ -> node, false;
        | Parent (x, y) ->  match split x with
                            | n, true -> Parent (n, y), true;
                            | n, false ->   let o, splt = split y;
                                            Parent (n, o), splt;

    match explode 4 0 node with
    | n, true, _, _ -> reduce n;
    | _, false, _, _ -> match split node with
                        | o, true -> reduce o;
                        | o, false -> o;

let add (a : Node) (b : Node) : Node =
    Parent (a, b)
    |> reduce;

let rec magnitude (node : Node) : int =
    match node with
    | Num n -> n;
    | Parent (x, y) -> (3 * magnitude x) + (2 * magnitude y);

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = File.ReadAllLines(file)
                |> Seq.toList
                |> List.map (Seq.toList >> parse []);

    input
    |> (fun is -> List.head is, List.tail is)
    ||> List.fold (fun a c -> add a c)
    |> magnitude
    |> printfn "Day 18, part 1: %d";

    (input, input)
    ||> List.allPairs
    |> List.map (fun (a, b) -> add a b)
    |> List.map magnitude
    |> List.max
    |> printfn "Day 18, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
