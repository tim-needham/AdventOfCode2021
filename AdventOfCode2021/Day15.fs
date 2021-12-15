module Day15

open System;
open System.Diagnostics;
open System.IO;

let parse (input : string) : int list =
    input
    |> Seq.toList
    |> List.map (fun c -> Int32.Parse (c.ToString()));

let expandGrid (grid : int list list) : int list list =
    [ for j in 0..(5 * grid.Length) - 1 ->
        [ for i in 0..(5 * grid.[0].Length) - 1 ->
            let k = (i / grid.[0].Length) + (j / grid.Length);
            let v = (k + grid.[j%grid.Length].[i%grid.[0].Length]);
            if v > 9 then
                v - 9;
            else
                v;
        ]
    ];

let neighbours (grid : int list list) ((px, py) : int * int) : seq<int * int> =
    seq {
        if px > 0 then
            yield (px - 1, py);
        if px < grid.[0].Length - 1 then
            yield (px + 1, py);
        if py > 0 then
            yield (px, py - 1);
        if py < grid.Length - 1 then
            yield (px, py + 1);
    };

// in this case the cost is only dependent upon the target square
let cost (grid : int list list) ((ax, ay) : int * int) ((bx, by) : int * int) : int =
    grid.[by].[bx];

let estimate ((ax, ay) : int * int) ((bx, by) : int * int) : int =
    Math.Abs(ax - bx) + Math.Abs(ay - by);

let searchAStar (start : int * int)
                (target : int * int)
                (neighbours : (int * int) -> seq<int * int>)
                (calcCost : (int * int) -> (int * int) -> int)
                (calcEstimate : (int * int) -> (int * int) -> int)
                : seq<int * int> =

    let rec crawlPath (paths : Map<int * int, int * int>) (point : int * int) : seq<int * int> =
        seq {
            yield point;
            match Map.tryFind point paths with
            | None -> ();
            | Some prev -> yield! crawlPath paths prev;
        };

    let rec searchStep  (seen : Set<int *int>) 
                        (nexts : (int * int) list)
                        (costs : Map<int * int, int>)
                        (estimates : Map<int * int, int>) 
                        (previous : Map<int * int, int * int>) 
                        : seq<int * int> option =
        match nexts |> List.sortBy (fun n -> Map.find n estimates) with
        | curr::_ when curr = target -> crawlPath previous curr |> Seq.rev |> Some;
        | curr::cs ->   let cost = Map.find curr costs;
                        let (nexts', costs', estimates', previous') = 
                                neighbours curr
                                 |> Seq.filter (fun n -> not (Set.contains n seen))
                                 |> Seq.fold (fun (nxts, csts, ests, prevs) n ->
                                            let pCost = cost + calcCost curr n
                                            if (List.contains n nxts) && (pCost >= Map.find n csts) then
                                                (nxts, csts, ests, prevs);
                                            else
                                                let newNexts = if List.contains n nxts then nxts else n::nxts;
                                                let newCosts = Map.add n pCost csts;
                                                let newEsts = Map.add n (pCost + calcEstimate n target) ests;
                                                let newPrevs = Map.add n curr prevs;
                                                (newNexts, newCosts, newEsts, newPrevs);
                                 ) (cs, costs, estimates, previous);
                        searchStep (Set.add curr seen) nexts' costs' estimates' previous';
        | _ -> None;

    let initialCosts = Map.ofList [ (start, 0) ];
    let initialEsts = Map.ofList [ (start, calcEstimate start target )];
    match searchStep Set.empty [start] initialCosts initialEsts Map.empty with
    | Some p -> p;
    | None -> failwith "Unable to find path!";

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = File.ReadAllLines(file)
                |> Seq.toList
                |> List.map parse;


    let costs = cost input;
    let neigh = neighbours input;

    searchAStar (0, 0) (input.[0].Length - 1, input.Length - 1) neigh costs estimate
    |> Seq.tail
    |> Seq.fold (fun a p -> a + costs (0, 0) p) 0
    |> printfn "Day 15, part 1: %d";

    let fullGrid = expandGrid input;
    let costs2 = cost fullGrid;
    let neigh2 = neighbours fullGrid;

    searchAStar (0, 0) (fullGrid.[0].Length - 1, fullGrid.Length - 1) neigh2 costs2 estimate
    |> Seq.tail
    |> Seq.fold (fun a p -> a + costs2 (0, 0) p) 0
    |> printfn "Day 15, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
