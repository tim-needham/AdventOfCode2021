module Day16

open Math2;
open Packet;
open System.Diagnostics;
open System.IO;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = File.ReadAllLines(file)
                |> Seq.toList
                |> List.collect Seq.toList
                |> List.collect hexToBin;

    let packets = input
                    |> parsePackets;

    packets                        
    |> sumVersions
    |> printfn "Day 16, part 1: %d";

    packets
    |> List.map evalPacket
    |> List.head
    |> printfn "Day 16, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
