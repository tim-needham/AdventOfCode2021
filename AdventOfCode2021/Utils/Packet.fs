module Packet

open Math2;

// recursive type, models a Packet per day 16
type Packet = {
        Version : int;
        Type : PacketType;
    }
    and PacketType =
        | Sum of Packet list
        | Product of Packet list
        | Minimum of Packet list
        | Maximum of Packet list
        | Literal of int64
        | GreaterThan of Packet list
        | LessThan of Packet list
        | EqualTo of Packet list
    ;;

let parsePackets (input : int list) : Packet list =

    // packets : all the top-level packets in the current hierarchy
    // limit : a value of None gives unbounded parsing, Some n parses exactly n packets from the input
    // input : the remaining binary input
    let rec parsePacket (packets : Packet list) (limit : int option) (input : int list) : Packet list * int list =
        // return the next limit value
        let newLimit (limit : int option) : int option =
            match limit with
            | None -> None;
            | Some l -> Some (l - 1);

        // return the packet version, packet type and remainder
        let packetHeader (input : int list) : int * int * int list =
            input
            |> List.splitAt 3
            |> (fun (a, b) -> binToDec a, List.splitAt 3 b)
            |> (fun (a, (b, c)) -> a, binToDec b, c);

        // gather all the groups of bits for a literal packet value
        let rec parseLiteral (value : int list) (input : int list) : int list * int list =
            let group = List.take 5 input
            match List.head group, List.tail group with
            | 0, tail -> (value @ tail), List.skip 5 input;
            | 1, tail -> parseLiteral (value @ tail) (List.skip 5 input);
            | _, _ -> failwith "Invalid input";

        match List.length input with
        // as some packets have padding, bail out if there isn't enough remaning input to parse a whole packet
        | n when n < 8 -> (packets, input);
        | _ ->  match limit with
                | Some 0 -> (packets, input);
                | _ ->  let l = newLimit limit;
                        match packetHeader input with
                        | v, 0, r ->    let ps, remainder = parseSubPackets r;
                                        { Version = v; Type = Sum ps }, remainder;
                        | v, 1, r ->    let ps, remainder = parseSubPackets r;
                                        { Version = v; Type = Product ps }, remainder;
                        | v, 2, r ->    let ps, remainder = parseSubPackets r;
                                        { Version = v; Type = Minimum ps }, remainder;
                        | v, 3, r ->    let ps, remainder = parseSubPackets r;
                                        { Version = v; Type = Maximum ps }, remainder;
                        | v, 4, r ->    let value, remainder = parseLiteral [] r;
                                        { Version = v; Type = Literal (binToLong value) }, remainder;
                        | v, 5, r ->    let ps, remainder = parseSubPackets r;
                                        { Version = v; Type = GreaterThan ps }, remainder;
                        | v, 6, r ->    let ps, remainder = parseSubPackets r;
                                        { Version = v; Type = LessThan ps }, remainder;
                        | v, 7, r ->    let ps, remainder = parseSubPackets r;
                                        { Version = v; Type = EqualTo ps }, remainder;
                        | _, _, _ -> failwith "Parse error in packet!";
                        |> (fun (p, r) -> parsePacket (packets @ [p]) l r);
    and parseSubPackets (input : int list) : Packet list * int list =
        match List.splitAt 1 input with
        | [0], tail ->  tail
                        |> List.splitAt 15
                        |> (fun (a, b) -> binToDec a, b)
                        ||> List.splitAt
                        |> (fun (a, b) -> parsePacket [] None a |> fst, b);
        | [1], tail ->  tail
                        |> List.skip 11
                        |> parsePacket [] (tail |> List.take 11 |> binToDec |> Some);
        | _, _ -> failwith "Parse error in sub-packets!";

    parsePacket [] None input
    |> fst;

let rec sumVersions (packets : Packet list) : int =
    packets
    |> List.fold (fun a p ->    a + p.Version + 
                                match p.Type with
                                | Literal _ -> 0;
                                | Sum ps | Product ps | Minimum ps | Maximum ps | GreaterThan ps | LessThan ps | EqualTo ps -> sumVersions ps;
                ) 0;

let rec evalPacket (packet : Packet) : int64 =
    match packet.Type with
    | Literal n -> n;
    | Sum ps -> ps |> List.map evalPacket |> List.fold (fun a p -> a + p) 0L;
    | Product ps -> ps |> List.map evalPacket |> List.fold (fun a p -> a * p) 1L;
    | Minimum ps -> ps |> List.map evalPacket |> List.min;
    | Maximum ps -> ps |> List.map evalPacket |> List.max;
    | GreaterThan [p; q] -> if evalPacket p > evalPacket q then 1L else 0L;
    | LessThan [p; q] -> if evalPacket p < evalPacket q then 1L else 0L;
    | EqualTo [p; q] -> if evalPacket p = evalPacket q then 1L else 0L;
    | _ -> failwith "Invalid packet format!";
