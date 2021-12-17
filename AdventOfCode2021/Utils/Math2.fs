module Math2

// convert a hecidecimal character to its binary representation
let hexToBin (input : char) : int list =
    match input with
    | '0' -> [0; 0; 0; 0];
    | '1' -> [0; 0; 0; 1];
    | '2' -> [0; 0; 1; 0];
    | '3' -> [0; 0; 1; 1];
    | '4' -> [0; 1; 0; 0];
    | '5' -> [0; 1; 0; 1];
    | '6' -> [0; 1; 1; 0];
    | '7' -> [0; 1; 1; 1];
    | '8' -> [1; 0; 0; 0];
    | '9' -> [1; 0; 0; 1];
    | 'A' -> [1; 0; 1; 0];
    | 'B' -> [1; 0; 1; 1];
    | 'C' -> [1; 1; 0; 0];
    | 'D' -> [1; 1; 0; 1];
    | 'E' -> [1; 1; 1; 0];
    | 'F' -> [1; 1; 1; 1];
    | _ -> failwithf "Invalid hex input '%c'" input;

// convert a binary number represented as a big-end list of integers to a decimal integer
let binToDec (input : int list) : int =
    let rec b2d (total : int) (mul : int) (input : int list) : int =
        match input with
        | [] -> total;
        | x::xs -> b2d (total + (mul * x)) (mul * 2) xs;

    input |> List.rev |> b2d 0 1;
        
// convert a binary number represented as a big-end list of integers to a decimal long integer
let binToLong (input : int list) : int64 =
    let rec b2d (total : int64) (mul : int64) (input : int list) : int64 =
        match input with
        | [] -> total;
        | x::xs -> b2d (total + (mul * int64 x)) (mul * 2L) xs;

    input |> List.rev |> b2d 0L 1L

// calculate the nth triangular number
let rec triangular (n : int) : int =
    n * (n + 1) / 2;