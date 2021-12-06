module Memo

// memoize a given function
let memoize (f : 'TArg -> 'TResult) : ('TArg -> 'TResult) =
    let cache = new System.Collections.Generic.Dictionary<_, _>();

    (fun x ->
        match cache.TryGetValue x with
        | true, v -> v;
        | false, _ ->   let v = f x;
                        cache.Add(x, v);
                        v);

// memoize only works on functions with one parameter so use this overload for 2 parameters
let memoize2 (f': 'TArg1 -> 'TArg2 -> 'TResult) : ('TArg1 -> 'TArg2 -> 'TResult) =
    let f = memoize (fun (a, b) -> f' a b);
    (fun a b -> f(a, b));

// memoize only works on functions with one parameter so use this overload for 3 parameters
let memoize3 (f': 'TArg1 -> 'TArg2 -> 'TArg3 -> 'TResult) : ('TArg1 -> 'TArg2 -> 'TArg3 -> 'TResult) =
    let f = memoize (fun (a, b, c) -> f' a b c);
    (fun a b c -> f(a, b, c));