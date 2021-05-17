(* Util module *)

(* Equivalent to Haskell's take *)
let take (n : int) (src : 'a list) : 'a list =
    let rec f n src tgt = match n with
          0 -> tgt
        | _ -> f (n - 1) (List.tl src) ((List.hd src)::tgt) 
    in
        List.rev (f n src [])

(* Equivalent to Haskell's drop *)
let rec drop (n : int) (l : 'a list) : 'a list =
    match n with
          0 -> l
        | _ -> drop (n-1) (List.tl l)
