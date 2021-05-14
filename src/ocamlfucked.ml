open Format

(********************************* Type defs **********************************)
type instr =
      Next of int
    | Prev of int
    | Plus of int
    | Minus of int
    | Input
    | Output
    | Loop of instr list

(* A tape is a tuple (['a], ['a]). *)
type 'a tape = 'a list * 'a list

(*************************** Useful helper functions **************************)
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

(***************************** Execute a program ******************************)

(* exec :: [instr] -> char tape -> char tape. Executes program on a memory tape. 
   Returns the resulting memory. *) 
let rec exec (program : instr list) (memory : char tape) : char tape =
    (* IO *)
    let read_char () = Scanf.scanf "%c" (fun c -> c) in
    let put_char c = printf "%c%!" c in

    (* arithmetic on chars *)
    (* Comment: Please don't laugh. I know this ugly & unsafe*)
    let (++) (c : char) (i : int) : char = Char.chr ((Char.code c + i + 256) mod 256) in 
    let (--) (c : char) (i : int) : char = Char.chr ((Char.code c - i + 256) mod 256) in

    match program, memory with
           (Next i)::xs,  (l, r)      -> exec xs (List.rev_append (take i r) l, drop i r)
        |  (Prev i)::xs,  (l, r)      -> exec xs (drop i l, List.rev_append (take i l) r)
        |  (Plus i)::xs,  (l, c::r)   -> exec xs (l, (c ++ i)::r)
        |  (Minus i)::xs, (l, c::r)   -> exec xs (l, (c -- i)::r)
        |  Input::xs,     (l, c::r)   -> exec xs (l, read_char()::r)
        |  Output::xs,    (_, c::_)   -> put_char c; exec xs memory
        |  (Loop l)::xs,  (_, c::_)   -> 
            if Char.code c == 0 then
                exec xs memory
            else
                let n_mem = exec l memory in
                    exec program n_mem
        |  [],            _          -> memory
        |  _,             _          -> eprintf "Something went wrong. Terminating.\n"; memory


(************************ Read & parse a .bf file *****************************)

(* read_file :: in_channel -> [char]. Reads chars from file into a list *) 
let read_file (f : in_channel) : char list =
    let rec read_file (cl : char list) (f : in_channel) : char list =
        try
            let c = input_char f in
                read_file (c::cl) f
        with
            End_of_file -> cl
    in
        List.rev (read_file [] f)

(* group :: [instr] -> [instr]. Groups adjacent instructions of the same kind*) 
let rec group (instrs : instr list) : instr list =
    let f = fun acc elem -> match acc, elem with
          (Next i)::xs, (Next j)   -> (Next (i+j))::xs
        | (Prev i)::xs, (Prev j)   -> (Prev (i+j))::xs
        | (Plus i)::xs, (Plus j)   -> (Plus (i+j))::xs
        | (Minus i)::xs, (Minus j) -> (Minus (i+j))::xs
        | acc, (Loop loop_instrs)  -> (Loop (group loop_instrs))::acc
        | acc, elem                -> elem::acc 
    in
        List.rev (List.fold_left f [] instrs)

(* parse :: [char] -> [instr]. Parses a list of chars into instructions*) 
let parse (tokens : char list) : instr list =
    let rec parse_chars (tokens : char list) (instrs : instr list) : instr list * char list =
        match tokens with
              '>'::xs -> parse_chars xs ((Next 1)::instrs)
            | '<'::xs -> parse_chars xs ((Prev 1)::instrs)
            | '+'::xs -> parse_chars xs ((Plus 1)::instrs)
            | '-'::xs -> parse_chars xs ((Minus 1)::instrs)
            | ','::xs -> parse_chars xs (Input::instrs)
            | '.'::xs -> parse_chars xs (Output::instrs)
            | '['::xs -> 
                    let pl = parse_chars xs [] in
                    let remaining_instrs = snd pl in
                    let loop = Loop (List.rev (fst pl)) in
                    parse_chars remaining_instrs (loop::instrs) 
            | ']'::xs -> (instrs, xs)
            | []      -> (instrs, [])
            |  _::xs  -> parse_chars xs instrs
    in
        List.rev (fst (parse_chars tokens []))

(************************************** run ***********************************)
(* TODO Support for command line args. E.g $./ocamlfucked test/hanoi.b*)
let memory : char tape = ([], List.init 10000 (fun x -> Char.chr 0));;
let file = open_in "test/mandelbrot.b" in
let chars = read_file file in
let program1 = parse chars in
let program2 = group program1 in
exec program2 memory
