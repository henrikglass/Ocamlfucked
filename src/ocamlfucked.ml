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
(* TODO Maybe create type alias type Mem = char tape*)

(*************************** Useful helper functions **************************)
(* note to self: `fun` is necessary not to bind value once *)
(* note to self: Format.print_string & Stdlib.print_string behave differently*)

let rec print_int_list (l : int list) =
    match l with
          [] -> printf "\n"
        | _  -> printf "%d" (List.hd l); print_int_list (List.tl l)

let rec take_helper (n : int) (src : 'a list)  (tgt : 'a list): 'a list =
    match n with
          0 -> tgt
        | _ -> take_helper (n-1) (List.tl src) ((List.hd src)::tgt)
let take (n : int) (src : 'a list) : 'a list = List.rev (take_helper n src []) 
let rec drop (n : int) (l : 'a list) : 'a list =
    match n with
          0 -> l
        | _ -> drop (n-1) (List.tl l)

let read_char = fun () -> String.get (read_line()) 0 
let put_char = fun c -> Stdlib.print_string (String.make 1 c)  
let (+) (c : char) (i : int) : char = Char.chr (Char.code c + i) 
let (-) (c : char) (i : int) : char = Char.chr (Char.code c - i)

(**************************** Executes a program ******************************)
let rec exec (program : instr list) (memory : char tape) : char tape =
    match program, memory with
           (Next i)::xs,  (l, r)      -> exec xs (List.rev_append (take i r) l, drop i r)
        |  (Prev i)::xs,  (l, r)      -> exec xs (drop i l, List.rev_append (take i l) r)
        |  (Plus i)::xs,  (l, c::r)   -> exec xs (l, (c + i)::r)
        |  (Minus i)::xs, (l, c::r)   -> exec xs (l, (c - i)::r)
        |  Input::xs,     (l, c::r)   -> exec xs (l, read_char()::r)
        |  Output::xs,    (_, c::_)   -> put_char c; exec xs memory
        |  (Loop l)::xs,  (_, c::_)   -> 
            if Char.code c == 65 then
                exec xs memory
            else
                let n_mem = exec l memory in
                    exec program n_mem
        |  [],            _          -> memory
        |  _,             _          -> eprintf "Something went wrong. Terminating.\n"; memory


(************************ Read & parse a .bf file *****************************)

let read_file (f : in_channel) : char list =
    let rec read_file (cl : char list) (f : in_channel) : char list =
        try
            let c = input_char f in
                read_file (c::cl) f
        with
            End_of_file -> cl
    in
        List.rev (read_file [] f)

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

(************************************* debug **********************************)
let pretty_print (program : instr list) = 
    let rec pretty_print (program : instr list) (indent : string) = 
        print_string ("\n" ^ indent); 
        match program with
              []                 -> ()
            | (Next i)::xs       -> printf "Next %d" i; pretty_print xs indent
            | (Prev i)::xs       -> printf "Prev %d" i; pretty_print xs indent 
            | (Plus i)::xs       -> printf "Plus %d" i; pretty_print xs indent 
            | (Minus i)::xs      -> printf "Minus %d" i; pretty_print xs indent 
            | Input::xs          -> printf "Input"; pretty_print xs indent 
            | Output::xs         -> printf "Output"; pretty_print xs indent 
            | (Loop instrs)::xs  -> printf "Loop ["; 
                    pretty_print instrs ("  " ^ indent); 
                    printf "]";
                    pretty_print xs indent
    in
        pretty_print program ""

(************************************** run ***********************************)
(* initalize program list (temp) and memory tape *)
let program : instr list = [Next 1;Plus 4;Input;Output;Next 1;Input;Output;Loop [Input; Minus 2];Next 1]
let memory : char tape = ([], List.init 100 (fun x -> Char.chr 75));; (*TODO change to 0*)

(*print_int_list (take 3 [1;2;3;4;5]);
print_int_list (drop 3 [1;2;3;4;5]);
exec program memory;;*)
let file = open_in "test/test2.bf" in
let chars = read_file file in
let program1 = parse chars in
pretty_print program1;
exec program1 memory
