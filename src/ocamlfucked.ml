open Format

(********************************* Type defs **********************************)
type instr =
      Nop 
    | Next
    | Prev
    | Plus
    | Minus
    | Input
    | Output
    | Loop of instr list

(* A tape is a tuple (['a], ['a]). *)
type 'a tape = 'a list * 'a list
(* TODO Maybe create type alias type Mem = char tape*)

(*************************** Useful helper functions **************************)
(* note to self: `fun` is necessary not to bind value once *)
let read_char = fun () -> String.get (read_line()) 0 
let (+) (c : char) (i : int) : char = Char.chr (Char.code c + i) 
let (-) (c : char) (i : int) : char = Char.chr (Char.code c - i) 

(**************************** Executes a program ******************************)
let rec exec (program : instr list) (memory : char tape) : char tape =
    match program, memory with
           Nop::xs,       _          -> exec xs memory
        |  Next::xs,     (l,c::r)    -> exec xs (c::l, r)
        |  Prev::xs,     (c::l, r)   -> exec xs (l, c::r)
        |  Plus::xs,     (l, c::r)   -> exec xs (l, (c + 1)::r)
        |  Minus::xs,    (l, c::r)   -> exec xs (l, (c - 1)::r)
        |  Input::xs,    (l, c::r)   -> exec xs (l, read_char()::r)
        |  Output::xs,   (_, c::_)   -> printf "%c" c; exec xs memory
        |  (Loop l)::xs, (_, c::_)   -> 
            if Char.code c == 0x00 then
                exec xs memory
            else
                let n_mem = exec l memory in
                    exec program n_mem
        |  [],            _          -> memory
        |  _,             _          -> eprintf "Something went wrong. Terminating.\n"; memory


(************************************** run ***********************************)
(* initalize program list (temp) and memory tape *)
let program : instr list = [Nop;Next;Plus;Input;Plus;Output;Next;Loop [Input; Minus];Next]
let memory : char tape = ([], List.init 100 (fun x -> Char.chr 65));; (*TODO change to 0*)

exec program memory;;

