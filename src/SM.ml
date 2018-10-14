open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)
 let evalComm (stack, (olds, inp, out)) comm = match comm with
        | CONST x  -> (x :: stack, (olds, inp, out))
        | BINOP op -> begin match stack with
                        | x1 :: x2 :: stack' ->
                            let res = Language.Expr.eval
                                    Language.Expr.empty
                                        (Language.Expr.Binop op
                                            (Language.Expr.Const x2)
                                            (Language.Expr.Const x1)
                                        ) in
                                (res :: stack', (olds, inp, out))
                        | _                 -> failwith "Too few values at stack, can't eval BINOP"
                      end

        | READ     -> begin match inp with
                        | x :: inp' -> (x :: stack, (olds, inp', out))
                        | []          -> failwith "Too few values to READ"
                      end

        | WRITE    -> begin match stack with
                        | x :: stack' -> (stack', (olds, inp, out @ [x]))
                        | []           -> failwith "Too few values at stack, can't WRITE"
                      end

        | LD v   -> (olds v :: stack, (olds, inp, out))
        | ST v   -> begin match stack with
                        | x :: stack' -> (stack', (Language.Expr.update v x olds, inp, out))
                        | []           -> failwith "Too few values at stack"
                      end


let rec eval conf pr = match pr with
        | head :: pr' -> eval (evalComm conf head) pr'
        | []                 -> conf

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in
  function
  | Stmt.Seq (s1, s2)  -> compile s1 @ compile s2
  | Stmt.Read x        -> [READ; ST x]
  | Stmt.Write e       -> expr e @ [WRITE]
  | Stmt.Assign (x, e) -> expr e @ [ST x]
