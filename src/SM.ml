open GT
open Syntax

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
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)
let eval config instructions =
    let evalHelper conf instr = match conf, instr with
        | (stack, conf), (CONST c) -> (c :: stack, conf)
        | (x :: xs, (state, input, output)), (WRITE) -> (xs, (state, input, x :: output))
        | (stack, (state, (x :: input), output)), (READ) -> (x :: stack, (state, input, output))
        | (stack, ((state, _, _) as conf)), (LD x) -> ((state x) :: stack, conf)
        | ((x :: xs), (state, input, output)), (ST y) -> (xs, (Expr.update y x state, input, output))
        | (x :: y :: zs, conf), (BINOP op) -> ((Expr.evalB op y x) :: zs, conf)
    in List.fold_left evalHelper config instructions

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile stmt =
    let rec compileHelper expr = match expr with
        | Expr.Const x -> [CONST x]
        | Expr.Var x -> [LD x]
        | Expr.Binop (op, x, y) -> compileHelper x @ compileHelper y @ [BINOP op]
    in match stmt with
        | Stmt.Seq (x, y) -> compile x @ compile y
        | Stmt.Assign (x, e) -> compileHelper e @ [ST x]
        | Stmt.Read x -> [READ; ST x]
        | Stmt.Write e -> compileHelper e @ [WRITE]
