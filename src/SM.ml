open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string with show
                                                   
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
        | LABEL _ -> (stack, (olds, inp, out))
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


let rec eval env ((stack, (_, _, _)) as config) pr = match pr with
        | []          -> config
        | head :: pr' -> match head with
            | JMP label    -> eval env config (env#labeled label)
            | CJMP(s, label) ->
                            (match stack with
                              | [] -> config
                              | l :: _ -> if ((s = "z") = (l = 0))
                                           then eval env config (env#labeled label)
                                           else eval env config pr')
        | _ -> eval env (evalComm config head) pr'

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o

let nameGenerator = object
    val mutable i = 0
    method name =
        i <- (i + 1);
        (Printf.sprintf "label%d" i)
end

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
  | Stmt.Skip -> []
  | Stmt.If (condition, thenBranch, elseBranch) ->
              let elseLabel = nameGenerator#name in
              let endLabel = nameGenerator#name in
              expr condition @ [CJMP ("z", elseLabel)] @ (compile thenBranch) @ [JMP endLabel] @ [LABEL elseLabel] @ (compile elseBranch) @ [LABEL endLabel]
 | Stmt.While (condition, body) ->
         let beginLabel = nameGenerator#name in
         let endLabel = nameGenerator#name in
         [LABEL beginLabel] @ (expr condition) @ [CJMP ("z", endLabel)] @ (compile body) @ [JMP beginLabel] @ [LABEL endLabel]
 | Stmt.Repeat (body, condition) ->
         let beginLabel = nameGenerator#name in
         [LABEL beginLabel] @ compile body @ expr condition @ [CJMP ("z", beginLabel)]