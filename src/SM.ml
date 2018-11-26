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
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string list * string list
(* end procedure definition        *) | END
(* calls a procedure               *) | CALL  of string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list
                            
(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)

let evalComm (stack, (olds, inp, out, r)) comm = match comm with
        | LABEL _ -> (stack, (olds, inp, out, r))
        | CONST x  -> (x :: stack, (olds, inp, out, r))
        | BINOP op -> begin match stack with
                        | x1 :: x2 :: stack' -> ((Expr.to_func op x2 x1) :: stack', (olds, inp, out, r))
                        | _                 -> failwith "Too few values at stack, can't eval BINOP"
                      end

        | READ     -> begin match inp with
                        | x :: inp' -> (x :: stack, (olds, inp', out, r))
                        | []          -> failwith "Too few values to READ"
                      end

        | WRITE    -> begin match stack with
                        | x :: stack' -> (stack', (olds, inp, out @ [x], r))
                        | []           -> failwith "Too few values at stack, can't WRITE"
                      end

        | LD v   -> ((State.eval olds v) :: stack, (olds, inp, out, r))
        | ST v   -> begin match stack with
                        | x :: stack' -> (stack', ((State.update v (x) olds), inp, out, r))
                        | []           -> failwith "Too few values at stack"
                      end


let rec eval env ((c, stack, (st, input, output, r)) as config) pr = match pr with
        | []          -> config
        | head :: pr' -> match head with
            | JMP label    -> eval env config (env#labeled label)
            | CJMP(s, label) ->
                            (match stack with
                              | [] -> config
                              | l :: ls -> if ((s = "z") = (l = 0))
                                           then eval env (c, ls, (st, input, output, r)) (env#labeled label)
                                           else eval env (c, ls, (st, input, output, r)) pr')
            | CALL name ->
              let x = ((pr', st) :: c, stack, (st, input, output, r)) in eval env x (env#labeled name)
            | BEGIN (names, locals) ->
              let ss = State.enter st (names @ locals) in
              let (s', stack') = List.fold_left (fun (a, x::xs) n -> (State.update n x a, xs)) (ss, stack) names in
                    eval env (c, stack', (s', input, output, r)) pr'
            | END -> (match c with
              | [] -> config
              | ((p', st') :: xs)  -> eval env (xs, stack, (State.leave st st', input, output, r)) p')
            | _ ->
                let (ns, nsc) = evalComm (stack, (st, input, output, r)) head
                            in eval env (c, ns, nsc) pr'

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  (*print_prg p;*)
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, _, (_, _, o, _)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [], None)) p in o


let nameGen = object
    val mutable i = 0
    method name =
        i <- (i + 1);
        (Printf.sprintf "label%d" i)
end

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile (fs, pr) =
  let rec exprC = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> exprC x @ exprC y @ [BINOP op]
  | Expr.Call (f, arg) -> List.concat (List.map exprC (List.rev arg))  @ [CALL f]
  in
    let rec helper expr = match expr with
    | Stmt.Seq (s1, s2)  -> helper s1 @ helper s2
    | Stmt.Read x        -> [READ; ST x]
    | Stmt.Write e       -> exprC e @ [WRITE]
    | Stmt.Assign (x, e) -> exprC e @ [ST x]
    | Stmt.Skip -> []
    | Stmt.If (condition, thenBranch, elseBranch) ->
              let elseLabel = nameGen#name in
              let endLabel = nameGen#name in
              exprC condition @ [CJMP ("z", elseLabel)] @ (helper thenBranch) @ [JMP endLabel] @ [LABEL elseLabel] @ (helper elseBranch) @ [LABEL endLabel]
    | Stmt.While (condition, body) ->
         let beginLabel = nameGen#name in
         let endLabel = nameGen#name in
         [LABEL beginLabel] @ (exprC condition) @ [CJMP ("z", endLabel)] @ (helper body) @ [JMP beginLabel] @ [LABEL endLabel]
    | Stmt.Repeat (body, condition) ->
         let beginLabel = nameGen#name in
         [LABEL beginLabel] @ helper body @ exprC condition @ [CJMP ("z", beginLabel)]
    | Stmt.Call (names, arg) ->
        List.concat (List.map exprC arg) @ [CALL names]
    | Stmt.Return x -> (match x with
                    | None -> [END]
                    | Some x -> exprC x @ [END])
    in let label = nameGen#name in
    let compile' (n, (p, l, b)) =
          [LABEL n] @ [BEGIN (p, l)] @ helper b @ [END] in
    [JMP label] @ List.concat (List.map compile' fs) @ [LABEL label] @ (helper pr)
