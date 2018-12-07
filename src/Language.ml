(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators

(* States *)
module State =
  struct

    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let empty =
      let e x = failwith (Printf.sprintf "Undefined variable: %s" x) in
      {g = e; l = e; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let u x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope then {s with l = u x v s.l} else {s with g = u x v s.g}

    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = (if List.mem x s.scope then s.l else s.g) x

    (* Creates a new scope, based on a given state *)
    let enter st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let leave st st' = {st' with g = st.g}

  end

(* Simple expressions: syntax and semantics *)
module Expr =
  struct

    (* The type for expressions. Note, in regular OCaml there is no "@type..."
       notation, it came from GT.
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t
    (* function call    *) | Call  of string * t list with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)

    (* The type of configuration: a state, an input stream, an output stream, an optional value *)
    type config = State.t * int list * int list * int option

    (* Expression evaluator

          val eval : env -> config -> t -> int * config


       Takes an environment, a configuration and an expresion, and returns another configuration. The
       environment supplies the following method

           method definition : env -> string -> int list -> config -> config

       which takes an environment (of the same type), a name of the function, a list of actual parameters and a configuration,
       an returns a pair: the return value for the call and the resulting configuration
    *)

    let b2i b = if b then 1 else 0
    let i2b i = if (i == 0) then false else true

    let to_func op =
      let bti   = function true -> 1 | _ -> 0 in
      let itb b = b <> 0 in
      let (|>) f g   = fun x y -> f (g x y) in
      match op with
      | "+"  -> (+)
      | "-"  -> (-)
      | "*"  -> ( * )
      | "/"  -> (/)
      | "%"  -> (mod)
      | "<"  -> bti |> (< )
      | "<=" -> bti |> (<=)
      | ">"  -> bti |> (> )
      | ">=" -> bti |> (>=)
      | "==" -> bti |> (= )
      | "!=" -> bti |> (<>)
      | "&&" -> fun x y -> bti (itb x && itb y)
      | "!!" -> fun x y -> bti (itb x || itb y)
      | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)

    let rec eval env ((st, i, o, _) as conf) expr =
      match expr with
      | Const n -> n, (st, i, o, Some n)
      | Var   x -> (State.eval st x), (st, i, o, Some ((State.eval st x)))
      | Binop (op, x, y) ->  let a, config = eval env conf x in
                             let b, (st', i', o', _) = eval env config y in
                             let res = to_func op a b in res, (st', i', o', Some res)
      | Call (f, arg) ->
        let eval' env conf expr = List.fold_left (fun (con, xs) ex -> let x, con' = eval env con ex in (con', xs @ [x])) (conf, []) expr in
        let (con, res) = eval' env conf arg in
        let (st, i, o, Some x) = env#definition env f res con in x, (st, i, o, Some x)


    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
    *)
    ostap (
      parse:
	  !(Ostap.Util.expr
             (fun x -> x)
	     (Array.map (fun (a, s) -> a,
                           List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
                        )
              [|
		`Lefta, ["!!"];
		`Lefta, ["&&"];
		`Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
		`Lefta, ["+" ; "-"];
		`Lefta, ["*" ; "/"; "%"];
              |]
	     )
	     primary);

      primary:
        n:DECIMAL {Const n}
      | f:IDENT "(" arg:!(Ostap.Util.list0)[parse] ")" { Call (f, arg) }
      | x:IDENT   {Var x}
      | -"(" parse -")"
    )

  end

(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* return statement                 *) | Return of Expr.t option
    (* call a procedure                 *) | Call   of string * Expr.t list with show

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The
       environment is the same as for expressions
    *)

    let seq a b = match b with
        | Skip -> a
        | b -> Seq (a, b)

    let rec eval env ((state, input, output, _) as config) k stm = match stm with
        | Seq (x, y) -> eval env config (seq y k) x
        | Assign (x, e) ->
            let a, (state', input', output', _) = Expr.eval env config e in
            let res = ((State.update x a state'), input', output', None) in
            eval env res Skip k
        | Read x -> (match input with
                    | y :: ys -> eval env (State.update x y state, ys, output, None) Skip k)
        | Write e -> let x, (state', input', output', _) = Expr.eval env config e in
              eval env (state', input', output' @ [x], None) Skip k
        | Skip -> (if k == Skip then config else eval env config Skip k)
        | If (condition, thenBranch, elseBranch) ->
            let x, y = Expr.eval env config condition in
            (match (Expr.i2b x, y) with
            | (true, (state', input', output', _)) -> eval env (state', input', output', None) k thenBranch
            | (false, (state', input', output', _)) -> eval env (state', input', output', None) k elseBranch)
        | While (condition, body) ->
            let x, y = Expr.eval env config condition in
            (match (Expr.i2b x, y) with
            | (true, (state', input', output', _)) -> eval env (state', input', output', None) (seq stm k) body
            | (false, (state', input', output', _)) -> eval env (state', input', output', None) Skip k)
        | Repeat (body, condition) ->
            eval env config (seq (While (Expr.Binop ("==", condition, Const 0), body)) k) body
        | Call (f, arg) ->
            let eval' env conf expr = List.fold_left (fun (con, xs) ex -> let x, con' = Expr.eval env con ex in (con', xs @ [x])) (conf, []) expr in
            let ((st, i, o, x), xs) = eval' env config arg in
            eval env (env#definition env f xs (st, i, o, None)) Skip k
        | Return x -> (match x with
            | None -> config
            | Some expr -> let _, s = Expr.eval env config expr in s)


    (* Statement parser *)
    ostap (

        elif_stmt:
            condition:!(Expr.parse) "then" thenBranch:parse "elif" elifBranch:elif_stmt
                {If (condition, thenBranch, elifBranch)}
            | condition:!(Expr.parse) "then" thenBranch:parse "else" elseBranch:parse
                 {If (condition, thenBranch, elseBranch)}
            | condition:!(Expr.parse) "then" thenBranch:parse
                {If (condition, thenBranch, Skip)};

        simple_stmt:
              x:IDENT ":=" e:!(Expr.parse)                       {Assign(x, e)}
              | "read" "(" x:IDENT ")"                           {Read(x)}
              | "write" "(" e:!(Expr.parse) ")"                   {Write(e)}
              | "skip"                                           {Skip}
              | "if" condition:!(Expr.parse) "then" thenBranch:parse "elif" elifBranch:elif_stmt "fi"
                    {If (condition, thenBranch, elifBranch)}
              | "if" condition:!(Expr.parse) "then" thenBranch:parse "else" elseBranch:parse "fi"
                    {If (condition, thenBranch, elseBranch)}
              | "if" condition:!(Expr.parse) "then" thenBranch:parse "fi"
                    {If (condition, thenBranch, Skip)}
              | "while" condition:!(Expr.parse)
                "do" body:parse
                "od"
                {While (condition, body)}
              | "repeat" body:parse
                "until" condition:!(Expr.parse)
                {Repeat (body, condition)}
              | "for" init:simple_stmt
                "," condition:!(Expr.parse)
                "," inc:simple_stmt
                "do" body:parse
                "od"
                {Seq(init, While(condition, Seq(body, inc)))}
              | "return" x:!(Expr.parse)?
                { Return  x }
              | name:IDENT "(" args:!(Ostap.Util.list0)[Expr.parse] ")" { Call (name, args) };

        parse: <s::ss> : !(Ostap.Util.listBy)[ostap (";")][simple_stmt] {List.fold_left (fun s ss -> Seq (s, ss)) s ss}
    )

  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (
      arg  : IDENT;
      parse: %"fun" name:IDENT "(" args:!(Util.list0 arg) ")"
         locs:(%"local" !(Util.list arg))?
        "{" body:!(Stmt.parse) "}" {
        (name, (args, (match locs with None -> [] | Some l -> l), body))
      }
    )

  end

(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let module M = Map.Make (String) in
  let m          = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in
  let _, _, o, _ =
    Stmt.eval
      (object
         method definition env f args (st, i, o, r) =
           let xs, locs, s      = snd @@ M.find f m in
           let st'              = List.fold_left (fun st (x, a) -> State.update x a st) (State.enter st (xs @ locs)) (List.combine xs args) in
           let st'', i', o', r' = Stmt.eval env (st', i, o, r) Stmt.Skip s in
           (State.leave st'' st, i', o', r')
       end)
      (State.empty, i, [], None)
      Stmt.Skip
      body
  in
  o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))