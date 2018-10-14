(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)

    let b2i b = if b then 1 else 0
    let i2b i = if (i == 0) then false else true

    let rec evalB op x y = match op with
        | "+" -> x + y
        | "-" -> x - y
        | "*" -> x * y
        | "/" -> x / y
        | "%" -> x mod y
        | "<" -> b2i(x < y)
        | "<=" -> b2i(x <= y)
        | ">" -> b2i(x > y)
        | ">=" -> b2i(x >= y)
        | "==" -> b2i(x == y)
        | "!=" -> b2i(x <> y)
        | "&&" -> b2i(i2b x && i2b y)
        | "!!" -> b2i(i2b x || i2b y)

    let rec eval state expr = match expr with
        | Const x -> x
        | Var x -> state x
        | Binop (op, x, y) -> evalB op (eval state x) (eval state y)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    (* https://github.com/dboulytchev/ostap/blob/master/sample/Sample.ml#L188 *)
    ostap (
        expr:
            !(Ostap.Util.expr
                (fun x -> x)
                [|
                    `Lefta , [
                            ostap ("!!"), (fun x y -> Binop ("!!", x, y))];
                    `Lefta , [
                            ostap ("&&"), (fun x y -> Binop ("&&", x, y))];
                    `Nona , [
                            ostap ("=="), (fun x y -> Binop ("==", x, y));
                            ostap ("!="), (fun x y -> Binop ("!=", x, y));
                            ostap (">="), (fun x y -> Binop (">=", x, y));
                            ostap ("<="), (fun x y -> Binop ("<=", x, y));
                            ostap (">"), (fun x y -> Binop (">", x, y));
                            ostap ("<"), (fun x y -> Binop ("<", x, y))];
                    `Lefta , [
                            ostap ("-"), (fun x y -> Binop ("-", x, y));
                            ostap ("+"), (fun x y -> Binop ("+", x, y))];
                    `Lefta , [
                            ostap ("*"), (fun x y -> Binop ("*", x, y));
                            ostap ("%"), (fun x y -> Binop ("%", x, y));
                            ostap ("/"), (fun x y -> Binop ("/", x, y))];
                |]
                parse
                );

        parse: x:IDENT {Var x} | x:DECIMAL {Const x} |  -"(" expr -")"
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
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval ((state, input, output) as config) stm = match stm with
        | Seq (x, y) -> eval (eval config x) y
        | Assign (x, e) -> ((Expr.update x (Expr.eval state e) state), input, output)
        | Read x -> (match input with
                    | y :: ys -> ((Expr.update x y state), ys, output))
        | Write e -> (state, input, output @ [Expr.eval state e])

    (* Statement parser *)
    (* https://github.com/dboulytchev/ostap/blob/master/sample/Sample.ml#L244 *)
    ostap (
      simple_stmt:
              x:IDENT ":=" e:!(Expr.expr)                   {Assign(x, e)}
              | "read" "(" x:IDENT ")"                           {Read(x)}
            | "write" "(" e:!(Expr.expr) ")"                   {Write(e)};
      parse: <s::ss> : !(Ostap.Util.listBy)[ostap (";")][simple_stmt] {List.fold_left (fun s ss -> Seq (s, ss)) s ss}
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     
