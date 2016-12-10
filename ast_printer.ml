open Ast
open Odot


let sprintf = Format.sprintf


let node lbl name =
  Stmt_node
    ((dblq_node_id lbl), [(Simple_id "label",
                           Some (Double_quoted_id name))])

let edge ?(color="black")  id1 id2 = Stmt_edge
    ((Edge_node_id (dblq_node_id id1)),
     [Edge_node_id (dblq_node_id id2)], [(Simple_id "color",Some (Simple_id color))])

let _id = ref(0)
let next_id () = _id := !_id + 1; string_of_int !_id

let rec p_typ = function
  | Tint -> "int"
  | Tchar -> "char"
  | Tbool -> "bool"
  | TypeNull -> "null"
  | TRecord (i,r) -> "record("^r^")"
  | TAccessRecord (i,r) -> "access("^r^")"
  | TypeError -> "ERR"
  | TypeNone -> "none"
  | TType (i,t) -> "type "^t
  | _ -> "def"

let p_op = function
  | OpAnd -> "and"
  | OpAndThen -> "and then"
  | OpOr -> "or"
  | OpOrElse -> "or else"
  | OpDiv -> "/"
  | OpMinus -> "-"
  | OpPlus -> "+"
  | OpTimes -> "*"
  | OpRem -> "rem"
  | OpEq -> "="
  | OpNeq -> "!="
  | OpGt -> ">"
  | OpGet -> ">="
  | OpLt -> "<"
  | OpLet -> "<="

let lst = ref([])

let add n = (lst := n::(!lst))

let rec explore_expr id (expr:texpr) =
  let typ s =  (s^" : "^(p_typ expr.typ)) in
  let add n = (lst := n::(!lst)) in
  let it ex = let id_next = next_id () in
    explore_expr id_next ex;
    add (edge ~color:("green") id id_next) in
  let it_list lst = List.iter (fun e -> it e) lst in
  match expr.expr with
  | EInt i -> add (node id ( typ ("EInt("^(string_of_int i)^")")))
  | EChar c -> add (node id (typ ("EChar("^(String.make 1 c)^")")))
  | ETrue -> add (node id (typ "ETrue"))
  | EFalse -> add (node id (typ "EFalse"))
  | ENull -> add (node id (typ "ENull"))
  | EAccess (None,s) -> add (node id (typ s))
  | EAccess (Some e, s) ->
    add (node id (typ ("."^s)));it e
  | EOp (e1,op,e2) ->
    add (node id (typ (p_op op)));it e1;it e2
  | ENot e ->
    add (node id (typ "!"));it e
  | EMinus e ->
    add (node id (typ "-")); it e
  | ENew i -> add (node id (typ ("ENew ("^i^")")))
  | EEval (i,lexpr) -> add (node id (typ (i^"(...)"))); it_list lexpr
  | EChr e -> add (node id (typ "EChr")); it e


let rec explore_instr id instr =
  let typ s =  (s^" : "^(p_typ instr.typ)) in
  let it ex = let id_next = next_id () in
  explore_instr id_next ex;
  add (edge ~color:("red") id id_next) in
  let it_list lst = List.iter (fun e -> it e) lst in
  let e_it ex = let id_next = next_id () in
  explore_expr id_next ex;
  add (edge ~color:("green") id id_next) in
  let e_it_list lst = List.iter (fun e -> e_it e) lst in
  match instr.instr with
  | IAssign ((None, s), e) -> add (node id (typ (s^" :="))); e_it e
  | IAssign ((Some e0, s), e) -> add (node id (typ ("."^s^" :="))); e_it e0; e_it e
  | IEval (s, ex) -> add (node id (typ (s^"(...)"))); e_it_list ex
  | IReturn None -> add (node id (typ "Return"))
  | IReturn (Some ex) -> add (node id (typ "Return ..")); e_it ex
  | IScope linstr -> add (node id (typ "begin..end")); it_list linstr
  | IConditional (cond, then_instrs, [], None) ->
    add (node id (typ "if .. then .. ")); e_it cond; it_list then_instrs
  | IConditional (cond, then_instrs, [], Some else_instrs) ->
    add (node id (typ "if .. then .. else ..")); e_it cond; it_list then_instrs; it_list else_instrs
  | IConditional (cond, then_instrs, (ex, elif_instr)::q, else_) ->
    add (node id (typ "if .. then .. else ..")); e_it cond; it_list then_instrs; it {instr with instr = IConditional(ex,elif_instr,q,else_)}
  | IFor (x, rev, e1, e2, instrs) ->
    add (node id (typ ("for on "^x))); e_it e1; e_it e2; it_list instrs
  | IWhile (e, instrs) ->
    add (node id (typ "while")); e_it e; it_list instrs

let rec explore_decl id decl =
  let it ex = let id_next = next_id () in
  explore_decl id_next ex;
  add (edge ~color:("blue") id id_next) in
  let it_list lst = List.iter (fun e -> it e) lst in
  let i_it ex = let id_next = next_id () in
  explore_instr id_next ex;
  add (edge ~color:("red") id id_next) in
  let i_it_list lst = List.iter (fun e -> i_it e) lst in
  let e_it ex = let id_next = next_id () in
  explore_expr id_next ex;
  add (edge ~color:("green") id id_next) in
  let p_ = function
    | TIdent s -> s
    | TAccess s -> "access "^s in
  begin
    match decl.decl with
    | DeclType s -> add (node id ("Type "^s))
    | DeclTypeAccess (s,x) -> add (node id ("type "^s^" = Access "^x))
    | DeclTypeRecord (s,champs) -> add (node id ("type "^s^" = Record"));
      List.iter (fun (ident_lst, typ_) ->
          List.iter (fun ident ->
                      let nid = next_id () in
                      add (node nid (ident^" of "^(p_ typ_)));
                      add (edge id nid)) ident_lst
        ) champs
    | Decl (ident_lst, typ_, None) -> add (node id ("Decl"));
      List.iter (fun ident ->
                  let nid = next_id () in
                  add (node nid ("var "^ident^" of type "^(p_ typ_)));
                  add (edge id nid)) ident_lst
    | Decl (ident_lst, typ_, Some ex) -> add (node id ("Decl"));
      List.iter (fun ident ->
                  let nid = next_id () in
                  add (node nid ("var "^ident^" of type "^(p_ typ_))); e_it ex;
                  add (edge id nid)) ident_lst
    | DeclProcedure (p, params, decls, instrs) | DeclFunction (p, params, _, decls, instrs) ->
      add (node id ("function "^p)); it_list decls; i_it_list instrs
  end

let print_program (i,ldecl,linstr) =
  add (node "0" ("programme"^i));
  List.iter (fun decl ->
      let id = next_id () in
      explore_decl id decl;
      add (edge ~color:("blue") "0" id)) ldecl;
  List.iter (fun instr ->
      let id = next_id ()in
      explore_instr id instr;
      add (edge ~color:("red") "0" id)) linstr;
  let stmt_list = !lst in
  let graph = {strict=true;kind=Digraph; stmt_list; id = None} in
  Odot.print_file "graph.dot" graph;
  ignore (Sys.command "dot -Tsvg graph.dot -o graph.svg")
