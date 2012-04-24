(include-lib "lfe_utils/include/all.lfe")
(defmodule generic
  (using lists)
  (export
    (children 1) (children 2)
    (universe 1) (universe 2)
    (context 1)
    (uniplate 1)
    (transform 2)
    (descend 2)
    (para 2)
  ))

;; The important thing is to only traverse data types, ignore everything
;; else. Returning just numbers or binaries doesn't give enough
;; information to match on to do interesting things.
;;
;; The 'lists are data types' thing is annoying, because they're not
;; interesting, but I'd have to complicate the implementation with extra
;; private functions... Feh.
;;
;; Note by Diego: Actually... I couldn't wrap my head about how to implement
;; transform without having the children up to the deepest level.
;; I changed the code to include the non 'data?' types as well.
(defn children
  [xs] (when (is_list xs))
    xs
  [tup] (when (is_tuple tup) (> (tuple_size tup) 1))
    (tuple_to_list tup)
  [other] '())

;; Renaming family to universe (As in Uniplate)
;; The children, and the children of the children recursively.
(defn universe [term]
  (-> (children term)
      (lists:map (fun universe 1) <>)
      lists:append
      (cons term <>)))

;; Examples:
;; (1 + A) * (2 - 10) + (5 * B)."
;; AST = {op,1,'+',
;;          {op,1,'*',
;;              {op,1,'+',{integer,1,1},{var,1,'A'}},
;;              {op,1,'-',{integer,1,2},{integer,1,10}}},
;;          {op,1,'*',{integer,1,5},{var,1,'B'}}}.
;; %% traversals:vars_in_expr(AST).
;; <==> [Name || {var, _, Name} <- generic:universe(AST)].
;; %% traversals:ints_in_expr(AST).
;; <==> [Value || {integer, _, Value} <- generic:universe(AST)].


(defn context
  [tup] (when (is_tuple tup))
    (fun list_to_tuple 1)
  [list]  (when (is_list list))
    (lambda [list] list)
  [other] (lambda [_] other))

(defn uniplate
  [x] (tuple (children x) (context x)))

(defn transform [f x]
  (let (((tuple chs ctx) (uniplate x)))
    (-> chs
        (lists:map (lambda [x] (transform f x)) <>)
        (funcall ctx <>)
        (funcall f <>))))

(defn descend [f x]
  (let (((tuple chs ctx) (uniplate x)))
    (-> chs
        (lists:map (lambda [x] (funcall f x)) <>)
        (funcall ctx <>))))

(defn para [f x]
    (-> (children x)
        (lists:map (lambda [x] (para f x)) <>)
        (funcall f x <>)))

;; Without lfe_utils' magic
;; (funcall f (funcall ctx (lists:map (lambda [x] (transform f x)) chs)))

;; Examples:
;; Normal nested structures
;; F = fun(X) -> (if is_integer(X) -> X + 1; true ->  X end) end.
;; (generic:transform(F, [1,2,3,[4,5,{5,6,7}]])).
;; Interpreter
;; G = fun(X) -> (
;;       case X of
;;       {op, _, Op, {integer, _, Left}, {integer, _, Right}} ->
;;           case Op of
;;          '+' -> {integer, 1, Left + Right};
;;          '*' -> {integer, 1, Left * Right};
;;          '/' -> {integer, 1, Left / Right};
;;          '-' -> {integer, 1, Left - Right}
;;          end;
;;       X ->  X
;;       end)
;;     end.

;; generic:transform(G, AST).

;; AST2 = {op,1,'-',
;;          {op,1,'+',
;;              {integer,1,1},
;;              {op,1,'*',{integer,1,2},{integer,1,3}}},
;;          {integer,1,6}}
;; generic:transform(G, AST).


;; Depth = fun(_, Y) -> (if is_integer(X) -> X + 1; true ->  X end) end.
;; Depth = fun(_, Y) -> (1 + lists:max([0|Y])) end.
;; generic:para(Depth, 1).
;; generic:para(Depth, [1]).
;; generic:para(Depth, [{1,2}]).

;; List comprehensions provide very elegant (and pretty efficient)
;; filtering of generic data. If multiple matches are required, however,
;; functions or funs are needed. Hence, these wrappers.
(defn children [f term]
  (filter-matches f (children term)))

(defn universe [f term]
  (filter-matches f (universe term)))

(defn filter-matches [f xs]
  (in (lists:foldr accum '() xs)
    [accum
      (fn [x acc]
        (try (funcall f x) ; LFE try clause is a bit of a bear
          [case (y (cons y acc))] ; wonder if I can improve it?
          [catch ((tuple 'error 'function_clause c) acc)]))]))
