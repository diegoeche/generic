(include-lib "lfe_utils/include/all.lfe")
(defmodule generic
  (using lists)
  (export
    (children 1) (children 2)
    (universe 1) (universe 2)
    (context 1)
    (uniplate 1)
    (transform 2)
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

(defn universe [term]
  (-> (children term)
      (lists:map (fun universe 1) <>)
      lists:append
      (cons term <>)))

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

;; Without lfe_utils' magic
;; (funcall f (funcall ctx (lists:map (lambda [x] (transform f x)) chs)))
;; Example:
;; F = fun(X) -> (if is_integer(X) -> X + 1; true ->  X end) end.
;; (generic:transform(F, [1,2,3,[4,5,{5,6,7}]])).

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
