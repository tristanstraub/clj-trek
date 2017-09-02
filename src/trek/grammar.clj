(ns trek.grammar
  (:require [trek.machine :as machine]
            [trek.rules :as rules]
            [clojure.string :as str]))

(def ^:dynamic *machine* nil)

(rules/defgrammar basic defrule)

(defn emit
  [statement-type & args]
  {:pre [*machine*]}
  (machine/emit *machine* statement-type args))

(defn parser []
  {:parser     (rules/parser basic)
   :transforms (rules/transforms basic)})

(defn parse [parser machine listing]
  {:pre [machine]}
  (binding [*machine* machine]
    (let [{:keys [parser transforms]} parser]
      (doall (rules/parse parser transforms listing :program)))))

(defrule "program = S (<\"\n\"> S)* <\"\n\"?>"
  [statement & statements]
  (emit :program `(~statement ~@statements)))

(defrule "S = line-number <ws+> statement <#';'?>"
  [n s]
  (emit :statement n s))

(defrule "line-number = #'[0-9]+'"
  [n]
  (Integer/parseInt n))

(defrule "statement = comment | gosub | print | print-using | input | if | assignment | dim | def | mat | for | goto | goto-of | next | image | return | end"
  [s]
  s)

(defrule "print = <\"PRINT\"> (<ws> | expression | \",\" | \";\")* "
  ([]
   (emit :print))
  ([& args]
   (emit :print args)))

(defrule "image = <\"IMAGE\"> <ws+> (format | quoted-string) (',' format-list)*"
  [& args]
  (emit :image args))

(defrule "goto = <\"GOTO\"> <ws> line-number"
  [n]
  (emit :goto n))

(defrule "goto-of = <\"GOTO\"> <ws> expression <ws> 'OF' <ws> integer (',' integer)*"
  [n & _]
  (emit :goto n))

(defrule "gosub = <\"GOSUB\"> <ws> line-number"
  [n]
  (emit :gosub n))

(defrule "return = <\"RETURN\">"
  []
  (emit :return))

(defrule "for = <\"FOR\"> <ws> identifier <ws*> <'='> expression <ws> <'TO'> <ws> expression"
  [identifier lower upper]
  (emit :for identifier lower upper))

(defrule "expression = value | <'('> expression <')'> | expression op expression"
  ([v]
   v)
  ([left op right]
   (emit :binary-operator left op right)))

(defrule "quoted-string = <'\"'> #'[^\"]*' <'\"'>"
  [v]
  (emit :value v))


(defrule "value = function-call | quoted-string | integer | variable | decimal"
  [x]
  x)

(defrule "integer = #'[-]?[0-9]+'"
  [v]
  (emit :value (Integer/parseInt v)))

(defrule "ws = #'[ \t]'")
(defrule "comment = <\"REM\" #'.*'>"
  []
  (emit :nop))

(defrule "input = <\"INPUT\"> <ws> identifier (\",\" identifier)*"
  [input & inputs]
  (emit :input `[~input ~@(apply concat inputs)]))

(defrule "if = <\"IF\"> <ws> bool-expression <ws> <'THEN'> <ws> line-number"
  [expression line-number]
  (emit :if expression (emit :goto line-number)))

(defrule "op = '*' | '+' | '-' | '^' | '/'"
  [v]
  v)

(defrule "variable = identifier | array-ref"
  [v]
  v)

(defrule "next = <\"NEXT\"> <ws> identifier"
  [identifier]
  (emit :next identifier))

(defrule "bool-expression = expression <ws*> comparison-op <ws*> expression (<ws> bool-op <ws> bool-expression)?"
  [a comparison-op b & [bool-op right]]
  (let [left (emit :compare a comparison-op b)]
    (if right
      (emit :bool-op left bool-op right)
      left)))

(defrule "assignment = variable <'='> assignment-rhs"
  [identifier rhs]
  (emit :assign identifier rhs))

(defrule "assignment-rhs = (expression | variable <'='> assignment-rhs)"
  ([expression]
   expression)
  ([identifier rhs]
   (emit :assign identifier rhs)))

(defrule "dim = <\"DIM\" ws> array-defs"
  [array-defs]
  (emit :dim array-defs))

(defrule "identifier = #'[A-Z][A-Z0-9]*[$]?'"
  [identifier]
  (emit :identifier identifier))

(defrule "array-ref = identifier <'['> array-indices <']'>"
  [identifier indices]
  (emit :array-ref identifier indices))

(defrule "array-indices = expression (<','> expression)*"
  [& indices]
  indices)

(defrule "array-def = identifier <'['> dimensions <']'>"
  [identifier dimensions]
  (emit :array-ref identifier dimensions))

(defrule "dimensions = integer (<','> integer)*"
  [& dimensions]
  dimensions)

(defrule "array-defs = array-def (<#','> array-ref)*"
  [& array-defs]
  array-defs)

(defrule "def = <\"DEF\" ws> identifier <'('> fn-args <')' '='> expression"
  [identifier fn-args expression]
  )

(defrule "end = \"END\""
  )

(defrule "format-list = (format | quoted-string) (',' format-list)*")
(defrule "format = integer format-type | format-type | (integer \"(\" format-list \")\")")
(defrule "format-type = \"D\" | \"X\" | \"A\"")
(defrule "print-using = \"PRINT\" ws \"USING\" ws integer (';' expression (',' expression)*)?")


(defrule "mat = \"MAT\" ws identifier '=ZER'")




(defrule "bool-op = 'OR'")
(defrule "fn-args = identifier (',' fn-args)?")




(defrule "function-call = function-name '(' expression ')'")
(defrule "function-name = #'[A-Z]+'")
(defrule "comparison-op = #'<>' | '>' | '<=' | '<' | '=' | '>='"
  [op]
  op)



(defrule "integer-expression = integer | (identifier op integer)")

(defrule "decimal = #'[0-9]*' '.'? #'[0-9]+'"
  [& values]
  (emit :value (Float/parseFloat (apply str values))))
