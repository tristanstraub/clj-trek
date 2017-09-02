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

(defn emitted
  [type]
  (machine/emitted *machine* type))

(defn parser []
  {:parser     (rules/parser basic)
   :transforms (rules/transforms basic)})

(defn parse
  ([parser machine listing]
   (parse parse machine listing :program))
  ([parser machine listing start]
   {:pre [machine]}
   (binding [*machine* machine]
     (let [{:keys [parser transforms]} parser]
       (doall (rules/parse parser transforms listing start))))))

(defrule "program = S (<\"\n\"> S)* <\"\n\"?>"
  [statement & statements]
  (emit :program `(~statement ~@statements)))

(defrule "S = line-number <ws+> statement <#';'?>"
  [n s]
  (emit :statement n s))

(defrule "line-number = #'[0-9]+'"
  [n]
  (Integer/parseInt n))

(defrule "statement = comment | gosub | print-using | print | input | if | assignment | dim | def | mat | for | goto | goto-of | next | image | return | end"
  [s]
  s)

(defrule "print = <\"PRINT\"> <ws>* (! 'USING' (<ws> | expression | <\",\"> | <\";\">))* "
  ([]
   (emit :print))
  ([& args]
   (emit :print args)))

(defrule "print-using = <'PRINT'> <ws+> <'USING'> <ws> line-number (<';'> expression (<','> expression)*)?"
  [line-number & expressions]
  (emit :print-using [line-number expressions]))

(defrule "goto = <\"GOTO\"> <ws> line-number"
  [n]
  (emit :goto n))

(defrule "goto-of = <\"GOTO\"> <ws> expression <ws> (<'OF'> <ws> line-number (<','> line-number)*)"
  ([n]
   (emit :goto n))
  ([expression & line-numbers]
   (emit :goto-of expression line-numbers)))

(defrule "gosub = <\"GOSUB\"> <ws> line-number"
  [n]
  (emit :gosub n))

(defrule "return = <\"RETURN\">"
  []
  (emit :return))

(defrule "for = <\"FOR\"> <ws> identifier <ws*> <'='> expression <ws> <'TO'> <ws> expression"
  [identifier lower upper]
  (emit :for identifier lower upper))

(defrule "expression = v-expression | m-expression | a-expression"
  [v]
  v)

(defrule "v-expression = value | <'('> expression <')'> | h-expression"
  ([v]
   v))

(defrule "a-expression = expression additive-op expression"
  [left op right]
  (emit :binary-operator left op right))

(defrule "m-expression = v-expression | m-expression multiplicative-op m-expression"
  ([v]
   v)
  ([left op right]
   (emit :binary-operator left op right)))

(defrule "h-expression = v-expression hat-op v-expression"
  [left op right]
  (emit :binary-operator left op right))

(defrule "additive-op = '+' | '-'"
  [v]
  v)

(defrule "multiplicative-op = '*' | '/'"
  [v]
  v)

(defrule "hat-op = '^'"
  [v]
  v)

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
   (emit :value expression))
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

(defrule "function-call = function-name <'('> expression <')'>"
  [fn-name arg]
  (emit :call fn-name arg))

(defrule "function-name = #'[A-Z]+'"
  [name]
  name)

(defrule "comparison-op = #'<>' | '>' | '<=' | '<' | '=' | '>='"
  [op]
  op)

(defrule "decimal = #'[0-9]*' '.'? #'[0-9]+'"
  [& values]
  (emit :value (Float/parseFloat (apply str values))))

(defrule "end = <\"END\">"
  []
  (emit :nop))

(defrule "def = <\"DEF\" ws> identifier <'('> identifier <')' '='> expression"
  [fn-name fn-arg expression]
  (emit :def fn-name fn-arg expression))

(defrule "mat = <\"MAT\" ws> identifier <'=ZER'>"
  [identifier]
  (emit :zero-array identifier))

(defrule "bool-op = <'OR'>"
  []
  :or)

(defrule "image = <\"IMAGE\"> <ws+> formatter-list"
  [fl]
  fl)

(defrule "formatter-list = formatter (<','> formatter)*"
  [a & bs]
  (apply emit :formatter-list `[~a ~@bs]))

(defrule "formatter = format-count format-type | quoted-string | format-type | (format-count \"(\" formatter-list \")\")"
  ([number type]
   (emit :format-type (str number (first (emitted type)))))
  ([type]
   type)
  ([number _ format-list _]
   (emit :format-repeat number format-list)))

(defrule "format-count = #'[-]?[0-9]+'"
  [number]
  (Integer/parseInt number))

(defrule "format-type = \"D\" | \"X\" | \"A\""
  [type]
  (emit :format-type type))
