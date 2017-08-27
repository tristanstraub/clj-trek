(ns trek.grammar
  (:require [trek
             [rules :refer [defrule]]]
            [trek.machine :as machine]
            [trek.rules :refer [ns-parser]]
            [trek.rules :refer [ns-transforms]]
            [clojure.string :as str]
            [trek.rules :refer [ns-parse]]))

(def ^:dynamic *machine* nil)

(defn emit [statement-type & args]
  {:pre [*machine*]}
  (machine/emit *machine* statement-type args))

(defn parse [machine listing]
  {:pre [machine]}
  (binding [*machine* machine]
    (let [parser     (ns-parser (the-ns 'trek.grammar))
          transforms (ns-transforms (the-ns 'trek.grammar))]
      (doall (ns-parse parser transforms listing :program)))))

;;(parse (trek.interpreter/interpreter) "10 PRINT\n20 GOTO 10\n")

(defrule "program = S (<\"\n\"> S)* <\"\n\"?>"
  [statement & statements]
  (emit :program `(~statement ~@statements)))

(defrule "S = line-number <ws> statement <#';'?>"
  [n s]
  (emit :statement n s))

(defrule "line-number = #'[0-9]+'"
  [n]
  (Integer/parseInt n))

(defrule "statement = comment | gosub | print | print-using | input | if | assignment | dim | def | mat | for | goto | goto-of | next | image | return | end"
  [s]
  s)

(defrule "print = <\"PRINT\"> (<ws> (expression | \",\" | \";\")* )?"
  [& args]
  (emit :print args))

(defrule "image = <\"IMAGE\"> <ws> (format | quoted-string) (',' format-list)*"
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

(defrule "ws = #'[ \t]*'")
(defrule "comment = \"REM\" #'.*'")
(defrule "end = \"END\"")
(defrule "format-list = (format | quoted-string) (',' format-list)*")
(defrule "format = integer format-type | format-type | (integer \"(\" format-list \")\")")
(defrule "format-type = \"D\" | \"X\" | \"A\"")
(defrule "print-using = \"PRINT\" ws \"USING\" ws integer (';' expression (',' expression)*)?")
(defrule "input = \"INPUT\" ws identifier (\",\" identifier)*")
(defrule "dim = \"DIM\" ws array-defs")
(defrule "def = \"DEF\" ws identifier '(' fn-args ')' '=' expression")
(defrule "mat = \"MAT\" ws identifier '=ZER'")
(defrule "for = \"FOR\" ws identifier ws* '=' expression ws 'TO' ws expression")
(defrule "if = \"IF\" ws bool-expression ws 'THEN' ws line-number")
(defrule "next = \"NEXT\" ws identifier")
(defrule "bool-expression = expression ws* comparison ws* expression (ws bool-op ws bool-expression)?")
(defrule "bool-op = 'OR'")
(defrule "fn-args = identifier (',' fn-args)?")
(defrule "assignment = variable assignment-rhs")
(defrule "assignment-rhs = '=' (expression | variable assignment-rhs?)")
(defrule "expression = value (op expression)? | '(' expression ')' (op expression)?"
  [v & _]
  v)
(defrule "op = '*' | '+' | '-' | '^' | '/'")
(defrule "value = function-call | quoted-string | integer | variable | decimal"
  [x]
  x)
(defrule "variable = identifier | array-ref"
  [v]
  v)
(defrule "array-ref = identifier '[' array-indices ']'")
(defrule "array-indices = expression (',' array-indices)?")
(defrule "function-call = function-name '(' expression ')'")
(defrule "function-name = #'[A-Z]+'")
(defrule "comparison = #'<>' | '>' | '<=' | '<' | '=' | '>='")
(defrule "array-defs = identifier '[' dimensions ']' (#',' array-defs)?")
(defrule "dimensions = integer (',' dimensions)?")
(defrule "integer = #'[-]?[0-9]+'")
(defrule "integer-expression = integer | (identifier op integer)")
(defrule "decimal = #'[0-9]*' '.'? #'[0-9]+'")
(defrule "identifier = #'[A-Z]' #'[A-Z0-9]*[$]?'")
(defrule "quoted-string = <'\"'> #'[^\"]*' <'\"'>"
  [v]
  (emit :value v))
