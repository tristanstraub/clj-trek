(ns trek.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [instaparse.core :as insta]))

(def basic
  (insta/parser
    "S = line-number ws+ statement #';'?
     statement = comment | gosub | print | print-using | input | if | assignment | dim | def | mat | for | goto | next | image | return | end
     ws = #'[ \t]'
     comment = \"REM\" #'.*'
     end = \"END\"
     return = \"RETURN\"
     image = \"IMAGE\" ws+ (format | quoted-string) (',' format-list)*
     format-list = (format | quoted-string) (',' format-list)*
     format = integer format-type | format-type | (integer \"(\" format-list \")\")
     format-type = \"D\" | \"X\" | \"A\"
     gosub = \"GOSUB\" ws+ line-number
     print = \"PRINT\" (ws+ expression ((\",\")? expression | ';')*)?
     print-using = \"PRINT\" ws+ \"USING\" ws+ integer (';' expression (',' expression)*)?
     input = \"INPUT\" ws+ identifier (\",\" identifier)*
     dim = \"DIM\" ws+ array-defs
     def = \"DEF\" ws+ identifier '(' fn-args ')' '=' expression
     mat = \"MAT\" ws+ identifier '=ZER'
     for = \"FOR\" ws+ identifier ws* '=' expression ws+ 'TO' ws+ expression
     if = \"IF\" ws+ bool-expression ws+ 'THEN' ws+ line-number
     goto = \"GOTO\" ws+ integer-expression (ws+ 'OF' ws+ integer (',' integer)*)?
     next = \"NEXT\" ws+ identifier
     bool-expression = expression ws* comparison ws* expression (ws+ bool-op ws+ bool-expression)?
     bool-op = 'OR'
     fn-args = identifier (',' fn-args)?
     assignment = variable assignment-rhs
     assignment-rhs = '=' (expression | variable assignment-rhs?)
     expression = value (op expression)? | '(' expression ')' (op expression)?
     op = '*' | '+' | '-' | '^' | '/'
     value = function-call | quoted-string | integer | variable | decimal
     variable = identifier | array-ref
     array-ref = identifier '[' array-indices ']'
     array-indices = expression (',' array-indices)?
     function-call = function-name '(' expression ')'
     function-name = #'[A-Z]+'
     comparison = #'<>' | '>' | '<=' | '<' | '=' | '>='
     array-defs = identifier '[' dimensions ']' (#',' array-defs)?
     dimensions = integer (',' dimensions)?
     integer = #'[-]?[0-9]+'
     integer-expression = integer | (identifier op integer)
     decimal = #'[0-9]*' '.'? #'[0-9]+'
     identifier = #'[A-Z]' #'[A-Z0-9]*[$]?'
     quoted-string = #'\"[^\"]*\"'
     line-number = #'[0-9]+'"))

(defn parse
  []
  (->> #"\n"
       (str/split (slurp (io/resource "STTR1.txt")))
       (map basic)
       (filter :reason)
       first))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
