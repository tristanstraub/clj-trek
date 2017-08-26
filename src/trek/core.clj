(ns trek.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [trek.rules :refer [defrule ns-parse ns-parser ns-transforms]]))

(defn pop-subroutine [env]
  env)

(defn parse
  []
  (let [parser     (ns-parser *ns*)
        transforms (ns-transforms *ns*)]
    (->> (str/split (slurp (io/resource "STTR1.txt")) #"\n")
         (take 100)
         (map #(ns-parse parser transforms % :S)))))

(defn parse-1
  [s]
  (let [parser     (ns-parser *ns*)
        transforms (ns-transforms *ns*)]
    (ns-parse parser transforms s :S)))

(defn move-to-next-instruction [program env]
  (cond-> env
    (not (:next-instruction-ptr env)) (update :instruction-ptr
                                              (fn [instruction-ptr]
                                                (->> (keys program)
                                                     (filter #(< instruction-ptr %))
                                                     sort
                                                     first)))
    (:next-instruction-ptr env) (-> (assoc :instruction-ptr (:next-instruction-ptr env)
                                           :next-instruction-ptr nil))))

(defn parse-eval []
  (let [parser     (ns-parser *ns*)
        transforms (ns-transforms *ns*)
        program    (->> (str/split (slurp (io/resource "sttr1.txt")) #"\n")
                        (reduce (fn [program line]
                                  (let [command-builder (ns-parse parser transforms line :S)]
                                    (if command-builder
                                      (command-builder program)
                                      ((instruction nil (fn [_ env]

                                                          env))
                                       program))))
                                {}))]
    (loop [env              {:instruction-ptr (apply min (keys program))}]
      (when-let [instruction-ptr (:instruction-ptr env)]
        (let [{:keys [eval]} (get program instruction-ptr)
              new-env        (if eval (eval program env) env)]
          (recur (move-to-next-instruction program new-env)))))))

(defn instruction [value eval]
  {:eval eval :value value})

(defn map-eval [program env args]
  (map (fn [arg]
            (if-let [eval (:eval arg)]
              (eval program env)))
          args))

(defn return-subroutine [env]
  (-> env
      (assoc :instruction-ptr (peek (:returns env)))
      (update :returns pop)))

(defrule "S = line-number ws statement #';'?"
  [n _ s & _]
  (fn [program]
    (assoc program n s)))

(defrule "line-number = #'[0-9]+'"
  [n & _]
  (Integer/parseInt n))

(defrule "statement = comment | gosub | print | print-using | input | if | assignment | dim | def | mat | for | goto | next | image | return | end"
  [s]
  s)

(defrule "print = \"PRINT\" (<ws> (expression | \",\" | \";\")* )?"
  [_ & args]
  (instruction `[:print ~@args]
               (fn [program env]
                 (apply println (map-eval program env args))
                 env)))

(defrule "ws = #'[ \t]*'"
  [& _])

(defrule "comment = \"REM\" #'.*'"
  [& _]
  (instruction nil (fn [progrm env] env)))

(defrule "end = \"END\""
  [& _])

(defrule "return = \"RETURN\""
  [& _]
  (instruction [:return]
               (fn [_ env]
                 (return-subroutine env))))

(defrule "image = \"IMAGE\" ws (format | quoted-string) (',' format-list)*"
  [& args]
  (instruction `[:image ,@args] (fn [_ env] env)))

(defrule "format-list = (format | quoted-string) (',' format-list)*"
  [& _])

(defrule "format = integer format-type | format-type | (integer \"(\" format-list \")\")"
  [& _])

(defrule "format-type = \"D\" | \"X\" | \"A\""
  [& _])

(defrule "gosub = \"GOSUB\" <ws> line-number"
  [_ n]
  (instruction [:gosub n] (fn [program env]
                            (-> env
                                (assoc :next-instruction-ptr n)
                                (update :returns conj n)))))

(defrule "print-using = \"PRINT\" ws \"USING\" ws integer (';' expression (',' expression)*)?"
  [& _])

(defrule "input = \"INPUT\" ws identifier (\",\" identifier)*"
  [& _])

(defrule "dim = \"DIM\" ws array-defs"
  [& _])

(defrule "def = \"DEF\" ws identifier '(' fn-args ')' '=' expression"
  [& _])

(defrule "mat = \"MAT\" ws identifier '=ZER'"
  [& _])

(defrule "for = \"FOR\" ws identifier ws* '=' expression ws 'TO' ws expression"
  [& _])

(defrule "if = \"IF\" ws bool-expression ws 'THEN' ws line-number"
  [& _])

(defrule "goto = \"GOTO\" <ws> line-number (ws 'OF' ws integer (',' integer)*)?"
  [& args]
  (let [[_ n & _] args]
    (instruction [:goto n] (fn [program env] (assoc env :next-instruction-ptr n)))))

(defrule "next = \"NEXT\" ws identifier"
  [& _])

(defrule "bool-expression = expression ws* comparison ws* expression (ws bool-op ws bool-expression)?"
  [& _])

(defrule "bool-op = 'OR'"
  [& _])

(defrule "fn-args = identifier (',' fn-args)?"
  [& _])

(defrule "assignment = variable assignment-rhs"
  [& _])

(defrule "assignment-rhs = '=' (expression | variable assignment-rhs?)"
  [& _])

(defrule "expression = value (op expression)? | '(' expression ')' (op expression)?"
  [v & _]
  v)

(defrule "op = '*' | '+' | '-' | '^' | '/'"
  [& _])

(defrule "value = function-call | quoted-string | integer | variable | decimal"
  [x]
  x)

(defrule "variable = identifier | array-ref"
  [& _])

(defrule "array-ref = identifier '[' array-indices ']'"
  [& _])

(defrule "array-indices = expression (',' array-indices)?"
  [& _])

(defrule "function-call = function-name '(' expression ')'"
  [& _])

(defrule "function-name = #'[A-Z]+'"
  [& _])

(defrule "comparison = #'<>' | '>' | '<=' | '<' | '=' | '>='"
  [& _])

(defrule "array-defs = identifier '[' dimensions ']' (#',' array-defs)?"
  [& _])

(defrule "dimensions = integer (',' dimensions)?"
  [& _])

(defrule "integer = #'[-]?[0-9]+'"
  [& _])

(defrule "integer-expression = integer | (identifier op integer)"
  [& _])

(defrule "decimal = #'[0-9]*' '.'? #'[0-9]+'"
  [& _])

(defrule "identifier = #'[A-Z]' #'[A-Z0-9]*[$]?'"
  [& _])

(defrule "quoted-string = <'\"'> #'[^\"]*' <'\"'>"
  [v]
  (instruction v (fn [_ _] v)))

(comment (parse-1 "780  PRINT \"YOU MUST DESTROY\"K9;\" KLINGONS IN\"T9;\" STARDATES WITH\"B9;\" STARBASES\""))
