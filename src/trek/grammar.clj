(ns trek.grammar
  (:require [trek.machine :as machine]
            [trek.rules :as rules]
            [clojure.string :as str]))

(def ^:dynamic *machine* nil)

(defn emit
  [statement-type & args]
  {:pre [*machine*]}
  (machine/emit *machine* statement-type args))

(defn emitted
  [type]
  (machine/emitted *machine* type))

(def basic
  (merge
   {"program = S (<\"\n\"> S)* <\"\n\"?>"
    (fn
      [statement & statements]
      (emit :program `(~statement ~@statements)))}

   {"S = line-number <ws+> statement <#';'?>" (fn
                                                [n s]
                                                (emit :statement n s))}

   {"line-number = #'[0-9]+'" (fn
                                [n]
                                (Integer/parseInt n))}

   {"statement = comment | gosub | print-using | print | input | if | assignment | dim | def | mat | for | goto | goto-of | next | image | return | end" (fn
                                                                                                                                                           [s]
                                                                                                                                                           s)}

   {"print = <\"PRINT\"> <ws>* (! 'USING' (<ws> | expression | <\",\"> | <\";\">))* " (fn
                                                                                        ([]
                                                                                         (emit :print))
                                                                                        ([& args]
                                                                                         (emit :print args)))}

   {"print-using = <'PRINT'> <ws+> <'USING'> <ws> line-number (<';'> expression (<','> expression)*)?" (fn
                                                                                                         [line-number & expressions]
                                                                                                         (emit :print-using [line-number expressions]))}

   {"goto = <\"GOTO\"> <ws> line-number" (fn
                                           [n]
                                           (assert (number? n))
                                           (emit :goto n))}

   {"goto-of = <\"GOTO\"> <ws> expression <ws> (<'OF'> <ws> line-number (<','> line-number)*)" (fn
                                                                                                 ([n]
                                                                                                  (emit :goto n))
                                                                                                 ([expression & line-numbers]
                                                                                                  (emit :goto-of expression line-numbers)))}

   {"gosub = <\"GOSUB\"> <ws> line-number" (fn
                                             [n]
                                             (assert (number? n) "parse:gosub")
                                             (emit :gosub n))}

   {"return = <\"RETURN\">" (fn
                              []
                              (emit :return))}

   {"for = <\"FOR\"> <ws> identifier <ws*> <'='> expression <ws> <'TO'> <ws> expression" (fn
                                                                                           [identifier lower upper]
                                                                                           (emit :for identifier lower upper))}

   {"expression = v-expression | m-expression | a-expression" (fn
                                                                [v]
                                                                v)}

   {"v-expression = value | <'('> expression <')'> | h-expression" (fn
                                                                     ([v]
                                                                      v))}

   {"a-expression = expression additive-op m-expression" (fn
                                                           [left op right]
                                                           (emit :binary-operator left op right))}

   {"m-expression = v-expression | m-expression multiplicative-op v-expression" (fn
                                                                                  ([v]
                                                                                   v)
                                                                                  ([left op right]
                                                                                   (emit :binary-operator left op right)))}

   {"h-expression = v-expression hat-op v-expression" (fn
                                                        [left op right]
                                                        (emit :binary-operator left op right))}

   {"additive-op = '+' | '-'" (fn
                                [v]
                                v)}

   {"multiplicative-op = '*' | '/'" (fn
                                      [v]
                                      v)}

   {"hat-op = '^'" (fn
                     [v]
                     v)}

   {"quoted-string = <'\"'> #'[^\"]*' <'\"'>" (fn
                                                [v]
                                                (emit :value v))}

   {"value = function-call | quoted-string | integer | variable | decimal" (fn
                                                                             [x]
                                                                             x)}

   {"integer = #'[-]?[0-9]+'" (fn
                                [v]
                                (emit :value (Integer/parseInt v)))}

   {"ws = #'[ \t]'" (fn [& _])}

   {"comment = <\"REM\" #'.*'>" (fn
                                  []
                                  (emit :nop))}

   {"input = <\"INPUT\"> <ws> identifier (<\",\"> identifier)*" (fn
                                                                  [input & inputs]
                                                                  (emit :input `[~input ~@inputs]))}

   {"if = <\"IF\"> <ws> bool-expression <ws> <'THEN'> <ws> line-number" (fn
                                                                          [expression line-number]
                                                                          (emit :if expression (emit :goto line-number)))}



   {"variable = identifier | array-ref" (fn
                                          [v]
                                          v)}

   {"next = <\"NEXT\"> <ws> identifier" (fn
                                          [identifier]
                                          (emit :next identifier))}

   {"bool-expression = expression <ws*> comparison-op <ws*> expression (<ws> bool-op <ws> bool-expression)?" (fn
                                                                                                               [a comparison-op b & [bool-op right]]
                                                                                                               (let [left (emit :compare a comparison-op b)]
                                                                                                                 (if right
                                                                                                                   (emit :bool-op left bool-op right)
                                                                                                                   left)))}

   {"assignment = variable <'='> assignment-rhs" (fn
                                                   [identifier rhs]
                                                   (emit :assign identifier rhs))}

   {"assignment-rhs = (expression | variable <'='> assignment-rhs)" (fn
                                                                      ([expression]
                                                                       (emit :value expression))
                                                                      ([identifier rhs]
                                                                       (emit :assign identifier rhs)))}

   {"dim = <\"DIM\" ws> array-defs" (fn
                                      [array-defs]
                                      (emit :dim array-defs))}

   {"identifier = #'[A-Z][A-Z0-9]*[$]?'" (fn
                                           [identifier]
                                           (emit :identifier identifier))}

   {"array-ref = identifier <'['> array-indices <']'>" (fn
                                                         [identifier indices]
                                                         (emit :array-ref identifier indices))}

   {"array-indices = expression (<','> expression)*" (fn
                                                       [& indices]
                                                       indices)}

   {"array-def = identifier <'['> dimensions <']'>" (fn
                                                      [identifier dimensions]
                                                      (emit :array-ref identifier dimensions))}

   {"dimensions = integer (<','> integer)*" (fn
                                              [& dimensions]
                                              dimensions)}

   {"array-defs = array-def (<#','> array-ref)*" (fn
                                                   [& array-defs]
                                                   array-defs)}

   {"function-call = function-name <'('> expression <')'>" (fn
                                                             [fn-name arg]
                                                             (emit :call fn-name arg))}

   {"function-name = #'[A-Z]+'" (fn
                                  [name]
                                  name)}

   {"comparison-op = #'<>' | '>' | '<=' | '<' | '=' | '>='" (fn
                                                              [op]
                                                              op)}

   {"decimal = #'[0-9]*' '.'? #'[0-9]+'" (fn
                                           [& values]
                                           (emit :value (Float/parseFloat (apply str values))))}

   {"end = <\"END\">" (fn
                        []
                        (emit :nop))}

   {"def = <\"DEF\" ws> identifier <'('> identifier <')' '='> expression" (fn
                                                                            [fn-name fn-arg expression]
                                                                            (emit :def fn-name fn-arg expression))}

   {"mat = <\"MAT\" ws> identifier <'=ZER'>" (fn
                                               [identifier]
                                               (emit :zero-array identifier))}

   {"bool-op = <'OR'>" (fn
                         []
                         :or)}

   {"image = <\"IMAGE\"> <ws+> formatter-list" (fn
                                                 [fl]
                                                 fl)}

   {"formatter-list = formatter (<','> formatter)*" (fn
                                                      [a & bs]
                                                      (apply emit :formatter-list `[~a ~@bs]))}

   {"formatter = format-count format-type | quoted-string | format-type | (format-count \"(\" formatter-list \")\")" (fn
                                                                                                                       ([number type]
                                                                                                                        (emit :format-type (str number (first (emitted type)))))
                                                                                                                       ([type]
                                                                                                                        type)
                                                                                                                       ([number _ format-list _]
                                                                                                                        (emit :format-repeat number format-list)))}

   {"format-count = #'[-]?[0-9]+'" (fn
                                     [number]
                                     (Integer/parseInt number))}

   {"format-type = \"D\" | \"X\" | \"A\"" (fn
                                            [type]
                                            (emit :format-type type))}))

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
