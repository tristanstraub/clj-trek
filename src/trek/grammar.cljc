(ns trek.grammar
  (:require [trek.machine :as machine]
            [trek.rules :as rules]
            [clojure.string :as str]))

(defn emit
  [machine statement-type & args]
  {:pre [machine]}
  (machine/emit machine statement-type args))

(defn emitted
  [machine type]
  (machine/emitted machine type))

(defn parse-int
  [v]
  #?(:clj (Integer/parseInt v)
     :cljs (js/parseInt v)))

(defn parse-float
  [v]
  #?(:clj (Float/parseFloat v)
     :cljs (js/parseFloat v)))

(def basic
  (merge
   {"S = line-number <ws> statement <#';'?>" (fn
                                                [machine n s]
                                                (emit machine :statement n s))}

   {"line-number = #'[0-9]+'" (fn
                                [machine n]
                                (parse-int n))}

   {"statement = comment | gosub | print-using | print | input | if | assignment | dim | def | mat | for | goto | goto-of | next | image | return | end" (fn
                                                                                                                                                           [machine s]
                                                                                                                                                           s)}

   {"print = <\"PRINT\"> <ws?> (! 'USING' (<ws> | expression | <\",\"> | <\";\">))* " (fn
                                                                                        ([machine]
                                                                                         (emit machine :print))
                                                                                        ([machine & args]
                                                                                         (emit machine :print args)))}

   {"print-using = <'PRINT'> <ws> <'USING'> <ws> line-number (<';'> expression (<','> expression)*)?" (fn
                                                                                                         [machine line-number & expressions]
                                                                                                         (emit machine :print-using [line-number expressions]))}

   {"goto = <\"GOTO\"> <ws> line-number" (fn
                                           [machine n]
                                           (assert (number? n))
                                           (emit machine :goto n))}

   {"goto-of = <\"GOTO\"> <ws> expression <ws> (<'OF'> <ws> line-number (<','> line-number)*)" (fn
                                                                                                 ([machine n]
                                                                                                  (emit machine :goto n))
                                                                                                 ([machine expression & line-numbers]
                                                                                                  (emit machine :goto-of expression line-numbers)))}

   {"gosub = <\"GOSUB\"> <ws> line-number" (fn
                                             [machine n]
                                             (assert (number? n) "parse:gosub")
                                             (emit machine :gosub n))}

   {"return = <\"RETURN\">" (fn
                              [machine]
                              (emit machine :return))}

   {"for = <\"FOR\"> <ws> identifier <ws?> <'='> expression <ws> <'TO'> <ws> expression" (fn
                                                                                           [machine identifier lower upper]
                                                                                           (emit machine :for identifier lower upper))}

   {"expression = v-expression | m-expression | a-expression" (fn
                                                                [machine v]
                                                                v)}

   {"v-expression = value | <'('> expression <')'> | h-expression" (fn
                                                                     ([machine v]
                                                                      v))}

   {"a-expression = expression additive-op m-expression" (fn
                                                           [machine left op right]
                                                           (emit machine :binary-operator left op right))}

   {"m-expression = v-expression | m-expression multiplicative-op v-expression" (fn
                                                                                  ([machine v]
                                                                                   v)
                                                                                  ([machine left op right]
                                                                                   (emit machine :binary-operator left op right)))}

   {"h-expression = v-expression hat-op v-expression" (fn
                                                        [machine left op right]
                                                        (emit machine :binary-operator left op right))}

   {"additive-op = '+' | '-'" (fn
                                [machine v]
                                v)}

   {"multiplicative-op = '*' | '/'" (fn
                                      [machine v]
                                      v)}

   {"hat-op = '^'" (fn
                     [machine v]
                     v)}

   {"quoted-string = <'\"'> #'[^\"]*' <'\"'>" (fn
                                                [machine v]
                                                (emit machine :value v))}

   {"value = function-call | quoted-string | integer | variable | decimal" (fn
                                                                             [machine x]
                                                                             x)}

   {"integer = #'[-]?[0-9]+'" (fn
                                [machine v]
                                (emit machine :value (parse-int v)))}

   {"ws = #'[ \t]+'" (fn [& _])}

   {"comment = <\"REM\" #'.*'>" (fn
                                  [machine]
                                  (emit machine :nop))}

   {"input = <\"INPUT\"> <ws> identifier (<\",\"> identifier)*" (fn
                                                                  [machine input & inputs]
                                                                  (emit machine :input `[~input ~@inputs]))}

   {"if = <\"IF\"> <ws> bool-expression <ws> <'THEN'> <ws> line-number" (fn
                                                                          [machine expression line-number]
                                                                          (emit machine :if expression (emit machine :goto line-number)))}



   {"variable = identifier | array-ref" (fn
                                          [machine v]
                                          v)}

   {"next = <\"NEXT\"> <ws> identifier" (fn
                                          [machine identifier]
                                          (emit machine :next identifier))}

   {"bool-expression = expression <ws?> comparison-op <ws?> expression (<ws> bool-op <ws> bool-expression)?" (fn
                                                                                                               [machine a comparison-op b & [bool-op right]]
                                                                                                               (let [left (emit machine :compare a comparison-op b)]
                                                                                                                 (if right
                                                                                                                   (emit machine :bool-op left bool-op right)
                                                                                                                   left)))}

   {"assignment = variable <'='> assignment-rhs" (fn
                                                   [machine identifier rhs]
                                                   (emit machine :assign identifier rhs))}

   {"assignment-rhs = (expression | variable <'='> assignment-rhs)" (fn
                                                                      ([machine expression]
                                                                       (emit machine :value expression))
                                                                      ([machine identifier rhs]
                                                                       (emit machine :assign identifier rhs)))}

   {"dim = <\"DIM\" ws> array-defs" (fn
                                      [machine array-defs]
                                      (emit machine :dim array-defs))}

   {"identifier = #'[A-Z][A-Z0-9]*[$]?'" (fn
                                           [machine identifier]
                                           (emit machine :identifier identifier))}

   {"array-ref = identifier <'['> array-indices <']'>" (fn
                                                         [machine identifier indices]
                                                         (emit machine :array-ref identifier indices))}

   {"array-indices = expression (<','> expression)*" (fn
                                                       [machine & indices]
                                                       indices)}

   {"array-def = identifier <'['> dimensions <']'>" (fn
                                                      [machine identifier dimensions]
                                                      (emit machine :array-ref identifier dimensions))}

   {"dimensions = integer (<','> integer)*" (fn
                                              [machine & dimensions]
                                              dimensions)}

   {"array-defs = array-def (<#','> array-ref)*" (fn
                                                   [machine & array-defs]
                                                   array-defs)}

   {"function-call = function-name <'('> expression <')'>" (fn
                                                             [machine fn-name arg]
                                                             (emit machine :call fn-name arg))}

   {"function-name = #'[A-Z]+'" (fn
                                  [machine name]
                                  name)}

   {"comparison-op = #'<>' | '>' | '<=' | '<' | '=' | '>='" (fn
                                                              [machine op]
                                                              op)}

   {"decimal = #'[0-9]*[.]?[0-9]+'" (fn
                                           [machine & values]
                                           (emit machine :value (parse-float (apply str values))))}

   {"end = <\"END\">" (fn
                        [machine]
                        (emit machine :nop))}

   {"def = <\"DEF\" ws> identifier <'('> identifier <')' '='> expression" (fn
                                                                            [machine fn-name fn-arg expression]
                                                                            (emit machine :def fn-name fn-arg expression))}

   {"mat = <\"MAT\" ws> identifier <'=ZER'>" (fn
                                               [machine identifier]
                                               (emit machine :zero-array identifier))}

   {"bool-op = <'OR'>" (fn
                         [machine]
                         :or)}

   {"image = <\"IMAGE\"> <ws> formatter-list" (fn
                                                 [machine fl]
                                                 fl)}

   {"formatter-list = formatter (<','> formatter)*" (fn
                                                      [machine a & bs]
                                                      (apply emit machine :formatter-list `[~a ~@bs]))}

   {"formatter = format-count format-type | quoted-string | format-type | (format-count \"(\" formatter-list \")\")" (fn
                                                                                                                       ([machine number type]
                                                                                                                        (emit machine :format-type (str number (first (emitted machine type)))))
                                                                                                                       ([machine type]
                                                                                                                        type)
                                                                                                                       ([machine number _ format-list _]
                                                                                                                        (emit machine :format-repeat number format-list)))}

   {"format-count = #'[-]?[0-9]+'" (fn
                                     [machine number]
                                     (parse-int number))}

   {"format-type = \"D\" | \"X\" | \"A\"" (fn
                                            [machine type]
                                            (emit machine :format-type type))}))

(defn parser
  [machine]
  {:parser     (rules/parser basic)
   :transforms (rules/transforms basic machine)})

(defn parse
  ([parser machine listing]
   (parse parse machine listing :S))
  ([parser machine listing start]
   {:pre [machine]}
   (let [{:keys [parser transforms]} parser]
     (emit machine :program (rules/parse-lines parser transforms listing start)))))
