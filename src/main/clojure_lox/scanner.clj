(ns clojure-lox.scanner
  (:require [clojure.spec.alpha :as s]
            [clojure-lox.error :as e]))

(def error (partial e/error ::error))

(s/def ::token-type #{
                      ;; single-character tokens
                      :left-paren :right-paren :left-brace :right-brace
                      :comma :dot :minus :plus :semicolon :slash :star

                      ;; one or two character tokens
                      :bang :bang-equal
                      :equal :equal-equal
                      :greater :greater-equal
                      :less :less-equal

                      ;; literals
                      :identifier :string :number

                      ;; keywords
                      :and :class :else :false :fun :for :if :nil :or
                      :print :return :super :this :true :var :while

                      :eof
                      })

(s/def ::token (s/keys :req-un [::token-type ::lexeme ::literal ::line]))

(def string->keyword
  {"and"    :and
   "class"  :class
   "else"   :else
   "false"  :false
   "fun"    :fun
   "for"    :for
   "if"     :if
   "nil"    :nil
   "or"     :or
   "print"  :print
   "return" :return
   "super"  :super
   "this"   :this
   "true"   :true
   "var"    :var
   "while"  :while})

(defn- scanning-finished? [text current]
  (>= current (count text)))

(defn- num-occurrences [s c]
  (count (filter #(= c %) s)))

(defn- digit? [c]
  (when c
    (<= (int \0) (int c) (int \9))))

(defn- alpha? [c]
  (when c
    (or (<= (int \a) (int c) (int \z))
        (<= (int \A) (int c) (int \Z))
        (= c \_))))

(defn- alphanumeric? [c]
  (or (digit? c) (alpha? c)))

(defn- scan-tokens* [text tokens errors start line]
  (let [char-at             (fn [n]
                              (get text n))
        current-char-equals (fn [v]
                              (= (char-at start) v))
        next-char-equals    (fn [v]
                              (= (char-at (inc start)) v))
        add-token           (fn add-token
                              ([token-type lexeme-end]
                               (add-token token-type lexeme-end nil))
                              ([token-type lexeme-end literal]
                               (let [lexeme (subs text start lexeme-end)]
                                 (conj tokens {:token-type token-type
                                               :literal    literal
                                               :lexeme     lexeme
                                               :line       line}))))
        jump-to             (fn [c] ; Moves `start` until it is at the next occurrence of the character c.
                              (loop [i (inc start)]
                                (if-not (or (scanning-finished? text i)
                                            (= (char-at i) c))
                                  (recur (inc i))
                                  i)))
        consume-while       (fn [pred start-index]
                              (loop [i start-index]
                                (if (pred (char-at i))
                                  (recur (inc i))
                                  i)))]
    (cond
      (scanning-finished? text start)
      [(conj tokens {:token-type :eof
                     :literal    nil
                     :lexeme     nil
                     :line       line})
       errors]

      (current-char-equals \()
      (recur text (add-token :left-paren (inc start)) errors (inc start) line)

      (current-char-equals \))
      (recur text (add-token :right-paren (inc start)) errors (inc start) line)

      (current-char-equals \{)
      (recur text (add-token :left-brace (inc start)) errors (inc start) line)

      (current-char-equals \})
      (recur text (add-token :right-brace (inc start)) errors (inc start) line)

      (current-char-equals \,)
      (recur text (add-token :comma (inc start)) errors (inc start) line)

      (current-char-equals \.)
      (recur text (add-token :dot (inc start)) errors (inc start) line)

      (current-char-equals \-)
      (recur text (add-token :minus (inc start)) errors (inc start) line)

      (current-char-equals \+)
      (recur text (add-token :plus (inc start)) errors (inc start) line)

      (current-char-equals \;)
      (recur text (add-token :semicolon (inc start)) errors (inc start) line)

      (current-char-equals \*)
      (recur text (add-token :star (inc start)) errors (inc start) line)

      (current-char-equals \!)
      (if (next-char-equals \=)
        (recur text (add-token :bang-equal (+ 2 start)) errors (+ 2 start) line)
        (recur text (add-token :bang (inc start)) errors (inc start) line))

      (current-char-equals \=)
      (if (next-char-equals \=)
        (recur text (add-token :equal-equal (+ 2 start)) errors (+ 2 start) line)
        (recur text (add-token :equal (inc start)) errors (inc start) line))

      (current-char-equals \<)
      (if (next-char-equals \=)
        (recur text (add-token :less-equal (+ 2 start)) errors (+ 2 start) line)
        (recur text (add-token :less (inc start)) errors (inc start) line))

      (current-char-equals \>)
      (if (next-char-equals \=)
        (recur text (add-token :greater-equal (+ 2 start)) errors (+ 2 start) line)
        (recur text (add-token :greater (inc start)) errors (inc start) line))

      (current-char-equals \/)
      (if (next-char-equals \/)
        (recur text tokens errors
               (jump-to \newline)
               line)
        (recur text (add-token :slash (inc start)) errors (inc start) line))

      (or (current-char-equals \space)
          (current-char-equals \tab)
          (current-char-equals \return))
      (recur text tokens errors (inc start) line)

      (current-char-equals \newline)
      (recur text tokens errors (inc start) (inc line))

      (current-char-equals \")
      (let [closing-quote-index (jump-to \")
            new-line-number     (+ line (num-occurrences (subs text start closing-quote-index) \newline))]
        (if (scanning-finished? text closing-quote-index)
          (recur text
                 tokens
                 (conj errors (error new-line-number (str "unterminated string"))) ; consider reporting the old line number instead so that we know where the string starts
                 closing-quote-index
                 new-line-number)
          (recur text
                 (add-token :string (inc closing-quote-index) (subs text (inc start) closing-quote-index))
                 errors
                 (inc closing-quote-index)
                 new-line-number)))

      (digit? (char-at start))
      (let [integral-part-end (consume-while digit? start)]
        (if (and (= \. (char-at integral-part-end))
                 (digit? (char-at (inc integral-part-end))))
          (let [number-end (consume-while digit? (inc integral-part-end))]
            (recur text
                   (add-token :number number-end (Double/parseDouble (subs text start number-end)))
                   errors
                   number-end
                   line))
          (recur text
                 (add-token :number integral-part-end (Double/parseDouble (subs text start integral-part-end)))
                 errors
                 integral-part-end
                 line)))

      (alpha? (char-at start))
      (let [identifier-end (consume-while alphanumeric? start)
            keyword-type   (string->keyword (subs text start identifier-end))]
        (if keyword-type
          (recur text
                 (add-token keyword-type identifier-end)
                 errors
                 identifier-end
                 line)
          (recur text
                 (add-token :identifier identifier-end)
                 errors
                 identifier-end
                 line)))

      :else
      (recur text
             tokens
             (conj errors (error line (str "invalid character " (char-at start))))
             (inc start)
             line))))

(defn scan-tokens [text]
  (scan-tokens* text [] [] 0 1))
