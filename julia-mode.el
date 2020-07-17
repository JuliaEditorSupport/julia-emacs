;;; julia-mode.el --- Major mode for editing Julia source code -*- lexical-binding: t -*-

;; Copyright (C) 2009-2014 Julia contributors, 2015-2020 julia-mode contributors
;; URL: https://github.com/JuliaEditorSupport/julia-emacs
;; Version: 0.4
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

;;; Usage:
;; Put the following code in your .emacs, site-load.el, or other relevant file
;; (add-to-list 'load-path "path-to-julia-mode")
;; (require 'julia-mode)

;;; Commentary:
;; This is the official Emacs mode for editing Julia programs.

;;; License:
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(require 'cl-lib)
(require 'julia-mode-latexsubs)

(defvar julia-mode-hook nil)

(defgroup julia ()
  "Major mode for the julia programming language."
  :group 'languages
  :prefix "julia-")
;; all custom variables are automatically added to the most recent defgroup

(defcustom julia-indent-offset 4
  "Number of spaces per indentation level."
  :type 'integer)

(defface julia-macro-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for Julia macro invocations.")

(defface julia-quoted-symbol-face
  '((t :inherit font-lock-constant-face))
  "Face for quoted Julia symbols, e.g. :foo.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))

(defvar julia-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?# "< 14" table)  ; # single-line and multiline start
    (modify-syntax-entry ?= ". 23bn" table)
    (modify-syntax-entry ?\n ">" table)  ; \n single-line comment end
    (modify-syntax-entry ?\{ "(} " table)
    (modify-syntax-entry ?\} "){ " table)
    (modify-syntax-entry ?\[ "(] " table)
    (modify-syntax-entry ?\] ")[ " table)
    (modify-syntax-entry ?\( "() " table)
    (modify-syntax-entry ?\) ")( " table)
    ;; Here, we treat ' as punctuation (when it's used for transpose),
    ;; see our use of `julia-char-regex' for handling ' as a character
    ;; delimiter
    (modify-syntax-entry ?'  "." table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?` "\"" table)
    ;; Backslash has escape syntax for use in strings but
    ;; julia-syntax-propertize-function sets punctuation syntax on it
    ;; outside strings.
    (modify-syntax-entry ?\\ "\\" table)

    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?? "." table)
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?% "." table)

    (modify-syntax-entry ?′ "w" table) ; \prime is a word constituent
    table)
  "Syntax table for `julia-mode'.")

(eval-when-compile
  (defconst julia-char-regex
    (rx (or (any "-" ";" "\\" "^" "!" "|" "?" "*" "<" "%" "," "=" ">" "+" "/" "&" "$" "~" ":")
            (syntax open-parenthesis)
            (syntax whitespace)
            bol)
        (group "'")
        (or (repeat 0 8 (not (any "'"))) (not (any "\\"))
            "\\\\")
        (group "'"))))

(defconst julia-hanging-operator-regexp
  ;; taken from julia-parser.scm
  (rx (or
       ;; conditional
       "?"
       ;; assignment
       "=" ":=" "+=" "-=" "*=" "/=" "//=" ".//=" ".*=" "./=" "\\=" ".\\="
       "^=" ".^=" "÷=" ".÷=" "%=" ".%=" "|=" "&=" "$=" "=>" "<<=" ">>="
       ">>>=" "~" ".+=" ".-="
       ;; arrow
       "--" "-->" "←" "→" "↔" "↚" "↛" "↠" "↣" "↦" "↮" "⇎" "⇏" "⇒" "⇔" "⇴"
       "⇶" "⇷" "⇸" "⇹" "⇺" "⇻" "⇼" "⇽" "⇾" "⇿" "⟵" "⟶" "⟷" "⟷" "⟹"
       "⟺" "⟻" "⟼" "⟽" "⟾" "⟿" "⤀" "⤁" "⤂" "⤃" "⤄" "⤅" "⤆" "⤇" "⤌"
       "⤍" "⤎" "⤏" "⤐" "⤑" "⤔" "⤕" "⤖" "⤗" "⤘" "⤝" "⤞" "⤟" "⤠" "⥄" "⥅"
       "⥆" "⥇" "⥈" "⥊" "⥋" "⥎" "⥐" "⥒" "⥓" "⥖" "⥗" "⥚" "⥛" "⥞" "⥟" "⥢"
       "⥤" "⥦" "⥧" "⥨" "⥩" "⥪" "⥫" "⥬" "⥭" "⥰" "⧴" "⬱" "⬰" "⬲" "⬳" "⬴"
       "⬵" "⬶" "⬷" "⬸" "⬹" "⬺" "⬻" "⬼" "⬽" "⬾" "⬿" "⭀" "⭁" "⭂" "⭃" "⭄"
       "⭇" "⭈" "⭉" "⭊" "⭋" "⭌" "￩" "￫"
       ;; or and and
       "&&" "||"
       ;; comparison
       ">" "<" ">=" "≥" "<=" "≤" "==" "===" "≡" "!=" "≠" "!==" "≢" ".>"
       ".<" ".>=" ".≥" ".<=" ".≤" ".==" ".!=" ".≠" ".=" ".!" "<:" ">:" "∈"
       "∉" "∋" "∌" "⊆" "⊈" "⊂" "⊄" "⊊" "∝" "∊" "∍" "∥" "∦" "∷" "∺" "∻" "∽"
       "∾" "≁" "≃" "≄" "≅" "≆" "≇" "≈" "≉" "≊" "≋" "≌" "≍" "≎" "≐" "≑" "≒"
       "≓" "≔" "≕" "≖" "≗" "≘" "≙" "≚" "≛" "≜" "≝" "≞" "≟" "≣" "≦" "≧" "≨"
       "≩" "≪" "≫" "≬" "≭" "≮" "≯" "≰" "≱" "≲" "≳" "≴" "≵" "≶" "≷" "≸" "≹"
       "≺" "≻" "≼" "≽" "≾" "≿" "⊀" "⊁" "⊃" "⊅" "⊇" "⊉" "⊋" "⊏" "⊐" "⊑" "⊒"
       "⊜" "⊩" "⊬" "⊮" "⊰" "⊱" "⊲" "⊳" "⊴" "⊵" "⊶" "⊷" "⋍" "⋐" "⋑" "⋕" "⋖"
       "⋗" "⋘" "⋙" "⋚" "⋛" "⋜" "⋝" "⋞" "⋟" "⋠" "⋡" "⋢" "⋣" "⋤" "⋥" "⋦" "⋧"
       "⋨" "⋩" "⋪" "⋫" "⋬" "⋭" "⋲" "⋳" "⋴" "⋵" "⋶" "⋷" "⋸" "⋹" "⋺" "⋻" "⋼"
       "⋽" "⋾" "⋿" "⟈" "⟉" "⟒" "⦷" "⧀" "⧁" "⧡" "⧣" "⧤" "⧥" "⩦" "⩧" "⩪" "⩫"
       "⩬" "⩭" "⩮" "⩯" "⩰" "⩱" "⩲" "⩳" "⩴" "⩵" "⩶" "⩷" "⩸" "⩹" "⩺" "⩻" "⩼"
       "⩽" "⩾" "⩿" "⪀" "⪁" "⪂" "⪃" "⪄" "⪅" "⪆" "⪇" "⪈" "⪉" "⪊" "⪋" "⪌" "⪍"
       "⪎" "⪏" "⪐" "⪑" "⪒" "⪓" "⪔" "⪕" "⪖" "⪗" "⪘" "⪙" "⪚" "⪛" "⪜" "⪝" "⪞"
       "⪟" "⪠" "⪡" "⪢" "⪣" "⪤" "⪥" "⪦" "⪧" "⪨" "⪩" "⪪" "⪫" "⪬" "⪭" "⪮" "⪯"
       "⪰" "⪱" "⪲" "⪳" "⪴" "⪵" "⪶" "⪷" "⪸" "⪹" "⪺" "⪻" "⪼" "⪽" "⪾" "⪿" "⫀"
       "⫁" "⫂" "⫃" "⫄" "⫅" "⫆" "⫇" "⫈" "⫉" "⫊" "⫋" "⫌" "⫍" "⫎" "⫏" "⫐" "⫑"
       "⫒" "⫓" "⫔" "⫕" "⫖" "⫗" "⫘" "⫙" "⫷" "⫸" "⫹" "⫺" "⊢" "⊣"
       ;; pipe, colon
       "|>" "<|" ":" ".."
       ;; plus
       "+" "-" "⊕" "⊖" "⊞" "⊟" ".+" ".-" "++" "|" "∪" "∨" "$" "⊔" "±" "∓"
       "∔" "∸" "≂" "≏" "⊎" "⊻" "⊽" "⋎" "⋓" "⧺" "⧻" "⨈" "⨢" "⨣" "⨤" "⨥" "⨦"
       "⨧" "⨨" "⨩" "⨪" "⨫" "⨬" "⨭" "⨮" "⨹" "⨺" "⩁" "⩂" "⩅" "⩊" "⩌" "⩏" "⩐"
       "⩒" "⩔" "⩖" "⩗" "⩛" "⩝" "⩡" "⩢" "⩣"
       ;; bitshift
       "<<" ">>" ">>>" ".<<" ".>>" ".>>>"
       ;; times
       "*" "/" "./" "÷" ".÷" "%" "⋅" "∘" "×" ".%" ".*" "\\"
       ".\\" "&" "∩" "∧" "⊗" "⊘" "⊙" "⊚" "⊛" "⊠" "⊡" "⊓" "∗" "∙" "∤" "⅋"
       "≀" "⊼" "⋄" "⋆" "⋇" "⋉" "⋊" "⋋" "⋌" "⋏" "⋒" "⟑" "⦸" "⦼" "⦾" "⦿" "⧶"
       "⧷" "⨇" "⨰" "⨱" "⨲" "⨳" "⨴" "⨵" "⨶" "⨷" "⨸" "⨻" "⨼" "⨽" "⩀" "⩃" "⩄"
       "⩋" "⩍" "⩎" "⩑" "⩓" "⩕" "⩘" "⩚" "⩜" "⩞" "⩟" "⩠" "⫛" "⊍" "▷" "⨝" "⟕"
       "⟖" "⟗"
       ;; rational
       "//" ".//"
       ;; power
       "^" ".^" "↑" "↓" "⇵" "⟰" "⟱" "⤈" "⤉" "⤊" "⤋" "⤒" "⤓" "⥉" "⥌" "⥍"
       "⥏" "⥑" "⥔" "⥕" "⥘" "⥙" "⥜" "⥝" "⥠" "⥡" "⥣" "⥥" "⥮" "⥯" "￪" "￬"
       ;; decl, dot
       "::" ".")
      (* blank)
      (or eol ?#)))

(defconst julia-unquote-regex
  "\\(\\s(\\|\\s-\\|-\\|[,%=<>\\+*/?&|!\\^~\\\\;:]\\|^\\)\\($[a-zA-Z0-9_]+\\)")

(defconst julia-forloop-in-regex
  "for +.*[^
].* \\(in\\)\\(\\s-\\|$\\)+")

(defconst julia--forloop-=-regex
  (rx "for"
      (1+ ? )
      ;; expression can't end and no newline before = portion of forloop.
      (1+ (not (any ?\n ?\; ?=)))
      (group (any ?= ?∈))
      ;; don't want to match on == expression instead of =.
      (not (any ?=))))

(defconst julia-ternary-regex
  " +\\(\\?\\)[
 ]+[^
]* +\\(:\\)[
 ]+")

;; functions of form "function f(x) nothing end"
(defconst julia-function-regex
  (rx line-start (* (or space "@inline" "@noinline")) symbol-start
      "function"
      (1+ space)
      ;; Don't highlight module names in function declarations:
      (* (seq (1+ (or word (syntax symbol))) "."))
      ;; The function name itself
      (group (1+ (or word (syntax symbol))))))

;; functions of form "f(x) = nothing"
(defconst julia-function-assignment-regex
  (rx line-start (* (or space "@inline" "@noinline")) symbol-start
      (* (seq (1+ (or word (syntax symbol))) ".")) ; module name
      (group (1+ (or word (syntax symbol))))
      "(" (* (or
              (seq "(" (* (not (any "(" ")"))) ")")
              (not (any "(" ")"))))
      ")"
      (* space)
      (? "::" (* space) (1+ (not (any space))))
      (* space)
      (* (seq "where" (or "{" (+ space)) (+ (not (any "=")))))
      "="
      (not (any "="))))

(defconst julia-type-regex
  (rx symbol-start (or ;;"immutable" "type" ;; remove after 0.6
                       "abstract type" "primitive type" "struct" "mutable struct")
      (1+ space) (group (1+ (or word (syntax symbol))))))

(defconst julia-type-annotation-regex
  (rx "::" (0+ space) (group (1+ (or word (syntax symbol))))))

(defconst julia-subtype-regex
  (rx "<:" (0+ space) (group (1+ (or word (syntax symbol)))) (0+ space) (or "\n" "{" "}" "end" ",")))

(defconst julia-macro-regex
  (rx symbol-start (group "@" (1+ (or word (syntax symbol))))))

(defconst julia-keyword-regex
  (regexp-opt
   '("if" "else" "elseif" "while" "for" "begin" "end" "quote"
     "try" "catch" "return" "local" "function" "macro"
     "finally" "break" "continue" "global" "where"
     "module" "using" "import" "export" "const" "let" "do"
     "baremodule"
     ;; "importall" ;; deprecated in 0.7
     ;; "immutable" "type" "bitstype" "abstract" "typealias" ;; removed in 1.0
     "abstract type" "primitive type" "struct" "mutable struct")
   'symbols))

(defconst julia-quoted-symbol-regex
  ;; :foo and :foo2 are valid, but :123 is not.
  (rx (or bol whitespace "(" "[" "," "=")
      (group ":" (or letter (syntax symbol)) (0+ (or word (syntax symbol))))))

(defconst julia-font-lock-keywords
  ;; font-lock-builtin-face intentionally unused since any name from
  ;; names(Base) can be aliased in a baremodule.
  (list
   ;; Highlight quoted symbols before keywords, so :function is not
   ;; highlighted as a keyword.
   (list julia-quoted-symbol-regex 1 ''julia-quoted-symbol-face)
   (cons julia-keyword-regex 'font-lock-keyword-face)
   (cons julia-macro-regex ''julia-macro-face)
   (cons
    (regexp-opt
     ;; constants defined in Core plus true/false
     '("true" "false" "Cvoid" "Inf" "NaN" "Inf32" "NaN32" "nothing" "undef" "missing")
     'symbols)
    'font-lock-constant-face)
   (cons "ccall" 'font-lock-builtin-face)
   (list julia-unquote-regex 2 'font-lock-constant-face)
   (list julia-forloop-in-regex 1 'font-lock-keyword-face)
   (list julia--forloop-=-regex 1 'font-lock-keyword-face)
   (list julia-ternary-regex (list 1 'font-lock-keyword-face) (list 2 'font-lock-keyword-face))
   (list julia-function-regex 1 'font-lock-function-name-face)
   (list julia-function-assignment-regex 1 'font-lock-function-name-face)
   (list julia-type-regex 1 'font-lock-type-face)
   ;; font-lock-type-face is for the point of type definition rather
   ;; than usage, but using for type annotations is an acceptable pun.
   (list julia-type-annotation-regex 1 'font-lock-type-face)
   (list julia-subtype-regex 1 'font-lock-type-face)))

(defconst julia-block-start-keywords
  (list "if" "while" "for" "begin" "try" "function" "let" "macro"
        "quote" "do" "module" "baremodule"
        ;; "immutable" "type" ;; remove after 0.6
        "abstract type" "primitive type" "struct" "mutable struct"))

;; For keywords that begin a block without additional indentation
(defconst julia-block-start-keywords-no-indent
  (list "module" "baremodule"))

(defconst julia-block-end-keywords
  (list "end" "else" "elseif" "catch" "finally"))

(defsubst julia-syntax-comment-or-string-p (&optional syntax-ppss)
  "Return non-nil if SYNTAX-PPSS is inside string or comment."
  (nth 8 (or syntax-ppss (syntax-ppss))))

(defun julia-in-comment (&optional syntax-ppss)
  "Return non-nil if point is inside a comment using SYNTAX-PPSS.
Handles both single-line and multi-line comments."
  (nth 4 (or syntax-ppss (syntax-ppss))))

(defun julia-in-string (&optional syntax-ppss)
  "Return non-nil if point is inside a string using SYNTAX-PPSS.
Note this is Emacs' notion of what is highlighted as a string.
As a result, it is true inside \"foo\", `foo` and 'f'."
  (nth 3 (or syntax-ppss (syntax-ppss))))

(defconst julia-syntax-propertize-function
  (syntax-propertize-rules
   ;; triple-quoted strings are a single string rather than 3
   ("\"\"\""
    (0 (ignore (julia-syntax-stringify))))
   ;; same with triple-quoted backticks
   ("```"
    (0 (ignore (julia-syntax-stringify))))
   ;; backslash acts as an operator if it's not inside a string
   ("\\\\"
    (0 (unless (julia-in-string
                (save-excursion (syntax-ppss (match-beginning 0))))
         (string-to-syntax "."))))
   (julia-char-regex
    ;; treat ' in 'c' as string-delimiter
    (1 "\"")
    (2 "\""))))

(defun julia-syntax-stringify ()
  "Put `syntax-table' property correctly on triple-quoted strings and cmds."
  (let* ((ppss (save-excursion (syntax-ppss (match-beginning 0))))
         (string-open (and (not (nth 4 ppss)) (nth 8 ppss))))
    (cond
     ;; this set of quotes delimit the start of string/cmd
     ((not string-open)
      (put-text-property (match-beginning 0) (1+ (match-beginning 0))
                         'syntax-table (string-to-syntax "|")))
     ;; this set of quotes closes the current string/cmd
     ((and
       ;; check that """ closes """ and ``` closes ```
       (eq (char-before) (char-after string-open))
       ;; check that triple quote isn't escaped by odd number of backslashes
       (let ((i 0))
         (while (and (< (point-min) (- (match-beginning 0) i))
                     (eq (char-before (- (match-beginning 0) i)) ?\\))
           (setq i (1+ i)))
         (cl-evenp i)))
      (put-text-property (1- (match-end 0)) (match-end 0)
                         'syntax-table (string-to-syntax "|")))
     ;; Put point after (match-beginning 0) to account for possibility
     ;; of overlapping triple-quotes with first escaped
     ((backward-char 2)))))

(defun julia-in-brackets ()
  "Return non-nil if point is inside square brackets."
  (let ((start-pos (point))
        (open-count 0))
    ;; Count all the [ and ] characters on the current line.
    (save-excursion
      (beginning-of-line)

      (while (< (point) start-pos)
        ;; Don't count [ or ] inside strings, characters or comments.
        (unless (julia-syntax-comment-or-string-p)

          (when (looking-at (rx "["))
            (cl-incf open-count))
          (when (looking-at (rx "]"))
            (cl-decf open-count)))

        (forward-char 1)))

    ;; If we've opened more than we've closed, we're inside brackets.
    (cl-plusp open-count)))

(defun julia-at-keyword (kw-list)
  "Return the word at point if it matches any keyword in KW-LIST.
KW-LIST is a list of strings.  The word at point is not considered
a keyword if used as a field name, X.word, or quoted, :word."
  (and (or (bobp)
	   (and (not (equal (char-before (point)) ?.))
		(not (equal (char-before (point)) ?:))))
       (not (looking-at "("))           ; handle "function(" when on (
       (member (current-word t) kw-list)
       ;; 'end' is not a keyword when used for indexing, e.g. foo[end-2]
       (or (not (equal (current-word t) "end"))
           (not (julia-in-brackets)))
       (not (julia-in-comment))))

;; if backward-sexp gives an error, move back 1 char to move over the '('
(defun julia-safe-backward-sexp ()
  (if (condition-case nil (backward-sexp) (error t))
      (ignore-errors (backward-char))))

(defun julia-following-import-export-using ()
  "If the current line follows an `export` or `import` keyword
with valid syntax, return the position of the keyword, otherwise
`nil`. Works by stepping backwards through comma-separated
symbol, gives up when this is not true."
  ;; Implementation accepts a single Module: right after the keyword, and saves
  ;; the module name for future use, but does not enforce that `export` has no
  ;; module name.
  (let ((done nil)                      ; find keyword or give up
        (module nil))                   ; found "Module:"
    (save-excursion
      (beginning-of-line)
      (while (and (not done) (< (point-min) (point)))
        (julia-safe-backward-sexp)
        (cond
         ((looking-at (regexp-opt (list "import" "export" "using")))
          (setf done (point)))
         ((looking-at (rx (group (* (or word (syntax symbol)))) (0+ space) ":"))
          (if module
              (setf done 'broken)
            (setf module (match-string-no-properties 1))))
         ((looking-at (rx (* (or word (syntax symbol))) (0+ space) ","))
          (when module (setf done 'broken)))
         (t (setf done 'broken)))))
    (if (eq done 'broken)
        nil
      done)))

(defun julia-last-open-block-pos (min)
  "Return the position of the last open block, if one found.
Do not move back beyond position MIN."
  (save-excursion
    (let ((nesting-count 0))
      (while (not (or (> nesting-count 0) (<= (point) min)))
        (julia-safe-backward-sexp)
        (setq nesting-count
              (cond ((julia-at-keyword julia-block-start-keywords)
                     (+ nesting-count 1))
                    ((julia-at-keyword '("end"))
                     (- nesting-count 1))
                    (t nesting-count))))
      (if (> nesting-count 0)
          (point)
        nil))))

(defun julia-last-open-block (min)
  "Move back and return indentation level for last open block.
Do not move back beyond MIN."
  ;; Ensure MIN is not before the start of the buffer.
  (setq min (max min (point-min)))
  (let ((pos (julia-last-open-block-pos min)))
    (and pos
	 (progn
	   (goto-char pos)
	   (+ julia-indent-offset (current-indentation))))))

(defcustom julia-max-block-lookback 20000
  "When indenting, don't look back more than this many characters
to see if there are unclosed blocks.

This variable has a small effect on indent performance if set
too high, but stops indenting in the middle of long blocks if set
too low."
  :type 'integer)

(defun julia-paren-indent ()
  "Return the column of the text following the innermost
containing paren before point, so we can align succeeding code
with it. Returns nil if we're not within nested parens."
  (save-excursion
    (beginning-of-line)
    (let ((parser-state (syntax-ppss)))
      (cond ((nth 3 parser-state) nil)       ;; strings
            ((= (nth 0 parser-state) 0) nil) ;; top level
            (t
             (ignore-errors ;; return nil if any of these movements fail
               (beginning-of-line)
               (skip-syntax-forward " ")
               (let ((possibly-close-paren-point (point)))
                 (backward-up-list)
                 (let ((open-paren-point (point)))
                   (forward-char)
                   (skip-syntax-forward " ")
                   (if (eolp)
                       (progn
                         (up-list)
                         (backward-char)
                         (let ((paren-closed (= (point) possibly-close-paren-point)))
                           (goto-char open-paren-point)
                           (beginning-of-line)
                           (skip-syntax-forward " ")
                           (+ (current-column)
                              (if paren-closed
                                  0
                                julia-indent-offset))))
                     (current-column))))))))))

(defun julia-prev-line-skip-blank-or-comment ()
  "Move point to beginning of previous line skipping blank lines
and lines including only comments. Returns number of lines moved.
A return of -1 signals that we moved to the first line of
the (possibly narrowed) buffer, so there is nowhere else to go."
  (catch 'result
    (let ((moved 0) this-move)
      (while t
        (setq this-move (forward-line -1))
        (cond
         ;; moved into comment or blank
         ((and (= 0 this-move)
               (or (looking-at-p "^\\s-*\\(?:#.*\\)*$")
                   (julia-in-comment)))
          (cl-incf moved))
         ;; success
         ((= 0 this-move)
          (throw 'result (1+ moved)))
         ;; on first line and in comment
         ((and (bobp)
               (or (looking-at-p "^\\s-*\\(?:#.*\\)*$")
                   (julia-in-comment)))
          (throw 'result -1))
         ((bobp)
          (throw 'result moved))
         (t
          (throw 'result 0)))))))

(defun julia--hanging-operator-p ()
  "Return t if current line ends with a hanging operator."
  (and (re-search-forward julia-hanging-operator-regexp (line-end-position) t)
       (not (julia-syntax-comment-or-string-p
             (save-excursion (syntax-ppss (match-beginning 0)))))))

(defun julia-indent-hanging ()
  "Calculate indentation for lines that follow \"hanging\"
operators (operators that end the previous line) as defined in
`julia-hanging-operator-regexp'. An assignment operator ending
the previous line increases the indent as do the other operators
unless another operator is found two lines up. Previous line
means previous line after skipping blank lines and lines with
only comments."
  (let (prev-indent)
    (save-excursion
      (when (> (julia-prev-line-skip-blank-or-comment) 0)
        (setq prev-indent (current-indentation))
        (when (julia--hanging-operator-p)
          (if (and (> (julia-prev-line-skip-blank-or-comment) 0)
                   (julia--hanging-operator-p))
              ;; two preceding hanging operators => indent same as line
              ;; above
              prev-indent
            ;; one preceding hanging operator => increase indent from line
            ;; above
            (+ julia-indent-offset prev-indent)))))))

(defun julia-indent-in-string ()
  "Indentation inside strings with newlines is \"manual\",
meaning always increase indent on TAB and decrease on S-TAB."
  (save-excursion
    (beginning-of-line)
    (when (julia-in-string)
      (if (member this-command '(julia-latexsub-or-indent
                                 ess-indent-or-complete))
          (+ julia-indent-offset (current-indentation))
        ;; return the current indentation to prevent other functions from
        ;; indenting inside strings
        (current-indentation)))))

(defun julia-indent-import-export-using ()
  "Indent offset for lines that follow `import` or `export`, otherwise nil."
  (when (julia-following-import-export-using)
    julia-indent-offset))

(defun julia-indent-line ()
  "Indent current line of julia code."
  (interactive)
  (let* ((point-offset (- (current-column) (current-indentation))))
    (indent-line-to
     (or
      ;; note: if this first function returns nil the beginning of the line
      ;; cannot be in a string
      (julia-indent-in-string)
      ;; If we're inside an open paren, indent to line up arguments. After this,
      ;; we cannot be inside parens which includes brackets
      (julia-paren-indent)
      ;; indent due to hanging operators (lines ending in an operator)
      (julia-indent-hanging)
      ;; indent for import and export
      (julia-indent-import-export-using)
      ;; Indent according to how many nested blocks we are in.
      (save-excursion
        (beginning-of-line)
        ;; jump out of any comments
        (let ((state (syntax-ppss)))
          (when (nth 4 state)
            (goto-char (nth 8 state))))
        (forward-to-indentation 0)
        (let ((endtok (julia-at-keyword julia-block-end-keywords))
              (last-open-block (julia-last-open-block (- (point) julia-max-block-lookback))))
          (max 0 (+ (or last-open-block 0)
                    (if (or endtok
                            (julia-at-keyword julia-block-start-keywords-no-indent))
                        (- julia-indent-offset) 0)))))))
    ;; Point is now at the beginning of indentation, restore it
    ;; to its original position (relative to indentation).
    (when (>= point-offset 0)
      (move-to-column (+ (current-indentation) point-offset)))))


;;; Navigation
;; based off python.el
(defconst julia-beginning-of-defun-regex
  (concat julia-function-regex "\\|"
          julia-function-assignment-regex "\\|"
          "\\_<macro\\_>")
  "Regex matching beginning of Julia function or macro.")

(defun julia-syntax-context-type (&optional syntax-ppss)
  "Return the context type using SYNTAX-PPSS.
TYPE can be `comment', `string' or `paren'."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cond
     ((nth 8 ppss) (if (nth 4 ppss) 'comment 'string))
     ((nth 1 ppss) 'paren))))

(defun julia-looking-at-beginning-of-defun (&optional syntax-ppss)
  "Check if point is at `beginning-of-defun' using SYNTAX-PPSS."
  (and (not (julia-syntax-comment-or-string-p (or syntax-ppss (syntax-ppss))))
       (save-excursion
         (beginning-of-line 1)
         (looking-at julia-beginning-of-defun-regex))))

(defun julia--beginning-of-defun (&optional arg)
  "Internal implementation of `julia-beginning-of-defun'.
With positive ARG search backwards, else search forwards."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let* ((re-search-fn (if (> arg 0)
                           #'re-search-backward
                         #'re-search-forward))
         (line-beg-pos (line-beginning-position))
         (line-content-start (+ line-beg-pos (current-indentation)))
         (pos (point-marker))
         (beg-indentation
          (and (> arg 0)
               (save-excursion
                 (while (and (not (julia-looking-at-beginning-of-defun))
                             ;; f(x) = ... function bodies may span multiple lines
                             (or (and (julia-indent-hanging)
                                      (forward-line -1))
                                 ;; inside dangling parameter list
                                 (and (eq 'paren (julia-syntax-context-type))
                                      (backward-up-list))
                                 (julia-last-open-block (point-min)))))
                 (or (and (julia-looking-at-beginning-of-defun)
                          (+ (current-indentation) julia-indent-offset))
                     0))))
         (found
          (progn
            (when (and (< arg 0)
                       (julia-looking-at-beginning-of-defun))
              (end-of-line 1))
            (while (and (funcall re-search-fn
                                 julia-beginning-of-defun-regex nil t)
                        (or (julia-syntax-comment-or-string-p)
                            ;; handle nested defuns when moving backwards
                            ;; by checking matching indentation
                            (and (> arg 0)
                                 (not (= (current-indentation) 0))
                                 (>= (current-indentation) beg-indentation)))))
            (and (julia-looking-at-beginning-of-defun)
                 (or (not (= (line-number-at-pos pos)
                             (line-number-at-pos)))
                     (and (>= (point) line-beg-pos)
                          (<= (point) line-content-start)
                          (> pos line-content-start)))))))
    (if found
        (or (beginning-of-line 1) (point))
      (and (goto-char pos) nil))))

(defun julia-beginning-of-defun (&optional arg)
  "Move point to `beginning-of-defun'.
With positive ARG search backwards else search forward.
ARG nil or 0 defaults to 1.  When searching backwards,
nested defuns are handled depending on current point position.
Return non-nil (point) if point moved to `beginning-of-defun'."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let ((found))
    (while (and (not (= arg 0))
                (let ((keep-searching-p
                       (julia--beginning-of-defun arg)))
                  (when (and keep-searching-p (null found))
                    (setq found t))
                  keep-searching-p))
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))
    found))

(defun julia-end-of-defun ()
  "Move point to the end of the current function.
Return nil if point is not in a function, otherwise point."
  (interactive)
  (let ((beg-defun-indent))
    (when (or (julia-looking-at-beginning-of-defun)
              (julia-beginning-of-defun 1)
              (julia-beginning-of-defun -1))
      (beginning-of-line)
      (if (looking-at-p julia-function-assignment-regex)
          ;; f(x) = ...
          (progn
            ;; skip any dangling lines
            (while (and (forward-line)
                        (not (eobp))
                        (or (julia-indent-hanging)
                            ;; dangling closing paren
                            (and (eq 'paren (julia-syntax-context-type))
                                 (search-forward ")"))))))
        ;; otherwise skip forward to matching indentation (not in string/comment)
        (setq beg-defun-indent (current-indentation))
        (while (and (not (eobp))
                    (forward-line 1)
                    (or (julia-syntax-comment-or-string-p)
                        (> (current-indentation) beg-defun-indent)))))
      (end-of-line)
      (point))))

;;; IMENU
(defvar julia-imenu-generic-expression
  ;; don't use syntax classes, screws egrep
  '(("Function (_)" "[ \t]*function[ \t]+\\(_[^ \t\n]*\\)" 1)
    ("Function" "^[ \t]*function[ \t]+\\([^_][^\t\n]*\\)" 1)
    ("Const" "[ \t]*const \\([^ \t\n]*\\)" 1)
    ("Type"  "^[ \t]*[a-zA-Z0-9_]*type[a-zA-Z0-9_]* \\([^ \t\n]*\\)" 1)
    ("Require"      " *\\(\\brequire\\)(\\([^ \t\n)]*\\)" 2)
    ("Include"      " *\\(\\binclude\\)(\\([^ \t\n)]*\\)" 2)
    ;; ("Classes" "^.*setClass(\\(.*\\)," 1)
    ;; ("Coercions" "^.*setAs(\\([^,]+,[^,]*\\)," 1) ; show from and to
    ;; ("Generics" "^.*setGeneric(\\([^,]*\\)," 1)
    ;; ("Methods" "^.*set\\(Group\\|Replace\\)?Method(\"\\(.+\\)\"," 2)
    ;; ;;[ ]*\\(signature=\\)?(\\(.*,?\\)*\\)," 1)
    ;; ;;
    ;; ;;("Other" "^\\(.+\\)\\s-*<-[ \t\n]*[^\\(function\\|read\\|.*data\.frame\\)]" 1)
    ;; ("Package" "^.*\\(library\\|require\\)(\\(.*\\)," 2)
    ;; ("Data" "^\\(.+\\)\\s-*<-[ \t\n]*\\(read\\|.*data\.frame\\).*(" 1)))
    ))

;;;###autoload
(define-derived-mode julia-mode prog-mode "Julia"
  "Major mode for editing julia code."
  :group 'julia
  (set-syntax-table julia-mode-syntax-table)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults '(julia-font-lock-keywords))
  (setq-local syntax-propertize-function julia-syntax-propertize-function)
  (setq-local indent-line-function #'julia-indent-line)
  (setq-local beginning-of-defun-function #'julia-beginning-of-defun)
  (setq-local end-of-defun-function #'julia-end-of-defun)
  (setq indent-tabs-mode nil)
  (setq imenu-generic-expression julia-imenu-generic-expression)
  (imenu-add-to-menubar "Imenu"))

(defun julia-manual-deindent ()
  "Deindent by `julia-indent-offset' regardless of current
indentation context. To be used to manually indent inside
strings."
  (interactive)
  (indent-line-to (max 0 (- (current-indentation) julia-indent-offset))))
(define-key julia-mode-map (kbd "<backtab>") 'julia-manual-deindent)

;; (See Julia issue #8947 for why we don't use the Emacs tex input mode.)
(defun julia-latexsub ()
  "Perform a LaTeX-like Unicode symbol substitution."
  (interactive "*i")
  (let ((orig-pt (point)))
    (while (not (or (bobp) (= ?\\ (char-before))
		    (= ?\s (char-syntax (char-before)))))
      (backward-char))
    (if (and (not (bobp)) (= ?\\ (char-before)))
        (progn
          (backward-char)
          (let ((sub (gethash (buffer-substring (point) orig-pt) julia-mode-latexsubs)))
            (if sub
                (progn
                  (delete-region (point) orig-pt)
                  (insert sub))
              (goto-char orig-pt))))
      (goto-char orig-pt))))

(defun julia-latexsub-or-indent (arg)
  "Either indent according to mode or perform a LaTeX-like symbol substution"
  (interactive "*i")
  (if (julia-latexsub)
      (indent-for-tab-command arg)))
(define-key julia-mode-map (kbd "TAB") 'julia-latexsub-or-indent)

;; Math insertion in julia. Use it with
;; (add-hook 'julia-mode-hook 'julia-math-mode)
;; (add-hook 'inferior-julia-mode-hook 'julia-math-mode)

(when (featurep 'latex)
  (declare-function LaTeX-math-abbrev-prefix "latex")

  (defun julia-math-insert (s)
    "Inserts math symbol given by `s'"
    (when s
      (let ((sym (gethash (concat "\\" s) julia-mode-latexsubs)))
        (when sym
          (insert sym)))))

  (with-no-warnings
    (define-minor-mode julia-math-mode
      "A minor mode with easy access to TeX math commands. The
command is only entered if it is supported in Julia. The
following commands are defined:

\\{LaTeX-math-mode-map}"
      nil nil (list (cons (LaTeX-math-abbrev-prefix) LaTeX-math-keymap))
      (if julia-math-mode
          (setq-local LaTeX-math-insert-function #'julia-math-insert)))))

;; Code for `inferior-julia-mode'
(require 'comint)

(defcustom julia-program "julia"
  "Path to the program used by `inferior-julia'."
  :type 'string
  :group 'julia)

(defcustom julia-arguments '("-i" "--color=yes")
  "Commandline arguments to pass to `julia-program'."
  :type '(repeat (string :tag "argument"))
  :group 'julia)

(defvar julia-prompt-regexp "^\\w*> "
  "Regexp for matching `inferior-julia' prompt.")

(defvar inferior-julia-mode-map
  (let ((map2 (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map2 (kbd "TAB") 'julia-latexsub-or-indent)
    map2)
  "Basic mode map for `inferior-julia-mode'.")

;;;###autoload
(defun inferior-julia ()
    "Run an inferior instance of julia inside Emacs."
    (interactive)
    (let ((julia-program julia-program))
      (when (not (comint-check-proc "*Julia*"))
        (apply #'make-comint-in-buffer "Julia" "*Julia*"
               julia-program nil julia-arguments))
      (pop-to-buffer-same-window "*Julia*")
      (inferior-julia-mode)))

(defun inferior-julia--initialize ()
    "Helper function to initialize `inferior-julia'."
    (setq comint-use-prompt-regexp t))

(define-derived-mode inferior-julia-mode comint-mode "Julia"
  "Major mode for `inferior-julia'.

\\<inferior-julia-mode-map>"
  nil "Julia"
  (setq-local comint-prompt-regexp julia-prompt-regexp)
  (setq-local comint-prompt-read-only t)
  (setq-local font-lock-defaults '(julia-font-lock-keywords t))
  (setq-local paragraph-start julia-prompt-regexp)
  (setq-local indent-line-function #'julia-indent-line))

(add-hook 'inferior-julia-mode-hook #'inferior-julia--initialize)

;;;###autoload
(defalias 'run-julia #'inferior-julia
  "Run an inferior instance of julia inside Emacs.")

(provide 'julia-mode)

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not obsolete)
;; End:
;;; julia-mode.el ends here
