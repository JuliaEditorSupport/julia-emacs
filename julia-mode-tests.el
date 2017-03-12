;;; julia-mode-tests.el --- Tests for julia-mode.el

;; Copyright (C) 2009-2014 Julia contributors
;; URL: https://github.com/JuliaLang/julia
;; Version: 0.3
;; Keywords: languages

;;; Usage:

;; From command line:
;;
;; emacs -batch -L . -l ert -l julia-mode-tests.el -f  ert-run-tests-batch-and-exit

;;; Commentary:
;; Contains ert tests for julia-mode.el

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

;; We can't use cl-lib whilst supporting Emacs 23 users who don't use
;; ELPA.
(with-no-warnings
  (require 'cl)) ;; incf, decf, plusp

(require 'julia-mode)
(require 'ert)

(defmacro julia--should-indent (from to)
  "Assert that we indent text FROM producing text TO in `julia-mode'."
  `(with-temp-buffer
     (let ((julia-indent-offset 4))
       (julia-mode)
       (insert ,from)
       (indent-region (point-min) (point-max))
       (should (equal (buffer-substring-no-properties (point-min) (point-max))
                      ,to)))))

(defmacro julia--should-font-lock (text pos face)
  "Assert that TEXT at position POS gets font-locked with FACE in `julia-mode'."
  `(with-temp-buffer
     (julia-mode)
     (insert ,text)
     (if (fboundp 'font-lock-ensure)
         (font-lock-ensure (point-min) (point-max))
       (with-no-warnings
         (font-lock-fontify-buffer)))
     (should (eq ,face (get-text-property ,pos 'face)))))

(ert-deftest julia--test-indent-if ()
  "We should indent inside if bodies."
  (julia--should-indent
     "
if foo
bar
end"
     "
if foo
    bar
end"))

(ert-deftest julia--test-indent-else ()
  "We should indent inside else bodies."
  (julia--should-indent
     "
if foo
    bar
else
baz
end"
     "
if foo
    bar
else
    baz
end"))

(ert-deftest julia--test-indent-toplevel ()
  "We should not indent toplevel expressions. "
  (julia--should-indent
     "
foo()
bar()"
     "
foo()
bar()"))

(ert-deftest julia--test-indent-nested-if ()
  "We should indent for each level of indentation."
  (julia--should-indent
     "
if foo
    if bar
bar
    end
end"
     "
if foo
    if bar
        bar
    end
end"))

(ert-deftest julia--test-indent-module-keyword ()
  "Module should not increase indentation at any level."
  (julia--should-indent
   "
module
begin
    a = 1
end
end"
   "
module
begin
    a = 1
end
end")
  (julia--should-indent
   "
begin
module
foo
end
end"
   "
begin
    module
    foo
    end
end"))

(ert-deftest julia--test-indent-function ()
  "We should indent function bodies."
  (julia--should-indent
     "
function foo()
bar
end"
     "
function foo()
    bar
end"))

(ert-deftest julia--test-indent-begin ()
  "We should indent after a begin keyword."
  (julia--should-indent
     "
@async begin
bar
end"
     "
@async begin
    bar
end"))

(ert-deftest julia--test-indent-paren ()
  "We should indent to line up with the text after an open paren."
  (julia--should-indent
     "
foobar(bar,
baz)"
     "
foobar(bar,
       baz)"))

(ert-deftest julia--test-indent-paren-space ()
  "We should indent to line up with the text after an open
paren, even if there are additional spaces."
  (julia--should-indent
     "
foobar( bar,
baz )"
     "
foobar( bar,
        baz )"))

(ert-deftest julia--test-indent-paren-newline ()
  "python-mode-like indentation."
  (julia--should-indent
     "
foobar(
bar,
baz)"
     "
foobar(
    bar,
    baz)")
  (julia--should-indent
     "
foobar(
bar,
baz
)"
     "
foobar(
    bar,
    baz
)"))

(ert-deftest julia--test-indent-equals ()
  "We should increase indent on a trailing =."
  (julia--should-indent
     "
foo() =
bar"
     "
foo() =
    bar"))

(ert-deftest julia--test-indent-operator ()
  "We should increase indent after the first trailing operator
but not again after that."
  (julia--should-indent
   "
foo() |>
bar |>
baz
qux"
   "
foo() |>
    bar |>
    baz
qux"))

(ert-deftest julia--test-indent-ignores-blank-lines ()
  "Blank lines should not affect indentation of non-blank lines."
  (julia--should-indent
     "
if foo

bar
end"
     "
if foo

    bar
end"))

(ert-deftest julia--test-indent-comment-equal ()
  "`=` at the end of comment should not increase indent level."
  (julia--should-indent
     "
# a =
# b =
c"
     "
# a =
# b =
c"))

(ert-deftest julia--test-indent-leading-paren ()
  "`(` at the beginning of a line should not affect indentation."
  (julia--should-indent
     "
\(1)"
     "
\(1)"))

(ert-deftest julia--test-top-level-following-paren-indent ()
  "`At the top level, a previous line indented due to parens should not affect indentation."
  (julia--should-indent
     "y1 = f(x,
       z)
y2 = g(x)"
     "y1 = f(x,
       z)
y2 = g(x)"))

(ert-deftest julia--test-indentation-of-multi-line-strings ()
  "Indentation should only affect the first line of a multi-line string."
    (julia--should-indent
     "   a = \"\"\"
    description
begin
    foo
bar
end
\"\"\""
     "a = \"\"\"
    description
begin
    foo
bar
end
\"\"\""))

(ert-deftest julia--test-indent-of-end-in-brackets ()
  "Ignore end keyword in brackets for the purposes of indenting blocks."
  (julia--should-indent
   "begin
    begin
        arr[1: end - 1]
        end
end"
   "begin
    begin
        arr[1: end - 1]
    end
end"))

(ert-deftest julia--test-indent-after-commented-keyword ()
  "Ignore keywords in comments when indenting."
  (julia--should-indent
   "# if foo
a = 1"
   "# if foo
a = 1"))

(ert-deftest julia--test-indent-after-commented-end ()
  "Ignore `end` in comments when indenting."
  (julia--should-indent
   "if foo
a = 1
#end
b = 1
end"
   "if foo
    a = 1
    #end
    b = 1
end"))

(ert-deftest julia--test-indent-import-export-using ()
  "Toplevel using, export, and import."
  (julia--should-indent
   "export bar, baz,
quux"
   "export bar, baz,
    quux")
  (julia--should-indent
   "using Foo: bar ,
baz,
quux
notpartofit"
   "using Foo: bar ,
    baz,
    quux
notpartofit"))

(ert-deftest julia--test-indent-anonymous-function ()
  "indentation for function(args...)"
  (julia--should-indent
   "function f(x)
function(y)
x+y
end
end"
   "function f(x)
    function(y)
        x+y
    end
end"))

(ert-deftest julia--test-backslash-indent ()
  "indentation for function(args...)"
  (julia--should-indent
   "(\)
   1
   (:\)
       1"
   "(\)
1
(:\)
1"))

(ert-deftest julia--test-indent-keyword-paren ()
  "indentation for ( following keywords"
  "if( a>0 )
end

    function( i=1:2 )
        for( j=1:2 )
            for( k=1:2 )
            end
            end
        end"
  "if( a>0 )
end

function( i=1:2 )
    for( j=1:2 )
        for( k=1:2 )
        end
    end
end")

(ert-deftest julia--test-symbol-font-locking-at-bol ()
  "Symbols get font-locked at beginning or line."
  (julia--should-font-lock
   ":a in keys(Dict(:a=>1))" 1 'julia-quoted-symbol-face))

(ert-deftest julia--test-symbol-font-locking-after-backslash ()
  "Even with a \ before the (, it is recognized as matching )."
  (let ((string "function \\(a, b)"))
    (julia--should-font-lock string (1- (length string)) nil)))

(ert-deftest julia--test-function-assignment-font-locking ()
  (julia--should-font-lock
   "f(x) = 1" 1 'font-lock-function-name-face)
  (julia--should-font-lock
   "Base.f(x) = 1" 6 'font-lock-function-name-face)
  (julia--should-font-lock
   "f(x) where T = 1" 1 'font-lock-function-name-face)
  (julia--should-font-lock
   "f(x) where{T} = 1" 1 'font-lock-function-name-face)
  (dolist (def '("f(x)::T = 1" "f(x) :: T = 1" "f(x::X)::T where X = x"))
    (julia--should-font-lock def 1 'font-lock-function-name-face)))

(ert-deftest julia--test-where-keyword-font-locking ()
  (julia--should-font-lock
   "f(x) where T = 1" 6 'font-lock-keyword-face)
  (dolist (pos '(22 30))
    (julia--should-font-lock
     "function f(::T, ::Z) where T where Z
          1
      end"
     pos 'font-lock-keyword-face)))

(defun julia--run-tests ()
  (interactive)
  (if (featurep 'ert)
      (ert-run-tests-interactively "julia--test")
    (message "Can't run julia-mode-tests because ert is not available.")))

(provide 'julia-mode-tests)
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not obsolete)
;; End:
;;; julia-mode-tests.el ends here
