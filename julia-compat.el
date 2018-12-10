;;; julia-compat.el --- Compatability with older emacses -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2014 Julia contributors
;; URL: https://github.com/JuliaLang/julia
;; Version: 0.3
;; Keywords: languages

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

;;; Usage:

;;; Commentary:

;; This is simple compatability for older emacses, eg. prior to addition of
;; cl-lib in emacs 24.3

;;; Code:

;; We can't use cl-lib whilst supporting Emacs 23 users who don't use
;; ELPA.
(if (version< emacs-version "24.3")
    (with-no-warnings
      (require 'cl) ;; cl-incf, cl-decf, cl-plusp
      (defmacro cl-incf (num) (incf num))
      (defmacro cl-decf (num) (decf num))
      (defmacro cl-plusp (num) (plusp num)))
  (require 'cl-lib))

;; define ignore-errors macro if it isn't present
;; (necessary for emacs 22 compatibility)
(when (not (fboundp 'ignore-errors))
  (defmacro ignore-errors (body) `(condition-case nil ,body (error nil))))

(defun julia--regexp-opt (strings &optional paren)
  "Emacs 23 provides `regexp-opt', but it does not support PAREN taking the \
value 'symbols.
This function provides equivalent functionality, but makes no efforts to \
optimise the regexp."
  (cond
   ((>= emacs-major-version 24)
    (regexp-opt strings paren))
   ((not (eq paren 'symbols))
    (regexp-opt strings paren))
   ((null strings)
    "")
   ('t
    (rx-to-string `(seq symbol-start (or ,@strings) symbol-end)))))

(provide 'julia-compat)
;;; julia-compat.el ends here
