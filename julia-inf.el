;;; julia-inf.el --- Inferior Julia shell -*- lexical-binding: t; -*-

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

;; Support for running an inferior Julia shell in comint mode.
;; inferior shell runs in `inferior-julia-mode'

;;; Code:
(require 'julia-mode)
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
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map (kbd "TAB") 'julia-latexsub-or-indent)
    map)
  "Basic mode map for `inferior-julia-mode'.")

(defvar inferior-julia-buffer-name "*Julia*"
  "Name of buffer in which to run inferior Julia process.")

;;;###autoload
(defalias 'run-julia #'inferior-julia
  "Run an inferior instance of `julia' inside Emacs.")

;;;###autoload
(defun inferior-julia ()
    "Run an inferior instance of `julia' inside Emacs."
    (interactive)
    (let ((julia-program julia-program)
          (buffer (get-buffer-create inferior-julia-buffer-name)))
      (when (not (comint-check-proc buffer))
        (apply #'make-comint-in-buffer "Julia" buffer
               julia-program nil julia-arguments))
      (pop-to-buffer-same-window buffer)
      (inferior-julia-mode)))

(defun inferior-julia--initialize ()
    "Helper function to initialize `inferior-julia'."
    (setq comint-use-prompt-regexp t))

(define-derived-mode inferior-julia-mode comint-mode "Julia"
  "Major mode for `inferior-julia'.

\\<inferior-julia-mode-map>"
  nil "Julia"
  (setq comint-prompt-regexp julia-prompt-regexp)
  (setq comint-prompt-read-only t)
  (set (make-local-variable 'font-lock-defaults) '(julia-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) julia-prompt-regexp)
  (set (make-local-variable 'indent-line-function) 'julia-indent-line))

(add-hook 'inferior-julia-mode-hook 'inferior-julia--initialize)

(provide 'julia-inf)
;; Local Variables:
;; coding: utf-8
;; End:
;;; julia-inf.el ends here
