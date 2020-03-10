;;; inferior-julia.el --- Major mode for interacting with an inferior Julia process -*- lexical-binding: t -*-

;; Copyright (C) 2020 julia-mode contributors

;;; Commentary:
;;
;; `inferior-julia-mode' provides a Julia REPL from within emacs. To
;; use it, execute M-x `run-julia'.

;;; Code:

(require 'julia-mode)
(require 'comint)

(defgroup inferior-julia ()
  "Major mode for interacting with an inferior Julia process."
  :group 'julia
  :prefix "inferior-julia-")

(defcustom inferior-julia-program "julia"
  "Path to the program used by `inferior-julia'."
  :type 'string)

(defcustom inferior-julia-buffer "*Inferior Julia*"
  "Name of buffer for running an inferior Julia process."
  :type 'string)

(defcustom inferior-julia-arguments '("-i" "--color=yes" "-q")
  "Commandline arguments to pass to `inferior-julia-program'."
  :type '(repeat (string :tag "argument")))

(defcustom inferior-julia-prompt-read-only comint-prompt-read-only
  "If non-nil, the Julia prompt is read only."
  :type 'boolean)

;; We need to know where this location is in order to correctly
;; support visiting source of stacktrace in compilation-mode
(defvar inferior-julia-depot
  (with-output-to-string
    (call-process inferior-julia-program nil standard-output nil
                  "--startup-file=no" "-e" "print(last(DEPOT_PATH))"))
  "Path to directory containing Julia base and stdlib.")

(defvar inferior-julia--depot-base-format
  (concat
   (file-name-as-directory (expand-file-name "base" inferior-julia-depot))
   "%s")
  "Format string to use with stacktraces with Julia Base paths.")

(defvar inferior-julia-prompt-regexp "^\\w*> "
  "Regexp for matching `inferior-julia' prompt.")

(defvar inferior-julia-error-regexp-alist
  (list
   ;; TODO: see https://github.com/JuliaLang/julia/issues/35191 for
   ;; reason why this doesn't work yet for stdlib.
   (list (rx line-start " [" (1+ num) "] " (1+ nonl)
             " at " (group-n 3
                             (group-n 1 ?/ (1+ (not (any ?\n ?:))))
                             ?:
                             (group-n 2 (1+ num))))
         1 2 nil nil 3)
   ;; The FILE for stack frames from Base looks like "./array.jl"
   (list (rx line-start " [" (1+ num) "] " (1+ nonl)
             " at " (group-n 3
                             ?. (any ?\\ ?/) (group-n 1 (1+ (not (any ?\n ?:))))
                             ?:
                             (group-n 2 (1+ num))))
         (list 1 inferior-julia--depot-base-format) 2 nil nil 3)
   (list (rx line-start " [" (1+ num) "] " (1+ nonl) " at " (group "none:1"))
         nil nil nil nil 1))
  "Value for `compilation-error-regexp-alist' in inferior Julia.")

(defvar inferior-julia-mode-map
  (nconc (make-sparse-keymap) comint-mode-map)
  "Basic mode map for `inferior-julia-mode'.")

(defvar inferior-julia-mode-syntax-table
  (make-syntax-table julia-mode-syntax-table)
  "Syntax table for use in `inferior-julia-mode' buffers.")

(defun inferior-julia--send (proc string)
  "Send STRING to inferior Julia PROC.
Checks if first character in STRING is special. \"?\" invokes Julia
help, \"]\" uses the Pkg repl, and \";\" sends shell commands."
  (let* ((c (aref string 0))
         (wrapped-string (substring string 1))
         (wrapper (cond
                   ((char-equal c ??)
                    "eval(_InferiorJulia.REPL.helpmode(\"%s\"))")
                   ((char-equal c ?\])
                    "_InferiorJulia.Pkg.pkg\"%s\"")
                   ((char-equal c ?\;)
                    "Base.repl_cmd(`%s`, stdout)")
                   (t
                    (setq wrapped-string string)
                    "%s"))))
    (comint-simple-send proc (format wrapper wrapped-string))))

;;;###autoload
(defun inferior-julia (&optional arg)
    "Run an inferior instance of julia inside Emacs."
    (interactive "P")
    (let ((buffer (get-buffer-create inferior-julia-buffer)))
      (unless arg
        (pop-to-buffer buffer))
      (with-current-buffer buffer
        (let ((proc (apply #'make-comint
                           (substring inferior-julia-buffer 1 -1)
                           inferior-julia-program nil
                           inferior-julia-arguments)))
          ;; Pkg and REPL modules must be accessible for ] and ? magics
          (comint-send-string
           proc "baremodule _InferiorJulia import Pkg, REPL end;\n"))
        (inferior-julia-mode))
      buffer))

(define-derived-mode inferior-julia-mode comint-mode "Inferior Julia"
  "Major mode for interacting with an inferior Julia process.

Key bindings:
\\<inferior-julia-mode-map>"
  :group 'inferior-julia
  (set-syntax-table inferior-julia-mode-syntax-table)
  (setq-local comment-use-syntax t)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults '(julia-font-lock-keywords t))
  (setq-local syntax-propertize-function julia-syntax-propertize-function)
  (setq-local indent-line-function #'julia-indent-line)
  (setq-local beginning-of-defun-function #'julia-beginning-of-defun)
  (setq-local end-of-defun-function #'julia-end-of-defun)
  (setq-local indent-tabs-mode nil)

  (setq-local comint-prompt-regexp inferior-julia-prompt-regexp)
  (setq-local comint-prompt-read-only inferior-julia-prompt-read-only)

  (setq-local compilation-error-regexp-alist inferior-julia-error-regexp-alist)
  (compilation-shell-minor-mode 1)
  (compilation-forget-errors)

  (setq-local paragraph-start inferior-julia-prompt-regexp)

  (setq-local comint-input-sender #'inferior-julia--send))

;;;###autoload
(defalias 'run-julia #'inferior-julia
  "Run an inferior instance of julia inside Emacs.")

(provide 'inferior-julia)
;;; inferior-julia.el ends here
