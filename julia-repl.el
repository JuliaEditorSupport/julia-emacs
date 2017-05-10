(require 'term)

;;; Fully functional Julia REPL using screen
;;; Send line or buffer for evaluation with [ctrl-c ctrl-r]
;;; based on http://emacs.stackexchange.com/questions/18775/how-to-get-a-fully-functional-julia-repl-in-emacs

(defvar julia-program-name "julia"  "julia executable name")

(defun julia-repl-send-line (astring)
  ;;send home if we have a regular Julia prompt this line
  ;;otherwise, simulate "C-b" until we get to the beginning
  (if (equal (buffer-substring (- (line-beginning-position) 7) (line-beginning-position)) "julia> ") (term-send-home)
    (progn (while (> (- (point) (line-beginning-position)) 7) (term-send-string "\^b"))))
  (term-send-raw-string "\^k") ;sends a C-k, which kills the Julia line
  ;;This is a Julia REPL functionality - the ordinary emacs kill-line
  ;;function wouldn't work.
  (term-send-raw-string astring) ;send the argument as Julia input
  (term-send-raw-string "\^M") ;sends a return to execute astring
  ;;emacs treats C-m and return as equivalent in many ways
  (term-send-raw-string "\^y") ;sends a C-y, which yanks the killed line
  ;;this is also a Julia REPL functionality, not an emacs functionality
  )

(defun julia-repl-other-window ()
  "Runs Julia in a `term' buffer in another window."
  (interactive)
  (let ((termbuf (apply 'make-term "julia" "screen" nil
			(split-string-and-unquote julia-program-name))))
    (set-buffer termbuf)
    (term-mode)
    (term-set-escape-char 24) ;this sets the escape char to C-x instead of C-c
    (term-char-mode)
    (switch-to-buffer-other-window termbuf)))

(defun julia-send-line-to-repl (&optional line) (interactive)
       (let ((jbuf (get-buffer "*julia*"))
         (cbuf (current-buffer))
         (cwin (selected-window))
         (linecontents
          (progn (when line ;if a line is passed to the function, go there
               (goto-char (point-min))
               (forward-line (- line 1)))
             (buffer-substring (line-beginning-position) (line-end-position))))
         (pos (line-beginning-position 2))) ;save pos of start of next line
     (if jbuf (switch-to-buffer jbuf)
         ;;if there is not a Julia REPl open, open it and wait for prompt
         (progn (julia-repl-other-window)
            (while (or (< (point) 7) (not (equal (buffer-substring (- (point) 7) (point)) "julia> ")))
              (sit-for 0.1))))
       (julia-repl-send-line linecontents)
       (select-window cwin)
       (switch-to-buffer cbuf)
       (goto-char pos)))

(defun julia-send-buffer-to-repl () (interactive)
       (let ((jbuf (get-buffer "*julia*"))
         (cbuf (current-buffer))
         (cwin (selected-window))
         (linecontents
          (progn 
             (buffer-substring (region-beginning) (region-end))))) 
     (if jbuf (switch-to-buffer jbuf)
         ;;if there is not a Julia REPl open, open it and wait for prompt
         (progn (julia-repl-other-window)
            (while (or (< (point) 7) (not (equal (buffer-substring (- (point) 7) (point)) "julia> ")))
              (sit-for 0.1))))
       (julia-repl-send-line linecontents)
       (select-window cwin)
       (switch-to-buffer cbuf)))

(defun julia-send-line-or-buffer-to-repl () (interactive)
       (if (and transient-mark-mode mark-active ;; xemacs doesn't have use-region-p
		(> (region-end) (region-beginning)))
	   (julia-send-buffer-to-repl)
	 (julia-send-line-to-repl)))

(define-key julia-mode-map "\C-c\C-r" 'julia-send-line-or-buffer-to-repl)

(provide 'julia-repl)
