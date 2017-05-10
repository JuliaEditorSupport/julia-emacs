# Emacs major mode for the julia programming language

[![Build Status](https://travis-ci.org/JuliaEditorSupport/julia-emacs.svg?branch=master)](https://travis-ci.org/JuliaEditorSupport/julia-emacs)

# Install

Put the following code in your `.emacs`, `site-load.el`, or other relevant file

```elisp
(add-to-list 'load-path "path-to-julia-mode")
(require 'julia-mode)
```

If you want fully functioning Julia REPL you need "screen" and

```elisp
(require 'julia-repl)
```

You can send current line or selection to REPL with C-c C-r
