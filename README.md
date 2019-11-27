# Emacs major mode for the julia programming language

[![Build Status](https://travis-ci.org/JuliaEditorSupport/julia-emacs.svg?branch=master)](https://travis-ci.org/JuliaEditorSupport/julia-emacs)

[![MELPA](https://melpa.org/packages/julia-mode-badge.svg)](https://melpa.org/#/julia-mode)

# Installation

## Installing from MELPA

Unless you want to develop this package, it is recommended that you use it from MELPA:

1. [Enable the MELPA repository](https://melpa.org/#/getting-started).

2. Add `(require 'julia-mode)` to your Emacs init file.

## Using the source repository directly

Clone this repository, then use

```elisp
(add-to-list 'load-path "path-to-julia-mode")
(require 'julia-mode)
```
