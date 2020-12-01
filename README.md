# Emacs major mode for the julia programming language

[![Build Status](https://travis-ci.org/JuliaEditorSupport/julia-emacs.svg?branch=master)](https://travis-ci.org/JuliaEditorSupport/julia-emacs)
[![MELPA](https://melpa.org/packages/julia-mode-badge.svg)](https://melpa.org/#/julia-mode)

# Installation

## Installing from MELPA

Unless you want to develop this package, it is recommended that you use it from MELPA:

1. [Enable the MELPA repository](https://melpa.org/#/getting-started).

2. Add `(package-install 'julia-mode)` to your Emacs init file.

3. Add `(require 'julia-mode)` to your Emacs init file.

## Using the source repository directly

Clone this repository, then use

```elisp
(add-to-list 'load-path "path-to-julia-mode")
(require 'julia-mode)
```

# Contributing

Contributions are welcome, in the form of pull requests.

Please

1. add unit tests whenever possible. This may require that functions are broken up into an interface and a backend function, then you can test the backend one.

2. add a short summary in the [Unreleased section of the CHANGELOG](CHANGELOG.md#unreleased).

3. use the `rx` macro (S-expressions) whenever rewriting existing regular expressions or introducing new ones; it keeps the code much more readable.

We do our best to provide feedback within 2 weeks, feel free to bump in a comment after that.
