# julia-mode

[![MELPA](https://melpa.org/packages/julia-mode-badge.svg)](https://melpa.org/#/julia-mode)
[![Build Status](https://travis-ci.org/JuliaEditorSupport/julia-emacs.svg?branch=master)](https://travis-ci.org/JuliaEditorSupport/julia-emacs)

[Emacs](https://www.gnu.org/software/emacs/) major mode for [the Julia programming language](https://julialang.org/).


## Installation

### Installing from MELPA

Unless you want to develop this package, it is recommended that you use it from MELPA:

1. Enable [the MELPA repository](https://melpa.org/#/getting-started).

2. Enable the package by adding these lines to to your Emacs init file, e.g., `~/.emacs`:

```elisp
(package-install 'julia-mode)
(require 'julia-mode)
```

Alternatively, if you are using [`use-package`](https://github.com/jwiegley/use-package), which has been [part of Emacs](https://www.gnu.org/software/emacs/manual/html_mono/use-package.html) since version 29.1, you can use

```elisp
(use-package julia-mode
  :ensure t)
```

### A note on versioning

The code has been on a “rolling release” model before version 1.0: not all code changes were accompanied by a version increment. From version 1.0, we follow [semantic versioning](https://semver.org/).

### Installing from Source

To get the latest version of `julia-mode`, clone this repository and then use:

```elisp
(add-to-list 'load-path "<path-to-julia-mode>")
(require 'julia-mode)
```

## Contributing

Contributions are welcome, in the form of pull requests.

We do our best to provide feedback within 2 weeks. Feel free bump the PR thread with a comment after that.


### Submitting Pull Requests

- Do add unit tests whenever possible. Consider breaking functions into an interface and a backend function for convenient testing.

- Do add a short summary in the *Unreleased* section of the [CHANGELOG](CHANGELOG.md#unreleased).

- Do use the `rx` macro (S-expressions) whenever rewriting regular expressions or introducing new ones. This keeps the code much more readable.


### Working With Tests

It's easy to add new [ERT](https://www.gnu.org/software/emacs/manual/html_node/ert/index.html) tests to the `julia-mode` test suite.

You only need to prepare a new `ert-deftest` definition in `julia-mode-tests.el`.

You can run the test suite from the command line with:

```
emacs -batch -L . -l ert -l julia-mode-tests.el -f  ert-run-tests-batch-and-exit
```
