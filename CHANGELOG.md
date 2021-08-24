# Unreleased

- fix indentation of submodule imports

# 0.4

- increase lookback ([#98](https://github.com/JuliaEditorSupport/julia-emacs/pull/98)), fixes [#5](https://github.com/JuliaEditorSupport/julia-emacs/issues/5)

- fix derived parent mode ([#66](https://github.com/JuliaEditorSupport/julia-emacs/pull/66))

- load LaTeX substitution table as a feature ([#93](https://github.com/JuliaEditorSupport/julia-emacs/pull/93))

- drop support for Emacs earlier than 24.3, use `cl-lib` ([#87](https://github.com/JuliaEditorSupport/julia-emacs/pull/87)), reorganize test framework accordingly ([#95](https://github.com/JuliaEditorSupport/julia-emacs/pull/95))

- remove `latexsub` alias for `julia-latexsub` [#101](https://github.com/JuliaEditorSupport/julia-emacs/pull/101)

- correctly font-lock for-loops and ternary expressions as keywords [#102](https://github.com/JuliaEditorSupport/julia-emacs/pull/102)

- use font-lock-constant-face instead of font-lock-preprocessor-face for quoted symbols [#102](https://github.com/JuliaEditorSupport/julia-emacs/pull/102)

# 0.3

This is the first actual release, and the last one to support Emacs 23.
