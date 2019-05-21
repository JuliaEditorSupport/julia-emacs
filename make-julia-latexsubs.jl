#####
##### Generate the file julia-latexsubs.el. Invoke from the shell as
#####
##### julia make-julia-latexsubs.jl
#####

@assert VERSION ≥ v"1"          # use a recent Julia version

import REPL.REPLCompletions: latex_symbols, emoji_symbols

"""
Write Emacs lisp code that populates the hash table named `varname` to `dest`, using
key-value pairs from `src`.
"""
function write_latexsubs_hashtable(src, dest::IO, varname::AbstractString)
    for (k, v) in sort!(collect(src), by = last)
        ks = escape_string(k)
        vs = escape_string(v)
        if occursin(r"^\\U[0-9A-Fa-f]+$", vs)
            # codepoints outside the BMP can be problematic in older Emacsen
            cp = vs[3:end]
            println(dest, "(let ((c (decode-char 'ucs #x$cp)))\n",
                    "  (if c (puthash \"$ks\" (char-to-string c) $(varname))))")
        else
            println(dest, "(puthash \"$ks\" \"$vs\" $(varname))")
        end
    end
end

open("julia-latexsubs.el", "w") do io
    write_latexsubs_hashtable(merge(latex_symbols, emoji_symbols), io, "julia-latexsubs")
end

@info "generated latex substitutions"
