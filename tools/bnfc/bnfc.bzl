"""Bazel rule for BNFC grammar processing."""

def _bnfc_grammar_impl(ctx):
    """Implementation of the bnfc_grammar rule."""
    grammar_file = ctx.file.grammar
    bnfc_tool = ctx.executable.bnfc_tool
    
    # Get grammar name from file (e.g., "Calc" from "Calc.cf")
    grammar_name = grammar_file.basename.replace(".cf", "")
    
    # Define expected output files (excluding Test file to avoid main module conflict)
    # Make ErrM unique per grammar to avoid conflicts when using multiple grammars
    outputs = [
        ctx.actions.declare_file("Abs{}.hs".format(grammar_name)),
        ctx.actions.declare_file("Lex{}.hs".format(grammar_name)),
        ctx.actions.declare_file("Par{}.hs".format(grammar_name)),
        ctx.actions.declare_file("Print{}.hs".format(grammar_name)),
        ctx.actions.declare_file("ErrM{}.hs".format(grammar_name)),
    ]
    
    # Create command to generate files
    ctx.actions.run_shell(
        inputs = [grammar_file],
        outputs = outputs,
        tools = [bnfc_tool],
        command = """
        # Create temporary directory

        # Run BNFC to generate files (without -d to avoid subdirectory)
        {bnfc} -m {grammar}
        
        # Run Alex and Happy on generated files (files are in current directory)
        if [ -f "Lex{grammar_name}.x" ]; then
            alex Lex{grammar_name}.x -o Lex{grammar_name}.hs
        fi
        if [ -f "Par{grammar_name}.y" ]; then
            happy Par{grammar_name}.y -o Par{grammar_name}.hs
        fi
        
        # Copy outputs to their final locations
        cp Abs{grammar_name}.hs {abs_output}
        cp Lex{grammar_name}.hs {lex_output}
        cp Par{grammar_name}.hs {par_output}
        cp Print{grammar_name}.hs {print_output}
        
        # Rename ErrM to be grammar-specific and update module name
        sed 's/module ErrM/module ErrM{grammar_name}/' ErrM.hs > {errm_output}
        
        """.format(
            bnfc = bnfc_tool.path,
            grammar = grammar_file.path,
            grammar_name = grammar_name,
            abs_output = outputs[0].path,
            lex_output = outputs[1].path,
            par_output = outputs[2].path,
            print_output = outputs[3].path,
            errm_output = outputs[4].path,
        ),
        mnemonic = "BnfcGenerate",
        progress_message = "Generating parser from {}".format(grammar_file.short_path),
    )
    
    return [DefaultInfo(files = depset(outputs))]

bnfc_grammar = rule(
    implementation = _bnfc_grammar_impl,
    attrs = {
        "grammar": attr.label(
            allow_single_file = [".cf"],
            mandatory = True,
            doc = "The BNFC grammar file (.cf)",
        ),
        "bnfc_tool": attr.label(
            executable = True,
            cfg = "exec",
            default = "@bnfc_linux//:bnfc_tool",
            doc = "The BNFC binary to use",
        ),
    },
    doc = "Generate Haskell parser files from BNFC grammar",
)

