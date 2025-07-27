"""Bazel rule for BNFC grammar processing."""

def _bnfc_grammar_impl(ctx):
    """Implementation of the bnfc_grammar rule."""
    grammar_file = ctx.file.grammar
    bnfc_tool = ctx.executable.bnfc_tool
    
    # Get grammar name from file (e.g., "Calc" from "Calc.cf")
    grammar_name = grammar_file.basename.replace(".cf", "")
    
    # BNFC preserves the exact case of the grammar file name when generating files
    # For "SystemVerilogSimple.cf" -> grammar_name = "SystemVerilogSimple" -> bnfc_name = "SystemVerilogSimple"
    # For "systemverilog_combined.cf" -> grammar_name = "systemverilog_combined" -> bnfc_name = "SystemverilogCombined"
    if "_" in grammar_name:
        # snake_case to CamelCase
        words = grammar_name.split("_")
        bnfc_name = "".join([word.capitalize() for word in words])
    else:
        # Already CamelCase or single word, preserve as-is
        bnfc_name = grammar_name
    
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

        # Run BNFC to generate files (without -d to avoid subdirectory)
        {bnfc} -m {grammar}
        
        # Run Alex and Happy on generated files with proper flags (from BNFC Makefile)
        if [ -f "Lex{bnfc_name}.x" ]; then
            alex --ghc Lex{bnfc_name}.x -o Lex{bnfc_name}.hs
        fi
        if [ -f "Par{bnfc_name}.y" ]; then
            happy --array --info --ghc --coerce Par{bnfc_name}.y -o Par{bnfc_name}.hs
        fi
        
        # Copy outputs to their final locations
        cp Abs{bnfc_name}.hs {abs_output}
        cp Lex{bnfc_name}.hs {lex_output}
        cp Par{bnfc_name}.hs {par_output}
        cp Print{bnfc_name}.hs {print_output}
        
        # Rename ErrM to be grammar-specific and update module name
        sed 's/module ErrM/module ErrM{grammar_name}/' ErrM.hs > {errm_output}
        
        """.format(
            bnfc = bnfc_tool.path,
            grammar = grammar_file.path,
            grammar_name = grammar_name,
            bnfc_name = bnfc_name,
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

def _combine_grammar_impl(ctx):
    """Implementation of the combine_grammar rule."""
    output = ctx.outputs.out
    inputs = ctx.files.srcs
    combine_script = ctx.executable.combine_script
    
    # Create command to combine files
    ctx.actions.run_shell(
        inputs = inputs,
        outputs = [output],
        tools = [combine_script],
        command = "{script} {output} {inputs}".format(
            script = combine_script.path,
            output = output.path,
            inputs = " ".join([f.path for f in inputs]),
        ),
        mnemonic = "CombineGrammar",
        progress_message = "Combining grammar files into {}".format(output.short_path),
    )
    
    return [DefaultInfo(files = depset([output]))]

combine_grammar = rule(
    implementation = _combine_grammar_impl,
    attrs = {
        "srcs": attr.label_list(
            allow_files = [".cf"],
            mandatory = True,
            doc = "The BNFC grammar section files to combine",
        ),
        "combine_script": attr.label(
            executable = True,
            cfg = "exec",
            default = "//tools/bnfc:combine_grammar_script",
            doc = "The script to combine grammar files",
        ),
    },
    outputs = {
        "out": "%{name}.cf",
    },
    doc = "Combine multiple BNFC grammar section files into a single grammar",
)

