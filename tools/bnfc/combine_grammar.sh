#!/bin/bash

# Script to combine multiple BNFC grammar files into a single file
# Usage: combine_grammar.sh output.cf input1.cf input2.cf ...

if [ $# -lt 2 ]; then
    echo "Usage: $0 output.cf input1.cf input2.cf ..."
    exit 1
fi

OUTPUT="$1"
shift

# Create the output file
cat > "$OUTPUT" << 'EOF'
-- Combined SystemVerilog Grammar
-- Generated from IEEE 1800 SystemVerilog Standard sections
-- This file is auto-generated - do not edit manually

EOF

# Combine all input files
for INPUT in "$@"; do
    echo "-- ============================================================================" >> "$OUTPUT"
    echo "-- $(basename "$INPUT" .cf)" >> "$OUTPUT"
    echo "-- ============================================================================" >> "$OUTPUT"
    echo "" >> "$OUTPUT"
    
    # Remove comment headers and add content
    sed '/^--.*IEEE.*SystemVerilog.*Standard/d; /^--.*Based on.*IEEE.*1800/d' "$INPUT" >> "$OUTPUT"
    echo "" >> "$OUTPUT"
done

echo "Combined grammar written to $OUTPUT" 