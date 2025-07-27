# SystemVerilog BNFC Parser

This project implements a SystemVerilog parser using BNFC (BNF Converter). It's designed to parse SystemVerilog source code and provide analysis capabilities for hardware description language (HDL) processing.

## Overview

This SystemVerilog parser is structured as a multi-grammar BNFC project, separating concerns into different grammar files for better maintainability and modularity:

- **`SystemVerilogModule.cf`** - Handles module definitions, port lists, and top-level constructs
- **`SystemVerilogExpression.cf`** - Handles SystemVerilog expressions, operators, and literals

## Current Status

🚧 **This is a scaffolded project awaiting the complete BNF specification.**

The current grammar files contain placeholder rules that demonstrate the structure but are not yet the complete SystemVerilog specification. These will be replaced when the full BNF is provided.

## Grammar Files

### SystemVerilogModule.cf (Placeholder)
- **Module definitions**: `module ... endmodule`
- **Port declarations**: `input`, `output`, `inout` with types
- **Wire/reg declarations**: Basic signal declarations
- **Assign statements**: Continuous assignments
- **Always blocks**: Procedural blocks (basic structure)

### SystemVerilogExpression.cf (Placeholder)  
- **Logical operators**: `||`, `&&`, `!`
- **Bitwise operators**: `|`, `^`, `&`, `~`, `<<`, `>>`
- **Arithmetic operators**: `+`, `-`, `*`, `/`, `%`, `**`
- **Comparison operators**: `==`, `!=`, `<`, `>`, `<=`, `>=`
- **Primary expressions**: identifiers, numbers, bit strings
- **Bit selection**: `signal[index]`, `signal[high:low]`
- **Concatenation**: `{signal1, signal2, ...}`

## Building and Running

```bash
# Build the SystemVerilog parser
bazel build //haskell/app/bnfc-systemverilog-parser:bnfc-systemverilog-parser

# Run the interactive demo
bazel run //haskell/app/bnfc-systemverilog-parser:bnfc-systemverilog-parser
```

## Usage Examples (After BNF Implementation)

### Module Parser
```
module: module counter(input clk, input rst, output [7:0] count); endmodule
Module statistics: ModuleStats {moduleCount = 1, portCount = 3, ...}
```

### Expression Parser
```
expr: data[7:0] & mask
Parsed expression: EBitAnd (EBitSelect "data" (ENumber 7) (ENumber 0)) (EIdentifier "mask")
```

## Architecture

### Multi-Grammar Design

This parser follows the multi-grammar pattern from the `bnfc-multi-grammar` example:

1. **Separate Concerns**: Module-level constructs vs expressions
2. **Qualified Imports**: Avoid module name conflicts
3. **Shared Types**: Common expression types used across grammars
4. **Modular Analysis**: Different analysis functions for different constructs

### Generated Modules

Each grammar generates:
- `AbsSystemVerilog<Type>.hs` - Abstract syntax tree types
- `LexSystemVerilog<Type>.hs` - Lexer (tokenizer)
- `ParSystemVerilog<Type>.hs` - Parser with `p<Type>` functions
- `PrintSystemVerilog<Type>.hs` - Pretty printer
- `ErrMSystemVerilog<Type>.hs` - Error handling monad

### Import Pattern

```haskell
-- Module parser
import qualified ParSystemVerilogModule as ParMod
import qualified AbsSystemVerilogModule as AbsMod

-- Expression parser  
import qualified ParSystemVerilogExpression as ParExpr
import qualified AbsSystemVerilogExpression as AbsExpr

-- Error handling
import ErrMSystemVerilogModule (Err(..))
```

## Analysis Capabilities

The parser will provide analysis for:

- **Module Statistics**: Count of modules, ports, declarations
- **Port Analysis**: Input/output/inout classification, bit widths
- **Signal Analysis**: Wire vs reg usage, bit widths, arrays
- **Structural Analysis**: Always blocks, assign statements, instantiations
- **Expression Analysis**: Operator usage, signal dependencies
- **Hierarchy Analysis**: Module instantiation relationships

## SystemVerilog-Specific Features

The parser is designed to handle SystemVerilog-specific constructs:

- **Enhanced Data Types**: `logic`, `bit`, packed arrays
- **Interfaces**: Interface definitions and modports
- **Classes**: Object-oriented programming constructs
- **Packages**: Package imports and scope resolution
- **Assertions**: SVA (SystemVerilog Assertions)
- **Constraints**: Randomization constraints
- **Covergroups**: Functional coverage constructs

## Next Steps

1. **📋 Provide BNF Specification**: Replace placeholder grammars with complete SystemVerilog BNF
2. **🔧 Update Main.hs**: Implement proper analysis functions for SystemVerilog constructs
3. **🧪 Add Test Cases**: Create comprehensive test suite with SystemVerilog examples
4. **📈 Enhance Analysis**: Add more sophisticated analysis capabilities
5. **🔍 Error Reporting**: Improve error messages and recovery
6. **⚡ Optimization**: Performance tuning for large SystemVerilog files

## Contributing

When the BNF is provided:

1. Replace the `.cf` files with the complete grammar specification
2. Update `Main.hs` analysis functions to match the new AST types
3. Add comprehensive test cases
4. Update this README with actual usage examples

## Related Projects

- **bnfc-multi-grammar**: Reference implementation for multi-grammar BNFC projects
- **BNFC**: The BNF Converter tool for generating parsers
- **SystemVerilog Standard**: IEEE 1800 SystemVerilog specification

This parser aims to be a comprehensive tool for SystemVerilog analysis, supporting both academic research and industrial HDL processing workflows. 