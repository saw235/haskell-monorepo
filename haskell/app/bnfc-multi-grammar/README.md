# Multi-Grammar BNFC Example

This example demonstrates how to use multiple BNFC grammar files in a single project, useful for large applications that need to parse different kinds of input or implement modular parsers.

## Overview

This project showcases a simple programming language parser split across two separate grammar files:

- **`Expression.cf`** - Handles arithmetic expressions, variables, and function calls
- **`Statement.cf`** - Handles control flow, assignments, and function definitions

## Grammar Files

### Expression.cf
- **Arithmetic operations**: +, -, *, /, %, ^ (with proper precedence)
- **Unary operators**: +, - (positive/negative)
- **Primary expressions**: integers, doubles, variables, function calls, parentheses
- **Function calls**: `func(arg1, arg2, ...)`

### Statement.cf
- **Variable declarations**: `var x = expression;`
- **Assignments**: `x = expression;`
- **Control flow**: `if`, `if-else`, `while`, `for` loops
- **Function definitions**: `function name(param1, param2) { ... }`
- **Return statements**: `return expression;` or `return;`
- **Expression statements**: `expression;`
- **Print statements**: `print(expression);`

## Building and Running

```bash
# Build the multi-grammar example
bazel build //haskell/app/bnfc-multi-grammar:bnfc-multi-grammar

# Run the interactive demo
bazel run //haskell/app/bnfc-multi-grammar:bnfc-multi-grammar
```

## Usage Examples

### Expression Parser
```
expr: 2^3 + 4*5
Result: 28.0

expr: -(5 + 3) * 2  
Result: -16.0

expr: 10 % 3 + 1
Result: 2.0
```

### Statement Parser
```
prog: var x = 10; var y = x + 5; print(y);
Statistics: StmtStats {assignments = 2, ifStatements = 0, whileLoops = 0, functionDefs = 0, expressions = 1}
```

## Multi-Grammar Architecture

### Key Challenges Solved

1. **Module Name Conflicts**: Both grammars generate `ErrM.hs` files. This is solved by:
   - Renaming generated ErrM modules to `ErrM<GrammarName>.hs`
   - Using qualified imports for each grammar's modules

2. **Shared Types**: Some types (like expressions) might be needed in both grammars:
   - Currently handled by duplicating basic expression rules in Statement.cf
   - For production use, consider creating a shared base grammar

3. **Build System Integration**: Each grammar gets its own `bnfc_grammar` target:
   ```bazel
   bnfc_grammar(name = "expression_grammar", grammar = "Expression.cf")
   bnfc_grammar(name = "statement_grammar", grammar = "Statement.cf")
   ```

### Generated Modules

Each grammar generates 5 modules:
- `Abs<Grammar>.hs` - Abstract syntax tree types
- `Lex<Grammar>.hs` - Lexer (tokenizer)  
- `Par<Grammar>.hs` - Parser with `p<Type>` functions and `myLexer`
- `Print<Grammar>.hs` - Pretty printer
- `ErrM<Grammar>.hs` - Error handling monad (renamed to avoid conflicts)

### Import Pattern

```haskell
-- Expression parser
import qualified ParExpression as ParExpr
import qualified AbsExpression as AbsExpr
import qualified ErrMExpression as ErrMExpr

-- Statement parser
import qualified ParStatement as ParStmt  
import qualified AbsStatement as AbsStmt
import qualified ErrMStatement as ErrMStmt

-- Use one ErrM for consistency
import ErrMExpression (Err(..))
```

## Advanced Multi-Grammar Techniques

### 1. Grammar Composition
For larger projects, consider these patterns:

**Shared Base Grammar**:
```
CommonTypes.cf    -- Basic types, identifiers, literals
Expression.cf     -- Expressions (imports CommonTypes)
Statement.cf      -- Statements (imports CommonTypes)
Program.cf        -- Top-level constructs
```

**Domain-Specific Grammars**:
```
SQLGrammar.cf     -- Database queries
JSONGrammar.cf    -- Configuration files  
TemplateGrammar.cf -- Template language
```

### 2. Modular Parsing Strategy

```haskell
-- Different parsers for different contexts
parseExpression :: String -> Either String Expr
parseStatement  :: String -> Either String Stmt  
parseProgram    :: String -> Either String Program
parseConfig     :: String -> Either String Config
```

### 3. Cross-Grammar References

When one grammar needs to reference types from another:

```haskell
-- Convert between different AST types
exprToStmtExpr :: AbsExpr.Expr -> AbsStmt.Expr
stmtExprToExpr :: AbsStmt.Expr -> AbsExpr.Expr
```

## Performance Considerations

1. **Separate Compilation**: Each grammar is compiled independently, improving build times
2. **Selective Parsing**: Only parse what you need for each input type
3. **Memory Usage**: Multiple parsers increase memory usage but improve modularity

## Best Practices

1. **Naming Conventions**: Use descriptive grammar names (`UserInterface.cf`, `DatabaseQuery.cf`)
2. **Module Organization**: Keep related grammars in the same directory
3. **Documentation**: Document the purpose and scope of each grammar file
4. **Testing**: Test each grammar independently and together
5. **Version Control**: Track grammar changes carefully as they affect generated code

## Extending This Example

To add a new grammar:

1. Create `NewGrammar.cf` with your syntax rules
2. Add `bnfc_grammar(name = "new_grammar", grammar = "NewGrammar.cf")` to BUILD.bazel
3. Add `:new_grammar` to the `srcs` list of your `haskell_binary`
4. Import and use the generated modules in your Haskell code

This approach scales well for complex language implementations, DSLs, and applications requiring multiple parsing contexts.