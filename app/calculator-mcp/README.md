# Calculator MCP Server

A Model Context Protocol (MCP) server that provides calculator functionality through tools, prompts, and resources.

## Features

### Tools
- **add**: Add two numbers together
- **subtract**: Subtract the second number from the first
- **multiply**: Multiply two numbers
- **divide**: Divide the first number by the second
- **power**: Raise a number to a given exponent
- **sqrt**: Calculate the square root of a number

### Prompts
- **calculate_expression**: Get help with calculating mathematical expressions
- **get_help**: Show help information for the calculator

### Resources
- **operations**: List of supported mathematical operations
- **constants**: Common mathematical constants (π, e, φ, etc.)

## Building

```bash
bazel build //app/calculator-mcp:calculator-mcp
```

## Running

```bash
bazel run //app/calculator-mcp:calculator-mcp
```

## Usage with MCP Clients

This server can be used with any MCP-compatible client (like Claude Desktop, etc.) by configuring the client to use this binary.

### Example Configuration

For Claude Desktop, add to your `claude_desktop_config.json`:

```json
{
    "mcpServers": {
        "calculator": {
            "command": "bazel",
            "args": [
                "run",
                "//app/calculator-mcp:calculator-mcp"
            ]
        }
    }
}
```

## Implementation Details

This server uses the [haskell-mcp-server](https://github.com/drshade/haskell-mcp-server) library and demonstrates:

- Template Haskell derivation for automatic MCP schema generation
- Type-safe tool, prompt, and resource definitions
- Custom descriptions for better MCP integration
- Error handling for edge cases (division by zero, negative square roots)

## Dependencies

- `mcp-server`: The MCP server framework
- `text`: Text handling utilities
- `base`: Standard Haskell libraries 