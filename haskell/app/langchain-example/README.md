# LangChain-Style Prompt Templates Example

This example demonstrates the core concepts of [langchain-hs](https://hackage.haskell.org/package/langchain-hs), a Haskell library for building LLM-powered applications.

## What is langchain-hs?

langchain-hs is a Haskell implementation of LangChain that provides:

- **Prompt Templates**: Dynamic content generation with variable substitution
- **LLM Integrations**: Support for Ollama, OpenAI, and Huggingface
- **Conversational Memory**: Management of conversation context and history
- **Agents and Tools**: Building autonomous agents with tool capabilities
- **Document Loaders**: Reading various file formats (PDF, text, etc.)
- **Vector Storage**: Embeddings and similarity search
- **Output Parsers**: Structured data extraction from LLM responses

## This Example

This example implements a simplified version of langchain-hs's prompt template system to demonstrate the core concepts:

1. **Basic Prompt Template**: Simple variable substitution
2. **Translation Template**: Multi-variable templates for specific tasks
3. **Code Generation**: Prompts for generating code
4. **Chain of Thought**: Structured reasoning prompts

## Build and Run

```bash
# Build the example
bazel build //haskell/app/langchain-example:langchain-example

# Run the example
bazel run //haskell/app/langchain-example:langchain-example
```

### Configuration

The application supports multiple ways to configure API credentials:

#### Option 1: Using environment variables (Recommended for Bazel)

Since Bazel runs in a sandbox, using environment variables is the most reliable approach:

```bash
export KIMI_API_KEY="your-actual-api-key"
export KIMI_ENDPOINT="https://api.moonshot.ai/v1"  # Optional
export KIMI_MODEL="moonshot-v1-8k"                # Optional
bazel run //haskell/app/langchain-example:langchain-example
```

#### Option 2: Using .env file

The application can load a `.env` file from the current working directory. However, note that Bazel runs in a sandbox, so this works best when running the compiled binary directly:

```bash
# Create .env file in the project root
cp haskell/app/langchain-example/.env.example .env

# Edit with your API key
nano .env  # or your preferred editor

# Run the compiled binary directly (not through bazel run)
bazel build //haskell/app/langchain-example:langchain-example
./bazel-bin/haskell/app/langchain-example/langchain-example
```

Example `.env` file:
```bash
KIMI_API_KEY=your-actual-api-key
KIMI_ENDPOINT=https://api.moonshot.ai/v1
KIMI_MODEL=moonshot-v1-8k
```

### Environment Variables

- `KIMI_API_KEY`: Your Kimi/Moonshot API key (default: "your-api-key-here")
- `KIMI_ENDPOINT`: API endpoint URL (default: "https://api.moonshot.ai/v1")
- `KIMI_MODEL`: Model name to use (default: "moonshot-v1-8k")

Available models: `moonshot-v1-8k`, `moonshot-v1-32k`, `moonshot-v1-128k`

## Note on Dependencies

The actual langchain-hs library has dependency conflicts with `pdf-toolbox-core` on the current LTS-24.19 snapshot due to `bytestring` version requirements. This example provides a conceptual demonstration of the library's approach using a custom implementation.

To use the full langchain-hs library, you would need to:
1. Resolve the `pdf-toolbox-core` dependency conflicts
2. Add `langchain-hs` and `ollama-haskell` to your dependencies
3. Install and run Ollama locally for LLM inference

## Using with Ollama

To integrate with actual LLMs using langchain-hs:

```bash
# Install Ollama
curl -fsSL https://ollama.ai/install.sh | sh

# Download and run a model
ollama run llama3.2
```

Then in your Haskell code:

```haskell
import Langchain.LLM.Ollama
import Langchain.LLM.Core
import Langchain.PromptTemplate

main :: IO ()
main = do
  let ollamaLLM = Ollama "llama3.2" [stdOutCallback]
      prompt = PromptTemplate "Translate to French: {text}"
      input = Map.fromList [("text", "Hello, world!")]

  case renderPrompt prompt input of
    Right renderedPrompt -> do
      result <- generate ollamaLLM renderedPrompt Nothing
      print result
```

## Resources

- [langchain-hs on Hackage](https://hackage.haskell.org/package/langchain-hs)
- [langchain-hs GitHub Repository](https://github.com/tusharad/langchain-hs)
- [Ollama](https://ollama.ai/)
- [LangChain Documentation](https://www.langchain.com/)
