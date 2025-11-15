# Feature Specification: Agentic Framework Library

**Feature Branch**: `002-agentic-framework`
**Created**: 2025-01-15
**Status**: Draft
**Input**: User description: "I want to create a library focusing on agentic work build on top of langchain. Should define what an Agent is, the system prompt, the input prompt to the agent, the tools that the agent is allowed to access, the common tools like file operations, websearch (some of these langchain might already have, so we can wrap around it). How to orchestrate the agents, can agent use another agent as tools (handoff flows). And mechanism for agents to get access to common methodology/skills that is described in MD files. Refer to this (https://learn.microsoft.com/en-us/agent-framework/user-guide/workflows/orchestrations/overview) for orchestration concepts. A layer to report/log/observe the agent interactions/conversations/reflections or tool callings etc."

## Clarifications

### Session 2025-11-15

- Q: How should agents pass context/information to subsequent agents in orchestration flows? → A: Structured handoff object: Explicit data structure (summary + key outputs + metadata) passed between agents with programmatic access
- Q: What should be the default timeout value for individual tool executions? → A: 10s
- Q: What structure should skill markdown files follow? → A: Structured headers with label and description at top (loaded initially for discovery), full content (## Purpose, ## Methodology, ## Examples) loaded on-demand to save context. Template generation tool provided.
- Q: When a tool call fails, how should the agent respond? → A: Retry with reasoning (agent receives error, can retry with modified parameters, max 3 attempts)
- Q: Where should the library write execution logs? → A: Pluggable handlers with default monad-logger integration; colorized stdout support; developers can provide custom handlers

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Create and Run a Single Agent with Tools (Priority: P1)

A developer wants to create an autonomous agent that can answer questions by accessing tools like file operations and web search. The agent should reason about which tools to use and execute them automatically.

**Why this priority**: This is the foundational capability - a single working agent demonstrates core value and can be used immediately for practical tasks without needing multi-agent orchestration.

**Independent Test**: Can be fully tested by creating one agent with 2-3 tools (calculator, file reader), giving it a task requiring tool usage, and verifying it produces correct results. Delivers immediate value for single-agent use cases.

**Acceptance Scenarios**:

1. **Given** a developer has defined an agent with a system prompt and available tools, **When** they send a query requiring tool usage, **Then** the agent autonomously selects and executes the appropriate tool(s) and provides an answer
2. **Given** an agent with file reading capabilities, **When** asked to summarize a document, **Then** the agent reads the file and produces a summary without manual intervention
3. **Given** an agent is executing a task, **When** the agent generates intermediate reasoning steps, **Then** these steps are logged and observable

---

### User Story 2 - Load Skills from Markdown Files (Priority: P2)

A developer wants to equip agents with reusable methodologies and best practices stored in markdown files. For example, a "debugging checklist" or "code review guidelines" that the agent can reference when performing tasks.

**Why this priority**: This enables knowledge reuse across agents and makes agent behavior more maintainable and consistent. Can be added independently after basic agent execution works.

**Independent Test**: Can be tested by creating a markdown file with a methodology (e.g., "testing best practices"), configuring an agent to access it, giving the agent a testing task, and verifying it follows the documented methodology.

**Acceptance Scenarios**:

1. **Given** a markdown file containing a methodology exists in a designated skills directory, **When** an agent is configured to access that skill, **Then** the agent can reference and apply the methodology during task execution
2. **Given** multiple skill files exist, **When** an agent needs to perform a task, **Then** it can discover and select the most relevant skill file automatically
3. **Given** a skill file is updated, **When** an agent accesses it, **Then** the agent uses the latest version without requiring restart

---

### User Story 3 - Orchestrate Multiple Agents with Sequential Handoffs (Priority: P3)

A developer wants to create a workflow where multiple specialized agents collaborate. For example, one agent analyzes requirements, passes results to a second agent that generates code, and a third agent that reviews the code.

**Why this priority**: Enables complex workflows but builds on the foundation of working individual agents. Less critical than basic agent functionality but highly valuable for sophisticated use cases.

**Independent Test**: Can be tested by creating 3 agents (analyzer, generator, reviewer), defining a sequential workflow, running a task through all three, and verifying each agent receives the previous agent's output and completes its portion.

**Acceptance Scenarios**:

1. **Given** three agents are configured in a sequential workflow, **When** a task is submitted to the first agent, **Then** each agent executes in order and the final output reflects contributions from all agents
2. **Given** an agent in a sequence encounters an error, **When** the error occurs, **Then** the workflow stops gracefully and reports which agent failed and why
3. **Given** a workflow with agent handoffs, **When** execution proceeds, **Then** all agent interactions and handoff points are logged for review

---

### User Story 4 - Parallel Agent Execution (Priority: P4)

A developer wants to run multiple agents concurrently on the same task to compare outputs or gather diverse perspectives. For example, running three different coding agents simultaneously to generate alternative implementations.

**Why this priority**: Useful for ensemble approaches but not essential for core functionality. Can be implemented once sequential orchestration works.

**Independent Test**: Can be tested by configuring multiple agents to run on the same input concurrently, verifying all complete independently, and collecting all outputs.

**Acceptance Scenarios**:

1. **Given** multiple agents are configured for concurrent execution, **When** a task is submitted, **Then** all agents execute simultaneously and return independent results
2. **Given** concurrent agents with different execution times, **When** some complete before others, **Then** results are collected as each agent finishes
3. **Given** one agent in a parallel set fails, **When** the failure occurs, **Then** other agents continue execution unaffected

---

### User Story 5 - Agent-as-Tool Pattern (Priority: P5)

A developer wants one agent to use another agent as a specialized tool. For example, a coordinator agent delegates specific subtasks to expert agents (e.g., a "SQL expert" agent or "Python expert" agent) when needed.

**Why this priority**: Advanced capability that enables hierarchical agent systems but requires stable individual agents and basic orchestration first.

**Independent Test**: Can be tested by creating a coordinator agent and two specialist agents, giving the coordinator a task requiring both specialists, and verifying the coordinator calls the appropriate specialist agents and combines their outputs.

**Acceptance Scenarios**:

1. **Given** an agent is configured with other agents as tools, **When** it needs specialized expertise, **Then** it can invoke the appropriate agent-tool and incorporate its response
2. **Given** nested agent calls (agent A calls agent B which calls agent C), **When** execution occurs, **Then** all levels complete successfully and maintain proper context
3. **Given** an agent-tool invocation, **When** the tool agent executes, **Then** its internal operations are logged separately from the calling agent's log

---

### User Story 6 - Comprehensive Observability and Logging (Priority: P3)

A developer wants to understand what their agents are doing internally - which tools they call, what reasoning they perform, how they make decisions, and where they succeed or fail.

**Why this priority**: Critical for debugging and trust but can be partially implemented alongside P1 and enhanced incrementally. Essential for production use.

**Independent Test**: Can be tested by running an agent through a complex task, then examining logs to verify all tool calls, reasoning steps, decisions, and errors are captured with timestamps and context.

**Acceptance Scenarios**:

1. **Given** an agent performs a task, **When** execution completes, **Then** a complete log shows all tool invocations, inputs, outputs, and timestamps
2. **Given** an agent makes decisions or performs reasoning, **When** reviewing logs, **Then** internal thought processes and decision rationale are visible
3. **Given** multiple agents are orchestrated, **When** execution occurs, **Then** logs clearly distinguish which agent performed which actions
4. **Given** an error occurs during agent execution, **When** reviewing logs, **Then** the error context, stack trace, and agent state are captured

---

### Edge Cases

- What happens when an agent requests a tool that doesn't exist or isn't authorized?
- How does the system handle circular agent dependencies (agent A calls agent B which calls agent A)?
- What happens when a skill markdown file is malformed or contains conflicting instructions?
- How does orchestration handle timeout scenarios when one agent in a sequence takes too long?
- What happens when an agent produces output that exceeds memory or token limits?
- How are concurrent agents handled if they try to modify the same shared resource?
- What happens when skill files reference tools or other skills that don't exist?

## Requirements *(mandatory)*

### Functional Requirements

#### Agent Definition and Configuration

- **FR-001**: System MUST allow defining an agent with a unique identifier, system prompt, and list of available tools
- **FR-002**: System MUST support configuring agent-specific parameters such as temperature, maximum tokens, and model selection
- **FR-003**: System MUST allow specifying which tools an agent is authorized to access
- **FR-004**: System MUST prevent agents from accessing tools they aren't explicitly authorized to use
- **FR-005**: System MUST support defining agent input prompts dynamically at execution time

#### Tool System

- **FR-006**: System MUST provide built-in tools for common operations: file read, file write, file list, directory operations
- **FR-007**: System MUST provide a built-in tool for web search capabilities
- **FR-008**: System MUST allow wrapping existing langchain-hs tools (Calculator, Wikipedia, WebScraper) for agent use
- **FR-009**: System MUST provide an interface for creating custom tools with defined input/output schemas
- **FR-010**: System MUST execute tool calls with proper error handling and timeout protection (default timeout: 10 seconds per tool call, configurable per tool)
- **FR-011**: System MUST return tool execution results in a format the agent can understand and process

#### Skills and Knowledge Management

- **FR-012**: System MUST allow loading methodology and best practice documents from markdown files in a designated directory
- **FR-012a**: System MUST require skill files to have label (unique identifier for referencing) and description (brief summary) at the top; full content sections (## Purpose, ## Methodology, ## Examples) are recommended but flexible
- **FR-012b**: System MUST load only skill labels and descriptions initially to minimize context usage; full skill content MUST be loaded on-demand when agent explicitly requests a specific skill
- **FR-012c**: System MUST provide a template generation tool that creates properly structured skill file scaffolds
- **FR-013**: System MUST make loaded skill documents accessible to agents as a special tool or knowledge source
- **FR-014**: System MUST support organizing skills by category or domain (e.g., "testing", "debugging", "code-review")
- **FR-015**: System MUST allow agents to search or query available skills by label, description, or category
- **FR-016**: System MUST reload skill files dynamically when they change without requiring system restart

#### Agent Orchestration

- **FR-017**: System MUST support sequential orchestration where agents execute in a defined order
- **FR-018**: System MUST support parallel orchestration where multiple agents execute simultaneously on the same or different inputs
- **FR-019**: System MUST support handoff patterns where one agent can transfer control to another agent based on context
- **FR-020**: System MUST allow defining an agent as a tool that other agents can invoke
- **FR-021**: System MUST prevent infinite recursion when agents call other agents
- **FR-022**: System MUST support configuring maximum depth for nested agent calls (default: 5 levels)
- **FR-023**: System MUST pass context between agents using a structured HandoffObject containing: task summary, key outputs from previous agent, metadata (agent ID, timestamp, success/failure status), and optional custom fields
- **FR-023a**: System MUST allow developers to programmatically access and modify HandoffObject fields before passing to next agent
- **FR-023b**: System MUST serialize HandoffObject contents into the next agent's input prompt in a structured, parseable format
- **FR-024**: System MUST collect and aggregate results from parallel agent executions

#### Observability and Logging

- **FR-025**: System MUST log every tool invocation with timestamp, agent ID, tool name, input parameters, and output
- **FR-026**: System MUST log agent reasoning steps and internal thought processes when available from the LLM
- **FR-027**: System MUST log agent decisions such as which tool to use and why
- **FR-028**: System MUST log orchestration events including agent start, completion, handoffs, and errors
- **FR-029**: System MUST provide structured logging output that can be filtered and searched by agent ID, tool name, timestamp, or error status
- **FR-029a**: System MUST integrate with monad-logger as the default logging backend, allowing developers to configure log destinations using monad-logger's standard mechanisms
- **FR-029b**: System MUST provide a pluggable log handler interface, allowing developers to implement custom logging backends beyond monad-logger
- **FR-029c**: System MUST support colorized log output when logging to stdout, with color coding by log level and agent ID for improved readability
- **FR-030**: System MUST log errors with full context including agent state, current task, and stack trace
- **FR-031**: System MUST support configurable log levels (DEBUG, INFO, WARN, ERROR)
- **FR-032**: System MUST provide default log handler implementations for: colorized stdout output and file-based logging with rotation support

#### Error Handling and Resilience

- **FR-033**: System MUST gracefully handle tool execution failures without crashing the agent
- **FR-034**: System MUST provide detailed error messages to agents when tools fail, including error type, reason, and context, enabling the agent to reason about recovery strategies
- **FR-035**: System MUST implement timeout protection for both tool calls (default: 10 seconds) and overall agent execution (configurable, no default limit)
- **FR-036**: System MUST stop sequential workflows gracefully when an agent encounters an unrecoverable error
- **FR-037**: System MUST allow agents to retry failed tool calls with modified parameters up to 3 attempts total; agent receives error feedback after each failure to inform retry strategy
- **FR-037a**: System MUST log each retry attempt with reasoning and parameter modifications for observability
- **FR-038**: System MUST detect and prevent circular agent call dependencies

### Key Entities

- **Agent**: Represents an autonomous reasoning entity with a system prompt, available tools, configuration parameters, and execution capabilities. Each agent has a unique identifier and maintains state during execution.

- **Tool**: Represents a capability that agents can invoke, with defined input schema, output schema, execution function, and authorization rules. Tools can be simple functions, API calls, file operations, or even other agents.

- **Skill**: Represents a reusable methodology, best practice, or knowledge document stored as markdown. Contains: label (unique identifier), description (brief summary for discovery), category, and full content. Initially loaded with only label and description to minimize context; full content loaded on-demand when agent requests it. Recommended structure includes Purpose, Methodology, and Examples sections, though flexible formats are supported.

- **Orchestration**: Represents a workflow pattern (sequential, parallel, handoff) that coordinates multiple agents. Contains execution order, data flow rules, and error handling policies.

- **ExecutionLog**: Represents a record of agent activity including tool calls, reasoning steps, decisions, errors, and timing information. Logs are structured for querying and analysis.

- **AgentContext**: Represents the execution state and environment for an agent including current task, conversation history, available tools, loaded skills, orchestration metadata, and incoming HandoffObject (if receiving context from previous agent).

- **HandoffObject**: Represents structured context passed between agents in orchestration workflows. Contains: taskSummary (concise description of work completed), keyOutputs (important results/data from previous agent), metadata (sourceAgentId, timestamp, executionStatus, toolsUsed), and customFields (map for workflow-specific data). Serializable to JSON and text formats for agent prompt injection.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: A developer can create and execute a basic agent with 3 tools in under 10 minutes using provided examples
- **SC-002**: Single agent execution completes tasks requiring 3-5 tool calls within 30 seconds for typical use cases
- **SC-003**: Agents successfully complete multi-step tasks requiring tool usage with 90% accuracy when compared to manual execution
- **SC-004**: Log output captures 100% of tool invocations, agent decisions, and errors with complete context
- **SC-005**: Sequential workflows with 3 agents complete successfully 95% of the time without manual intervention
- **SC-006**: Parallel agent execution shows linear scaling up to 5 concurrent agents (5x speedup with 5 agents)
- **SC-007**: Skill loading and access adds less than 500ms overhead to agent execution time
- **SC-008**: Agent-as-tool patterns support nesting up to 5 levels deep without stack overflow
- **SC-009**: Error recovery mechanisms prevent complete workflow failure in 80% of partial failure scenarios
- **SC-010**: Developers can locate specific agent actions in logs within 2 minutes using filtering capabilities

## Out of Scope *(optional)*

The following are explicitly NOT part of this initial library release:

- **Distributed Agent Execution**: Running agents across multiple machines or network locations
- **Agent Training**: Fine-tuning or training custom LLM models for agents
- **Visual Workflow Builders**: GUI tools for designing agent workflows
- **Persistent Agent Memory**: Long-term memory storage beyond single execution sessions
- **Multi-modal Agents**: Agents that process images, audio, or video
- **Real-time Streaming**: Live agent execution with real-time output streaming to users
- **Authentication and Authorization**: User management or multi-tenant agent systems
- **Production Deployment**: Containerization, scaling, monitoring infrastructure
- **Cost Optimization**: Automatic selection of cheapest LLM for tasks

## Assumptions *(optional)*

- **A-001**: Developers using this library have basic Haskell programming knowledge
- **A-002**: The underlying langchain-hs library is stable and provides reliable LLM integration
- **A-003**: LLM providers (Ollama, OpenAI, etc.) are available and configured by the user
- **A-004**: Skill markdown files ideally follow recommended structure (Purpose, Methodology, Examples sections) but can vary; template generation tool assists developers in creating well-structured skills
- **A-005**: File operations are performed on the local filesystem where the library runs
- **A-006**: Web search capabilities require external API access (e.g., SerpAPI, Google Custom Search)
- **A-007**: Developers are responsible for managing API keys and credentials for LLM services
- **A-008**: Agent execution is synchronous (blocking) unless explicitly using parallel orchestration
- **A-009**: Library provides default log handlers (colorized stdout, file-based with rotation) via monad-logger; developers can customize log destinations using monad-logger configuration or implement custom handlers
- **A-010**: Tool execution happens in the same process as the agent for initial implementation

## Dependencies *(optional)*

### External Dependencies

- **langchain-hs**: Core LLM integration, existing tool implementations, and ReAct agent framework
- **Haskell base libraries**: Text manipulation, file I/O, concurrency primitives
- **aeson**: JSON parsing for structured logging and configuration
- **directory**: File system operations for skill loading and file tools
- **http-conduit**: HTTP client for web search tool implementation
- **monad-logger**: Default logging backend with pluggable handler support
- **ansi-terminal**: Terminal color support for colorized stdout logging

### Integration Points

- Must integrate with langchain-hs LLM providers (Ollama, OpenAI, Huggingface)
- Must wrap langchain-hs existing tools (Calculator, Wikipedia, WebScraper)
- Must use langchain-hs ReAct agent as foundation for agent reasoning
- May integrate with external search APIs for web search capabilities

## Notes *(optional)*

### Design Considerations

The library should be designed as a higher-level abstraction over langchain-hs, similar to how LangChain provides orchestration layers over basic LLM calls. Key design principles:

1. **Composability**: Agents, tools, and orchestrations should be composable building blocks
2. **Type Safety**: Leverage Haskell's type system for safe tool definitions and agent configurations
3. **Observability First**: Logging and monitoring should be built-in, not retrofitted
4. **Extensibility**: Easy to add custom tools, skills, and orchestration patterns
5. **Progressive Disclosure**: Simple use cases should be simple; complex use cases should be possible

### Inspiration Sources

- **Microsoft Agent Framework**: Orchestration patterns (sequential, parallel, handoff, group chat)
- **LangGraph**: State machine approach to multi-agent workflows
- **AutoGen**: Multi-agent conversation patterns
- **CrewAI**: Role-based agent teams and task delegation

### Future Enhancements

Potential future additions not in initial scope:

- **Conditional Orchestration**: If-then-else logic in workflows based on agent outputs
- **Agent Memory Systems**: Vector stores and retrieval for long-term agent memory
- **Human-in-the-Loop**: Breakpoints where agents request human input
- **Agent Marketplace**: Sharing and discovering pre-built agents and skills
- **Performance Profiling**: Detailed timing analysis of agent and tool execution
- **Agent Testing Framework**: Tools for unit testing agent behaviors
