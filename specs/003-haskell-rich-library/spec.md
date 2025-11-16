# Feature Specification: Haskell Rich Terminal Library

**Feature Branch**: `003-haskell-rich-library`
**Created**: 2025-11-15
**Status**: Draft
**Input**: User description: "Create the spec for haskell/libs/rich and clarify the ambiguous area."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Display Styled Terminal Text (Priority: P1)

Developers building command-line applications need to display colored and formatted text (bold, italic, underline, colors) to improve user experience and highlight important information like errors, warnings, and success messages.

**Why this priority**: Core functionality that all other features depend on. Without styled text output, the library provides no value.

**Independent Test**: Can be fully tested by creating various Style objects, applying them to text, and verifying ANSI escape codes are correctly generated when rendered to the terminal. Delivers immediate value for basic CLI beautification.

**Acceptance Scenarios**:

1. **Given** a developer creates a Style with red foreground color, **When** they render text with that style, **Then** the output includes correct ANSI escape codes for red text
2. **Given** a developer combines multiple style attributes (bold, underline, green), **When** they render text with the combined style, **Then** all attributes are applied correctly in the terminal output
3. **Given** a developer uses RGB colors (255, 100, 50), **When** they render text with RGB color, **Then** the terminal displays the custom color using 24-bit ANSI codes
4. **Given** a developer applies background colors with onRed, **When** rendering text, **Then** the background color is displayed correctly
5. **Given** a developer uses the Console interface to print styled messages, **When** calling printSuccess/printError/printWarning/printInfo, **Then** appropriate colors and symbols (✓, ✗, ⚠, ℹ) are displayed

---

### User Story 2 - Create Formatted Tables (Priority: P2)

Developers need to display tabular data in a visually appealing format with borders, proper alignment, and automatic column sizing for presenting structured information like reports, lists, or comparisons.

**Why this priority**: Tables are a common requirement for CLI tools displaying structured data. This is independent of other features and delivers standalone value.

**Independent Test**: Can be fully tested by creating tables with headers and rows, applying different border styles, and verifying the rendered output matches expected table formatting with proper alignment and borders.

**Acceptance Scenarios**:

1. **Given** a developer provides headers ["Name", "Age", "City"] and data rows, **When** they create a simple table, **Then** the table renders with automatic column sizing based on content width
2. **Given** a developer specifies column alignment (left, center, right), **When** rendering the table, **Then** content in each column is aligned according to specification
3. **Given** a developer selects a border style (SimpleBorder, RoundedBorder, DoubleBorder, HeavyBorder), **When** rendering the table, **Then** the correct Unicode box-drawing characters are used
4. **Given** a developer creates a table with a title, **When** rendering, **Then** the title is displayed centered at the top of the table
5. **Given** a developer sets custom column widths, **When** rendering, **Then** columns respect the specified widths with appropriate padding

---

### User Story 3 - Display Content in Panels (Priority: P2)

Developers want to highlight important content by surrounding it with decorative borders and padding, similar to alert boxes or callouts in documentation.

**Why this priority**: Panels provide visual emphasis for important messages. Independent feature that can be developed and tested separately from tables and trees.

**Independent Test**: Can be fully tested by creating panels with various configurations (title, subtitle, padding, border styles) and verifying the rendered output has correct borders, spacing, and content alignment.

**Acceptance Scenarios**:

1. **Given** a developer creates a panel with title "Welcome" and content text, **When** rendering the panel, **Then** the title appears in the top border with content properly padded inside
2. **Given** a developer sets custom padding (top, right, bottom, left), **When** rendering, **Then** the specified spacing is applied around the content
3. **Given** a developer selects a border style for the panel, **When** rendering, **Then** the panel uses the specified border characters
4. **Given** a developer adds both title and subtitle to a panel, **When** rendering, **Then** both appear in their designated positions with appropriate styling
5. **Given** a developer sets panel width explicitly, **When** rendering, **Then** the panel respects the specified width even if content is shorter

---

### User Story 4 - Visualize Hierarchical Data as Trees (Priority: P3)

Developers need to display hierarchical structures (file systems, organization charts, dependency graphs) in a tree format with connecting lines and proper indentation.

**Why this priority**: Trees are useful for specific use cases but less universally needed than tables or styled text. Can be developed independently after core styling is working.

**Independent Test**: Can be fully tested by building tree structures with nodes and children, rendering them, and verifying proper indentation, connecting lines (├──, └──, │), and hierarchy visualization.

**Acceptance Scenarios**:

1. **Given** a developer creates a tree node with label "project" and adds child nodes, **When** rendering the tree, **Then** children are indented with proper connecting line characters
2. **Given** a developer creates a multi-level tree (parent -> child -> grandchild), **When** rendering, **Then** vertical guide lines (│) connect all levels appropriately
3. **Given** a developer adds multiple children to a node, **When** rendering, **Then** the last child uses └── while others use ├──
4. **Given** a developer applies custom styles to tree guides and labels, **When** rendering, **Then** the specified colors and formatting are applied
5. **Given** a developer creates a leaf node (no children), **When** rendering, **Then** it displays without guide extensions below it

---

### User Story 5 - Show Progress with Animated Bars (Priority: P3)

Developers running long-running operations need to display progress visually with a bar showing completion percentage and optional description text.

**Why this priority**: Progress bars enhance user experience for time-consuming tasks but are not core to basic CLI output. Independent feature focused on visual feedback during operations.

**Independent Test**: Can be fully tested by creating progress bars with various current/total values, rendering them at different completion percentages, and verifying the visual representation (filled vs unfilled characters) and percentage display are accurate.

**Acceptance Scenarios**:

1. **Given** a developer creates a progress bar with total=100 and current=50, **When** rendering with percentage, **Then** the output shows 50% with half the bar filled
2. **Given** a developer sets a description "Processing", **When** rendering the progress bar, **Then** the description appears before the bar
3. **Given** a developer customizes complete and incomplete characters (█ vs ░), **When** rendering, **Then** the specified characters are used
4. **Given** a developer sets custom width for the progress bar, **When** rendering, **Then** the bar displays with the specified character width
5. **Given** a developer updates current value in a loop, **When** re-rendering with \r carriage return, **Then** the progress bar updates in place without creating new lines

---

### Edge Cases

- What happens when non-ASCII Unicode characters affect text length calculations? ANSI escape codes don't count toward display width, but emoji and wide characters (CJK characters) need special handling for accurate column width calculation.
- What happens when a progress bar receives current > total? Should it cap at 100% or allow overflow display?
- How does the library handle concurrent output from multiple threads? Should Console provide thread-safe printing to prevent interleaved output?
- What happens when terminal width detection fails or returns invalid values (0, negative, extremely large)? Should the library fall back to a default width assumption?
- How should multi-line cells handle vertical alignment - top-aligned, center-aligned, or bottom-aligned within the expanded row?
- When wrapping table content due to terminal width limits, should the library preserve table structure with smaller columns or switch to a different display mode?
- What happens when a terminal reports capability X but doesn't actually render it correctly? Should there be manual override options?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a Style type that supports foreground colors (16 standard ANSI + RGB)
- **FR-002**: System MUST provide a Style type that supports background colors (16 standard ANSI + RGB)
- **FR-003**: System MUST support text attributes: bold, italic, underline, dim, blink, reverse, hidden, strikethrough
- **FR-004**: System MUST allow composing multiple style attributes using function composition
- **FR-005**: System MUST render styled text by converting Style to appropriate ANSI escape sequences
- **FR-006**: System MUST provide Table functionality with customizable columns (header, alignment, optional width)
- **FR-007**: System MUST support table border styles: NoBorder, SimpleBorder, RoundedBorder, DoubleBorder, HeavyBorder
- **FR-008**: System MUST automatically calculate column widths based on content unless explicitly specified
- **FR-009**: System MUST support column alignment: left, center, right
- **FR-010**: System MUST provide Panel functionality with optional title, subtitle, and customizable padding
- **FR-011**: System MUST support panel border styles using the same BorderStyle options as tables
- **FR-012**: System MUST provide Tree functionality for displaying hierarchical data with connecting lines
- **FR-013**: System MUST use appropriate Unicode characters for tree structure (├──, └──, │)
- **FR-014**: System MUST provide ProgressBar functionality with current/total tracking
- **FR-015**: System MUST render progress bars with customizable filled/unfilled characters
- **FR-016**: System MUST calculate and display progress percentage (0-100%)
- **FR-017**: System MUST provide Console interface with print/printLn functions
- **FR-018**: System MUST provide convenience functions for success/error/warning/info messages with appropriate symbols
- **FR-019**: System MUST provide horizontal rule function with optional centered title
- **FR-020**: System MUST maintain minimal dependency footprint - only `base`, `text`, terminal detection library, and Unicode width calculation library
- **FR-021**: System MUST provide renderText function that returns Text type (not IO action) for composability
- **FR-022**: Tables MUST support setting an optional title displayed at the top
- **FR-023**: System MUST handle empty tables and panels gracefully without crashes
- **FR-024**: Progress bars MUST accept Double type for current and total to support fractional progress
- **FR-025**: Tree nodes MUST support adding multiple children
- **FR-026**: System MUST provide both simple constructors (e.g., simpleTable, simplePanel) and detailed builders for flexibility
- **FR-027**: System MUST support multi-line cells with proper row height expansion - when cell content contains newlines, the table row height adjusts to accommodate the tallest cell, with all cells in that row vertically aligned
- **FR-028**: System MUST detect terminal width and provide responsive rendering - tables and panels automatically truncate or wrap content when they exceed terminal dimensions to prevent overflow
- **FR-029**: System MUST detect terminal capabilities and gracefully degrade color output - when terminals don't support RGB or 256-color mode, the library falls back to 16-color ANSI or strips formatting as appropriate
- **FR-030**: System MUST handle wide characters (emoji, CJK) correctly when calculating display widths for alignment and sizing
- **FR-031**: System MUST provide fallback behavior when terminal width detection fails, using reasonable default width (e.g., 80 columns)
- **FR-032**: Multi-line table cells MUST align content vertically (top-aligned by default) within expanded rows

### Key Entities

- **Style**: Represents text formatting with optional foreground color, optional background color, and a list of style attributes. Immutable value type that can be composed via function application.

- **Color**: Represents terminal colors - either standard ANSI colors (Black, Red, Green, Yellow, Blue, Magenta, Cyan, White), bright variants (BrightBlack through BrightWhite), or RGB values (three Word8 components).

- **Table**: Contains a list of Column definitions, list of Row data, optional title, border style, and optional title style. Represents complete table structure before rendering.

- **Column**: Defines table column with header text, alignment preference, optional custom style, and optional width override.

- **Cell**: Individual table cell containing text content and optional style override.

- **Panel**: Contains optional title, optional subtitle, content text, padding specification (top, right, bottom, left), panel style configuration, and optional width.

- **Tree**: Recursive structure with label text, list of child trees, and tree style configuration for guides and labels.

- **ProgressBar**: Tracks current progress value, total value, optional description, bar width in characters, and progress style configuration.

- **Console**: Wrapper around IO Handle (stdout) providing convenient printing interface.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can apply styled text (colors, bold, underline) with no more than 3 lines of code for basic use cases
- **SC-002**: Tables automatically render with correct column widths without requiring manual calculation by developers
- **SC-003**: All visual components (tables, panels, trees, progress bars) render correctly in standard terminal emulators (xterm, iTerm2, Windows Terminal) without visual artifacts
- **SC-004**: Library maintains minimal dependency footprint with only essential libraries (base, text, terminal detection, Unicode width) verified by dependency analysis
- **SC-005**: Progress bars update smoothly with carriage return (\r) without flickering or leaving remnants on screen
- **SC-006**: Developers can create a styled table displaying data in under 5 lines of code using simple constructors
- **SC-007**: Library provides composable API where 90%+ of functions return values (not IO actions) for easy testing and composition
- **SC-008**: Unicode box-drawing characters render correctly on all supported platforms (Linux, macOS, Windows with UTF-8 terminals)
- **SC-009**: Style combination through function composition allows unlimited attribute stacking without errors
- **SC-010**: Documentation examples run successfully without modification on fresh project setup

## Assumptions

- Developers using this library have terminal emulators that support ANSI escape codes and UTF-8 encoding (minimum 16-color ANSI support)
- The library targets a wide range of terminal emulators with automatic degradation for limited capability terminals
- Terminal width detection works reliably on target platforms (Linux, macOS, Windows) through standard system APIs
- Users are responsible for handling terminal state (clearing screen, cursor positioning) outside basic line-based output
- Progress bar animation is achieved through application code calling render in a loop with \r, not built-in threading
- The library focuses on single-threaded output; concurrent printing from multiple threads is user's responsibility to synchronize
- Tables and panels are intended for moderate-sized data; extremely large tables (1000+ rows) may have performance implications
- Multi-line cell rendering is used sparingly; excessive use may impact readability and performance
- Wide character (emoji, CJK) width calculation relies on standard Unicode width specifications

## Dependencies and Integration

- **External Dependencies**:
  - `base` (Haskell standard library)
  - `text` (standard text processing)
  - Terminal detection library (for width detection and capability queries - specific library TBD during implementation)
  - Unicode width calculation library (for handling wide characters - e.g., `wcwidth` or `unicode-width`)
- **Build System**: Integrated into Bazel monorepo using rules_haskell
- **Platform Support**: Cross-platform (Linux, macOS, Windows) through standard ANSI codes with platform-specific terminal detection
- **Terminal Requirements**: UTF-8 capable terminal with ANSI escape code support (minimum 16-color support, with graceful degradation)

## Out of Scope

The following features are explicitly NOT included in this specification:

- **Syntax highlighting**: Code syntax highlighting for programming languages (mentioned as planned in comparison)
- **Markdown rendering**: Converting Markdown to styled terminal output (mentioned as planned)
- **Live display**: Real-time updating of multiple progress bars or dynamic layouts without manual refresh (users manage refresh loops)
- **Mouse interaction**: Click handling or mouse-based UI elements
- **Alternative text protocols**: Kitty graphics protocol, Sixel, or image rendering
- **Logging framework integration**: Built-in adapters for logging libraries (users can build their own)
- **Configuration files**: Loading styles or themes from external configuration
- **Interactive TUI elements**: Input fields, buttons, or other interactive widgets beyond static output
- **Automatic scrolling**: Managing terminal scrollback or viewport positioning
