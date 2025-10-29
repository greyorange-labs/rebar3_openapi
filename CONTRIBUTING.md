# Contributing to rebar3_openapi

Thank you for your interest in contributing to `rebar3_openapi`! This document provides guidelines and information for contributors.

## Table of Contents

- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Project Structure](#project-structure)
- [Architecture](#architecture)
- [Code Style](#code-style)
- [Testing](#testing)
- [Submitting Changes](#submitting-changes)
- [Future Enhancements](#future-enhancements)

## Getting Started

### Prerequisites

- **Erlang/OTP 24+**
- **Rebar3 3.20+**
- **Git**
- **openapi-generator CLI** (optional for testing)
- **Docker** (optional fallback)

### Fork and Clone

```bash
# Fork the repository on GitHub, then:
git clone https://github.com/YOUR_USERNAME/rebar3_openapi.git
cd rebar3_openapi
```

## Development Setup

### 1. Install Dependencies

```bash
rebar3 get-deps
```

### 2. Compile the Project

```bash
rebar3 compile
```

### 3. Test Your Setup

```bash
# Test generation (dry-run)
rebar3 openapi generate \
    --spec examples/simple_api.yml \
    --handler test_handler \
    --dry-run

# Actual generation
rebar3 openapi generate \
    --spec examples/simple_api.yml \
    --handler test_handler

# Check generated files
ls -la src/test_*

# Clean up
rm src/test_*
```

### 4. Run Dialyzer (Optional)

```bash
rebar3 dialyzer
```

## Project Structure

```
rebar3_openapi/
├── src/
│   ├── rebar3_openapi.erl                  # Main plugin entry point
│   ├── rebar3_openapi_parser.erl           # OpenAPI YAML/JSON parser
│   ├── rebar3_openapi_generator.erl        # openapi-generator wrapper
│   ├── rebar3_openapi_transformer.erl      # Code transformation
│   ├── rebar3_openapi_extractor.erl        # Code → OpenAPI extraction
│   ├── rebar3_openapi_validator.erl        # Validation logic
│   ├── rebar3_openapi_prv_generate.erl     # Generate command
│   ├── rebar3_openapi_prv_extract.erl      # Extract command
│   ├── rebar3_openapi_prv_validate.erl     # Validate command
│   ├── rebar3_openapi_utils.erl            # Utilities
│   └── rebar3_openapi.app.src             # Application metadata
├── examples/
│   ├── simple_api.yml                      # Example OpenAPI spec
│   └── bsh_http_handler_refactoring/      # Refactoring guide
├── test/                                   # Test files (future)
├── README.md                               # Main documentation
├── EXAMPLES.md                             # Usage examples
├── CONTRIBUTING.md                         # This file
├── rebar.config                            # Dependencies
└── .tool-versions                          # ASDF version manager
```

## Architecture

### Module Responsibilities

#### Core Modules

**rebar3_openapi.erl**
- Main plugin entry point
- Registers all providers (generate, extract, validate)
- Integrates with rebar3 plugin system

**rebar3_openapi_parser.erl**
- Parses YAML and JSON OpenAPI specs
- Validates OpenAPI structure
- Extracts paths, operations, schemas
- Supports OpenAPI 3.0.x

**rebar3_openapi_generator.erl**
- Wraps openapi-generator CLI
- Detects installation (local or Docker)
- Handles both local and Docker execution
- Captures and logs output

**rebar3_openapi_transformer.erl**
- Transforms generated code to hybrid pattern
- Generates router modules with trails() compatibility
- Creates REST handler modules
- Produces logic handler skeletons
- Groups operations by path and method

#### Command Providers

**rebar3_openapi_prv_generate.erl**
- Provider for `rebar3 openapi generate` command
- Full command-line interface
- Dry-run mode
- Update mode (basic)
- Manifest management

**rebar3_openapi_prv_extract.erl**
- Provider for `rebar3 openapi extract` command
- Single handler extraction
- Batch extraction mode
- YAML and JSON output formats

**rebar3_openapi_prv_validate.erl**
- Provider for `rebar3 openapi validate` command
- Single handler validation
- Batch validation from manifest
- CI-friendly strict mode

#### Support Modules

**rebar3_openapi_extractor.erl**
- Parses Erlang handler modules
- Extracts route information
- Converts Jesse schemas to OpenAPI
- Finds handlers in project

**rebar3_openapi_validator.erl**
- Compares specs against implementations
- Identifies missing/extra endpoints
- Reports differences clearly

**rebar3_openapi_utils.erl**
- Configuration management
- File operations
- Logging helpers
- Path resolution
- Error formatting

### Data Flow

#### Generation Flow (OpenAPI → Code)

```
OpenAPI Spec (YAML/JSON)
        ↓
    Parser (parse_file/1)
        ↓
    Validator (validate_spec/1)
        ↓
    Generator (openapi-generator CLI)
        ↓
    Transformer (to hybrid pattern)
        ↓
Generated Files:
  - Router (with trails/0)
  - Handler (cowboy_rest)
  - Logic Handler (skeleton)
        ↓
    Manifest Update
```

#### Extraction Flow (Code → OpenAPI)

```
Erlang Handler Module
        ↓
    Extractor (parse module)
        ↓
Extract:
  - Routes
  - Operations
  - Methods
  - Schemas
        ↓
    Build OpenAPI structure
        ↓
    Output (YAML/JSON)
```

#### Validation Flow

```
OpenAPI Spec          Handler Code
     ↓                      ↓
Parse Spec            Extract from Code
     ↓                      ↓
     └─────→ Comparator ←──┘
              ↓
        Find Differences
              ↓
        Report Results
```

### Hybrid Pattern Design

The plugin implements a **Hybrid Pattern** that bridges openapi-generator output with existing Cowboy infrastructure:

```
┌──────────────────────────────────────┐
│       OpenAPI Specification          │
└──────────────────────────────────────┘
                 ↓
┌──────────────────────────────────────┐
│      openapi-generator (Erlang)      │
└──────────────────────────────────────┘
                 ↓
┌──────────────────────────────────────┐
│         Transformation Layer         │
│  (rebar3_openapi_transformer)        │
└──────────────────────────────────────┘
                 ↓
    ┌────────────┴────────────┐
    ↓            ↓             ↓
┌────────┐  ┌─────────┐  ┌────────────┐
│ Router │  │ Handler │  │   Logic    │
│(trails)│  │(cowboy) │  │  Handler   │
└────────┘  └─────────┘  └────────────┘
    ↓            ↓             ↓
└───────────────────────────────────────┘
         Cowboy Routes Manager
```

**Why This Works:**
- ✅ Leverages battle-tested openapi-generator
- ✅ Maintains compatibility with existing infrastructure
- ✅ Clear separation of concerns
- ✅ Gradual migration path

## Code Style

### Erlang Conventions

Follow standard Erlang/OTP conventions:

**Module Structure:**
```erlang
%%%-------------------------------------------------------------------
%%% @doc Module description
%%% @end
%%%-------------------------------------------------------------------
-module(module_name).

-export([public_function/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Function description
-spec public_function(term()) -> ok | {error, term()}.
public_function(Arg) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec private_function(term()) -> term().
private_function(Arg) ->
    Arg.
```

**Formatting:**
- Use 4 spaces for indentation (no tabs)
- Maximum line length: 100 characters
- Add blank lines between function definitions
- Group related functions together

**Naming:**
- Use `snake_case` for functions and variables
- Use `CamelCase` for module names (following OTP convention)
- Prefix private functions with underscore if needed for clarity

**Documentation:**
- Add `@doc` tags for all exported functions
- Include `-spec` type specifications
- Add module-level documentation with `@doc`

### Error Handling

**Use tagged tuples:**
```erlang
case do_something() of
    {ok, Result} ->
        process(Result);
    {error, Reason} ->
        handle_error(Reason)
end.
```

**Provide meaningful error messages:**
```erlang
format_error({missing_spec, File}) ->
    io_lib:format("OpenAPI spec file not found: ~s", [File]);
format_error({invalid_yaml, File, Reason}) ->
    io_lib:format("Invalid YAML in ~s: ~p", [File, Reason]).
```

### Logging

Use the utilities module for consistent logging:

```erlang
rebar3_openapi_utils:info("Generation completed", []),
rebar3_openapi_utils:warn("File exists, skipping: ~s", [File]),
rebar3_openapi_utils:error("Generation failed: ~p", [Reason]),
rebar3_openapi_utils:debug("Parsing spec: ~s", [SpecFile]).
```

## Testing

### Current Testing

Currently, testing is primarily manual. Run these commands to test functionality:

```bash
# Test generation
rebar3 openapi generate --spec examples/simple_api.yml --handler test_handler --dry-run
rebar3 openapi generate --spec examples/simple_api.yml --handler test_handler

# Test extraction
rebar3 openapi extract --handler test_handler --output /tmp/test.yml

# Test validation
rebar3 openapi validate --handler test_handler --spec examples/simple_api.yml

# Cleanup
rm src/test_*
```

### Future Testing Framework

We plan to add:

**Unit Tests (EUnit):**
```erlang
%% test/rebar3_openapi_parser_tests.erl

-module(rebar3_openapi_parser_tests).
-include_lib("eunit/include/eunit.hrl").

parse_valid_spec_test() ->
    Spec = #{<<"openapi">> => <<"3.0.0">>,
             <<"info">> => #{<<"title">> => <<"Test">>},
             <<"paths">> => #{}},
    ?assertEqual(ok, rebar3_openapi_parser:validate_spec(Spec)).
```

**Integration Tests (Common Test):**
```erlang
%% test/integration_SUITE.erl

-module(integration_SUITE).
-include_lib("common_test/include/ct.hrl").

all() -> [generate_and_validate].

generate_and_validate(_Config) ->
    %% Full workflow test
    ok.
```

### Adding Tests

To contribute tests:

1. Create test files in `test/` directory
2. Follow EUnit or Common Test conventions
3. Run tests with: `rebar3 eunit` or `rebar3 ct`
4. Ensure all tests pass before submitting PR

## Submitting Changes

### Workflow

1. **Create a branch:**
   ```bash
   git checkout -b feature/my-feature
   # or
   git checkout -b fix/my-bugfix
   ```

2. **Make your changes:**
   - Follow code style guidelines
   - Add tests if applicable
   - Update documentation if needed

3. **Commit your changes:**
   ```bash
   git add .
   git commit -m "feat: add new feature"

   # Or for bug fixes:
   git commit -m "fix: resolve issue with generation"
   ```

   **Commit message format:**
   - `feat:` - New feature
   - `fix:` - Bug fix
   - `docs:` - Documentation changes
   - `refactor:` - Code refactoring
   - `test:` - Adding tests
   - `chore:` - Maintenance tasks

4. **Push and create PR:**
   ```bash
   git push origin feature/my-feature
   ```
   Then create a pull request on GitHub.

### Pull Request Guidelines

**PR Title:**
- Use same format as commit messages
- Be descriptive: "feat: add support for OpenAPI 3.1"

**PR Description:**
```markdown
## Description
Brief description of changes

## Motivation
Why is this change needed?

## Changes Made
- Change 1
- Change 2

## Testing
How was this tested?

## Checklist
- [ ] Code follows style guidelines
- [ ] Tests added/updated
- [ ] Documentation updated
- [ ] Compilation successful
- [ ] Dialyzer passes (if applicable)
```

### Review Process

1. Maintainers will review your PR
2. Address any feedback or requested changes
3. Once approved, changes will be merged

## Future Enhancements

Areas where contributions are especially welcome:

### High Priority

**1. Full AST Parsing for --update Mode**
- Current: Logs what it would do
- Needed: Intelligent code merging using AST parsing
- Impact: High - enables safe incremental updates
- Skills: Erlang AST manipulation, `erl_parse`, `erl_syntax`

**2. Comprehensive Test Suite**
- Unit tests for all modules
- Integration tests for full workflows
- Property-based testing with PropEr
- CI/CD integration

**3. Advanced Code Extraction**
- Better pattern recognition for existing handlers
- Support for more handler patterns
- Extract request/response schemas
- Handle complex routing logic

### Medium Priority

**4. Jesse Schema Integration**
- Extract schemas from `priv/` directories
- Convert Jesse schemas to OpenAPI
- Request/response validation using Jesse
- Schema generation from types

**5. Template Customization**
- Allow custom Mustache templates
- Configurable code generation patterns
- Plugin architecture for transformations

**6. Enhanced Validation**
- Schema validation
- Request/response example validation
- Compatibility checks (OpenAPI versions)

### Low Priority

**7. Documentation & Examples**
- Video tutorials
- More example projects
- Blog posts/articles
- Conference talks

**8. Tooling**
- Visual Studio Code extension
- IntelliJ plugin integration
- Emacs/Vim plugins

**9. Publishing**
- Publish to hex.pm
- Create releases
- Version management

## Development Guidelines

### Working on New Features

1. **Discuss first**: Open an issue to discuss major features
2. **Start small**: Break large features into smaller PRs
3. **Document**: Update README.md and EXAMPLES.md
4. **Test**: Manual testing at minimum, automated tests preferred

### Working on Bug Fixes

1. **Reproduce**: Document steps to reproduce
2. **Fix**: Implement the fix
3. **Test**: Verify the fix works
4. **Prevent**: Add tests to prevent regression

### Code Review Checklist

Before submitting:
- [ ] Code compiles without errors
- [ ] No new compiler warnings (except expected ones)
- [ ] Follows code style guidelines
- [ ] Documentation updated
- [ ] Manual testing performed
- [ ] Commit messages follow format

## Questions?

- Open an issue on GitHub
- Review existing issues and PRs
- Check [EXAMPLES.md](EXAMPLES.md) for usage patterns
- Read the [implementation plan](../rebar3_openapi_implementation_plan.md)

## License

By contributing, you agree that your contributions will be licensed under Apache 2.0 License.

## Credits

Thank you to all contributors!

Special thanks to:
- OpenAPI Tools community
- Erlang/OTP team
- Cowboy framework maintainers
- Butler Core team for the original requirements

---

**Happy coding!** 🚀

