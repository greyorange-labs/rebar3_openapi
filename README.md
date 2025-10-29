# rebar3_openapi

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Erlang](https://img.shields.io/badge/Erlang-OTP%2024%2B-red.svg)](https://www.erlang.org/)

A rebar3 plugin for bidirectional code generation between OpenAPI specifications and Erlang/Cowboy REST handlers.

## Features

- 🔄 **Bidirectional Generation**: OpenAPI ↔ Erlang code
- 🔌 **Seamless Integration**: Works with existing Cowboy projects via trails() compatibility
- ✅ **Validation**: Validate code against OpenAPI specs
- 🎯 **Hybrid Pattern**: Leverages openapi-generator with custom transformations
- 📝 **CI Ready**: Validate code against OpenAPI specs in CI pipelines
- 🐳 **Docker Support**: Falls back to Docker if openapi-generator CLI not available

## Table of Contents

- [Installation](#installation)
- [Quick Start](#quick-start)
- [Requirements](#requirements)
- [Commands](#commands)
- [Configuration](#configuration)
- [Architecture](#architecture)
- [Integration](#integration)
- [Status](#status)
- [Documentation](#documentation)
- [Contributing](#contributing)
- [License](#license)

## Installation

Add to your project's `rebar.config`:

```erlang
{plugins, [
    {rebar3_openapi, {git, "https://github.com/your-org/rebar3_openapi.git", {branch, "main"}}}
]}.
```

### Optional Configuration

```erlang
{openapi, [
    {specs_dir, "specs"},
    {manifest_file, ".openapi_manifest.json"},
    {server_url, "http://localhost:8080"},
    {validation_on_generate, true}
]}.
```

## Quick Start

### Generate Handler from OpenAPI Spec

```bash
rebar3 openapi generate \
    --spec /path/to/specs/my_api.yml \
    --handler /path/to/src/my_api_handler.erl \
    --logic-module my_api_logic_handler
```

This creates three files in the same directory as your handler:
- `my_api_router.erl` - Router with trails() compatibility
- `my_api_handler.erl` - REST handler with cowboy_rest callbacks
- `my_api_logic_handler.erl` - Logic handler skeleton (implement your business logic here)

### Extract OpenAPI from Existing Handler

```bash
rebar3 openapi extract \
    --handler /path/to/src/my_api_handler.erl \
    --output specs/my_api.yml
```

### Validate Code Against Spec

```bash
rebar3 openapi validate \
    --handler /path/to/src/my_api_handler.erl \
    --spec /path/to/specs/my_api.yml
```

**See [EXAMPLES.md](EXAMPLES.md) for detailed usage examples.**

## Requirements

### Mandatory
- **Erlang/OTP 24+**
- **Rebar3 3.20+**
- **Dependencies** (automatically installed):
  - `yamerl` - YAML parsing
  - `jsx` - JSON handling

### Optional
- **openapi-generator CLI** - For code generation (Docker fallback available)
  ```bash
  # Using npm
  npm install -g @openapitools/openapi-generator-cli

  # Or using Homebrew (macOS)
  brew install openapi-generator

  # Or use Docker (no installation needed)
  docker pull openapitools/openapi-generator-cli
  ```

## Commands

### Generate

Generate Erlang handlers from OpenAPI specification.

```bash
rebar3 openapi generate --spec SPEC_PATH --handler HANDLER_PATH [OPTIONS]
```

**Required Options:**
- `--spec, -s` - Full path to OpenAPI spec file (YAML or JSON)
- `--handler, -h` - Full path to handler file (e.g., `/path/to/src/my_handler.erl`)

**Optional Options:**
- `--logic-module, -l` - Logic handler module name (default: `<handler>_logic_handler`)
- `--package-name, -p` - Package name for openapi-generator (default: handler name)
- `--update, -u` - Update existing handler (preserves business logic)
- `--dry-run, -d` - Preview changes without creating files
- `--force, -f` - Force overwrite existing files

**Example:**
```bash
rebar3 openapi generate \
    --spec /Users/me/project/specs/my_api.yml \
    --handler /Users/me/project/apps/my_app/src/my_api_handler.erl \
    --dry-run
```

### Extract

Extract OpenAPI specification from Erlang handlers.

```bash
rebar3 openapi extract --handler HANDLER_PATH --output OUTPUT [OPTIONS]
```

**Required Options:**
- `--handler, -h` - Full path to handler file (required)
- `--output, -o` - Output file path (required)

**Optional Options:**
- `--format, -f` - Output format: `yaml` or `json` (auto-detected from file extension)

**Example:**
```bash
rebar3 openapi extract \
    --handler /Users/me/project/apps/my_app/src/my_handler.erl \
    --output specs/my_api.yml
```

### Validate

Validate handler implementation against OpenAPI spec.

```bash
rebar3 openapi validate --handler HANDLER --spec SPEC [OPTIONS]
```

**Options:**
- `--handler, -h` - Handler module name (required unless --all)
- `--spec, -s` - OpenAPI spec file (required unless --all)
- `--app, -a` - Application name
- `--strict` - Strict mode (fail on any differences, for CI)
- `--all` - Validate all handlers tracked in manifest

**Example:**
```bash
# Single handler
rebar3 openapi validate \
    --handler my_handler \
    --spec specs/my_api.yml

# CI mode (all handlers, strict)
rebar3 openapi validate --all --strict
```

## Configuration

### Project Configuration

Add to your `rebar.config`:

```erlang
{openapi, [
    %% Directory for OpenAPI specs
    {specs_dir, "specs"},

    %% Manifest file to track generated handlers
    {manifest_file, ".openapi_manifest.json"},

    %% Default server URL for generated specs
    {server_url, "http://localhost:8080"},

    %% Validate specs on generation
    {validation_on_generate, true}
]}.
```

### Manifest File

The plugin creates `.openapi_manifest.json` to track generated handlers:

```json
{
  "version": "1.0",
  "handlers": {
    "my_api_handler": {
      "spec_file": "specs/my_api.yml",
      "router_module": "my_api_router",
      "logic_module": "my_api_logic_handler",
      "output_dir": "apps/my_app/src",
      "spec_version": "1.0.0",
      "last_generated": "2025-10-29T12:34:56Z"
    }
  }
}
```

This enables:
- Batch validation (`rebar3 openapi validate --all`)
- Tracking which specs generated which handlers
- CI/CD integration

## Architecture

### Hybrid Pattern

The plugin implements a **Hybrid Pattern** that combines the power of openapi-generator with compatibility for existing Cowboy infrastructure:

```
OpenAPI Spec
     ↓
openapi-generator (initial generation)
     ↓
Transformation to Hybrid Pattern
     ↓
┌─────────────┬──────────────┬─────────────────┐
│   Router    │   Handler    │  Logic Handler  │
│  (trails)   │ (cowboy_rest)│ (your business) │
└─────────────┴──────────────┴─────────────────┘
```

**Three Generated Modules:**

1. **Router Module** (`<handler>_router.erl`)
   - Implements `trails_handler` behavior
   - Provides `trails/0` for cowboy_routes_manager
   - Maps paths to operations

2. **Handler Module** (`<handler>.erl`)
   - Implements `cowboy_rest` behavior
   - Handles HTTP callbacks
   - Delegates to logic handler

3. **Logic Handler** (`<logic_module>.erl`)
   - Contains business logic callbacks
   - Separated from routing concerns
   - Easy to test and maintain

### Why This Works

- ✅ **No changes to existing infrastructure** - Works with current cowboy_routes_manager
- ✅ **Gradual migration** - Convert handlers one at a time
- ✅ **Future-proof** - Easy to migrate to full openapi-generator pattern later
- ✅ **Clear separation** - Router → Handler → Logic provides clean architecture

### Core Modules

The plugin consists of 10 Erlang modules:

- `rebar3_openapi.erl` - Main plugin entry point
- `rebar3_openapi_parser.erl` - OpenAPI YAML/JSON parser
- `rebar3_openapi_generator.erl` - openapi-generator CLI wrapper
- `rebar3_openapi_transformer.erl` - Code transformation engine
- `rebar3_openapi_extractor.erl` - Code to OpenAPI extraction
- `rebar3_openapi_validator.erl` - Validation logic
- `rebar3_openapi_prv_generate.erl` - Generate command provider
- `rebar3_openapi_prv_extract.erl` - Extract command provider
- `rebar3_openapi_prv_validate.erl` - Validate command provider
- `rebar3_openapi_utils.erl` - Common utilities

## Integration

### With Cowboy Application

In your application startup code:

```erlang
%% src/my_app_app.erl

start(_StartType, _StartArgs) ->
    %% Compile routes using trails
    Trails = trails:trails([
        my_api_router,     % Your generated router
        other_handler      % Other handlers
    ]),

    trails:store(Trails),
    Dispatch = trails:single_host_compile(Trails),

    %% Start Cowboy
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

    my_app_sup:start_link().
```

### CI/CD Integration

Add to your CI pipeline:

```yaml
# .github/workflows/ci.yml
name: CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '26'
          rebar3-version: '3.22'

      - name: Compile
        run: rebar3 compile

      - name: Validate OpenAPI
        run: rebar3 openapi validate --all --strict
```

This ensures your code always matches your OpenAPI specs.

## Status

### ✅ Core Implementation Complete

**Phases 1-3 Implemented:**
- ✅ OpenAPI parsing (YAML/JSON)
- ✅ Code generation from OpenAPI specs
- ✅ Hybrid pattern transformation (trails() compatible)
- ✅ OpenAPI extraction from handlers
- ✅ Validation and comparison
- ✅ Manifest tracking
- ✅ CI-ready strict mode
- ✅ Docker fallback for openapi-generator

### 🚧 Partial Implementation

These features work at a basic level but could be enhanced:

- **--update mode** - Basic structure exists, needs full AST parsing for intelligent merging
- **Advanced extraction** - Currently uses simplified pattern matching, full AST parsing would improve accuracy
- **Schema extraction from priv/** - Helper functions exist, not fully wired up

### 📋 Future Enhancements

Planned for future releases:
- Full AST parsing for robust code modification
- Advanced schema extraction from Jesse files
- Request/response validation using Jesse
- Template customization support
- Plugin architecture for custom transformations
- Comprehensive test suite
- Publishing to hex.pm

## Documentation

- **[EXAMPLES.md](EXAMPLES.md)** - Detailed usage examples and workflows
- **[CONTRIBUTING.md](CONTRIBUTING.md)** - Development guide and contribution guidelines
- **[examples/simple_api.yml](examples/simple_api.yml)** - Example OpenAPI specification
- **[Implementation Plan](../rebar3_openapi_implementation_plan.md)** - Original design and architecture decisions

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for:
- Development setup
- Project structure
- Code style guidelines
- How to submit pull requests

## Known Limitations

1. **Update Mode**: Currently logs what it would do but doesn't perform full AST-based updates. Manual merge may be required when using `--update`.

2. **Extraction Accuracy**: Extraction from existing handlers is simplified and may not capture all nuances of complex handlers.

3. **Schema Conversion**: Jesse to OpenAPI schema conversion covers common cases but may not handle all edge cases.

4. **Operation Routing**: The generated handler uses simplified operation routing. Complex routing logic may need customization.

## Troubleshooting

### openapi-generator not found

```
Error: openapi-generator not found
```

**Solution**: Install openapi-generator or ensure Docker is available:
```bash
npm install -g @openapitools/openapi-generator-cli
# or
docker pull openapitools/openapi-generator-cli
```

### Handler not found during extraction

```
Error: Handler module not found: my_handler
```

**Solution**: Use `--app` to specify the application:
```bash
rebar3 openapi extract \
    --handler my_handler \
    --app my_app \
    --output spec.yml
```

### Spec validation failed

```
Error: Invalid YAML in spec.yml
```

**Solution**: Validate your OpenAPI spec:
- [Swagger Editor](https://editor.swagger.io/)
- [OpenAPI Validator](https://apitools.dev/swagger-parser/online/)

## License

Apache 2.0

## Credits

Built with ❤️ for the Butler Core team

Based on the [Hybrid Pattern design](../rebar3_openapi_implementation_plan.md) that leverages openapi-generator while maintaining compatibility with existing Cowboy infrastructure.

## Support

For issues and questions:
- File an issue on GitHub
- Check [EXAMPLES.md](EXAMPLES.md) for usage patterns
- Review [CONTRIBUTING.md](CONTRIBUTING.md) for development details
