# Usage Examples

This document provides comprehensive examples for using `rebar3_openapi` in various scenarios.

## Table of Contents

- [Installation](#installation)
- [Basic Usage](#basic-usage)
- [Generate Command Examples](#generate-command-examples)
- [Extract Command Examples](#extract-command-examples)
- [Validate Command Examples](#validate-command-examples)
- [Implementing Business Logic](#implementing-business-logic)
- [Real-World Workflows](#real-world-workflows)
- [CI/CD Integration](#cicd-integration)
- [Troubleshooting](#troubleshooting)

## Installation

### Prerequisites

Install openapi-generator (optional - Docker fallback available):

```bash
# Using npm
npm install -g @openapitools/openapi-generator-cli

# Or using Homebrew (macOS)
brew install openapi-generator

# Or use Docker (no installation needed)
docker pull openapitools/openapi-generator-cli
```

### Add Plugin to Project

In your project's `rebar.config`:

```erlang
{plugins, [
    {rebar3_openapi, {git, "https://github.com/your-org/rebar3_openapi.git", {branch, "main"}}}
]}.

{openapi, [
    {specs_dir, "specs"},
    {manifest_file, ".openapi_manifest.json"},
    {server_url, "http://localhost:8080"},
    {validation_on_generate, true}
]}.
```

## Basic Usage

### Simple Generation

Generate a handler from an OpenAPI spec:

```bash
rebar3 openapi generate \
    --spec /path/to/specs/my_api.yml \
    --handler /path/to/src/my_api_handler.erl
```

This creates in the same directory as the handler:
- `my_api_router.erl`
- `my_api_handler.erl`
- `my_api_logic_handler.erl`

### Simple Extraction

Extract OpenAPI from an existing handler:

```bash
rebar3 openapi extract \
    --handler /path/to/src/my_api_handler.erl \
    --output specs/generated.yml
```

### Simple Validation

Validate handler against spec:

```bash
rebar3 openapi validate \
    --handler /path/to/src/my_api_handler.erl \
    --spec /path/to/specs/my_api.yml
```

## Generate Command Examples

### Example 1: Basic Generation

```bash
rebar3 openapi generate \
    --spec examples/simple_api.yml \
    --handler src/simple_api_handler.erl
```

**Output:**
```
Parsing OpenAPI spec: examples/simple_api.yml
Generating code with openapi-generator...
Transforming to hybrid pattern...
✓ Generation completed successfully!
Generated files:
  simple_api_router.erl
  simple_api_handler.erl
  simple_api_logic_handler.erl
```

### Example 2: Dry Run Mode

Preview what would be generated:

```bash
rebar3 openapi generate \
    --spec examples/simple_api.yml \
    --handler test_handler \
    --dry-run
```

**Output:**
```
=== DRY RUN MODE ===
Spec file: examples/simple_api.yml
Handler: test_handler
Logic module: test_handler_logic_handler
Output directory: src/

Operations to generate:
  GET /api/health -> get_health
  GET /api/users -> list_users
  POST /api/users -> create_user
  GET /api/users/{userId} -> get_user
  PUT /api/users/{userId} -> update_user
  DELETE /api/users/{userId} -> delete_user

Files that would be generated:
  test_handler_router.erl
  test_handler.erl
  test_handler_logic_handler.erl
```

### Example 3: Generate with Custom Logic Module

```bash
rebar3 openapi generate \
    --spec specs/users_api.yml \
    --app my_app \
    --handler users_handler \
    --logic-module users_business_logic
```

### Example 4: Update Existing Handler

```bash
rebar3 openapi generate \
    --spec specs/my_api.yml \
    --handler my_api_handler \
    --update
```

**Note**: `--update` mode preserves your business logic but updates routing structures.

### Example 5: Force Overwrite

```bash
rebar3 openapi generate \
    --spec specs/my_api.yml \
    --handler my_api_handler \
    --force
```

**Warning**: This will overwrite existing files without prompting.

## Extract Command Examples

### Example 1: Extract Single Handler to YAML

```bash
rebar3 openapi extract \
    --handler simple_api_handler \
    --app my_app \
    --output specs/simple_api.yml
```

**Output:**
```
Extracting OpenAPI from handler: simple_api_handler
✓ OpenAPI spec written to: specs/simple_api.yml
```

### Example 2: Extract to JSON

```bash
rebar3 openapi extract \
    --handler my_handler \
    --output specs/my_api.json \
    --format json
```

### Example 3: Batch Extract All Handlers

Extract all handlers in an application:

```bash
rebar3 openapi extract \
    --app my_app \
    --output-dir specs/
```

**Output:**
```
Extracting OpenAPI specs from all handlers
Output directory: specs/
Found 3 handler(s)

✓ Extracted: users_handler -> specs/users_handler.yml
✓ Extracted: posts_handler -> specs/posts_handler.yml
✓ Extracted: comments_handler -> specs/comments_handler.yml

✓ All extractions completed successfully
```

### Example 4: Extract from Multi-App Project

```bash
rebar3 openapi extract \
    --app my_core_app \
    --output-dir specs/core/
```

## Validate Command Examples

### Example 1: Basic Validation

```bash
rebar3 openapi validate \
    --handler my_api_handler \
    --spec specs/my_api.yml
```

**Output (Success):**
```
=== OpenAPI Validation ===
Handler: my_api_handler
Spec: specs/my_api.yml
Mode: normal

Validating handler against spec...
✓ Handler matches spec perfectly
✓ Validation passed!
```

**Output (Differences Found):**
```
=== OpenAPI Validation ===
Handler: my_api_handler
Spec: specs/my_api.yml
Mode: normal

Validating handler against spec...
⚠ Handler differs from spec:
  - Operation in spec but not implemented: POST /api/users
  + Operation implemented but not in spec: GET /api/admin

⚠ Validation completed with differences
Found 2 difference(s) (non-strict mode)
```

### Example 2: Strict Mode for CI

```bash
rebar3 openapi validate \
    --handler my_api_handler \
    --spec specs/my_api.yml \
    --strict
```

**Output (Failure):**
```
✗ Validation failed in strict mode
Error: Validation failed with 2 difference(s)
```

Exit code: 1 (fails CI)

### Example 3: Validate All Handlers

```bash
rebar3 openapi validate --all
```

**Output:**
```
=== Batch OpenAPI Validation ===
Found 3 handler(s) in manifest

=== OpenAPI Validation ===
Handler: users_handler
Spec: specs/users_api.yml
✓ Validation passed!

=== OpenAPI Validation ===
Handler: posts_handler
Spec: specs/posts_api.yml
✓ Validation passed!

=== OpenAPI Validation ===
Handler: comments_handler
Spec: specs/comments_api.yml
⚠ Validation completed with differences

✓ All validations passed!
```

### Example 4: CI Pipeline Validation

```bash
rebar3 openapi validate --all --strict
```

## Implementing Business Logic

After generation, implement your business logic in the logic handler:

### Example: User Management API

```erlang
%% src/users_logic_handler.erl

-module(users_logic_handler).
-export([provide_callback/4, accept_callback/4]).

%%%===================================================================
%%% GET Operations
%%%===================================================================

%% GET /api/users - List all users
provide_callback(_Class, 'list_users', Req, Context) ->
    %% Call your controller or database
    Users = user_repository:find_all(),
    Response = [user_to_map(U) || U <- Users],
    {ok, Response, Req, Context};

%% GET /api/users/{userId} - Get user by ID
provide_callback(_Class, 'get_user', Req, Context) ->
    UserId = cowboy_req:binding(userId, Req),

    case user_repository:find_by_id(UserId) of
        {ok, User} ->
            Response = user_to_map(User),
            {ok, Response, Req, Context};
        {error, not_found} ->
            Error = #{error => <<"not_found">>,
                     message => <<"User not found">>},
            Req2 = cowboy_req:reply(404, #{}, jsx:encode(Error), Req),
            {error, not_found, Req2, Context}
    end;

%%%===================================================================
%%% POST/PUT/DELETE Operations
%%%===================================================================

%% POST /api/users - Create user
accept_callback(_Class, 'create_user', Req, Context) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    UserData = jsx:decode(Body, [return_maps]),

    %% Validate input
    case validate_user_data(UserData) of
        ok ->
            case user_repository:create(UserData) of
                {ok, User} ->
                    Response = user_to_map(User),
                    Req3 = cowboy_req:set_resp_body(jsx:encode(Response), Req2),
                    UserId = maps:get(id, User),
                    Location = <<"/api/users/", UserId/binary>>,
                    Req4 = cowboy_req:set_resp_header(<<"location">>, Location, Req3),
                    {ok, cowboy_req:reply(201, Req4), Context};
                {error, Reason} ->
                    Error = #{error => <<"creation_failed">>,
                             message => error_to_binary(Reason)},
                    Req3 = cowboy_req:reply(500, #{}, jsx:encode(Error), Req2),
                    {error, Reason, Req3, Context}
            end;
        {error, ValidationError} ->
            Error = #{error => <<"validation_failed">>,
                     message => ValidationError},
            Req3 = cowboy_req:reply(400, #{}, jsx:encode(Error), Req2),
            {error, validation_failed, Req3, Context}
    end;

%% PUT /api/users/{userId} - Update user
accept_callback(_Class, 'update_user', Req, Context) ->
    UserId = cowboy_req:binding(userId, Req),
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    UpdateData = jsx:decode(Body, [return_maps]),

    case user_repository:update(UserId, UpdateData) of
        {ok, User} ->
            Response = user_to_map(User),
            {ok, cowboy_req:reply(200, #{}, jsx:encode(Response), Req2), Context};
        {error, not_found} ->
            Error = #{error => <<"not_found">>, message => <<"User not found">>},
            Req3 = cowboy_req:reply(404, #{}, jsx:encode(Error), Req2),
            {error, not_found, Req3, Context}
    end;

%% DELETE /api/users/{userId} - Delete user
accept_callback(_Class, 'delete_user', Req, Context) ->
    UserId = cowboy_req:binding(userId, Req),

    case user_repository:delete(UserId) of
        ok ->
            {ok, cowboy_req:reply(204, Req), Context};
        {error, not_found} ->
            Error = #{error => <<"not_found">>, message => <<"User not found">>},
            Req2 = cowboy_req:reply(404, #{}, jsx:encode(Error), Req),
            {error, not_found, Req2, Context}
    end;

%%%===================================================================
%%% Fallback
%%%===================================================================

provide_callback(_Class, OperationId, Req, Context) ->
    io:format("Unimplemented operation: ~p~n", [OperationId]),
    {error, not_implemented, Req, Context}.

accept_callback(_Class, OperationId, Req, Context) ->
    io:format("Unimplemented operation: ~p~n", [OperationId]),
    {error, not_implemented, Req, Context}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

user_to_map(User) ->
    #{
        id => User#user.id,
        username => User#user.username,
        email => User#user.email,
        created_at => iso8601_timestamp(User#user.created_at)
    }.

validate_user_data(Data) ->
    %% Add your validation logic
    ok.

error_to_binary(Error) when is_atom(Error) ->
    atom_to_binary(Error, utf8);
error_to_binary(Error) when is_binary(Error) ->
    Error;
error_to_binary(Error) ->
    list_to_binary(io_lib:format("~p", [Error])).

iso8601_timestamp(Timestamp) ->
    list_to_binary(calendar:system_time_to_rfc3339(Timestamp)).
```

## Real-World Workflows

### Workflow 1: Starting from Scratch

**Scenario**: Building a new API from an OpenAPI spec.

```bash
# 1. Create or obtain OpenAPI spec
vim specs/my_api.yml

# 2. Generate handler
rebar3 openapi generate \
    --spec specs/my_api.yml \
    --app my_app \
    --handler my_api_handler

# 3. Implement business logic
vim src/my_api_logic_handler.erl

# 4. Register routes in your application
vim src/my_app_app.erl

# 5. Test
rebar3 compile
rebar3 shell

# 6. Validate
rebar3 openapi validate \
    --handler my_api_handler \
    --spec specs/my_api.yml
```

### Workflow 2: Refactoring Existing Handler

**Scenario**: You have an existing handler and want to migrate to OpenAPI.

```bash
# 1. Backup existing handler
cp src/my_handler.erl src/my_handler.erl.backup

# 2. Extract current API to OpenAPI
rebar3 openapi extract \
    --handler my_handler \
    --output specs/my_api.yml

# 3. Review and enhance the spec
vim specs/my_api.yml
# - Add descriptions
# - Add examples
# - Refine schemas
# - Get approval from team

# 4. Generate hybrid handler
rebar3 openapi generate \
    --spec specs/my_api.yml \
    --handler my_handler \
    --logic-module my_handler_logic \
    --update

# 5. Move business logic
# Copy functions from my_handler.erl.backup to my_handler_logic.erl
# Update signatures to match provide_callback/accept_callback

# 6. Test thoroughly
rebar3 eunit
# Manual API testing

# 7. Validate
rebar3 openapi validate \
    --handler my_handler \
    --spec specs/my_api.yml

# 8. Commit
git add src/my_handler*.erl specs/my_api.yml .openapi_manifest.json
git commit -m "Refactor my_handler to use OpenAPI"
```

### Workflow 3: API Evolution

**Scenario**: Adding new endpoints to an existing API.

```bash
# 1. Update OpenAPI spec with new endpoints
vim specs/my_api.yml

# 2. Regenerate with --update
rebar3 openapi generate \
    --spec specs/my_api.yml \
    --handler my_api_handler \
    --update

# 3. Implement new operations in logic handler
vim src/my_api_logic_handler.erl

# 4. Test new endpoints
curl -X POST http://localhost:8080/api/new-endpoint

# 5. Validate
rebar3 openapi validate \
    --handler my_api_handler \
    --spec specs/my_api.yml \
    --strict
```

## CI/CD Integration

### GitHub Actions

```yaml
# .github/workflows/ci.yml
name: CI

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26'
          rebar3-version: '3.22'

      - name: Restore dependencies cache
        uses: actions/cache@v3
        with:
          path: _build
          key: ${{ runner.os }}-mix-${{ hashFiles('**/rebar.lock') }}

      - name: Install dependencies
        run: rebar3 get-deps

      - name: Compile
        run: rebar3 compile

      - name: Run tests
        run: rebar3 eunit

      - name: Validate OpenAPI specs
        run: rebar3 openapi validate --all --strict

      - name: Check dialyzer
        run: rebar3 dialyzer
```

### GitLab CI

```yaml
# .gitlab-ci.yml
image: erlang:26

stages:
  - build
  - test
  - validate

cache:
  paths:
    - _build/

build:
  stage: build
  script:
    - rebar3 compile

test:
  stage: test
  script:
    - rebar3 eunit
    - rebar3 ct

validate_openapi:
  stage: validate
  script:
    - rebar3 openapi validate --all --strict
  only:
    - branches
```

### Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

echo "Validating OpenAPI specs..."
rebar3 openapi validate --all --strict

if [ $? -ne 0 ]; then
    echo "❌ OpenAPI validation failed"
    echo "Fix the discrepancies or update your OpenAPI specs"
    exit 1
fi

echo "✅ OpenAPI validation passed"
```

Make it executable:
```bash
chmod +x .git/hooks/pre-commit
```

## Troubleshooting

### Issue: openapi-generator not found

**Error:**
```
Error: openapi-generator not found
```

**Solution:**
```bash
# Option 1: Install via npm
npm install -g @openapitools/openapi-generator-cli

# Option 2: Use Docker (plugin will auto-detect)
docker pull openapitools/openapi-generator-cli

# Option 3: Install via Homebrew (macOS)
brew install openapi-generator
```

### Issue: Handler not found during extraction

**Error:**
```
Error: Handler module not found: my_handler
```

**Solutions:**
```bash
# Specify the application
rebar3 openapi extract \
    --handler my_handler \
    --app my_app \
    --output spec.yml

# Check if the handler file exists
ls -la apps/my_app/src/my_handler.erl

# If in umbrella project, navigate to app first
cd apps/my_app
rebar3 openapi extract --handler my_handler --output ../../specs/my_api.yml
```

### Issue: Invalid YAML in spec

**Error:**
```
Error: Invalid YAML in spec.yml: ...
```

**Solutions:**
```bash
# Validate YAML syntax
yamllint specs/my_api.yml

# Validate OpenAPI structure online
# Visit: https://editor.swagger.io/
# Paste your spec and check for errors

# Use OpenAPI CLI validator
npx @apidevtools/swagger-cli validate specs/my_api.yml
```

### Issue: Validation shows differences

**Output:**
```
⚠ Handler differs from spec:
  - Operation in spec but not implemented: POST /api/users
  + Operation implemented but not in spec: GET /api/admin
```

**Solutions:**

1. **Add missing implementation:**
   ```bash
   # Regenerate to add missing operations
   rebar3 openapi generate \
       --spec specs/my_api.yml \
       --handler my_handler \
       --update

   # Implement the new operation in logic handler
   vim src/my_handler_logic.erl
   ```

2. **Update spec to match implementation:**
   ```bash
   # Extract current implementation
   rebar3 openapi extract \
       --handler my_handler \
       --output specs/updated.yml

   # Review and merge with existing spec
   diff specs/my_api.yml specs/updated.yml
   ```

3. **Remove extra implementations:**
   - Manually remove unused operations from handler
   - Or add them to the OpenAPI spec if they should be documented

## Advanced Examples

### Example: Using with Umbrella Projects

```bash
# In umbrella root
cd apps/my_core_app

rebar3 openapi generate \
    --spec ../../specs/core_api.yml \
    --app my_core_app \
    --handler core_handler

cd ../..
rebar3 compile
```

### Example: Multiple APIs in One Application

```bash
# Generate users API
rebar3 openapi generate \
    --spec specs/users_api.yml \
    --app my_app \
    --handler users_handler

# Generate posts API
rebar3 openapi generate \
    --spec specs/posts_api.yml \
    --app my_app \
    --handler posts_handler

# Validate all
rebar3 openapi validate --all
```

### Example: Custom Package Names

```bash
rebar3 openapi generate \
    --spec specs/my_api.yml \
    --handler my_handler \
    --package-name my_custom_package
```

## Example OpenAPI Spec

See [examples/simple_api.yml](examples/simple_api.yml) for a complete example.

## Next Steps

- Review [README.md](README.md) for command reference
- Check [CONTRIBUTING.md](CONTRIBUTING.md) for development guide
- Explore [examples/](examples/) directory for more samples

