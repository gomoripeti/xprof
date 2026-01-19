# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

XProf is a visual tracer and profiler for Erlang/Elixir applications. It allows real-time tracing of function execution, collecting latency statistics, capturing arguments/results, and analyzing performance bottlenecks via a web interface.

**Key Features:**
- Real-time function call tracing with HDR histogram statistics
- Web UI for visualizing call counts, latency percentiles (p50, p90, p99, etc.)
- Argument/result capture for slow calls
- Extended query syntax with match-spec functions and commands
- Dual-mode support for Erlang and Elixir syntax
- Pluggable command system for different profiling modes

**Supported OTP Versions:** 18-26

## Architecture

XProf is an Erlang **umbrella application** with four apps:

### 1. `xprof` (apps/xprof/)
- Convenience wrapper that starts both core and GUI
- Entry point: `xprof:start/0`
- Depends on: `xprof_core`, `xprof_gui`

### 2. `xprof_core` (apps/xprof_core/)
- **The tracing engine** - no HTTP dependencies
- Uses Erlang's built-in `erlang:trace/3` and `erlang:trace_pattern/3`
- Key components:
  - `xprof_core_tracer` - Central gen_server orchestrating all tracing
  - `xprof_core_trace_handler` - One gen_server per traced MFA, collects statistics
  - `xprof_core_trace_handler_sup` - Dynamic supervisor (simple_one_for_one) for handlers
  - `xprof_core_records` - Manages Erlang record definitions for pretty-printing
  - Command modules (`xprof_core_cmd_*`) - Pluggable profiling modes

**Supervision Tree:**
```
xprof_core_sup (rest_for_one)
├── xprof_core_records
├── xprof_core_trace_handler_sup (simple_one_for_one)
│   └── xprof_core_trace_handler (dynamic children, one per traced function)
└── xprof_core_tracer
```

**How Tracing Works:**
1. User submits query (e.g., `funlatency(lists:map/2)`)
2. Query parser converts to command + parameters
3. Tracer spawns handler under supervisor
4. Handler calls `erlang:trace_pattern/3` with match-spec
5. VM sends `{trace_ts, Pid, call, MFA, Args, Timestamp}` messages
6. Tracer routes messages to handlers via local process registration
7. Handler updates HDR histogram, creates per-second ETS snapshots
8. Web UI polls ETS tables for statistics

**Data Storage:**
- Each traced function gets a public ETS table: `xprof_<Module>_<Function>_<Arity>`
- Keys: `{sec, UnixTimestamp}` for per-second histogram snapshots
- 10-minute sliding window by default

### 3. `xprof_gui` (apps/xprof_gui/) - Legacy React GUI
- HTTP server (Cowboy) + REST API + React web interface
- Key modules:
  - `xprof_gui_rest` - Server-agnostic REST API logic
  - `xprof_gui_cowboy2_handler` / `xprof_gui_cowboy1_handler` - HTTP adapters
  - `xprof_gui_json` - Pluggable JSON encoder (supports jsone, jiffy, jsx, thoas)
- Frontend: React.js + ES6/7 + Webpack (source in `apps/xprof_gui/priv/src/`)
- Compiled assets in `apps/xprof_gui/priv/build/`
- Default: http://localhost:7890

### 4. `xprof_gui_liveview` (apps/xprof_gui_liveview/) - **New Phoenix LiveView GUI**
- **Status:** Under development - will replace React GUI
- Phoenix 1.8 + LiveView application
- Elixir/Mix-based (integrated with rebar3 umbrella)
- Depends on: `xprof_core`
- Port: 7890 (same as legacy GUI)
- Features:
  - Real-time updates via Phoenix LiveView
  - Server-side rendering with WebSocket push
  - Tailwind CSS + DaisyUI components
  - Same functionality as React GUI with improved UX
- To run: `cd apps/xprof_gui_liveview && mix phx.server`
- **Important:** Must set `XPROF_ERL_HIST=true` when compiling (NIF histogram has zlib dependency issues)

**REST API Endpoints:**
- `/api/mon_start?query=...` - Start tracing
- `/api/mon_stop?mod=...&fun=...&arity=...` - Stop tracing
- `/api/data?mod=...&fun=...&arity=...` - Get statistics
- `/api/capture?threshold=...&limit=...` - Start capturing slow calls
- `/api/capture_data?mod=...&fun=...` - Retrieve captured calls

## Common Commands

### Building and Testing

```bash
# Compile the project
make compile

# Run all tests (eunit + common_test)
make test

# Run dialyzer
make dialyzer

# Run tests with specific JSON library
make test_jiffy
make test_jsx
make test_thoas
```

### Development Mode

**Backend (Erlang):**
```bash
make dev_back_end
# Starts rebar3 shell with xprof loaded
# In shell: xprof:start().
# Then visit http://localhost:7890
```

**Frontend (JavaScript - first time setup):**
```bash
make bootstrap_front_end  # Install npm dependencies
```

**Frontend (development with auto-reload):**
```bash
make dev_front_end
# Starts webpack dev server with hot reload
```

**Full development mode (both):**
```bash
# Terminal 1:
make dev_back_end

# Terminal 2:
make dev_front_end
```

### Frontend-Only Commands

```bash
cd apps/xprof_gui/priv

# Install dependencies
npm install

# Run tests
npm run test:single-run

# Build production assets
npm run build

# Dev server with Cowboy
npm run start:with-cowboy
```

### Running in Production

```erlang
%% In Erlang shell:
xprof:start().

%% Or start apps individually:
application:start(xprof_core).
application:start(xprof_gui).
```

```elixir
# In Elixir shell:
:xprof.start()
```

### Documentation

```bash
# Generate ExDoc documentation
make gen_ex_doc

# Generate edoc (legacy)
make doc
```

## Important Patterns

### Language Abstraction
- `xprof_core_language` behavior defines interface for language-specific parsing
- Implementations: `xprof_core_erlang_syntax`, `xprof_core_elixir_syntax`
- Auto-detection via checking if `elixir` application is running
- Manual override: `xprof_core:set_mode(erlang | elixir)`

### Command Plugin System
- Commands implement `xprof_core_cmd` behavior
- Built-in commands:
  - `funlatency` - Measures function call latencies (default)
  - `argdist` - Tracks argument value distributions
  - `gc` - Garbage collection tracing
- Each command has its own initialization, event handling, and snapshot logic

### Process Registration Pattern
- Handlers register locally as: `xprof_<Module>_<Function>_<Arity>`
- ETS tables use same name for statistics
- Enables message routing without maintaining PIDs

### Overflow Protection
- Monitors tracer message queue length
- Auto-pauses tracing if queue exceeds `max_tracer_queue_len` (default: 1000)
- Prevents memory exhaustion when tracing hot functions

## Configuration

Application environment variables (set in `sys.config` or `application:set_env/3`):

**xprof_core:**
- `mode` - `erlang` | `elixir` (auto-detected if not set)
- `max_duration` - Maximum call duration to track in ms (default: 30000)
- `ignore_recursion` - Only measure outermost recursive call (default: true)
- `max_tracer_queue_len` - Queue overflow threshold (default: 1000)
- `load_records` - List of modules to load record definitions from at startup

**xprof_gui:**
- `ip` - Listen address as tuple (default: any)
- `port` - HTTP port (default: 7890)
- `favourites_enabled` - Enable saving queries (default: true)
- `favourites_config` - Path to favourites file (default: ./favourites.cfg)

## Compile-Time Configuration

Set these as environment variables before compiling:

```bash
# Use native Erlang histogram instead of NIF
export XPROF_ERL_HIST=true

# Use Cowboy 1.x instead of 2.x (for OTP < 19)
export COWBOY_VERSION=1

# Use alternative JSON library
export XPROF_JSON_LIB='Elixir.Jason'  # or jiffy, jsx, thoas
export XPROF_JSON_ENC_FUN='encode!'   # if library uses different function name
```

## Query Syntax Examples

```erlang
%% Simple MFA
lists:map/2

%% With match-spec fun (filter calls)
funlatency(lists:map(_, [1,2,3]))

%% Capture arguments of slow calls
funlatency(my_module:slow_function/1, threshold => 100, limit => 10)

%% Argument distribution analysis
argdist(my_module:compute/2)

%% Garbage collection tracing
gc(my_module:process_data/1)
```

## Key Files for Development

**Core Tracing:**
- `apps/xprof_core/src/xprof_core_tracer.erl` - Main tracing orchestrator
- `apps/xprof_core/src/xprof_core_trace_handler.erl` - Per-MFA statistics collector
- `apps/xprof_core/src/xprof_core_cmd.erl` - Command framework
- `apps/xprof_core/src/xprof_core_cmd_*.erl` - Command implementations

**Query Parsing:**
- `apps/xprof_core/src/xprof_core_query.erl` - Entry point
- `apps/xprof_core/src/xprof_core_erlang_syntax.erl` - Erlang parser/formatter
- `apps/xprof_core/src/xprof_core_elixir_syntax.erl` - Elixir parser/formatter
- `apps/xprof_core/src/xprof_core_ms.erl` - Match-spec compiler

**HTTP/REST:**
- `apps/xprof_gui/src/xprof_gui_rest.erl` - REST API logic
- `apps/xprof_gui/src/xprof_gui_cowboy2_handler.erl` - HTTP handler

**Frontend:**
- `apps/xprof_gui/priv/src/` - React source code
- `apps/xprof_gui/priv/build/` - Compiled assets (served by Cowboy)
- `apps/xprof_gui/priv/package.json` - NPM configuration

## Testing

- **Framework:** Common Test (CT) and EUnit
- **Mocking:** Uses `meck` library
- **Coverage:** Enabled by default, results in `_build/test/cover/`
- **Test Files:** `apps/*/test/*_SUITE.erl` (CT) and `apps/*/test/*_tests.erl` (EUnit)

To run a single test suite:
```bash
rebar3 ct --suite apps/xprof_core/test/xprof_core_SUITE
```

## Notes

- XProf uses Erlang's tracing, which has minimal overhead when inactive but can impact performance when tracing hot functions
- The web interface polls for data every second - each traced function updates ETS tables with per-second snapshots
- Match-specs are compiled at query time using `ms_transform` parse transform
- Record definitions are extracted from beam file debug_info, so modules must be compiled with `debug_info`
- The project supports multiple Cowboy and JSON library versions via compile-time flags for compatibility
