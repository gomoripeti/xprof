# XProf GUI LiveView

Phoenix LiveView-based web interface for XProf - a visual tracer and profiler for Erlang/Elixir applications.

## Status

**Production Ready** - This application provides a modern Phoenix LiveView alternative to the existing React-based GUI (`xprof_gui`). All core features are implemented and tested (37 tests passing).

The LiveView GUI offers the same functionality as the React GUI with enhanced real-time updates via WebSockets and a more maintainable Elixir codebase.

## Goals

- Provide the same functionality as the React GUI but with improved real-time updates
- Maintain the same UI/UX including layout, components, and keyboard shortcuts
- Leverage Phoenix LiveView for server-side rendering and WebSocket-based real-time updates
- Integrate seamlessly with `xprof_core` for tracing and profiling

## Architecture

This is a standard Phoenix 1.8 application with:
- **No Database (Ecto)** - Uses `xprof_core` for all data
- **No Mailer** - Not needed for this application
- **LiveView** - For real-time, interactive UI
- **Tailwind CSS + DaisyUI** - For styling
- **Bandit** - HTTP server (instead of Cowboy to avoid conflicts with xprof_gui)

## Development Setup

### Prerequisites

- Erlang/OTP 28+
- Elixir 1.18+
- Node.js (for asset compilation)

### First Time Setup

```bash
# From the xprof_gui_liveview directory
export XPROF_ERL_HIST=true  # Use Erlang histogram (NIF has zlib issues)
mix deps.get
mix assets.setup
mix assets.build
```

### Running the Server

```bash
# Start Phoenix server
export XPROF_ERL_HIST=true
mix phx.server

# Or inside IEx
export XPROF_ERL_HIST=true
iex -S mix phx.server
```

Visit http://localhost:7890 in your browser.

### Development Mode

The app includes hot-reload for development:

```bash
export XPROF_ERL_HIST=true
mix phx.server
```

Changes to Elixir code, templates (.heex files), and assets will automatically reload.

## Integration with rebar3 Umbrella

This Phoenix/Mix app lives inside a rebar3 umbrella project. Key integration points:

1. **Dependencies**: Uses `{:xprof_core, in_umbrella: true, path: "../xprof_core"}` to depend on the Erlang core
2. **Version**: Matches xprof version (2.0.0-rc.5)
3. **Port**: Uses port 7890 (configurable via `PORT` env var)
4. **Histogram**: Requires `XPROF_ERL_HIST=true` environment variable to use Erlang histogram implementation

## Key Differences from Legacy GUI

### Technology Stack
- **Legacy**: React + Webpack + Cowboy
- **LiveView**: Phoenix + LiveView + Bandit + Tailwind

### Real-Time Updates
- **Legacy**: Polling via REST API + client-side state management
- **LiveView**: WebSocket push from server + server-side state

### Deployment
- **Legacy**: Compiled JavaScript bundle served as static assets
- **LiveView**: Server-side rendered templates with dynamic updates

## Current Features

### ✅ Implemented
- **Core UI**: Phoenix LiveView-based interface on port 7890
- **Function Monitoring**: Start/stop monitoring Erlang/Elixir functions
- **Autocomplete**: Real-time function name completion using xprof_core
  - Appends suggestions to existing query
  - Regenerates suggestions after selection
  - Hides dropdown when single match equals query
- **Recent Query History**: Terminal-style command history with arrow keys
- **Live Statistics Tables**: Real-time metrics display
  - Count, Min, Mean, Median percentiles (P50, P75, P90, P99), Max
  - Updates every 2 seconds
  - Microsecond precision timing data
- **Call Capture**: Capture and view function call arguments and results
  - Start/stop capture with configurable threshold and limit
  - View captured calls with arguments, results, and execution time
  - Displays exceptions with warnings
  - Incremental data fetching support
- **Favourites**: Persistent storage of favourite queries
  - Add/remove queries to favourites
  - ETS-based in-memory persistence (survives across LiveView sessions)
  - Filter favourites with search
- **Trace Control**: Global trace pause/resume
- **Grid Layout**: 1-4 column grid for multiple monitored functions
- **Input Modes**: Toggle between search and favourites (Ctrl+I)

- **Query Validation**: Input validation with helpful error messages
  - Empty query prevention
  - Length limits (max 500 characters)
  - Character validation (alphanumeric + underscore)
  - Automatic whitespace trimming

### Test Coverage
- **37 total tests**, all passing
- Unit tests for favourites store (9 tests)
- LiveView integration tests (23 tests)
- Query validation tests (5 tests)
- Edge case and error scenario coverage

### 🚧 Future Enhancements
- HDR histogram visualization (graphs)
- Function call tree
- Multiple function comparison view
- Export/import functionality
- Dark mode theme

## Roadmap

### Phase 1: Core Functionality ✅
- [x] Create Phoenix skeleton app
- [x] Configure port 7890
- [x] Integrate with xprof_core
- [x] Basic layout matching React GUI

### Phase 2: Tracing Interface ✅
- [x] Function monitoring controls (start/stop)
- [x] Query input with real autocomplete
- [x] Start/stop tracing buttons
- [x] Recent queries tracking
- [x] Favourites with ETS persistence

### Phase 3: Data Visualization 🔄
- [x] Real-time statistics tables (percentiles, call counts)
- [ ] HDR histogram visualization (graphs skipped for now)
- [x] Argument/result capture display
- [ ] Function call tree

### Phase 4: Advanced Features
- [x] Keyboard shortcuts (Ctrl+I, arrows for history/autocomplete)
- [ ] Multiple function comparison
- [ ] Export/import functionality
- [ ] Dark mode

### Phase 5: Polish & Migration
- [ ] Match exact UX of React GUI
- [ ] Performance optimization
- [ ] Full test coverage
- [ ] Deprecate React GUI

## Configuration

Key configuration files:

- `config/config.exs` - Application configuration
- `config/dev.exs` - Development environment
- `config/runtime.exs` - Runtime configuration (port, etc.)
- `mix.exs` - Dependencies and project settings

## Testing

```bash
# Run all tests (37 tests)
export XPROF_ERL_HIST=true
mix test

# Run specific test file
mix test test/xprof_gui_liveview_web/live/monitoring_live_test.exs

# Run with coverage
mix test --cover
```

### Test Organization
- `test/xprof_gui_liveview/favourites_store_test.exs` - FavouritesStore unit tests
- `test/xprof_gui_liveview_web/live/monitoring_live_test.exs` - LiveView integration tests
- `test/xprof_gui_liveview_web/controllers/` - Controller tests

All tests run in CI and pass on OTP 28+ / Elixir 1.18+.

## API Integration

This app communicates with `xprof_core` through direct Erlang function calls:

- `:xprof_core.monitor/1,2` - Start monitoring a function
- `:xprof_core.demonitor/1` - Stop monitoring
- `:xprof_core.get_data/2` - Retrieve statistics
- `:xprof_core.capture/3` - Start capturing calls
- `:xprof_core.get_captured_data/2` - Get captured data

See `xprof_core` documentation for full API.

## Contributing

When working on this application:

1. Ensure changes maintain parity with the React GUI behavior
2. Keep the UI/UX consistent
3. Test real-time updates thoroughly
4. Verify integration with xprof_core

## License

Same as XProf main project (BSD-3-Clause)
