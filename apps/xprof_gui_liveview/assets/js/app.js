// If you want to use Phoenix channels, run `mix help phx.gen.channel`
// to get started and then uncomment the line below.
// import "./user_socket.js"

// You can include dependencies in two ways.
//
// The simplest option is to put them in assets/vendor and
// import them using relative paths:
//
//     import "../vendor/some-package.js"
//
// Alternatively, you can `npm install some-package --prefix assets` and import
// them using a path starting with the package name:
//
//     import "some-package"
//
// If you have dependencies that try to import CSS, esbuild will generate a separate `app.css` file.
// To load it, simply add a second `<link>` to your `root.html.heex` file.

// Include phoenix_html to handle method=PUT/DELETE in forms and buttons.
import "phoenix_html"
// Establish Phoenix Socket and LiveView configuration.
import {Socket} from "phoenix"
import {LiveSocket} from "phoenix_live_view"
import {hooks as colocatedHooks} from "phoenix-colocated/xprof_gui_liveview"
import topbar from "../vendor/topbar"
import ApexCharts from "../vendor/apexcharts"

// Global keyboard shortcuts (lives here so it survives LiveView reconnects)
document.addEventListener("keydown", e => {
  if (e.ctrlKey && e.key === "i") {
    e.preventDefault()
    const btn = document.querySelector('[phx-click="toggle_input_type"]:not([disabled])')
    if (btn) btn.click()
  }
})

// Auto-focus query input after LiveView navigations
window.addEventListener("phx:page-loading-stop", () => {
  const input = document.getElementById("query-input")
  if (input) input.focus()
})

const Hooks = {}

// Auto-hide flash messages after 5 seconds
Hooks.AutoHideFlash = {
  mounted() {
    this.timer = setTimeout(() => {
      this.el.click()
    }, 5000)
  },
  destroyed() {
    clearTimeout(this.timer)
  }
}

// Prevent Tab/Arrow/Enter browser defaults on the search input when suggestions are active
Hooks.SearchInput = {
  mounted() {
    this.el.addEventListener("keydown", e => {
      const hasSuggestions = this.el.dataset.hasSuggestions === "true"
      const hasSelection = parseInt(this.el.dataset.position) >= 0

      if (e.key === "Tab" && hasSuggestions) {
        e.preventDefault()
      }
      if ((e.key === "ArrowUp" || e.key === "ArrowDown") && hasSuggestions) {
        e.preventDefault()
      }
      // Prevent form submit when a suggestion is highlighted; let key_down handler accept it
      if (e.key === "Enter" && hasSelection) {
        e.preventDefault()
      }
    })
  }
}

// Preserve collapse open/closed state across LiveView patches
Hooks.Collapse = {
  mounted() {
    this.input = this.el.querySelector('input[type="checkbox"]')
    this.isOpen = false
    if (this.input) {
      this.input.addEventListener('change', () => {
        this.isOpen = this.input.checked
      })
    }
  },
  updated() {
    const input = this.el.querySelector('input[type="checkbox"]')
    if (input && this.isOpen) {
      input.checked = true
    }
  }
}

// ApexCharts hook for rendering time-series graphs
Hooks.ApexChart = {
  mounted() {
    const chartData = JSON.parse(this.el.dataset.chart)
    this._applyFormatters(chartData)
    this.chart = new ApexCharts(this.el, chartData)
    this.chart.render()
  },
  updated() {
    if (!this.chart) return
    const chartData = JSON.parse(this.el.dataset.chart)
    this._applyFormatters(chartData)

    // Compute explicit x-axis bounds from the actual data timestamps so the visible
    // window is always anchored to the last data point, not to the system clock.
    // When tracing is paused, no new points arrive, so maxX stays fixed → chart freezes.
    const allX = chartData.series
      .flatMap(s => (s.data || []).map(p => p.x))
      .filter(x => x > 0)

    if (allX.length > 0) {
      const maxX = Math.max(...allX)
      chartData.xaxis = { ...chartData.xaxis, min: maxX - 120_000, max: maxX }
      delete chartData.xaxis.range
    }

    this.chart.updateOptions(chartData, false, false)
  },
  destroyed() {
    if (this.chart) {
      this.chart.destroy()
    }
  },
  _applyFormatters(chartData) {
    const formatTime = val => {
      if (val >= 1_000_000) return (val / 1_000_000).toFixed(2) + 's'
      if (val >= 1_000) return (val / 1_000).toFixed(1) + 'ms'
      return Math.round(val) + 'µs'
    }

    if (Array.isArray(chartData.yaxis)) {
      chartData.yaxis = chartData.yaxis.map(axis => {
        if (axis.seriesName === "Count") {
          // Only label integer ticks; return '' for fractional positions so ApexCharts
          // never shows duplicate labels (e.g. two "1"s) when the count range is small.
          return { ...axis, labels: { formatter: val => Number.isInteger(val) ? val : '' } }
        } else if (axis.show !== false) {
          return { ...axis, labels: { formatter: formatTime } }
        }
        return axis
      })
    }

    chartData.tooltip = {
      ...(chartData.tooltip || {}),
      y: {
        formatter: (val, { seriesIndex }) => {
          if (seriesIndex === 3) return Math.round(val) // Count series (index 3)
          return formatTime(val)
        }
      }
    }
  }
}

const csrfToken = document.querySelector("meta[name='csrf-token']").getAttribute("content")
const liveSocket = new LiveSocket("/live", Socket, {
  longPollFallbackMs: 2500,
  params: {_csrf_token: csrfToken},
  hooks: {...colocatedHooks, ...Hooks},
})

// Show progress bar on live navigation and form submits
topbar.config({barColors: {0: "#29d"}, shadowColor: "rgba(0, 0, 0, .3)"})
window.addEventListener("phx:page-loading-start", _info => topbar.show(300))
window.addEventListener("phx:page-loading-stop", _info => topbar.hide())

// connect if there are any LiveViews on the page
liveSocket.connect()

// expose liveSocket on window for web console debug logs and latency simulation:
// >> liveSocket.enableDebug()
// >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
// >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket

// The lines below enable quality of life phoenix_live_reload
// development features:
//
//     1. stream server logs to the browser console
//     2. click on elements to jump to their definitions in your code editor
//
if (process.env.NODE_ENV === "development") {
  window.addEventListener("phx:live_reload:attached", ({detail: reloader}) => {
    // Enable server log streaming to client.
    // Disable with reloader.disableServerLogs()
    reloader.enableServerLogs()

    // Open configured PLUG_EDITOR at file:line of the clicked element's HEEx component
    //
    //   * click with "c" key pressed to open at caller location
    //   * click with "d" key pressed to open at function component definition location
    let keyDown
    window.addEventListener("keydown", e => keyDown = e.key)
    window.addEventListener("keyup", _e => keyDown = null)
    window.addEventListener("click", e => {
      if(keyDown === "c"){
        e.preventDefault()
        e.stopImmediatePropagation()
        reloader.openEditorAtCaller(e.target)
      } else if(keyDown === "d"){
        e.preventDefault()
        e.stopImmediatePropagation()
        reloader.openEditorAtDef(e.target)
      }
    }, true)

    window.liveReloader = reloader
  })
}

