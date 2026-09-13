library(shiny)
library(dplyr)
library(plotly)
library(tidyr)

r_files <- c("utils.R", "data_sources.R", "indicators.R", "ui_theme.R")
module_dir <- if (dir.exists("R")) "R" else file.path("src", "R")
for (file_name in r_files) {
  source(file.path(module_dir, file_name), local = FALSE)
}

ui <- fluidPage(
  class = "cp-app",
  lang = "en",
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$meta(name = "theme-color", content = "#F5F5F7"),
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    tags$title("CryptoPulse"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "icons/icon.svg?v=2"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png?v=2"),
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "icons/apple-touch-icon.png?v=2"),
    tags$link(rel = "manifest", href = "site.webmanifest"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "app.js")
  ),
  tags$header(
    class = "cp-nav",
    tags$a(
      class = "cp-brand",
      href = "#",
      tags$img(src = "icons/icon.svg?v=2", alt = "CryptoPulse"),
      "CryptoPulse"
    ),
    tags$nav(
      class = "cp-nav-links",
      tags$a(href = "#", class = "cp-nav-link is-active", `data-tab` = "overview", "Overview"),
      tags$a(href = "#", class = "cp-nav-link", `data-tab` = "volume", "Volume"),
      tags$a(href = "#", class = "cp-nav-link", `data-tab` = "derivatives", "Derivatives"),
      tags$a(href = "#", class = "cp-nav-link", `data-tab` = "liquidations", "Liquidations")
    ),
    tags$div(
      class = "cp-search",
      textInput("navSearch", label = NULL, placeholder = "Search BTC, ETH, SOL, BNB")
    )
  ),
  tags$main(
    class = "cp-main",
    tags$section(
      class = "cp-toolbar",
      radioButtons(
        "cryptoSelect",
        "Asset",
        choices = ASSET_CHOICES,
        selected = "Bitcoin",
        inline = TRUE
      ),
      radioButtons(
        "timeframe",
        "Timeframe",
        choices = c("24H", "7D", "30D", "90D", "1Y", "ALL", "Custom"),
        selected = "90D",
        inline = TRUE
      ),
      selectInput(
        "chartType",
        "Chart",
        choices = c("Candlestick + MA200" = "candle", "Close + MA200" = "close"),
        width = "240px"
      ),
      tags$div(
        class = "cp-refresh",
        tags$label("Updates"),
        tags$div(
          class = "cp-refresh-row",
          checkboxInput("autoRefresh", "Auto", value = TRUE),
          selectInput(
            "refreshSecs",
            label = NULL,
            choices = c("15s" = 15, "30s" = 30, "60s" = 60),
            selected = 30,
            width = "88px"
          ),
          actionButton("refreshNow", "Refresh"),
          uiOutput("refreshCountdown", inline = TRUE)
        )
      ),
      conditionalPanel(
        condition = "input.timeframe == 'Custom'",
        uiOutput("dateSliderUI")
      )
    ),
    uiOutput("statusChips"),
    tabsetPanel(
      id = "tabs",
      type = "hidden",
      tabPanel(
        "overview",
        fluidRow(
          column(4, class = "cp-col", uiOutput("priceBox")),
          column(4, class = "cp-col", uiOutput("changeBox")),
          column(4, class = "cp-col", uiOutput("maBox"))
        ),
        fluidRow(
          column(4, class = "cp-col", uiOutput("spotBox")),
          column(4, class = "cp-col", uiOutput("futuresBox")),
          column(4, class = "cp-col", uiOutput("oiBox"))
        ),
        fluidRow(
          cp_panel(
            "Price and 200-day average",
            subtitle = "Liquidation bands appear when they fall inside the visible range.",
            plotlyOutput("pricePlot", height = "460px")
          )
        )
      ),
      tabPanel(
        "volume",
        fluidRow(
          column(4, class = "cp-col", uiOutput("spotBox2")),
          column(4, class = "cp-col", uiOutput("globalVolBox")),
          column(4, class = "cp-col", uiOutput("ratioBox"))
        ),
        fluidRow(
          cp_panel("Spot versus futures volume", plotlyOutput("volumeComparePlot", height = "400px"))
        ),
        fluidRow(
          cp_panel("Spot volume trend", plotlyOutput("spotVolumePlot", height = "340px"))
        )
      ),
      tabPanel(
        "derivatives",
        fluidRow(
          column(4, class = "cp-col", uiOutput("futuresBox2")),
          column(4, class = "cp-col", uiOutput("oiBox2")),
          column(4, class = "cp-col", uiOutput("lsBox"))
        ),
        fluidRow(
          cp_panel("Open interest", plotlyOutput("oiPlot", height = "380px"), width = 7),
          cp_panel("Market snapshot", uiOutput("derivSummary"), width = 5)
        )
      ),
      tabPanel(
        "liquidations",
        fluidRow(
          cp_panel("Estimated liquidation heatmap", plotlyOutput("liqHeatmap", height = "460px"), width = 8),
          cp_panel("Key liquidation lines", uiOutput("liqTable"), width = 4)
        )
      )
    )
  )
)

server <- function(input, output, session) {
  last_refresh <- reactiveVal(Sys.time())

  observeEvent(input$nav_tab, {
    updateTabsetPanel(session, "tabs", selected = input$nav_tab)
  }, ignoreInit = TRUE)

  observeEvent(input$navSearch, {
    match <- resolve_asset_query(input$navSearch)
    if (!is.null(match)) {
      updateRadioButtons(session, "cryptoSelect", selected = match)
    }
  }, ignoreInit = TRUE)

  observe({
    req(isTRUE(input$autoRefresh))
    interval <- as.numeric(input$refreshSecs)
    if (!is.finite(interval) || interval <= 0) {
      interval <- 30
    }
    invalidateLater(interval * 1000, session)
    isolate({
      refresh_market_data(full = interval >= 60)
      last_refresh(Sys.time())
    })
  })

  observeEvent(input$refreshNow, {
    refresh_market_data(full = TRUE)
    last_refresh(Sys.time())
  })

  clock_tick <- reactive({
    invalidateLater(1000, session)
    Sys.time()
  })

  output$refreshCountdown <- renderUI({
    now <- clock_tick()
    if (!isTRUE(input$autoRefresh)) {
      return(tags$span(class = "cp-chip", "Paused"))
    }
    interval <- as.numeric(null_coalesce(input$refreshSecs, 30))
    elapsed <- as.numeric(difftime(now, last_refresh(), units = "secs"))
    remain <- max(0, round(interval - elapsed))
    tags$span(class = "cp-chip live", paste0("Next ", remain, "s"))
  })

  output$dateSliderUI <- renderUI({
    df <- daily_full()
    req(!is.null(df), nrow(df) > 0)
    dateRangeInput(
      "dateRange",
      "Custom range",
      start = max(as.Date(df$Date)) - 365,
      end = max(as.Date(df$Date)),
      min = min(as.Date(df$Date)),
      max = max(Sys.Date(), max(as.Date(df$Date)))
    )
  })

  snapshot <- reactive({
    last_refresh()
    get_market_snapshot(input$cryptoSelect)
  })

  daily_full <- reactive({
    last_refresh()
    df <- get_daily_ohlcv(input$cryptoSelect)
    req(!is.null(df), nrow(df) > 0)
    add_moving_averages(df)
  })

  visible_data <- reactive({
    slice_by_timeframe(
      daily_full(),
      input$cryptoSelect,
      input$timeframe,
      input$dateRange
    )
  })

  futures_series <- reactive({
    spec <- timeframe_spec(input$timeframe)
    interval <- if (spec$mode == "intraday") spec$interval else "1d"
    limit <- if (spec$mode == "intraday") spec$limit else if (is.finite(null_coalesce(spec$days, 365))) {
      max(spec$days, 30)
    } else {
      1000
    }
    get_futures_volume_series(input$cryptoSelect, interval = interval, limit = limit)
  })

  oi_series <- reactive({
    spec <- timeframe_spec(input$timeframe)
    get_open_interest_series(input$cryptoSelect, period = spec$oi_period, limit = spec$oi_limit)
  })

  ma_state <- reactive({
    df <- daily_full()
    last <- utils::tail(df[!is.na(df$ma200), ], 1)
    if (nrow(last) == 0) {
      return(list(ma200 = NA, bias = NA, price = utils::tail(df$Close, 1), regime = ma200_regime(NA)))
    }
    snap <- snapshot()
    price <- null_coalesce(snap$price, last$Close)
    bias <- if (is.finite(last$ma200) && last$ma200 != 0) (price - last$ma200) / last$ma200 * 100 else NA
    list(ma200 = last$ma200, bias = bias, price = price, regime = ma200_regime(bias))
  })

  liq_model <- reactive({
    snap <- snapshot()
    estimate_liquidation_map(
      daily_full(),
      current_price = null_coalesce(snap$price, utils::tail(daily_full()$Close, 1)),
      open_interest_usd = snap$open_interest_usd,
      long_ratio = snap$long_ratio
    )
  })

  output$statusChips <- renderUI({
    snap <- snapshot()
    ma <- ma_state()
    live_class <- if (isTRUE(snap$live)) "live" else "offline"
    live_label <- if (isTRUE(snap$live)) "Live" else "Offline"
    tags$div(
      class = "cp-status",
      tags$span(class = paste("cp-chip", live_class), live_label),
      tags$span(class = "cp-chip", format(snap$as_of, "%H:%M")),
      tags$span(class = paste("cp-chip", ma$regime$tone), ma$regime$label),
      tags$span(class = "cp-chip", paste("Bias", fmt_pct(ma$bias)))
    )
  })

  output$priceBox <- renderUI({
    metric_ui(fmt_price(snapshot()$price), "Price", input$cryptoSelect)
  })

  output$changeBox <- renderUI({
    pct <- snapshot()$spot_change_pct
    metric_ui(fmt_pct(pct), "24-hour change", "Spot", change_tone(pct))
  })

  output$maBox <- renderUI({
    ma <- ma_state()
    metric_ui(fmt_price(ma$ma200), "MA200", ma$regime$label, regime_tone(ma$regime$tone))
  })

  render_spot <- function() {
    metric_ui(
      compact_number(snapshot()$spot_volume_quote, prefix = "$"),
      "Spot volume",
      "Binance 24h"
    )
  }
  output$spotBox <- renderUI(render_spot())
  output$spotBox2 <- renderUI(render_spot())

  render_fut <- function() {
    metric_ui(
      compact_number(snapshot()$futures_volume_quote, prefix = "$"),
      "Futures volume",
      "Binance 24h"
    )
  }
  output$futuresBox <- renderUI(render_fut())
  output$futuresBox2 <- renderUI(render_fut())

  render_oi <- function() {
    metric_ui(
      compact_number(snapshot()$open_interest_usd, prefix = "$"),
      "Open interest",
      "USDT-M"
    )
  }
  output$oiBox <- renderUI(render_oi())
  output$oiBox2 <- renderUI(render_oi())

  output$globalVolBox <- renderUI({
    metric_ui(
      compact_number(snapshot()$global_volume_usd, prefix = "$"),
      "Global volume",
      "CoinGecko 24h"
    )
  })

  output$ratioBox <- renderUI({
    snap <- snapshot()
    ratio <- if (is.finite(snap$spot_volume_quote) && snap$spot_volume_quote > 0) {
      snap$futures_volume_quote / snap$spot_volume_quote
    } else {
      NA
    }
    metric_ui(
      if (is.finite(ratio)) paste0(formatC(ratio, format = "f", digits = 2), "x") else "N/A",
      "Futures / spot",
      "24h notional"
    )
  })

  output$lsBox <- renderUI({
    snap <- snapshot()
    hint <- if (is.finite(snap$long_short_ratio)) {
      paste0("L/S ", formatC(snap$long_short_ratio, format = "f", digits = 2), "x")
    } else {
      "Account mix"
    }
    metric_ui(
      if (is.finite(snap$long_ratio)) fmt_pct(snap$long_ratio * 100, signed = FALSE) else "N/A",
      "Long accounts",
      hint
    )
  })

  output$pricePlot <- renderPlotly({
    data <- visible_data()
    req(nrow(data) > 0)
    full <- daily_full()
    ma <- ma_state()
    liq <- liq_model()
    is_intra <- inherits(data$Date, "POSIXct")

    plot_df <- data
    if (is_intra) {
      plot_df$ma200 <- ma$ma200
    } else if (!"ma200" %in% names(plot_df)) {
      plot_df <- merge(
        plot_df,
        full[, c("Date", "ma200", "ma200_bias_pct")],
        by = "Date",
        all.x = TRUE
      )
    }

    hover <- paste0(
      format(plot_df$Date),
      "<br>Close: ", fmt_price(plot_df$Close),
      if (!is_intra) paste0("<br>MA200: ", fmt_price(plot_df$ma200)) else ""
    )

    if (input$chartType == "candle") {
      p <- plot_ly(
        plot_df,
        x = ~Date,
        type = "candlestick",
        open = ~Open,
        high = ~High,
        low = ~Low,
        close = ~Close,
        name = input$cryptoSelect,
        increasing = list(line = list(color = "#34C759"), fillcolor = "#34C759"),
        decreasing = list(line = list(color = "#FF3B30"), fillcolor = "#FF3B30")
      )
    } else {
      p <- plot_ly(
        plot_df,
        x = ~Date,
        y = ~Close,
        type = "scatter",
        mode = "lines",
        name = "Close",
        line = list(color = "#007AFF", width = 2, shape = "spline"),
        text = hover,
        hoverinfo = "text"
      )
    }

    p <- p %>%
      add_lines(
        data = plot_df,
        x = ~Date,
        y = ~ma200,
        name = "MA200",
        line = list(color = "#86868B", width = 1.6),
        inherit = FALSE
      )

    shapes <- list()
    if (!is.null(liq)) {
      y_min <- min(plot_df$Low, na.rm = TRUE)
      y_max <- max(plot_df$High, na.rm = TRUE)
      visible_levels <- liq$key_levels[
        liq$key_levels$long_liq >= y_min * 0.98 & liq$key_levels$short_liq <= y_max * 1.02,
      ]
      if (nrow(visible_levels) == 0) {
        visible_levels <- utils::tail(liq$key_levels, 2)
      }
      for (i in seq_len(nrow(visible_levels))) {
        row <- visible_levels[i, ]
        shapes <- c(
          shapes,
          list(
            list(type = "line", x0 = 0, x1 = 1, xref = "paper",
                 y0 = row$long_liq, y1 = row$long_liq,
                 line = list(color = "rgba(255,59,48,0.28)", dash = "dot", width = 1)),
            list(type = "line", x0 = 0, x1 = 1, xref = "paper",
                 y0 = row$short_liq, y1 = row$short_liq,
                 line = list(color = "rgba(0,122,255,0.28)", dash = "dot", width = 1))
          )
        )
      }
    }

    y_vals <- c(plot_df$Low, plot_df$High, plot_df$ma200)
    y_min <- min(y_vals, na.rm = TRUE)
    y_max <- max(y_vals, na.rm = TRUE)
    pad <- max((y_max - y_min) * 0.05, abs(y_max) * 0.002)

    p %>%
      cp_layout(y_title = "USDT") %>%
      layout(
        xaxis = modifyList(apple_axis(), list(rangeslider = list(visible = FALSE))),
        yaxis = apple_axis("USDT", show_grid = TRUE, range = c(y_min - pad, y_max + pad)),
        shapes = shapes,
        annotations = list(list(
          x = 0,
          y = 1.04,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          align = "left",
          font = list(size = 12, color = "#86868B", family = cp_font),
          text = paste0(ma$regime$label, "  ·  ", fmt_pct(ma$bias), " vs MA200")
        ))
      )
  })

  output$spotVolumePlot <- renderPlotly({
    data <- visible_data()
    req(nrow(data) > 0)
    plot_ly(
      data,
      x = ~Date,
      y = ~Volume,
      type = "bar",
      name = "Spot",
      marker = list(color = "rgba(0,122,255,0.35)", line = list(width = 0))
    ) %>%
      cp_layout(y_title = "USDT")
  })

  output$volumeComparePlot <- renderPlotly({
    spot <- visible_data()
    fut <- futures_series()
    req(nrow(spot) > 0)

    if (is.null(fut) || nrow(fut) == 0) {
      cmp <- data.frame(Date = spot$Date, spot_volume = spot$Volume, futures_volume = NA_real_)
    } else if (inherits(spot$Date, "POSIXct") && inherits(fut$Date, "POSIXct")) {
      cmp <- data.frame(Date = spot$Date, spot_volume = spot$Volume)
      fut_aligned <- fut
      names(fut_aligned)[names(fut_aligned) == "Volume"] <- "futures_volume"
      cmp <- merge(cmp, fut_aligned, by = "Date", all.x = TRUE)
    } else {
      cmp <- spot_vs_futures(spot, fut)
    }
    req(!is.null(cmp), nrow(cmp) > 0)

    plot_ly(cmp, x = ~Date) %>%
      add_bars(
        y = ~spot_volume,
        name = "Spot",
        marker = list(color = "rgba(0,122,255,0.72)", line = list(width = 0))
      ) %>%
      add_bars(
        y = ~futures_volume,
        name = "Futures",
        marker = list(color = "rgba(88,86,214,0.45)", line = list(width = 0))
      ) %>%
      cp_layout(y_title = "USDT") %>%
      layout(barmode = "group")
  })

  output$oiPlot <- renderPlotly({
    oi <- oi_series()
    req(!is.null(oi), nrow(oi) > 0)
    plot_ly(
      oi,
      x = ~Date,
      y = ~sumOpenInterestValue,
      type = "scatter",
      mode = "lines",
      name = "Open interest",
      line = list(color = "#007AFF", width = 2, shape = "spline"),
      fill = "tozeroy",
      fillcolor = "rgba(0,122,255,0.08)"
    ) %>%
      cp_layout(y_title = "USDT")
  })

  output$derivSummary <- renderUI({
    snap <- snapshot()
    ma <- ma_state()
    ratio <- if (is.finite(snap$spot_volume_quote) && snap$spot_volume_quote > 0) {
      snap$futures_volume_quote / snap$spot_volume_quote
    } else {
      NA
    }
    tags$div(
      style = "padding: 8px 12px 16px;",
      tags$p(class = "cp-note", "Binance USDT-M perpetual as the derivatives proxy."),
      tags$table(
        class = "cp-level-table",
        tags$tr(tags$th("Metric"), tags$th("Value")),
        tags$tr(tags$td("Spot 24h"), tags$td(compact_number(snap$spot_volume_quote, prefix = "$"))),
        tags$tr(tags$td("Futures 24h"), tags$td(compact_number(snap$futures_volume_quote, prefix = "$"))),
        tags$tr(tags$td("Futures / spot"), tags$td(if (is.finite(ratio)) paste0(formatC(ratio, format = "f", digits = 2), "x") else "N/A")),
        tags$tr(tags$td("Open interest"), tags$td(compact_number(snap$open_interest_usd, prefix = "$"))),
        tags$tr(tags$td("OI contracts"), tags$td(compact_number(snap$open_interest_contracts, digits = 0))),
        tags$tr(tags$td("Long accounts"), tags$td(fmt_pct(snap$long_ratio * 100, signed = FALSE))),
        tags$tr(tags$td("Short accounts"), tags$td(fmt_pct(snap$short_ratio * 100, signed = FALSE))),
        tags$tr(tags$td("MA200"), tags$td(fmt_price(ma$ma200))),
        tags$tr(tags$td("MA200 bias"), tags$td(fmt_pct(ma$bias)))
      )
    )
  })

  output$liqHeatmap <- renderPlotly({
    liq <- liq_model()
    req(!is.null(liq))
    heat <- liq$heatmap
    heat$long_label <- compact_number(heat$Long, prefix = "$")
    heat$short_label <- compact_number(heat$Short, prefix = "$")
    plot_ly(heat, y = ~price_level) %>%
      add_bars(
        x = ~-Long,
        name = "Long",
        orientation = "h",
        marker = list(color = "rgba(255,59,48,0.62)", line = list(width = 0)),
        hovertemplate = "Long @ %{y:.2f}<br>%{customdata}<extra></extra>",
        customdata = ~long_label
      ) %>%
      add_bars(
        x = ~Short,
        name = "Short",
        orientation = "h",
        marker = list(color = "rgba(0,122,255,0.62)", line = list(width = 0)),
        hovertemplate = "Short @ %{y:.2f}<br>%{customdata}<extra></extra>",
        customdata = ~short_label
      ) %>%
      cp_layout(y_title = "Price", x_title = "Estimated notional") %>%
      layout(
        barmode = "overlay",
        annotations = list(list(
          x = 0,
          y = liq$current_price,
          xref = "x",
          yref = "y",
          text = paste("Mark", fmt_price(liq$current_price)),
          showarrow = FALSE,
          font = list(size = 12, color = "#1D1D1F", family = cp_font),
          xanchor = "left"
        )),
        shapes = list(list(
          type = "line",
          x0 = 0,
          x1 = 1,
          xref = "paper",
          y0 = liq$current_price,
          y1 = liq$current_price,
          line = list(color = "rgba(29,29,31,0.35)", dash = "dot", width = 1)
        ))
      )
  })

  output$liqTable <- renderUI({
    liq <- liq_model()
    req(!is.null(liq))
    rows <- lapply(seq_len(nrow(liq$key_levels)), function(i) {
      row <- liq$key_levels[i, ]
      tags$tr(
        tags$td(row$leverage),
        tags$td(class = "long", fmt_price(row$long_liq)),
        tags$td(class = "short", fmt_price(row$short_liq))
      )
    })
    tags$div(
      style = "padding: 8px 12px 16px;",
      tags$p(class = "cp-note", liq$model),
      tags$table(
        class = "cp-level-table",
        tags$tr(tags$th("Lev"), tags$th("Long"), tags$th("Short")),
        rows
      )
    )
  })
}

shinyApp(ui = ui, server = server)
