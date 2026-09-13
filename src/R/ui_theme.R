# Apple-style UI helpers and a shared Plotly theme.

metric_ui <- function(value, label, hint = NULL, tone = "neutral") {
  tags$article(
    class = paste("cp-metric", paste0("is-", tone)),
    tags$p(class = "cp-kicker", label),
    tags$p(class = "cp-value", value),
    if (!is.null(hint) && nzchar(hint)) tags$p(class = "cp-hint", hint)
  )
}

cp_panel <- function(title, ..., width = 12, subtitle = NULL) {
  column(
    width = width,
    class = "cp-col",
    tags$section(
      class = "cp-card",
      tags$header(
        class = "cp-card-header",
        tags$div(
          tags$h2(title),
          if (!is.null(subtitle)) tags$p(class = "cp-card-subtitle", subtitle)
        )
      ),
      tags$div(class = "cp-card-body", ...)
    )
  )
}

cp_font <- "Inter, -apple-system, BlinkMacSystemFont, 'SF Pro Display', 'Helvetica Neue', sans-serif"

apple_axis <- function(title = "", show_grid = FALSE, range = NULL) {
  axis <- list(
    title = list(text = title, font = list(size = 11, color = "#86868B"), standoff = 8),
    showgrid = show_grid,
    gridcolor = "#F2F2F7",
    zeroline = FALSE,
    showline = TRUE,
    linecolor = "#E5E5EA",
    tickfont = list(color = "#86868B", size = 11),
    tickcolor = "#E5E5EA",
    automargin = TRUE
  )
  if (!is.null(range)) {
    axis$range <- range
  }
  axis
}

cp_layout <- function(p, y_title = "", x_title = "", ...) {
  p %>%
    plotly::layout(
      title = NULL,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(family = cp_font, color = "#1D1D1F", size = 12),
      xaxis = apple_axis(x_title, show_grid = FALSE),
      yaxis = apple_axis(y_title, show_grid = TRUE),
      legend = list(
        orientation = "h",
        y = -0.18,
        x = 0,
        font = list(size = 12, color = "#86868B"),
        bgcolor = "rgba(0,0,0,0)"
      ),
      margin = list(t = 28, r = 16, b = 56, l = 58),
      hovermode = "x unified",
      hoverlabel = list(
        bgcolor = "#1D1D1F",
        font = list(color = "#F5F5F7", family = cp_font, size = 12),
        bordercolor = "rgba(0,0,0,0)"
      ),
      ...
    )
}

change_tone <- function(pct) {
  if (!is.finite(pct)) {
    "neutral"
  } else if (pct > 0) {
    "up"
  } else if (pct < 0) {
    "down"
  } else {
    "neutral"
  }
}

regime_tone <- function(tone) {
  switch(tone, bull = "up", bear = "down", hot = "warn", cold = "neutral", "neutral")
}
