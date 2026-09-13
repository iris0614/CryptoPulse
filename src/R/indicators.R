# Derived market structure: MA200 regime and estimated liquidation map.

add_moving_averages <- function(df, window = 200) {
  df <- df[order(df$Date), ]
  closes <- as.numeric(df$Close)
  df$ma200 <- as.numeric(stats::filter(closes, rep(1 / window, window), sides = 1))
  df$ma200_bias_pct <- ifelse(
    is.finite(df$ma200) & df$ma200 != 0,
    (df$Close - df$ma200) / df$ma200 * 100,
    NA_real_
  )
  df
}

ma200_regime <- function(bias_pct) {
  if (is.null(bias_pct) || length(bias_pct) == 0 || is.na(bias_pct) || !is.finite(bias_pct)) {
    return(list(
      label = "Insufficient history",
      detail = "Need 200 daily closes to compute MA200.",
      tone = "muted",
      color = "navy"
    ))
  }
  if (bias_pct >= 25) {
    list(label = "Extended bull", detail = "Price is stretched far above MA200.", tone = "hot", color = "yellow")
  } else if (bias_pct >= 0) {
    list(label = "Bull regime", detail = "Price holds above the 200-day average.", tone = "bull", color = "green")
  } else if (bias_pct > -25) {
    list(label = "Bear regime", detail = "Price is trading below MA200.", tone = "bear", color = "red")
  } else {
    list(label = "Deep discount", detail = "Price is stretched far below MA200.", tone = "cold", color = "maroon")
  }
}

# Approximate isolated-margin liquidation using common leverage buckets.
# Intensity is scaled by open interest and a volume-weighted entry distribution.
estimate_liquidation_map <- function(ohlcv, current_price, open_interest_usd,
                                     long_ratio = 0.5, lookback = 30) {
  if (is.null(ohlcv) || nrow(ohlcv) == 0 || !is.finite(current_price) || current_price <= 0) {
    return(NULL)
  }

  oi <- if (is.finite(open_interest_usd) && open_interest_usd > 0) {
    open_interest_usd
  } else {
    mean(tail(ohlcv$Volume, 7), na.rm = TRUE)
  }
  if (!is.finite(oi) || oi <= 0) {
    return(NULL)
  }

  long_ratio <- if (is.finite(long_ratio)) min(max(long_ratio, 0.15), 0.85) else 0.5
  recent <- utils::tail(ohlcv, min(lookback, nrow(ohlcv)))
  entries <- c(as.numeric(recent$Close), current_price)
  weights <- c(as.numeric(recent$Volume), mean(as.numeric(recent$Volume), na.rm = TRUE))
  weights[!is.finite(weights)] <- 0
  if (sum(weights) <= 0) {
    weights <- rep(1, length(entries))
  }
  weights <- weights / sum(weights)

  lev <- data.frame(
    leverage = c(5, 10, 20, 25, 50, 75, 100),
    share = c(0.08, 0.20, 0.22, 0.15, 0.18, 0.07, 0.10)
  )
  mmr <- 0.004

  n <- length(entries) * nrow(lev) * 2
  side <- character(n)
  price <- numeric(n)
  notional <- numeric(n)
  leverage <- numeric(n)
  idx <- 1L
  for (i in seq_along(entries)) {
    entry <- entries[i]
    if (!is.finite(entry) || entry <= 0) {
      next
    }
    for (j in seq_len(nrow(lev))) {
      L <- lev$leverage[j]
      share <- lev$share[j] * weights[i] * oi
      long_liq <- entry * (1 - 1 / L + mmr)
      short_liq <- entry * (1 + 1 / L - mmr)
      side[idx] <- "Long"
      price[idx] <- long_liq
      notional[idx] <- share * long_ratio
      leverage[idx] <- L
      idx <- idx + 1L
      side[idx] <- "Short"
      price[idx] <- short_liq
      notional[idx] <- share * (1 - long_ratio)
      leverage[idx] <- L
      idx <- idx + 1L
    }
  }

  raw <- data.frame(
    side = side[seq_len(idx - 1L)],
    price = price[seq_len(idx - 1L)],
    notional = notional[seq_len(idx - 1L)],
    leverage = leverage[seq_len(idx - 1L)]
  )

  lower <- current_price * 0.82
  upper <- current_price * 1.18
  raw <- raw[is.finite(raw$price) & raw$price >= lower & raw$price <= upper, ]
  if (nrow(raw) == 0) {
    return(NULL)
  }

  breaks <- seq(lower, upper, length.out = 49)
  raw$bin <- cut(raw$price, breaks = breaks, include.lowest = TRUE)
  midpoints <- (head(breaks, -1) + tail(breaks, -1)) / 2

  agg <- raw %>%
    dplyr::group_by(bin, side) %>%
    dplyr::summarise(notional = sum(notional, na.rm = TRUE), .groups = "drop")

  levels <- data.frame(
    bin = levels(raw$bin),
    price_level = midpoints,
    stringsAsFactors = FALSE
  )
  heat <- merge(levels, agg, by = "bin", all.x = TRUE)
  heat$notional[is.na(heat$notional)] <- 0

  wide <- tidyr::pivot_wider(
    heat[, c("price_level", "side", "notional")],
    names_from = "side",
    values_from = "notional",
    values_fill = 0
  )
  if (!"Long" %in% names(wide)) wide$Long <- 0
  if (!"Short" %in% names(wide)) wide$Short <- 0
  wide <- wide[order(wide$price_level), ]

  key_levels <- lapply(c(10, 25, 50, 100), function(L) {
    data.frame(
      leverage = paste0(L, "x"),
      long_liq = current_price * (1 - 1 / L + mmr),
      short_liq = current_price * (1 + 1 / L - mmr)
    )
  })
  key_levels <- dplyr::bind_rows(key_levels)

  list(
    heatmap = wide,
    key_levels = key_levels,
    current_price = current_price,
    model = paste0(
      "Estimated from Binance OI, account long/short mix, recent volume-weighted entries, ",
      "and common leverage buckets (5x-100x). Not exchange-reported liquidation orders."
    )
  )
}

spot_vs_futures <- function(spot_df, futures_df) {
  if (is.null(spot_df) || is.null(futures_df) || nrow(spot_df) == 0 || nrow(futures_df) == 0) {
    return(NULL)
  }
  spot <- data.frame(Date = as.Date(spot_df$Date), spot_volume = spot_df$Volume)
  fut <- data.frame(Date = as.Date(futures_df$Date), futures_volume = futures_df$Volume)
  merged <- dplyr::inner_join(spot, fut, by = "Date")
  if (nrow(merged) == 0) {
    return(NULL)
  }
  merged$ratio <- ifelse(merged$spot_volume > 0, merged$futures_volume / merged$spot_volume, NA)
  merged
}
