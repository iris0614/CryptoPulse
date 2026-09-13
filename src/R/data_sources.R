# Market data: local CSVs plus live Binance / CoinGecko public endpoints.

ASSET_CHOICES <- c(
  "BTC" = "Bitcoin",
  "ETH" = "Ethereum",
  "SOL" = "Solana",
  "BNB" = "BNB"
)

ASSET_MAP <- list(
  Bitcoin = list(
    symbol = "BTCUSDT",
    local_file = "BTC.csv",
    gecko_id = "bitcoin"
  ),
  Ethereum = list(
    symbol = "ETHUSDT",
    local_file = "ETH.csv",
    gecko_id = "ethereum"
  ),
  Solana = list(
    symbol = "SOLUSDT",
    local_file = NULL,
    gecko_id = "solana"
  ),
  BNB = list(
    symbol = "BNBUSDT",
    local_file = NULL,
    gecko_id = "binancecoin"
  )
)

resolve_asset_query <- function(query) {
  q <- tolower(trimws(null_coalesce(query, "")))
  if (!nzchar(q)) {
    return(NULL)
  }
  if (grepl("sol", q)) {
    "Solana"
  } else if (grepl("bnb|binance", q)) {
    "BNB"
  } else if (grepl("eth", q)) {
    "Ethereum"
  } else if (grepl("btc|bit", q)) {
    "Bitcoin"
  } else {
    NULL
  }
}

load_local_ohlcv <- function(asset) {
  meta <- ASSET_MAP[[asset]]
  if (is.null(meta) || is.null(meta$local_file) || !nzchar(meta$local_file)) {
    return(NULL)
  }
  path <- file.path(data_dir(), meta$local_file)
  if (!file.exists(path)) {
    return(NULL)
  }
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  keep <- c("Date", "Open", "High", "Low", "Close", "Volume")
  df <- df[, keep]
  df$source <- "local"
  df
}

parse_klines <- function(raw, as_date = TRUE) {
  if (is.null(raw) || length(raw) == 0) {
    return(NULL)
  }
  df <- as.data.frame(raw, stringsAsFactors = FALSE)
  if (ncol(df) < 8) {
    return(NULL)
  }
  names(df)[1:8] <- c(
    "open_time", "Open", "High", "Low", "Close",
    "BaseVolume", "close_time", "QuoteVolume"
  )
  num_cols <- c("Open", "High", "Low", "Close", "BaseVolume", "QuoteVolume")
  df[num_cols] <- lapply(df[num_cols], function(x) as.numeric(as.character(x)))
  ts <- as.POSIXct(as.numeric(df$open_time) / 1000, origin = "1970-01-01", tz = "UTC")
  if (as_date) {
    df$Date <- as.Date(ts, tz = "UTC")
  } else {
    df$Date <- ts
  }
  df$Volume <- df$QuoteVolume
  df[, c("Date", "Open", "High", "Low", "Close", "Volume")]
}

fetch_binance_klines <- function(symbol, interval = "1d", limit = 1000, as_date = TRUE) {
  url <- sprintf(
    "https://api.binance.com/api/v3/klines?symbol=%s&interval=%s&limit=%s",
    symbol, interval, limit
  )
  raw <- safe_get_json(url)
  parse_klines(raw, as_date = as_date)
}

fetch_binance_futures_klines <- function(symbol, interval = "1d", limit = 365) {
  url <- sprintf(
    "https://fapi.binance.com/fapi/v1/klines?symbol=%s&interval=%s&limit=%s",
    symbol, interval, limit
  )
  raw <- safe_get_json(url)
  parse_klines(raw, as_date = interval %in% c("1d", "3d", "1w"))
}

fetch_spot_ticker <- function(symbol) {
  url <- paste0("https://api.binance.com/api/v3/ticker/24hr?symbol=", symbol)
  safe_get_json(url)
}

fetch_futures_ticker <- function(symbol) {
  url <- paste0("https://fapi.binance.com/fapi/v1/ticker/24hr?symbol=", symbol)
  safe_get_json(url)
}

fetch_open_interest <- function(symbol) {
  url <- paste0("https://fapi.binance.com/fapi/v1/openInterest?symbol=", symbol)
  safe_get_json(url)
}

fetch_open_interest_hist <- function(symbol, period = "1d", limit = 30) {
  url <- sprintf(
    "https://fapi.binance.com/futures/data/openInterestHist?symbol=%s&period=%s&limit=%s",
    symbol, period, limit
  )
  raw <- safe_get_json(url)
  if (is.null(raw) || NROW(raw) == 0) {
    return(NULL)
  }
  df <- as.data.frame(raw, stringsAsFactors = FALSE)
  df$sumOpenInterest <- as.numeric(df$sumOpenInterest)
  df$sumOpenInterestValue <- as.numeric(df$sumOpenInterestValue)
  df$Date <- as.POSIXct(as.numeric(df$timestamp) / 1000, origin = "1970-01-01", tz = "UTC")
  df
}

fetch_long_short_ratio <- function(symbol, period = "1d", limit = 1) {
  url <- sprintf(
    "https://fapi.binance.com/futures/data/globalLongShortAccountRatio?symbol=%s&period=%s&limit=%s",
    symbol, period, limit
  )
  raw <- safe_get_json(url)
  if (is.null(raw) || NROW(raw) == 0) {
    return(NULL)
  }
  df <- as.data.frame(raw, stringsAsFactors = FALSE)
  df$longAccount <- as.numeric(df$longAccount)
  df$shortAccount <- as.numeric(df$shortAccount)
  df$longShortRatio <- as.numeric(df$longShortRatio)
  df
}

fetch_coingecko_snapshot <- function(gecko_id) {
  url <- paste0(
    "https://api.coingecko.com/api/v3/simple/price?ids=", gecko_id,
    "&vs_currencies=usd&include_24hr_vol=true&include_24hr_change=true"
  )
  safe_get_json(url, timeout_sec = 6)
}

merge_local_and_live <- function(local_df, live_df) {
  if (is.null(live_df) || nrow(live_df) == 0) {
    if (is.null(local_df) || nrow(local_df) == 0) {
      return(NULL)
    }
    local_df$source <- "local"
    return(local_df)
  }
  live_df$source <- "binance"
  live_df$Date <- as.Date(live_df$Date)
  if (is.null(local_df) || nrow(local_df) == 0) {
    return(live_df)
  }
  historical <- local_df[!local_df$Date %in% live_df$Date, ]
  combined <- dplyr::bind_rows(historical, live_df)
  combined <- combined[order(combined$Date), ]
  combined
}

enrich_ohlcv <- function(df) {
  df <- df[order(df$Date), ]
  df$price_change_per_day <- df$Close - df$Open
  df$price_change_ratio_per_day <- ifelse(df$Open == 0, NA, (df$Close - df$Open) / df$Open)
  df
}

get_daily_ohlcv <- function(asset) {
  meta <- ASSET_MAP[[asset]]
  cached(paste0("daily_", asset), function() {
    local_df <- load_local_ohlcv(asset)
    live_df <- fetch_binance_klines(meta$symbol, interval = "1d", limit = 1000, as_date = TRUE)
    enrich_ohlcv(merge_local_and_live(local_df, live_df))
  }, ttl_sec = 300)
}

get_intraday_ohlcv <- function(asset, interval = "1h", limit = 168) {
  meta <- ASSET_MAP[[asset]]
  cached(paste0("intra_", asset, "_", interval, "_", limit), function() {
    df <- fetch_binance_klines(meta$symbol, interval = interval, limit = limit, as_date = FALSE)
    if (is.null(df)) {
      return(NULL)
    }
    enrich_ohlcv(df)
  }, ttl_sec = 60)
}

get_futures_volume_series <- function(asset, interval = "1d", limit = 365) {
  meta <- ASSET_MAP[[asset]]
  cached(paste0("fvol_", asset, "_", interval, "_", limit), function() {
    df <- fetch_binance_futures_klines(meta$symbol, interval = interval, limit = limit)
    if (is.null(df)) {
      return(NULL)
    }
    df[, c("Date", "Volume")]
  }, ttl_sec = 180)
}

get_open_interest_series <- function(asset, period = "1d", limit = 30) {
  meta <- ASSET_MAP[[asset]]
  cached(paste0("oi_", asset, "_", period, "_", limit), function() {
    fetch_open_interest_hist(meta$symbol, period = period, limit = limit)
  }, ttl_sec = 180)
}

get_market_snapshot <- function(asset) {
  meta <- ASSET_MAP[[asset]]
  cached(paste0("snap_", asset), function() {
    spot <- fetch_spot_ticker(meta$symbol)
    fut <- fetch_futures_ticker(meta$symbol)
    oi <- fetch_open_interest(meta$symbol)
    ls <- fetch_long_short_ratio(meta$symbol)
    gecko <- fetch_coingecko_snapshot(meta$gecko_id)

    last_price <- as.numeric(null_coalesce(spot$lastPrice, NA))
    oi_contracts <- as.numeric(null_coalesce(oi$openInterest, NA))
    oi_usd <- if (is.finite(last_price) && is.finite(oi_contracts)) {
      oi_contracts * last_price
    } else {
      NA_real_
    }

    gecko_row <- if (!is.null(gecko) && !is.null(gecko[[meta$gecko_id]])) {
      gecko[[meta$gecko_id]]
    } else {
      NULL
    }

    list(
      asset = asset,
      symbol = meta$symbol,
      price = last_price,
      spot_change_pct = as.numeric(null_coalesce(spot$priceChangePercent, NA)),
      spot_high = as.numeric(null_coalesce(spot$highPrice, NA)),
      spot_low = as.numeric(null_coalesce(spot$lowPrice, NA)),
      spot_volume_quote = as.numeric(null_coalesce(spot$quoteVolume, NA)),
      spot_volume_base = as.numeric(null_coalesce(spot$volume, NA)),
      futures_volume_quote = as.numeric(null_coalesce(fut$quoteVolume, NA)),
      futures_change_pct = as.numeric(null_coalesce(fut$priceChangePercent, NA)),
      open_interest_contracts = oi_contracts,
      open_interest_usd = oi_usd,
      long_ratio = if (!is.null(ls) && nrow(ls) > 0) ls$longAccount[nrow(ls)] else NA_real_,
      short_ratio = if (!is.null(ls) && nrow(ls) > 0) ls$shortAccount[nrow(ls)] else NA_real_,
      long_short_ratio = if (!is.null(ls) && nrow(ls) > 0) ls$longShortRatio[nrow(ls)] else NA_real_,
      global_volume_usd = if (!is.null(gecko_row)) as.numeric(gecko_row$usd_24h_vol) else NA_real_,
      live = !is.null(spot),
      as_of = Sys.time()
    )
  }, ttl_sec = 30)
}

timeframe_spec <- function(timeframe) {
  switch(
    timeframe,
    "24H" = list(mode = "intraday", interval = "1h", limit = 24, oi_period = "1h", oi_limit = 24),
    "7D" = list(mode = "intraday", interval = "1h", limit = 168, oi_period = "4h", oi_limit = 42),
    "30D" = list(mode = "daily", days = 30, oi_period = "1d", oi_limit = 30),
    "90D" = list(mode = "daily", days = 90, oi_period = "1d", oi_limit = 90),
    "1Y" = list(mode = "daily", days = 365, oi_period = "1d", oi_limit = 365),
    "ALL" = list(mode = "daily", days = Inf, oi_period = "1d", oi_limit = 500),
    list(mode = "custom", days = NA_real_, oi_period = "1d", oi_limit = 180)
  )
}

slice_by_timeframe <- function(daily_df, asset, timeframe, custom_range = NULL) {
  spec <- timeframe_spec(timeframe)
  if (identical(spec$mode, "intraday")) {
    intra <- get_intraday_ohlcv(asset, interval = spec$interval, limit = spec$limit)
    if (!is.null(intra) && nrow(intra) > 0) {
      return(intra)
    }
  }

  df <- daily_df
  if (identical(spec$mode, "custom") && !is.null(custom_range) && length(custom_range) == 2 &&
      !any(is.na(custom_range))) {
    return(df[df$Date >= as.Date(custom_range[1]) & df$Date <= as.Date(custom_range[2]), ])
  }
  if (!is.null(spec$days) && length(spec$days) == 1 && is.finite(spec$days)) {
    cutoff <- max(as.Date(df$Date)) - spec$days + 1
    df <- df[as.Date(df$Date) >= cutoff, ]
  }
  df
}
