# Shared helpers: HTTP cache, number formatting, and path resolution.

CACHE <- new.env(parent = emptyenv())

resolve_path <- function(..., fallback = NULL) {
  candidates <- list(...)
  if (!is.null(fallback)) {
    candidates <- c(candidates, list(fallback))
  }
  for (path in candidates) {
    if (!is.null(path) && (file.exists(path) || dir.exists(path))) {
      return(path)
    }
  }
  candidates[[1]]
}

data_dir <- function() {
  resolve_path(
    file.path("..", "data", "raw"),
    file.path("data", "raw"),
    file.path("..", "..", "data", "raw")
  )
}

r_dir <- function() {
  resolve_path("R", file.path("src", "R"))
}

cached <- function(key, fetch_fn, ttl_sec = 60, force = FALSE) {
  now <- as.numeric(Sys.time())
  hit <- CACHE[[key]]
  if (!force && !is.null(hit) && (now - hit$ts) < ttl_sec) {
    return(hit$data)
  }

  data <- tryCatch(fetch_fn(), error = function(e) NULL)
  if (!is.null(data)) {
    CACHE[[key]] <- list(ts = now, data = data)
    return(data)
  }
  if (!is.null(hit)) {
    return(hit$data)
  }
  NULL
}

invalidate_cache <- function(prefix = NULL) {
  keys <- ls(envir = CACHE)
  if (!is.null(prefix) && nzchar(prefix)) {
    keys <- keys[startsWith(keys, prefix)]
  }
  if (length(keys) > 0) {
    rm(list = keys, envir = CACHE)
  }
}

refresh_market_data <- function(full = FALSE) {
  invalidate_cache("snap_")
  invalidate_cache("intra_")
  invalidate_cache("fvol_")
  invalidate_cache("oi_")
  if (isTRUE(full)) {
    invalidate_cache("daily_")
  }
}

safe_get_json <- function(url, timeout_sec = 8) {
  tryCatch({
    resp <- httr::GET(
      url,
      httr::timeout(timeout_sec),
      httr::user_agent("CryptoPulse/2.0 (research dashboard)")
    )
    if (httr::http_error(resp)) {
      return(NULL)
    }
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    if (is.null(txt) || !nzchar(txt)) {
      return(NULL)
    }
    jsonlite::fromJSON(txt)
  }, error = function(e) NULL)
}

.format_one <- function(x, prefix = "", suffix = "", digits = 2) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !is.finite(x)) {
    return("N/A")
  }
  ax <- abs(x)
  scaled <- if (ax >= 1e12) {
    paste0(formatC(x / 1e12, format = "f", digits = digits), "T")
  } else if (ax >= 1e9) {
    paste0(formatC(x / 1e9, format = "f", digits = digits), "B")
  } else if (ax >= 1e6) {
    paste0(formatC(x / 1e6, format = "f", digits = digits), "M")
  } else if (ax >= 1e3) {
    paste0(formatC(x / 1e3, format = "f", digits = digits), "K")
  } else {
    formatC(x, format = "f", digits = digits, big.mark = ",")
  }
  paste0(prefix, scaled, suffix)
}

compact_number <- function(x, prefix = "", suffix = "", digits = 2) {
  vapply(x, .format_one, character(1), prefix = prefix, suffix = suffix, digits = digits)
}

fmt_price <- function(x) {
  vapply(x, function(val) {
    if (is.null(val) || length(val) == 0 || is.na(val) || !is.finite(val)) {
      return("N/A")
    }
    digits <- if (val >= 1000) 0 else if (val >= 1) 2 else 4
    paste0("$", formatC(val, format = "f", digits = digits, big.mark = ","))
  }, character(1))
}

fmt_pct <- function(x, digits = 2, signed = TRUE) {
  vapply(x, function(val) {
    if (is.null(val) || length(val) == 0 || is.na(val) || !is.finite(val)) {
      return("N/A")
    }
    sign <- if (signed && val > 0) "+" else ""
    paste0(sign, formatC(val, format = "f", digits = digits), "%")
  }, character(1))
}

null_coalesce <- function(x, default) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    default
  } else {
    x
  }
}
