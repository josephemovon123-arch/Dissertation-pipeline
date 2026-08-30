# =============================================================================
#
#   MSc FINANCIAL ECONOMETRICS DISSERTATION - COMPLETE PIPELINE
#   "Volatility and Pricing Dynamics of Cryptocurrencies", 2020-2026
#
#   ONE FILE. Open it in RStudio, do the two things in "BEFORE YOU RUN" below,
#   then press Ctrl+Shift+S (Source). It does everything.
#
# -----------------------------------------------------------------------------
#   BEFORE YOU RUN
# -----------------------------------------------------------------------------
#   1. Set the working directory:  Session > Set Working Directory >
#      Choose Directory. Pick an empty folder. Everything gets written there.
#
#   2. Paste your free CoinGecko Demo API key on the COINGECKO_API_KEY line
#      in PART 0 below. (Get one at coingecko.com - it takes two minutes.
#      The script still runs without it, but skips the reconciliation.)
#
#   Missing packages install themselves. First run takes 2-5 minutes.
#
# -----------------------------------------------------------------------------
#   WHAT IT PRODUCES
# -----------------------------------------------------------------------------
#   data/raw/     frozen source files - never edit these, they are your audit trail
#   data/clean/   panel_a_7day.csv, panel_b_aligned.csv, reconciliation_report.txt,
#                 data_provenance_log.txt, missing_days_log.csv
#   figures/      Figures 2, 2b, 3, 3b, 4, 5a, 5b as PNG (Word) and PDF (print)
#                 - they also appear in the Plots pane as the script runs
#   tables/       Tables 1-5 as CSV, plus regression_report.txt
#
# -----------------------------------------------------------------------------
#   THE ONE THING YOU MUST KNOW
# -----------------------------------------------------------------------------
#   CoinGecko's free Demo tier only serves the PAST 365 DAYS of history. It
#   CANNOT give you 2020-2026. That is a hard limit of the free plan, not a bug
#   in this code.
#
#   So the script pulls what CoinGecko will give, records exactly which window
#   was refused, and builds the panels from Yahoo Finance instead - keeping
#   CoinGecko as an INDEPENDENT CHECK on Yahoo's prices over the overlapping
#   year. That check (data/clean/reconciliation_report.txt) is your data-validity
#   evidence. Report the constraint in your methodology; do not hide it.
#
# =============================================================================


# =============================================================================
# PART 0 - SETUP AND SETTINGS
# =============================================================================

Sys.setenv(TZ = "UTC")   # keep every timestamp on one clock

# --- Install anything missing, then load ---
need <- c("httr", "jsonlite", "quantmod", "ggplot2", "scales")
miss <- need[!need %in% rownames(installed.packages())]
if (length(miss)) {
  message("Installing: ", paste(miss, collapse = ", "))
  install.packages(miss, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(quantmod)
  library(ggplot2); library(scales)
})

# --- YOUR API KEY GOES HERE --------------------------------------------------
COINGECKO_API_KEY <- "PASTE_YOUR_OWN_COINGECKO_DEMO_KEY_HERE"
COINGECKO_PLAN    <- "demo"      # "demo" (free) or "pro" (paid: Analyst+)

# --- Which parts to run ------------------------------------------------------
# After the first successful run you can set RUN_COLLECTION <- FALSE to redraw
# figures and re-estimate without hitting the APIs again.
RUN_COLLECTION  <- FALSE
RUN_FIGURES     <- TRUE
RUN_REGRESSIONS <- TRUE

# --- Sample window -----------------------------------------------------------
ANALYSIS_START <- as.Date("2020-01-01")   # first in-sample date
BUFFER_START   <- as.Date("2019-11-01")   # pulled early so the 30d/90d rolling
                                          # windows are full on 2020-01-01
END_DATE       <- as.Date(Sys.time(), tz = "UTC")
DROP_INCOMPLETE_LAST_DAY <- TRUE          # today's bar is a live, partial quote

# --- Assets ------------------------------------------------------------------
CG_COINS   <- c(bitcoin = "btc", ethereum = "eth")
YF_TICKERS <- c("BTC-USD" = "btc", "ETH-USD" = "eth",
                "^GSPC" = "spx", "^IXIC" = "ndx", "^VIX" = "vix")

PANEL_CRYPTO_SOURCE <- "auto"    # "auto" | "coingecko" | "yahoo"
COVERAGE_THRESHOLD  <- 0.95

# CoinGecko's daily point is a 00:00 UTC snapshot, so the one stamped 00:00 on
# day D+1 is really the CLOSE of day D. Yahoo's crypto bar closes at 23:59 UTC on
# day D. Shifting CoinGecko's dates back one day puts both on the same footing.
# The reconciliation below TESTS this rather than assuming it.
CG_RELABEL_TO_CLOSE <- TRUE

# --- Event / regime dates.  [VERIFY EVERY ONE AGAINST A PRIMARY SOURCE] -------
COVID_S  <- as.Date("2020-02-20"); COVID_E <- as.Date("2020-04-30")
TIGHT_S  <- as.Date("2022-03-16")   # first Fed hike of the cycle
ETF_S    <- as.Date("2024-01-11")   # US spot Bitcoin ETF trading begins
FTX_S    <- as.Date("2022-11-08"); FTX_E  <- as.Date("2022-11-30")
TIGHT_E  <- as.Date("2023-07-26")   # last hike of the cycle (for the shaded band)

VOL_WINDOW  <- 30    # trading days
CORR_WINDOW <- 90    # trading days
NW_LAG      <- NULL  # NULL -> standard rule L = floor(4(T/100)^(2/9))

# --- Folders -----------------------------------------------------------------
RAW_DIR <- "data/raw"; CLEAN_DIR <- "data/clean"
FIG_DIR <- "figures";  TAB_DIR   <- "tables"
for (.dir in c(RAW_DIR, CLEAN_DIR, FIG_DIR, TAB_DIR))
  dir.create(.dir, recursive = TRUE, showWarnings = FALSE)
PULL_STAMP <- format(Sys.Date(), "%Y%m%d")

cat("\nWorking directory:", getwd(), "\n")
cat("Everything will be written inside it.\n\n")


# =============================================================================
# PART 1 - DATA COLLECTION
# =============================================================================
if (RUN_COLLECTION) {

REQUEST_TIMEOUT <- 30; MAX_RETRIES <- 4; CG_SLEEP <- 2
PROVENANCE <- character(0)
log_msg <- function(...) { m <- paste0(...); cat(m, "\n", sep = "")
                           PROVENANCE <<- c(PROVENANCE, m); invisible(m) }

log_msg(strrep("=", 78))
log_msg("PART 1  DATA COLLECTION  |  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
log_msg("Requested: ", BUFFER_START, " -> ", END_DATE,
        "   (in-sample from ", ANALYSIS_START, ")")
log_msg(strrep("=", 78))

# --- Helpers -----------------------------------------------------------------
# Mirrors pandas .shift(): k > 0 moves values forward in time, k < 0 backward.
shift_vec <- function(x, k) {
  n <- length(x)
  if (k == 0) return(x)
  if (k > 0)  return(c(rep(NA_real_, k), head(x, n - k)))
  c(tail(x, n + k), rep(NA_real_, -k))
}
# r_t = ln(P_t) - ln(P_{t-1}). First element NA by design.
log_ret <- function(p) c(NA_real_, diff(log(p)))

# --- CoinGecko ---------------------------------------------------------------
CG_BASE <- if (COINGECKO_PLAN == "pro") {
  "https://pro-api.coingecko.com/api/v3"
} else {
  "https://api.coingecko.com/api/v3"
}
CG_HEADER  <- if (COINGECKO_PLAN == "pro") "x-cg-pro-api-key" else "x-cg-demo-api-key"
CG_HEADERS <- setNames(c("application/json", COINGECKO_API_KEY),
                       c("accept", CG_HEADER))

# GET with exponential backoff. A NULL payload means the request was rejected for
# a STRUCTURAL reason (plan limit, bad key), not a transient one.
cg_get <- function(endpoint, query = NULL) {
  url <- paste0(CG_BASE, endpoint); delay <- 2
  for (attempt in seq_len(MAX_RETRIES)) {
    resp <- tryCatch(GET(url, add_headers(.headers = CG_HEADERS), query = query,
                         timeout(REQUEST_TIMEOUT)), error = function(e) e)
    if (inherits(resp, "error")) {
      if (attempt == MAX_RETRIES)
        return(list(payload = NULL,
                    note = paste("network failure:", conditionMessage(resp))))
      Sys.sleep(delay); delay <- delay * 2; next
    }
    sc <- status_code(resp)
    if (sc == 429) {                                  # rate limited
      ra <- headers(resp)[["retry-after"]]
      wait <- if (!is.null(ra)) as.numeric(ra) else delay
      cat("    429 rate-limited; sleeping ", wait, "s\n", sep = "")
      Sys.sleep(wait); delay <- delay * 2; next
    }
    # 10012 = request exceeds the plan's allowed history. 10002 = bad key.
    if (sc %in% c(400L, 401L, 403L)) {
      body <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8")),
                       error = function(e) NULL)
      code <- tryCatch(body$status$error_code,    error = function(e) NA)
      msg  <- tryCatch(body$status$error_message, error = function(e) "")
      return(list(payload = NULL, note = sprintf("HTTP %d (code %s): %s", sc,
                                                 as.character(code), as.character(msg))))
    }
    if (sc >= 400) {
      if (attempt == MAX_RETRIES) return(list(payload = NULL, note = sprintf("HTTP %d", sc)))
      Sys.sleep(delay); delay <- delay * 2; next
    }
    return(list(payload = fromJSON(content(resp, "text", encoding = "UTF-8")),
                note = "ok"))
  }
  list(payload = NULL, note = "exhausted retries")
}

cg_ping <- function() {
  r <- cg_get("/ping")
  if (is.null(r$payload)) { log_msg("[CoinGecko] PING FAILED -> ", r$note); return(FALSE) }
  log_msg("[CoinGecko] ping OK - API key accepted."); TRUE
}

# Daily price / market cap / volume via /coins/{id}/market_chart/range.
#
# Two non-obvious API behaviours are handled here:
#  * GRANULARITY IS IMPLIED BY RANGE LENGTH. A span above 90 days returns daily
#    points; shorter spans silently return hourly data. Every chunk is therefore
#    forced to be at least 120 days wide, even the last one. Overlaps are
#    harmless - we de-duplicate by date.
#  * THE PLAN LIMIT IS ENFORCED PER REQUEST. On a Demo key, chunks older than 365
#    days are refused (error 10012) while recent chunks succeed. Rather than
#    hard-coding the limit we attempt every chunk and record what was served, so
#    the same code pulls full history unchanged if the key is ever upgraded.
cg_market_chart_range <- function(coin_id, start, end, chunk_days = 300) {
  MIN_SPAN <- 120
  start_ts <- as.POSIXct(paste(start, "00:00:00"), tz = "UTC")
  end_ts   <- as.POSIXct(paste(end + 1, "00:00:00"), tz = "UTC")

  edges <- list(); cursor <- start_ts
  while (cursor < end_ts) {
    nxt <- min(cursor + chunk_days * 86400, end_ts)
    edges[[length(edges) + 1]] <- c(cursor, nxt); cursor <- nxt
  }
  edges <- lapply(edges, function(e) c(min(e[1], e[2] - MIN_SPAN * 86400), e[2]))

  frames <- list(); rejected <- 0L
  for (e in edges) {
    a <- as.POSIXct(e[1], origin = "1970-01-01", tz = "UTC")
    b <- as.POSIXct(e[2], origin = "1970-01-01", tz = "UTC")
    r <- cg_get(sprintf("/coins/%s/market_chart/range", coin_id),
                query = list(vs_currency = "usd",
                             from = as.character(as.integer(a)),
                             to   = as.character(as.integer(b))))
    Sys.sleep(CG_SLEEP)

    px <- r$payload$prices
    if (is.null(r$payload) || is.null(px) || length(px) == 0) {
      rejected <- rejected + 1L
      cat(sprintf("    [%s] %s -> %s: refused (%s)\n", coin_id, as.Date(a), as.Date(b), r$note))
      next
    }
    cat(sprintf("    [%s] %s -> %s: %d points\n", coin_id, as.Date(a), as.Date(b), nrow(px)))

    writeLines(toJSON(r$payload, auto_unbox = TRUE),   # freeze the raw response
               file.path(RAW_DIR, sprintf("coingecko_%s_%s_%s_%s.json",
                                          coin_id, as.Date(a), as.Date(b), PULL_STAMP)))
    blk <- data.frame(ts = px[, 1], close = px[, 2])
    mc <- r$payload$market_caps; tv <- r$payload$total_volumes
    if (!is.null(mc) && nrow(mc) == nrow(px)) blk$mcap   <- mc[, 2]
    if (!is.null(tv) && nrow(tv) == nrow(px)) blk$volume <- tv[, 2]
    frames[[length(frames) + 1]] <- blk
  }
  if (length(frames) == 0) return(list(data = NULL, rejected = rejected))

  df <- do.call(rbind, frames)
  dt <- as.POSIXct(df$ts / 1000, origin = "1970-01-01", tz = "UTC")
  df$date <- as.Date(dt, tz = "UTC")
  df$mins <- as.numeric(difftime(dt, as.POSIXct(paste(df$date, "00:00:00"), tz = "UTC"),
                                 units = "mins"))
  # One row per UTC day - the FIRST observation, i.e. the 00:00 snapshot. The last
  # point of a range request is the live price at request time, so it is dropped.
  df <- df[order(df$date, df$mins), ]
  df <- df[!duplicated(df$date), ]
  df <- df[abs(df$mins) < 90, ]
  list(data = df[, intersect(c("date", "close", "mcap", "volume"), names(df))],
       rejected = rejected)
}

# --- Yahoo Finance -----------------------------------------------------------
fetch_yahoo <- function(tickers, from, to, attempts = 3) {
  cl_list <- list(); vo_list <- list()
  for (tk in names(tickers)) {
    tag <- tickers[[tk]]; x <- NULL
    for (k in seq_len(attempts)) {
      x <- tryCatch(suppressWarnings(getSymbols(tk, src = "yahoo", from = from,
                                                to = to, auto.assign = FALSE)),
                    error = function(e) { cat("    yahoo attempt ", k, " for ", tk,
                                              " failed: ", conditionMessage(e), "\n", sep = "")
                                          NULL })
      if (!is.null(x) && nrow(x) > 0) break
      Sys.sleep(3 * k)
    }
    if (is.null(x) || nrow(x) == 0)
      stop("Yahoo Finance failed for ", tk, ". Run install.packages('quantmod') to update it, then re-run.")
    d  <- as.Date(index(x)); cl <- as.numeric(Cl(x))
    vo <- tryCatch(as.numeric(Vo(x)), error = function(e) rep(NA_real_, length(d)))
    cl_list[[tag]] <- setNames(data.frame(d, cl), c("date", tag))
    vo_list[[tag]] <- setNames(data.frame(d, vo), c("date", tag))
  }
  merge_all <- function(L) Reduce(function(a, b) merge(a, b, by = "date", all = TRUE), L)
  list(close = merge_all(cl_list), volume = merge_all(vo_list))
}

# --- 1a. Pull CoinGecko ------------------------------------------------------
log_msg("\n[1a] CoinGecko pull")
cg <- NULL
if (!cg_ping()) {
  log_msg("[CoinGecko] Key rejected - continuing on Yahoo alone. Fix the key and")
  log_msg("            re-run before you write the reconciliation appendix.")
} else {
  for (coin_id in names(CG_COINS)) {
    tag <- CG_COINS[[coin_id]]
    res <- cg_market_chart_range(coin_id, BUFFER_START, END_DATE)
    if (is.null(res$data)) { log_msg("[CoinGecko] ", coin_id, ": NO data."); next }
    d <- res$data; names(d)[-1] <- paste0(tag, "_", names(d)[-1], "_cg")
    write.csv(d, file.path(RAW_DIR, sprintf("coingecko_%s_%s.csv", coin_id, PULL_STAMP)),
              row.names = FALSE)
    cg <- if (is.null(cg)) d else merge(cg, d, by = "date", all = TRUE)
    log_msg("[CoinGecko] ", coin_id, ": ", nrow(d), " daily rows, ", min(d$date),
            " -> ", max(d$date), " (", res$rejected, " chunk(s) refused by the plan limit)")
  }
}

coverage <- 0
if (!is.null(cg)) {
  needed <- seq(ANALYSIS_START, END_DATE, by = "day")
  coverage <- sum(cg$date %in% needed) / length(needed)
  log_msg(sprintf("[CoinGecko] Coverage of the %s->%s sample: %.1f%%",
                  ANALYSIS_START, END_DATE, 100 * coverage))
  if (coverage < COVERAGE_THRESHOLD)
    log_msg("[CoinGecko] >>> PLAN LIMIT CONFIRMED: the Demo tier serves only the ",
            "trailing 365 days. It cannot supply the 2020-2026 panel.")
}
if (CG_RELABEL_TO_CLOSE && !is.null(cg)) {
  cg$date <- cg$date - 1
  log_msg("[CoinGecko] Dates relabelled -1 day: each 00:00 UTC snapshot now sits on ",
          "the day it closes.")
}

# --- 1b. Pull Yahoo ----------------------------------------------------------
log_msg("\n[1b] Yahoo Finance pull")
yh <- fetch_yahoo(YF_TICKERS, BUFFER_START, END_DATE)
yf_close <- yh$close; yf_vol <- yh$volume
write.csv(yf_close, file.path(RAW_DIR, sprintf("yahoo_close_%s.csv", PULL_STAMP)),
          row.names = FALSE)
for (tag in setdiff(names(yf_close), "date")) {
  s <- yf_close$date[!is.na(yf_close[[tag]])]
  log_msg("[Yahoo] ", tag, ": ", length(s), " rows, ", min(s), " -> ", max(s))
}
cutoff <- END_DATE
if (DROP_INCOMPLETE_LAST_DAY) cutoff <- min(cutoff, END_DATE - 1)
yf_close <- yf_close[yf_close$date <= cutoff, ]
yf_vol   <- yf_vol[yf_vol$date <= cutoff, ]
if (!is.null(cg)) cg <- cg[cg$date <= cutoff, ]
log_msg("[Clean] Trimmed to the last complete day: ", cutoff)

# --- 2. Reconciliation -------------------------------------------------------
# Two independent vendors should price the same asset near-identically. Material
# divergence signals a timestamp error, not a market fact.
log_msg("\n[2] Reconciliation: CoinGecko vs Yahoo")
if (is.null(cg)) {
  log_msg("[Recon] Skipped - no CoinGecko data.")
} else {
  m <- merge(cg[, c("date", "btc_close_cg", "eth_close_cg")],
             setNames(yf_close[, c("date", "btc", "eth")],
                      c("date", "btc_close_yf", "eth_close_yf")), by = "date")
  m <- m[complete.cases(m), ]; m <- m[order(m$date), ]
  X <- as.matrix(m[, -1])
  recon <- c(sprintf("Overlapping observations: %d", nrow(m)),
             sprintf("Overlap window: %s -> %s", min(m$date), max(m$date)))

  # (a) LEVELS - expect ~1.000. A sanity check only: two trending series
  #     correlate whether or not their dates line up.
  recon <- c(recon, "", "Correlation matrix - closing price LEVELS:",
             capture.output(print(round(cor(X), 6))))

  # (b) LOG RETURNS - the real test. Returns are stationary, so a one-day
  #     misalignment collapses the correlation.
  R <- apply(X, 2, log_ret)[-1, , drop = FALSE]
  recon <- c(recon, "", "Correlation matrix - daily LOG RETURNS:",
             capture.output(print(round(cor(R), 6))))

  for (t in c("btc", "eth")) {
    d <- abs(m[[paste0(t, "_close_cg")]] / m[[paste0(t, "_close_yf")]] - 1) * 100
    recon <- c(recon, "",
      sprintf("%s |price gap|: mean %.4f%%, median %.4f%%, max %.4f%% (on %s)",
              toupper(t), mean(d), median(d), max(d), m$date[which.max(d)]),
      sprintf("%s return correlation (CG vs YF): %.6f", toupper(t),
              cor(R[, paste0(t, "_close_cg")], R[, paste0(t, "_close_yf")])))
  }

  # (c) LEAD-LAG. If the two vendors' day-stamps agree, the return correlation
  #     MUST peak at lag 0. A peak at +/-1 would mean every correlation and beta
  #     in the dissertation is off by one day. CHECK THIS BEFORE YOU TRUST ANYTHING.
  recon <- c(recon, "", "Lead-lag check on returns (peak must be at lag 0):")
  for (t in c("btc", "eth")) {
    ks <- -3:3
    cors <- sapply(ks, function(k) cor(shift_vec(R[, paste0(t, "_close_cg")], k),
                                       R[, paste0(t, "_close_yf")], use = "complete.obs"))
    best <- ks[which.max(abs(cors))]
    recon <- c(recon, paste0("  ", toupper(t), ": ",
                             paste(sprintf("lag %+d: %.3f", ks, cors), collapse = "  "),
                             sprintf("   -> peak at lag %+d", best)))
    if (best != 0) recon <- c(recon, sprintf(
      "  *** WARNING: %s peaks at lag %+d, not 0. Flip CG_RELABEL_TO_CLOSE and re-run. ***",
      toupper(t), best))
  }
  cat(paste(recon, collapse = "\n"), "\n")
  writeLines(c(paste("Source reconciliation -", format(Sys.time(), "%Y-%m-%d %H:%M")),
               strrep("=", 70), recon),
             file.path(CLEAN_DIR, "reconciliation_report.txt"))
  PROVENANCE <- c(PROVENANCE, "[Recon] -> data/clean/reconciliation_report.txt")
}

# --- 3. Which vendor supplies the panel prices? ------------------------------
use_cg <- PANEL_CRYPTO_SOURCE == "coingecko" ||
          (PANEL_CRYPTO_SOURCE == "auto" && coverage >= COVERAGE_THRESHOLD)
if (use_cg && !is.null(cg)) {
  crypto_close <- setNames(cg[, c("date", "btc_close_cg", "eth_close_cg")],
                           c("date", "btc", "eth"))
  price_source <- "CoinGecko"
} else {
  crypto_close <- yf_close[, c("date", "btc", "eth")]
  price_source <- "Yahoo Finance"
}
crypto_close <- crypto_close[!is.na(crypto_close$btc) | !is.na(crypto_close$eth), ]
log_msg("\n[3] Crypto prices for BOTH panels: ", price_source)
if (price_source == "Yahoo Finance")
  log_msg("    Rationale: the CoinGecko Demo tier cannot cover 2020-2026. One ",
          "consistent vendor across the whole sample beats splicing two vendors ",
          "mid-series. CoinGecko remains the independent validation source.")

# --- 4. PANEL A: 7-day crypto panel ------------------------------------------
# Crypto trades continuously, so Panel A keeps EVERY calendar day - returns are
# genuine 24-hour returns throughout, weekends included. Nothing is forward-
# filled: a missing calendar day is a fault to log, not to paper over.
cal <- seq(min(crypto_close$date), max(crypto_close$date), by = "day")
panel_a <- merge(data.frame(date = cal), crypto_close, by = "date", all.x = TRUE)
panel_a <- panel_a[order(panel_a$date), ]

missing <- panel_a$date[is.na(panel_a$btc) | is.na(panel_a$eth)]
if (length(missing)) {
  log_msg("[Panel A] ", length(missing), " calendar day(s) missing a crypto close ",
          "-> data/clean/missing_days_log.csv")
  write.csv(data.frame(missing_date = missing),
            file.path(CLEAN_DIR, "missing_days_log.csv"), row.names = FALSE)
} else log_msg("[Panel A] No missing calendar days.")

panel_a$btc_ret <- log_ret(panel_a$btc)
panel_a$eth_ret <- log_ret(panel_a$eth)

# CoinGecko close/volume/mcap for the appendix. NA outside the granted window -
# that absence is itself documented evidence. The CG close is carried through so
# Table 5 can re-estimate the primary model on CoinGecko prices.
if (!is.null(cg)) {
  extras <- grep("_(close|volume|mcap)_cg$", names(cg), value = TRUE)
  if (length(extras)) panel_a <- merge(panel_a, cg[, c("date", extras)],
                                       by = "date", all.x = TRUE)
}
# Yahoo's crypto volume covers the WHOLE sample, unlike CoinGecko's. Yahoo has no
# market cap, so mcap stays limited to the granted window - say so in the appendix.
yv <- setNames(yf_vol[, c("date", "btc", "eth")],
               c("date", "btc_volume_yf", "eth_volume_yf"))
panel_a <- merge(panel_a, yv, by = "date", all.x = TRUE)

names(panel_a)[names(panel_a) == "btc"] <- "btc_close"
names(panel_a)[names(panel_a) == "eth"] <- "eth_close"
panel_a$price_source <- price_source
panel_a$in_sample    <- panel_a$date >= ANALYSIS_START
panel_a <- panel_a[!(is.na(panel_a$btc_close) & is.na(panel_a$eth_close)), ]
panel_a <- panel_a[order(panel_a$date), ]
log_msg("[Panel A] ", nrow(panel_a), " rows (", sum(panel_a$in_sample),
        " in-sample), ", min(panel_a$date), " -> ", max(panel_a$date))

# --- 5. PANEL B: aligned to the US equity calendar ---------------------------
#
#  WEEKEND ALIGNMENT - the methodological core of this whole project.
#
#  Crypto trades 7 days a week; the S&P 500 trades ~252 days a year. A naive
#  merge either (a) drops weekend crypto returns, throwing away ~28% of crypto
#  price movement, or (b) pads equities across the weekend, manufacturing zero
#  returns that bias correlations toward zero. Both are wrong.
#
#  The correct treatment is to align PRICE LEVELS first and difference AFTER:
#
#    1. Take the S&P 500's own trading dates as the master calendar.
#    2. Reindex the crypto CLOSING PRICE series onto it. Saturday and Sunday
#       vanish as observations - but the information they carry is NOT lost. It
#       is embedded in Monday's price LEVEL.
#    3. Only then difference. Monday now differences against Friday, so
#            r_Monday = ln(P_Monday) - ln(P_Friday)
#       which is exactly the cumulative Friday-close to Monday-close return.
#       Holidays are absorbed by the same mechanism automatically.
#
#  So every Panel B row pairs a crypto return with an equity return realised over
#  the IDENTICAL wall-clock interval. Acknowledge in the write-up that Monday's
#  crypto observation spans 72 hours against a typical Tuesday's 24; the
#  is_multiday flag below supports the Monday-dummy robustness check.
#
spx <- yf_close[!is.na(yf_close$spx), c("date", "spx", "ndx", "vix")]
panel_b <- merge(data.frame(date = spx$date), crypto_close, by = "date", all.x = TRUE)
panel_b <- merge(panel_b, spx, by = "date", all.x = TRUE)
names(panel_b) <- c("date", "btc_close", "eth_close", "spx_close", "ndx_close", "vix_close")

gaps <- sum(!complete.cases(panel_b))
if (gaps) log_msg("[Panel B] ", gaps, " trading day(s) missing a series - dropped by the inner join.")
panel_b <- panel_b[complete.cases(panel_b), ]
panel_b <- panel_b[order(panel_b$date), ]

# Differencing happens HERE, after the join. That is what makes Monday's crypto
# return the cumulative Friday->Monday return.
panel_b$btc_ret <- log_ret(panel_b$btc_close)
panel_b$eth_ret <- log_ret(panel_b$eth_close)
panel_b$spx_ret <- log_ret(panel_b$spx_close)
panel_b$ndx_ret <- log_ret(panel_b$ndx_close)
# The VIX is already in annualised volatility points, so we take its first
# difference in LEVELS, not a log return.
panel_b$dvix <- c(NA_real_, diff(panel_b$vix_close))

panel_b$days_since_prev <- c(NA_real_, as.numeric(diff(panel_b$date)))
panel_b$is_multiday <- as.integer(panel_b$days_since_prev > 1)
panel_b$covid   <- as.integer(panel_b$date >= COVID_S & panel_b$date <= COVID_E)
panel_b$tighten <- as.integer(panel_b$date >= TIGHT_S)
panel_b$etf     <- as.integer(panel_b$date >= ETF_S)
panel_b$price_source <- price_source
panel_b$in_sample    <- panel_b$date >= ANALYSIS_START
panel_b <- panel_b[!is.na(panel_b$btc_ret) & !is.na(panel_b$eth_ret) &
                   !is.na(panel_b$spx_ret) & !is.na(panel_b$ndx_ret) &
                   !is.na(panel_b$dvix), ]

log_msg("[Panel B] ", nrow(panel_b), " rows (", sum(panel_b$in_sample),
        " in-sample), ", min(panel_b$date), " -> ", max(panel_b$date))
log_msg(sprintf("[Panel B] %d rows span a weekend/holiday gap (%.1f%%).",
                sum(panel_b$is_multiday), 100 * mean(panel_b$is_multiday)))

# Alignment proof for the appendix: a Monday must difference against a Friday.
mon <- which(format(panel_b$date, "%u") == "1"); mon <- mon[mon > 1]
if (length(mon)) {
  i <- mon[ceiling(length(mon) / 2)]
  manual <- log(panel_b$btc_close[i] / panel_b$btc_close[i - 1])
  log_msg(sprintf("[Panel B] Proof: %s (%s) differences against %s (%s); btc_ret=%.6f, recomputed=%.6f, match=%s",
                  panel_b$date[i], weekdays(panel_b$date[i]),
                  panel_b$date[i - 1], weekdays(panel_b$date[i - 1]),
                  panel_b$btc_ret[i], manual,
                  isTRUE(all.equal(manual, panel_b$btc_ret[i]))))
}

# --- 6. Export ---------------------------------------------------------------
write.csv(panel_a, file.path(CLEAN_DIR, "panel_a_7day.csv"),    row.names = FALSE)
write.csv(panel_b, file.path(CLEAN_DIR, "panel_b_aligned.csv"), row.names = FALSE)
log_msg("\n[Export] data/clean/panel_a_7day.csv    (", nrow(panel_a), " x ", ncol(panel_a), ")")
log_msg("[Export] data/clean/panel_b_aligned.csv (", nrow(panel_b), " x ", ncol(panel_b), ")")
writeLines(PROVENANCE, file.path(CLEAN_DIR, "data_provenance_log.txt"))
log_msg("[Export] data/clean/data_provenance_log.txt")

} else cat("PART 1 skipped (RUN_COLLECTION = FALSE); reusing the existing CSVs.\n")


# =============================================================================
# PART 2 - FIGURES
# =============================================================================
if (RUN_FIGURES) {

cat("\n", strrep("=", 78), "\nPART 2  FIGURES\n", strrep("=", 78), "\n", sep = "")

pa <- read.csv(file.path(CLEAN_DIR, "panel_a_7day.csv"),    stringsAsFactors = FALSE)
pb <- read.csv(file.path(CLEAN_DIR, "panel_b_aligned.csv"), stringsAsFactors = FALSE)
pa$date <- as.Date(pa$date); pb$date <- as.Date(pb$date)

# Trailing windows only. A centred window would use future information - fatal in
# a series that gets read as a monitoring indicator.
roll_sd  <- function(x, k) sapply(seq_along(x), function(i)
  if (i < k) NA_real_ else sd(x[(i - k + 1):i]))
roll_cor <- function(x, y, k) sapply(seq_along(x), function(i)
  if (i < k) NA_real_ else cor(x[(i - k + 1):i], y[(i - k + 1):i]))

# Volatility on Panel B with sqrt(252): BTC, ETH and the S&P are then annualised
# on the SAME calendar, the only way the three lines are legitimately comparable.
pb$btc_vol <- roll_sd(pb$btc_ret, VOL_WINDOW) * sqrt(252) * 100
pb$eth_vol <- roll_sd(pb$eth_ret, VOL_WINDOW) * sqrt(252) * 100
pb$spx_vol <- roll_sd(pb$spx_ret, VOL_WINDOW) * sqrt(252) * 100
pb$cor_btc <- roll_cor(pb$btc_ret, pb$spx_ret, CORR_WINDOW)
pb$cor_eth <- roll_cor(pb$eth_ret, pb$spx_ret, CORR_WINDOW)
pb$vix_btc <- roll_cor(pb$btc_ret, pb$dvix,    CORR_WINDOW)
pb$vix_eth <- roll_cor(pb$eth_ret, pb$dvix,    CORR_WINDOW)
pa$btc_vol <- roll_sd(pa$btc_ret, VOL_WINDOW) * sqrt(365) * 100
pa$eth_vol <- roll_sd(pa$eth_ret, VOL_WINDOW) * sqrt(365) * 100

# The buffer rows exist only to seed the windows. Never report them.
B <- pb[as.logical(pb$in_sample), ]
A <- pa[as.logical(pa$in_sample), ]

to_long <- function(df, cols, labels) {
  out <- do.call(rbind, lapply(seq_along(cols), function(i)
    data.frame(date = df$date, series = labels[i], value = df[[cols[i]]],
               stringsAsFactors = FALSE)))
  out$series <- factor(out$series, levels = labels)
  out[!is.na(out$value), ]
}

FONT <- ""   # set to "serif" if your dissertation body text is Times
PAL  <- c("Bitcoin" = "#E07B39", "Ethereum" = "#7B5EA7",
          "S&P 500" = "#2E5C8A", "Nasdaq" = "#4C9F70")

theme_diss <- function() {
  theme_minimal(base_size = 11, base_family = FONT) +
    theme(plot.title = element_text(face = "bold", size = 12, margin = margin(b = 2)),
          plot.subtitle = element_text(colour = "grey35", size = 9.5, margin = margin(b = 10)),
          plot.caption  = element_text(colour = "grey45", size = 8, hjust = 0,
                                       margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 8)),
          axis.title.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey92", linewidth = 0.3),
          legend.position = "top", legend.title = element_blank(),
          legend.key.width = unit(1.1, "lines"), legend.margin = margin(b = -6),
          plot.margin = margin(10, 14, 8, 10))
}

EV <- data.frame(lab = c("COVID", "Fed tightening", "FTX", "Spot BTC ETF"),
                 start = c(COVID_S, TIGHT_S, FTX_S, ETF_S),
                 stop  = c(COVID_E, TIGHT_E, FTX_E, ETF_S),
                 stringsAsFactors = FALSE)
EV_BAND <- EV[EV$start != EV$stop, ]
EV_LINE <- EV[EV$start == EV$stop, ]

# Drawn first, so the data lines sit on top of the shading rather than under it.
event_layers <- function(label_y) list(
  geom_rect(data = EV_BAND, inherit.aes = FALSE,
            aes(xmin = start, xmax = stop, ymin = -Inf, ymax = Inf),
            fill = "grey50", alpha = 0.10),
  geom_vline(data = EV_LINE, mapping = aes(xintercept = start),
             linetype = "dashed", colour = "grey45", linewidth = 0.35),
  geom_text(data = EV, inherit.aes = FALSE, aes(x = start, y = label_y, label = lab),
            angle = 90, hjust = 1, vjust = -0.4, size = 2.5, colour = "grey40"))

x_years <- scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y"),
                        expand = expansion(mult = c(0.01, 0.02)))

# --- Figure 2: the volatility exhibit ---
d2 <- to_long(B, c("btc_vol", "eth_vol", "spx_vol"), c("Bitcoin", "Ethereum", "S&P 500"))
fig2 <- ggplot(d2, aes(date, value, colour = series)) +
  event_layers(max(d2$value) * 1.02) + geom_line(linewidth = 0.45) +
  scale_colour_manual(values = PAL) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1),
                     expand = expansion(mult = c(0, 0.08))) + x_years +
  labs(title = "Figure 2  30-day rolling annualised volatility, 2020-2026",
       subtitle = "Bitcoin and Ethereum against the S&P 500",
       y = "Annualised volatility",
       caption = "Panel B (US equity trading days), 30-day trailing window, annualised on sqrt(252) so all three series share one calendar.\nShaded bands mark event windows; those dates require verification against primary sources.") +
  theme_diss()

# --- Figure 2b: crypto-native volatility ---
d2b <- to_long(A, c("btc_vol", "eth_vol"), c("Bitcoin", "Ethereum"))
fig2b <- ggplot(d2b, aes(date, value, colour = series)) +
  event_layers(max(d2b$value) * 1.02) + geom_line(linewidth = 0.45) +
  scale_colour_manual(values = PAL) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1),
                     expand = expansion(mult = c(0, 0.08))) + x_years +
  labs(title = "Figure 2b  30-day rolling annualised volatility, Panel A",
       subtitle = "All calendar days, annualised on sqrt(365) - the crypto-native convention",
       y = "Annualised volatility",
       caption = "Reported alongside Figure 2 to show the headline result is not an artefact of the annualisation convention.") +
  theme_diss()

# --- Figure 3: the interconnectedness exhibit ---
d3 <- to_long(B, c("cor_btc", "cor_eth"), c("Bitcoin", "Ethereum"))
fig3 <- ggplot(d3, aes(date, value, colour = series)) +
  event_layers(0.98) + geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.35) +
  geom_line(linewidth = 0.5) + scale_colour_manual(values = PAL) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) + x_years +
  labs(title = "Figure 3  90-day rolling correlation with the S&P 500",
       subtitle = "The direct visual test of the FPC's interconnectedness hypothesis",
       y = "Rolling correlation",
       caption = "Panel B, 90-day trailing window. Monday crypto returns are the cumulative Friday-close to Monday-close return.\nA rising level means absorption into the risk-asset complex; a flat one means integration has plateaued.") +
  theme_diss()

# --- Figure 3b: risk asset, or hedge? ---
d3b <- to_long(B, c("vix_btc", "vix_eth"), c("Bitcoin", "Ethereum"))
fig3b <- ggplot(d3b, aes(date, value, colour = series)) +
  event_layers(0.98) + geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.35) +
  geom_line(linewidth = 0.5) + scale_colour_manual(values = PAL) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) + x_years +
  labs(title = "Figure 3b  90-day rolling correlation with the change in the VIX",
       subtitle = "A persistently negative line is the signature of a risk asset, not a hedge",
       y = "Rolling correlation",
       caption = "Panel B, 90-day trailing window. Answers SQ3 visually before the regression answers it formally.") +
  theme_diss()

# --- Figure 4 (appendix): rebased indices, log scale ---
rb <- function(p) 100 * p / p[1]
R4 <- data.frame(date = B$date, btc = rb(B$btc_close), eth = rb(B$eth_close),
                 spx = rb(B$spx_close), ndx = rb(B$ndx_close))
d4 <- to_long(R4, c("btc", "eth", "spx", "ndx"),
              c("Bitcoin", "Ethereum", "S&P 500", "Nasdaq"))
fig4 <- ggplot(d4, aes(date, value, colour = series)) +
  geom_line(linewidth = 0.45) + scale_colour_manual(values = PAL) +
  scale_y_log10(labels = label_comma(accuracy = 1),
                breaks = c(50, 100, 200, 500, 1000, 2000, 5000, 10000)) + x_years +
  labs(title = "Figure 4  Cumulative price indices, rebased to 100",
       subtitle = "Log scale: equal vertical distances are equal percentage moves",
       y = "Index (start of sample = 100)",
       caption = "Panel B closing levels. On a linear scale Bitcoin's run flattens the S&P 500 into a horizontal line.") +
  theme_diss()

# --- Figure 5a / 5b: returns and fat tails ---
d5 <- to_long(B, c("btc_ret", "eth_ret", "spx_ret"), c("Bitcoin", "Ethereum", "S&P 500"))
d5$value <- 100 * d5$value
fig5a <- ggplot(d5, aes(date, value, colour = series)) +
  geom_segment(aes(xend = date, yend = 0), linewidth = 0.2) +
  facet_wrap(~ series, ncol = 3, scales = "free_y") +
  scale_colour_manual(values = PAL, guide = "none") +
  scale_x_date(breaks = date_breaks("2 years"), labels = date_format("%Y")) +
  labs(title = "Figure 5a  Daily log returns",
       subtitle = "Volatility clustering: large moves arrive next to other large moves",
       y = "Daily log return (%)",
       caption = "Panel B. Note the difference in vertical scale between the crypto panels and the equity panel.") +
  theme_diss() + theme(strip.text = element_text(face = "bold", size = 10))

fig5b <- ggplot(d5, aes(sample = value, colour = series)) +
  stat_qq(size = 0.5, alpha = 0.6) + stat_qq_line(colour = "grey30", linewidth = 0.4) +
  facet_wrap(~ series, ncol = 3, scales = "free_y") +
  scale_colour_manual(values = PAL, guide = "none") +
  labs(title = "Figure 5b  Normal quantile-quantile plots",
       subtitle = "Departure from the line at both ends is excess kurtosis - the fat tails Table 1 reports",
       x = "Theoretical quantiles", y = "Sample quantiles",
       caption = "Panel B daily log returns (%). This is the picture an examiner looks for when you claim non-normality.") +
  theme_diss() + theme(axis.title.x = element_text(margin = margin(t = 8)),
                       strip.text = element_text(face = "bold", size = 10))

FIGS <- list(figure2_rolling_volatility  = fig2,
             figure3_rolling_correlation = fig3,
             figure3b_correlation_dvix   = fig3b,
             figure4_rebased_prices      = fig4,
             figure5a_daily_returns      = fig5a,
             figure5b_qq_plots           = fig5b,
             figure2b_volatility_panelA  = fig2b)
OBJ <- c("fig2", "fig3", "fig3b", "fig4", "fig5a", "fig5b", "fig2b")

# --- SAVE FIRST --------------------------------------------------------------
# ggsave opens its own private graphics device, so this works even when RStudio's
# Plots pane is misbehaving. The files are the deliverable; the on-screen preview
# is a convenience. Never let the convenience be able to destroy the deliverable.
for (nm in names(FIGS)) {
  wide <- grepl("figure5", nm)
  ggsave(file.path(FIG_DIR, paste0(nm, ".png")), FIGS[[nm]],
         width = if (wide) 11 else 9, height = if (wide) 5 else 5.2, dpi = 300, bg = "white")
  ggsave(file.path(FIG_DIR, paste0(nm, ".pdf")), FIGS[[nm]],
         width = if (wide) 11 else 9, height = if (wide) 5 else 5.2, bg = "white")
}
cat("Seven figures saved to figures/ (PNG for Word, PDF for print).\n")

# --- THEN PREVIEW ON SCREEN --------------------------------------------------
# RStudio's Plots pane has a long-standing grid bug that throws
#   Error in UseMethod("depth") : ... applied to an object of class "NULL"
# when several plots are printed in quick succession by a sourced script. It is a
# rendering fault in the device, not in the plot. So each print is wrapped: if the
# pane chokes, we say so and move on rather than killing the run.
graphics.off(); invisible(gc())
failed <- character(0)
for (i in seq_along(FIGS)) {
  ok <- tryCatch({ print(FIGS[[i]]); TRUE }, error = function(e) FALSE)
  if (!ok) failed <- c(failed, OBJ[i])
}
if (length(failed)) {
  cat("\nNOTE: RStudio's Plots pane failed to render:", paste(failed, collapse = ", "), "\n")
  cat("      This is a known RStudio/grid bug, NOT a problem with your data - the\n")
  cat("      PNG and PDF files in figures/ are complete and correct. To see them on\n")
  cat("      screen: enlarge the Plots pane, then type the figure's name, e.g.  fig3\n")
} else {
  cat("All seven figures are in the Plots pane - use the arrows to flick between them.\n")
}
cat("Redraw any figure at any time by typing its name, e.g.   fig3\n")

} else cat("\nPART 2 skipped (RUN_FIGURES = FALSE).\n")


# =============================================================================
# PART 3 - TABLES AND REGRESSIONS
# =============================================================================
if (RUN_REGRESSIONS) {

REPORT <- character(0)
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); REPORT <<- c(REPORT, m) }

say("\n", strrep("=", 78))
say("PART 3  TABLES AND REGRESSIONS")
say(strrep("=", 78))

pa <- read.csv(file.path(CLEAN_DIR, "panel_a_7day.csv"),    stringsAsFactors = FALSE)
pb <- read.csv(file.path(CLEAN_DIR, "panel_b_aligned.csv"), stringsAsFactors = FALSE)
pa$date <- as.Date(pa$date); pb$date <- as.Date(pb$date)
pa$in_sample <- as.logical(pa$in_sample); pb$in_sample <- as.logical(pb$in_sample)

# --- Statistical machinery ---------------------------------------------------
skewness <- function(x) { x <- x[!is.na(x)]; n <- length(x); m <- mean(x)
  (sum((x - m)^3) / n) / (sum((x - m)^2) / n)^1.5 }
ex_kurtosis <- function(x) { x <- x[!is.na(x)]; n <- length(x); m <- mean(x)
  (sum((x - m)^4) / n) / (sum((x - m)^2) / n)^2 - 3 }
# Jarque-Bera: JB = (n/6)(S^2 + K^2/4), K = EXCESS kurtosis, ~ chi-sq(2) under H0.
jarque_bera <- function(x) { x <- x[!is.na(x)]; n <- length(x)
  s <- skewness(x); k <- ex_kurtosis(x); jb <- (n / 6) * (s^2 + k^2 / 4)
  c(JB = jb, p = pchisq(jb, 2, lower.tail = FALSE)) }

# Newey-West HAC covariance matrix.
#   V = (X'X)^-1 Omega (X'X)^-1,  Omega = c[S0 + sum_l w_l (S_l + S_l')]
#   w_l = 1 - l/(L+1)  (Bartlett weights: they decay to zero, which is what makes
#                       the estimate positive semi-definite)
#   c   = n/(n-k)      (finite-sample adjustment)
# Identical to sandwich::NeweyWest(fit, lag = L, prewhite = FALSE, adjust = TRUE);
# the script verifies exactly that at the end if you have sandwich installed.
hac_vcov <- function(fit, L = NULL) {
  X <- model.matrix(fit); u <- as.numeric(residuals(fit))
  n <- nrow(X); k <- ncol(X)
  if (is.null(L)) L <- floor(4 * (n / 100)^(2 / 9))
  bread <- solve(crossprod(X)); Xu <- X * u
  S <- crossprod(Xu)
  if (L > 0) for (l in seq_len(L)) {
    w  <- 1 - l / (L + 1)
    Sl <- crossprod(Xu[(l + 1):n, , drop = FALSE], Xu[1:(n - l), , drop = FALSE])
    S  <- S + w * (Sl + t(Sl))
  }
  S <- S * n / (n - k)
  V <- bread %*% S %*% bread
  attr(V, "lag") <- L; V
}
ols_hac <- function(formula, data, L = NW_LAG) {
  fit <- lm(formula, data = data); V <- hac_vcov(fit, L)
  b <- coef(fit); se <- sqrt(diag(V)); tv <- b / se
  n <- length(residuals(fit)); k <- length(b)
  list(fit = fit, vcov = V, coef = b, se = se, t = tv,
       p = 2 * pt(-abs(tv), df = n - k), n = n, lag = attr(V, "lag"),
       adj_r2 = summary(fit)$adj.r.squared)
}
# Wald test of H0: the named coefficients are jointly zero. This carries H3.
wald_hac <- function(m, terms) {
  terms <- intersect(terms, names(m$coef))
  b <- m$coef[terms]; V <- m$vcov[terms, terms, drop = FALSE]
  W <- as.numeric(t(b) %*% solve(V) %*% b)
  c(W = W, df = length(terms), p = pchisq(W, length(terms), lower.tail = FALSE))
}
# Breusch-Godfrey LM test. This is the diagnostic that JUSTIFIES using HAC - and,
# because there is a lagged dependent variable on the right-hand side, the one
# that tells you whether OLS is merely inefficient or actually inconsistent.
breusch_godfrey <- function(fit, p = 5) {
  u <- as.numeric(residuals(fit)); X <- model.matrix(fit); n <- length(u)
  U <- sapply(seq_len(p), function(l) c(rep(0, l), head(u, n - l)))
  aux <- lm.fit(cbind(X, U), u)
  r2 <- 1 - sum(aux$residuals^2) / sum((u - mean(u))^2)
  c(LM = n * r2, df = p, p = pchisq(n * r2, p, lower.tail = FALSE))
}
stars <- function(p) ifelse(is.na(p), "", ifelse(p < .01, "***",
                     ifelse(p < .05, "**", ifelse(p < .1, "*", ""))))
tidy_model <- function(m, nm) data.frame(
  Model = nm, Term = names(m$coef), Coefficient = round(m$coef, 6),
  HAC_SE = round(m$se, 6), t_stat = round(m$t, 3), p_value = round(m$p, 4),
  Sig = stars(m$p), N = m$n, Adj_R2 = round(m$adj_r2, 4), NW_lag = m$lag,
  stringsAsFactors = FALSE, row.names = NULL)
print_model <- function(m, title) {
  say("\n", title); say(strrep("-", nchar(title)))
  for (nm in names(m$coef))
    say(sprintf("  %-26s %10.5f  (%.5f) [t=%7.3f] %s", nm, m$coef[[nm]],
                m$se[[nm]], m$t[[nm]], stars(m$p[[nm]])))
  bg <- breusch_godfrey(m$fit, 5)
  say(sprintf("  %-26s %d    Adj R2 = %.4f    NW lag = %d", "N", m$n, m$adj_r2, m$lag))
  say(sprintf("  %-26s LM = %.2f (df=%d), p = %.4f  %s", "Breusch-Godfrey(5)",
              bg["LM"], bg["df"], bg["p"],
              ifelse(bg["p"] < .05, "-> serial correlation present; HAC required",
                     "-> no strong evidence of serial correlation")))
}

# --- Variables ---------------------------------------------------------------
lag1 <- function(x) c(NA_real_, head(x, -1))
pb$btc_lag <- lag1(pb$btc_ret); pb$eth_lag <- lag1(pb$eth_ret)
pb$dlvix   <- c(NA_real_, diff(log(pb$vix_close)))   # robustness transform

# MUTUALLY EXCLUSIVE regimes. The panel's own dummies are CUMULATIVE (tighten
# stays 1 forever once it turns on), so post-2024 rows are simultaneously
# "tightening" and "ETF". Interact those with the equity return and the deltas
# are overlapping increments nobody can interpret. These four are disjoint, and
# the omitted category is Baseline - so b1 IS the baseline beta and each delta
# is the CHANGE from it. Say that in the table note.
pb$RG_COVID <- as.integer(pb$date >= COVID_S & pb$date <= COVID_E)
pb$RG_TIGHT <- as.integer(pb$date >= TIGHT_S & pb$date <  ETF_S)
pb$RG_ETF   <- as.integer(pb$date >= ETF_S)

D <- pb[pb$in_sample & complete.cases(pb[, c("btc_ret", "eth_ret", "spx_ret",
        "ndx_ret", "dvix", "btc_lag", "eth_lag", "dlvix")]), ]
D <- D[order(D$date), ]
A <- pa[pa$in_sample, ]

say("Estimation sample: ", min(D$date), " -> ", max(D$date), "   N = ", nrow(D))
say("Regimes: baseline=", sum(D$RG_COVID + D$RG_TIGHT + D$RG_ETF == 0),
    "  covid=", sum(D$RG_COVID), "  tightening=", sum(D$RG_TIGHT),
    "  etf=", sum(D$RG_ETF))

# --- TABLE 1: descriptive statistics -----------------------------------------
# Panel A annualises on sqrt(365) (crypto trades daily); Panel B on sqrt(252)
# (equity calendar). Mixing the conventions in one table is a classic own goal,
# so every row states its panel.
describe <- function(x, label, ann) {
  x <- x[!is.na(x)]; jb <- jarque_bera(x)
  data.frame(Series = label, N = length(x), Mean_pct = 100 * mean(x),
             SD_pct = 100 * sd(x), Ann_vol_pct = 100 * sd(x) * sqrt(ann),
             Min_pct = 100 * min(x), Max_pct = 100 * max(x),
             Skewness = skewness(x), Excess_kurt = ex_kurtosis(x),
             Jarque_Bera = jb[["JB"]], JB_p = jb[["p"]],
             stringsAsFactors = FALSE, row.names = NULL)
}
table1 <- rbind(
  describe(A$btc_ret, "BTC return (Panel A, 7-day)",   365),
  describe(A$eth_ret, "ETH return (Panel A, 7-day)",   365),
  describe(D$btc_ret, "BTC return (Panel B, aligned)", 252),
  describe(D$eth_ret, "ETH return (Panel B, aligned)", 252),
  describe(D$spx_ret, "S&P 500 return (Panel B)",      252),
  describe(D$ndx_ret, "Nasdaq return (Panel B)",       252),
  describe(D$dvix,    "dVIX (Panel B, index points)",  252))
# dVIX is in VIX points, not percent - blank the percent-scaled columns rather
# than print a number that invites misreading.
table1[7, c("Mean_pct", "SD_pct", "Min_pct", "Max_pct")] <-
  table1[7, c("Mean_pct", "SD_pct", "Min_pct", "Max_pct")] / 100
table1[7, "Ann_vol_pct"] <- NA
nums <- sapply(table1, is.numeric); table1[nums] <- lapply(table1[nums], round, 4)
write.csv(table1, file.path(TAB_DIR, "table1_descriptive_statistics.csv"), row.names = FALSE)
say("\n== TABLE 1  Descriptive statistics ==")
say(paste(capture.output(print(table1[, c("Series", "N", "Mean_pct", "SD_pct",
    "Ann_vol_pct", "Skewness", "Excess_kurt", "Jarque_Bera")], row.names = FALSE)),
    collapse = "\n"))

# --- TABLE 2: correlation matrix ---------------------------------------------
cv <- D[, c("btc_ret", "eth_ret", "spx_ret", "ndx_ret", "dvix")]
names(cv) <- c("r_BTC", "r_ETH", "r_SPX", "r_NDX", "dVIX")
table2 <- round(cor(cv), 4)
write.csv(table2, file.path(TAB_DIR, "table2_correlation_matrix.csv"))
say("\n== TABLE 2  Full-sample correlation matrix (Panel B, in-sample) ==")
say(paste(capture.output(print(table2)), collapse = "\n"))

# --- TABLE 3: primary OLS (eq. 4) --------------------------------------------
#   r_i,t = a + b1 r_SPX,t + b2 dVIX_t + g r_i,t-1 + e_t
m3_btc <- ols_hac(btc_ret ~ spx_ret + dvix + btc_lag, D)
m3_eth <- ols_hac(eth_ret ~ spx_ret + dvix + eth_lag, D)
say("\n", strrep("=", 78))
say("TABLE 3  Primary OLS (eq. 4), Newey-West HAC standard errors")
say(strrep("=", 78))
print_model(m3_btc, "BTC:  r_BTC ~ r_SPX + dVIX + r_BTC(-1)")
print_model(m3_eth, "ETH:  r_ETH ~ r_SPX + dVIX + r_ETH(-1)")
write.csv(rbind(tidy_model(m3_btc, "Table 3 - BTC"), tidy_model(m3_eth, "Table 3 - ETH")),
          file.path(TAB_DIR, "table3_primary_ols.csv"), row.names = FALSE)

for (nm in c("BTC", "ETH")) {
  m <- if (nm == "BTC") m3_btc else m3_eth
  say(sprintf("\n[Reading - %s] A 1%% move in the S&P 500 is associated with a %.2f%% move in %s (%s).",
              nm, m$coef[["spx_ret"]], nm,
              ifelse(m$p[["spx_ret"]] < .05, "significant at 5%", "NOT significant at 5%")))
  say(sprintf("[Reading - %s] A 1-point rise in the VIX is associated with a %.3f%% move in %s, controlling for equities (%s).",
              nm, 100 * m$coef[["dvix"]], nm,
              ifelse(m$p[["dvix"]] < .05, "significant at 5%", "NOT significant at 5%")))
}
say("\n[Framing] A low adjusted R2 is EXPECTED, not a defect: daily crypto returns")
say("are dominated by idiosyncratic noise. The coefficient of interest is b1, not")
say("the fit. Say this in Ch. 4.4 before the examiner says it for you.")

# --- TABLE 4: regime interactions (eq. 5) + Wald tests ------------------------
f4 <- function(y, yl) as.formula(paste0(y, " ~ spx_ret + dvix + ", yl,
  " + I(RG_COVID*spx_ret) + I(RG_TIGHT*spx_ret) + I(RG_ETF*spx_ret)",
  " + RG_COVID + RG_TIGHT + RG_ETF"))
m4_btc <- ols_hac(f4("btc_ret", "btc_lag"), D)
m4_eth <- ols_hac(f4("eth_ret", "eth_lag"), D)
INTER <- c("I(RG_COVID * spx_ret)", "I(RG_TIGHT * spx_ret)", "I(RG_ETF * spx_ret)")

say("\n", strrep("=", 78))
say("TABLE 4  Regime-interaction OLS (eq. 5), Newey-West HAC standard errors")
say("         Omitted regime = Baseline (2020-01-01 to 2022-03-15, ex-COVID).")
say("         b1 is the BASELINE equity beta; each delta is the CHANGE from it.")
say(strrep("=", 78))
print_model(m4_btc, "BTC with regime interactions")
print_model(m4_eth, "ETH with regime interactions")
write.csv(rbind(tidy_model(m4_btc, "Table 4 - BTC"), tidy_model(m4_eth, "Table 4 - ETH")),
          file.path(TAB_DIR, "table4_regime_interaction_ols.csv"), row.names = FALSE)

say("\n--- H3: joint Wald test,  H0: d_COVID = d_TIGHT = d_ETF = 0 ---")
say("    (i.e. the equity beta did NOT shift across regimes)")
wald_rows <- NULL
for (nm in c("BTC", "ETH")) {
  m <- if (nm == "BTC") m4_btc else m4_eth
  w <- wald_hac(m, INTER); b1 <- m$coef[["spx_ret"]]
  say(sprintf("  %s:  Wald = %8.3f (df = %d), p = %.4f  %s", nm, w[["W"]], w[["df"]], w[["p"]],
              ifelse(w[["p"]] < .05,
                     "-> REJECT constant beta: integration is regime-dependent (H3 supported)",
                     "-> cannot reject constant beta: integration exists but is stable")))
  say(sprintf("     implied beta: baseline %.3f | covid %.3f | tightening %.3f | ETF era %.3f",
              b1, b1 + m$coef[[INTER[1]]], b1 + m$coef[[INTER[2]]], b1 + m$coef[[INTER[3]]]))
  wald_rows <- rbind(wald_rows, data.frame(Model = nm,
    Test = "Wald: d_COVID = d_TIGHT = d_ETF = 0", Statistic = round(w[["W"]], 4),
    df = w[["df"]], p_value = round(w[["p"]], 4), Beta_baseline = round(b1, 4),
    Beta_covid = round(b1 + m$coef[[INTER[1]]], 4),
    Beta_tightening = round(b1 + m$coef[[INTER[2]]], 4),
    Beta_etf = round(b1 + m$coef[[INTER[3]]], 4), stringsAsFactors = FALSE))
}
write.csv(wald_rows, file.path(TAB_DIR, "table4_wald_tests.csv"), row.names = FALSE)

# --- TABLE 5: robustness -----------------------------------------------------
# Each row answers: "is the headline finding an artefact of a choice I made?"
say("\n", strrep("=", 78))
say("TABLE 5  Robustness checks (appendix)")
say(strrep("=", 78))
rob <- list()

# (a) Nasdaq instead of the S&P - is the result specific to the benchmark?
rob[["(a) BTC, Nasdaq factor"]] <- ols_hac(btc_ret ~ ndx_ret + dvix + btc_lag, D)
rob[["(a) ETH, Nasdaq factor"]] <- ols_hac(eth_ret ~ ndx_ret + dvix + eth_lag, D)

# (b) dlog(VIX) instead of dVIX - the VIX is bounded below and right-skewed, so a
#     log change is arguably better behaved. If sign and significance survive, say so.
rob[["(b) BTC, dlog(VIX)"]] <- ols_hac(btc_ret ~ spx_ret + dlvix + btc_lag, D)
rob[["(b) ETH, dlog(VIX)"]] <- ols_hac(eth_ret ~ spx_ret + dlvix + eth_lag, D)

# (c) Drop the multi-day rows (weekends/holidays). The direct test that the
#     alignment convention is not driving the result. If beta survives, the
#     weekend protocol is vindicated.
Dm <- D[D$is_multiday == 0, ]
rob[["(c) BTC, no multi-day rows"]] <- ols_hac(btc_ret ~ spx_ret + dvix + btc_lag, Dm)
rob[["(c) ETH, no multi-day rows"]] <- ols_hac(eth_ret ~ spx_ret + dvix + eth_lag, Dm)
say(sprintf("\n(c) Monday/holiday-exclusion sample: %d of %d rows retained (%.1f%%).",
            nrow(Dm), nrow(D), 100 * nrow(Dm) / nrow(D)))

# (d) CoinGecko prices, where the Demo plan served them. That window is the
#     trailing year only, which sits inside ONE regime - so comparing it to the
#     full-sample beta would look like a vendor discrepancy when it is really a
#     regime difference. Yahoo is therefore estimated over the IDENTICAL window
#     and the two are reported as a PAIR. Compare each CoinGecko row only with
#     the matched Yahoo row directly above it.
if (all(c("btc_close_cg", "eth_close_cg") %in% names(pa)) &&
    sum(!is.na(pa$btc_close_cg)) > 120) {
  cgp <- pa[!is.na(pa$btc_close_cg), c("date", "btc_close_cg", "eth_close_cg")]
  Dcg <- merge(D[, c("date", "spx_ret", "dvix")], cgp, by = "date")
  Dcg <- Dcg[order(Dcg$date), ]
  Dcg$btc_ret <- c(NA, diff(log(Dcg$btc_close_cg)))   # differenced ON the trading
  Dcg$eth_ret <- c(NA, diff(log(Dcg$eth_close_cg)))   # calendar: same weekend rule
  Dcg$btc_lag <- lag1(Dcg$btc_ret); Dcg$eth_lag <- lag1(Dcg$eth_ret)
  Dcg <- Dcg[complete.cases(Dcg), ]
  if (nrow(Dcg) > 60) {
    Dyh <- D[D$date %in% Dcg$date, ]
    rob[["(d) BTC, Yahoo, matched window"]]  <- ols_hac(btc_ret ~ spx_ret + dvix + btc_lag, Dyh)
    rob[["(d) BTC, CoinGecko, same window"]] <- ols_hac(btc_ret ~ spx_ret + dvix + btc_lag, Dcg)
    rob[["(d) ETH, Yahoo, matched window"]]  <- ols_hac(eth_ret ~ spx_ret + dvix + eth_lag, Dyh)
    rob[["(d) ETH, CoinGecko, same window"]] <- ols_hac(eth_ret ~ spx_ret + dvix + eth_lag, Dcg)
    say(sprintf("(d) CoinGecko sub-window: %s -> %s, N = %d (compare ONLY against the matched Yahoo row).",
                min(Dcg$date), max(Dcg$date), nrow(Dcg)))
  }
} else {
  say("(d) No CoinGecko prices in Panel A - skipped. Cite reconciliation_report.txt instead.")
}

write.csv(do.call(rbind, lapply(names(rob), function(k) tidy_model(rob[[k]], k))),
          file.path(TAB_DIR, "table5_robustness.csv"), row.names = FALSE)

say("\nEquity beta across every specification - the row that decides the appendix:")
say(sprintf("  %-34s %8s %8s %8s", "Specification", "beta", "HAC t", "p"))
say(sprintf("  %-34s %8.4f %8.3f %8.4f", "Table 3 baseline - BTC",
            m3_btc$coef[["spx_ret"]], m3_btc$t[["spx_ret"]], m3_btc$p[["spx_ret"]]))
say(sprintf("  %-34s %8.4f %8.3f %8.4f", "Table 3 baseline - ETH",
            m3_eth$coef[["spx_ret"]], m3_eth$t[["spx_ret"]], m3_eth$p[["spx_ret"]]))
for (k in names(rob)) {
  m <- rob[[k]]; fx <- if ("ndx_ret" %in% names(m$coef)) "ndx_ret" else "spx_ret"
  say(sprintf("  %-34s %8.4f %8.3f %8.4f", k, m$coef[[fx]], m$t[[fx]], m$p[[fx]]))
}
say("\nIf the beta keeps its sign, rough magnitude and significance down this")
say("column, the finding is not an artefact of benchmark, VIX transform, weekend")
say("alignment, or data vendor. That sentence belongs in the appendix.")

# --- Verify the HAC implementation against sandwich, if it happens to be there --
if (requireNamespace("sandwich", quietly = TRUE)) {
  Vr <- sandwich::NeweyWest(m3_btc$fit, lag = m3_btc$lag, prewhite = FALSE, adjust = TRUE)
  dd <- max(abs(sqrt(diag(Vr)) - m3_btc$se))
  say(sprintf("\n[Verify] HAC SEs vs sandwich::NeweyWest: max difference = %.3e %s",
              dd, ifelse(dd < 1e-8, "-> identical", "-> INVESTIGATE")))
} else {
  say("\n[Verify] Package 'sandwich' is not installed, so the HAC implementation was")
  say("         not cross-checked. Run install.packages('sandwich') and re-run to")
  say("         verify it - the reported difference should be about 1e-16.")
}

writeLines(REPORT, file.path(TAB_DIR, "regression_report.txt"))

} else cat("\nPART 3 skipped (RUN_REGRESSIONS = FALSE).\n")


# =============================================================================
# PART 4 - EXTENSIONS: THE TREND IN BETA, AND THE DIAGNOSTICS AN EXAMINER ASKS FOR
# =============================================================================
#
#   WHY THIS PART EXISTS
#   --------------------
#   Part 3 answered "is crypto integrated with equities?" (yes: BTC beta ~0.70,
#   ETH ~1.00, both HAC-significant) and "is the beta constant?" (the joint Wald
#   said it could not be rejected).
#
#   But the Table 5 robustness rows quietly disagreed. Estimated on the trailing
#   CoinGecko window alone, the SAME specification returned betas near 1.8 (BTC)
#   and 2.6 (ETH) - and BOTH vendors agreed to two decimals, so that is not a
#   data artefact, it is a real subsample. The reason Table 4 misses it is that
#   the single ETF dummy spans Jan-2024 to today and AVERAGES ACROSS a period in
#   which beta appears to be climbing. A three-restriction joint Wald test over
#   coarse regimes has low power against a trend.
#
#   So Part 4 stops binning time into blocks and looks at beta CONTINUOUSLY.
#   That is the direct empirical analogue of the FPC's trigger variable: the
#   Committee's judgement is that crypto risk is "limited" GIVEN CURRENT
#   interconnectedness. The policy-relevant object is therefore not the average
#   beta over six years - it is the CURRENT beta and its TRAJECTORY.
#
#   IT ADDS
#     Figure 6   90-day rolling equity beta with a HAC confidence band  <- headline
#     Figure 7   Beta decomposed: beta = rho x (sigma_crypto / sigma_SPX)
#     Table 6    Calendar-year betas (a split no one can accuse you of mining)
#     Table 7    Downside asymmetry: is beta higher on down days?
#     Table 8    Extended regimes: the ETF era split by calendar year
#     Table 9    Diagnostics: ADF stationarity, VIF, NW-lag sensitivity, no-dVIX
#
#   AN HONESTY NOTE YOU MUST CARRY INTO THE WRITE-UP
#   ------------------------------------------------
#   The rolling-beta analysis was prompted by an anomaly in the appendix, i.e. it
#   is post hoc. That is legitimate - but only if you SAY so, and only if the
#   follow-up split is chosen on a rule you did not pick after seeing the answer.
#   That is exactly why Table 6 splits on CALENDAR YEARS: it is the one partition
#   that cannot be reverse-engineered to flatter the result. State this in Ch. 3.
#   An examiner forgives exploratory analysis that announces itself; they do not
#   forgive exploratory analysis dressed up as confirmatory.
#
#   Requires RUN_REGRESSIONS = TRUE in the same session (it reuses ols_hac,
#   hac_vcov, wald_hac, tidy_model, stars and the estimation sample D).
# =============================================================================

RUN_EXTENSIONS <- TRUE
BETA_WINDOW    <- 90     # trading days in the rolling beta window

if (RUN_EXTENSIONS && exists("ols_hac") && exists("D")) {

EXT <- character(0)
sayx <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); EXT <<- c(EXT, m) }

sayx("\n", strrep("=", 78))
sayx("PART 4  EXTENSIONS - THE TRAJECTORY OF BETA")
sayx(strrep("=", 78))


# -----------------------------------------------------------------------------
# 4.1  ROLLING EQUITY BETA  (Figure 6 - the exhibit the dissertation turns on)
# -----------------------------------------------------------------------------
# For every window of BETA_WINDOW trading days, re-estimate
#     r_i,t = a + b1 r_SPX,t + b2 dVIX_t + e_t
# and keep b1 with its Newey-West standard error. The band is b1 +/- 1.96 SE, so
# where the band clears 1.0 the crypto asset is amplifying equity moves, and
# where it clears 0 it is integrated at all. A LEVEL says how integrated crypto
# is; a SLOPE says whether the FPC's "growing interconnectedness" is happening.
#
# Note the dVIX control stays in: the rolling beta must be the SAME object as the
# Table 3 beta, or Figure 6 and Table 3 are not comparable and the examiner will
# say so. Do not be tempted to switch to the simpler cov/var bivariate beta.

roll_beta <- function(y, x, z, k) {
  n <- length(y); b <- se <- rep(NA_real_, n)
  for (i in k:n) {
    idx <- (i - k + 1):i
    dfw <- data.frame(y = y[idx], x = x[idx], z = z[idx])
    if (anyNA(dfw)) next
    fit <- lm(y ~ x + z, data = dfw)
    V   <- hac_vcov(fit, NULL)
    b[i]  <- coef(fit)[["x"]]
    se[i] <- sqrt(diag(V))[["x"]]
  }
  data.frame(beta = b, se = se)
}

sayx("\nEstimating rolling betas (", BETA_WINDOW, "-day window, HAC SEs) ...")
rb_btc <- roll_beta(D$btc_ret, D$spx_ret, D$dvix, BETA_WINDOW)
rb_eth <- roll_beta(D$eth_ret, D$spx_ret, D$dvix, BETA_WINDOW)

RB <- rbind(
  data.frame(date = D$date, series = "Bitcoin",  beta = rb_btc$beta, se = rb_btc$se),
  data.frame(date = D$date, series = "Ethereum", beta = rb_eth$beta, se = rb_eth$se))
RB <- RB[!is.na(RB$beta), ]
RB$lo <- RB$beta - 1.96 * RB$se
RB$hi <- RB$beta + 1.96 * RB$se
write.csv(RB, file.path(TAB_DIR, "rolling_beta_series.csv"), row.names = FALSE)

# The three numbers you will quote in Chapter 4.
last_yr <- max(D$date) - 365
for (s in c("Bitcoin", "Ethereum")) {
  r <- RB[RB$series == s, ]
  rl <- r[r$date >= last_yr, ]
  sayx(sprintf("  %-9s  full-sample rolling beta: mean %.3f | median %.3f | min %.3f | max %.3f",
               s, mean(r$beta), median(r$beta), min(r$beta), max(r$beta)))
  sayx(sprintf("  %-9s  trailing 12 months:       mean %.3f | latest %.3f (95%% CI %.3f to %.3f)",
               s, mean(rl$beta), tail(rl$beta, 1), tail(rl$lo, 1), tail(rl$hi, 1)))
  sayx(sprintf("  %-9s  share of windows with beta > 1: %.1f%%  (trailing 12m: %.1f%%)",
               s, 100 * mean(r$beta > 1), 100 * mean(rl$beta > 1)))
}
sayx("\n  [Read] If the trailing-12m mean sits clearly above the full-sample mean AND")
sayx("         the confidence band excludes 1.0, you have direct evidence that crypto")
sayx("         now AMPLIFIES equity moves rather than merely following them. That is a")
sayx("         materially stronger financial-stability claim than 'crypto is a risk")
sayx("         asset', and it is the sentence the FPC section is built on.")

# Fallback theme objects, in case Part 2 was skipped this session.
if (!exists("PAL")) PAL <- c("Bitcoin" = "#E07B39", "Ethereum" = "#7B5EA7",
                             "S&P 500" = "#2E5F8A", "Nasdaq" = "#4E9A6B")
if (!exists("theme_diss")) theme_diss <- function() theme_minimal(base_size = 11)
if (!exists("x_years")) x_years <- scale_x_date(breaks = date_breaks("1 year"),
                                                labels = date_format("%Y"))

fs_beta <- data.frame(series = c("Bitcoin", "Ethereum"),
                      b = c(m3_btc$coef[["spx_ret"]], m3_eth$coef[["spx_ret"]]))

fig6 <- ggplot(RB, aes(date, beta)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.35) +
  geom_hline(yintercept = 1, colour = "grey30", linetype = "dashed", linewidth = 0.4) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = series), alpha = 0.18, colour = NA) +
  geom_line(aes(colour = series), linewidth = 0.5) +
  geom_hline(data = fs_beta, aes(yintercept = b), colour = "grey20",
             linetype = "dotted", linewidth = 0.4) +
  facet_wrap(~ series, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = PAL, guide = "none") +
  scale_fill_manual(values = PAL, guide = "none") + x_years +
  labs(title = sprintf("Figure 6  %d-day rolling equity beta, with Newey-West 95%% band",
                       BETA_WINDOW),
       subtitle = "Dashed line = beta of 1 (crypto moves one-for-one with the S&P 500); dotted line = the full-sample estimate",
       y = expression(hat(beta)[1]~", coefficient on the S&P 500 return"),
       caption = paste0("Panel B. Each point re-estimates r_i = a + b1*r_SPX + b2*dVIX over the trailing ",
                        BETA_WINDOW, " trading days.\nA rising line is the FPC's 'growing interconnectedness' made visible; where the band clears 1.0, crypto amplifies equity moves.")) +
  theme_diss() + theme(strip.text = element_text(face = "bold", size = 10))

ggsave(file.path(FIG_DIR, "figure6_rolling_beta.png"), fig6,
       width = 9, height = 6.4, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "figure6_rolling_beta.pdf"), fig6,
       width = 9, height = 6.4, bg = "white")


# -----------------------------------------------------------------------------
# 4.2  WHY BETA MOVES  (Figure 7 - the decomposition)
# -----------------------------------------------------------------------------
#   beta = rho * (sigma_crypto / sigma_SPX)
# so a beta can rise for two very different reasons, and they carry OPPOSITE
# policy readings:
#   (i)  rho rises  -> crypto is genuinely more integrated. This is contagion risk:
#        losses now arrive at the same moment as losses everywhere else.
#   (ii) sigma_SPX falls (a calm equity market) while crypto stays wild -> beta
#        rises mechanically WITHOUT any deepening of the linkage.
# You must separate these before writing Chapter 5, or the entire policy argument
# rests on an arithmetic illusion. This figure does it; no examiner will have
# expected you to check it, and every good one will be pleased you did.

if (!exists("roll_sd")) {
  roll_sd  <- function(x, k) sapply(seq_along(x), function(i)
    if (i < k) NA_real_ else sd(x[(i - k + 1):i], na.rm = TRUE))
  roll_cor <- function(x, y, k) sapply(seq_along(x), function(i)
    if (i < k) NA_real_ else cor(x[(i - k + 1):i], y[(i - k + 1):i], use = "complete.obs"))
}

DEC <- do.call(rbind, lapply(c("btc", "eth"), function(a) {
  rho   <- roll_cor(D[[paste0(a, "_ret")]], D$spx_ret, BETA_WINDOW)
  ratio <- roll_sd(D[[paste0(a, "_ret")]], BETA_WINDOW) / roll_sd(D$spx_ret, BETA_WINDOW)
  data.frame(date = rep(D$date, 3),
             series = ifelse(a == "btc", "Bitcoin", "Ethereum"),
             part = rep(c("Correlation (rho) with the S&P 500",
                          "Volatility ratio: sigma(crypto) / sigma(S&P 500)",
                          "Implied beta = rho x volatility ratio"), each = nrow(D)),
             value = c(rho, ratio, rho * ratio))
}))
DEC <- DEC[!is.na(DEC$value), ]
DEC$part <- factor(DEC$part, levels = c("Correlation (rho) with the S&P 500",
                                        "Volatility ratio: sigma(crypto) / sigma(S&P 500)",
                                        "Implied beta = rho x volatility ratio"))
write.csv(DEC, file.path(TAB_DIR, "beta_decomposition_series.csv"), row.names = FALSE)

fig7 <- ggplot(DEC, aes(date, value, colour = series)) +
  geom_hline(yintercept = 0, colour = "grey65", linewidth = 0.3) +
  geom_line(linewidth = 0.45) +
  facet_wrap(~ part, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = PAL) + x_years +
  labs(title = "Figure 7  Why the beta moves: correlation, or relative volatility?",
       subtitle = "A beta can rise because crypto is more integrated (top), or merely because equities went quiet (middle)",
       y = NULL,
       caption = paste0("Panel B, ", BETA_WINDOW, "-day windows. If the beta in Figure 6 rises while the top panel is flat,\nthe rise is arithmetic, not economic - and the policy claim must be softened accordingly.")) +
  theme_diss() + theme(strip.text = element_text(face = "bold", size = 9.5))

ggsave(file.path(FIG_DIR, "figure7_beta_decomposition.png"), fig7,
       width = 9, height = 7.5, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "figure7_beta_decomposition.pdf"), fig7,
       width = 9, height = 7.5, bg = "white")

# Quantify the decomposition rather than leaving it to the eye.
sayx("\n--- Decomposition: full sample vs trailing 12 months ---")
for (a in c("btc", "eth")) {
  nmA  <- ifelse(a == "btc", "BTC", "ETH")
  rho  <- roll_cor(D[[paste0(a, "_ret")]], D$spx_ret, BETA_WINDOW)
  rat  <- roll_sd(D[[paste0(a, "_ret")]], BETA_WINDOW) / roll_sd(D$spx_ret, BETA_WINDOW)
  rec  <- D$date >= last_yr
  sayx(sprintf("  %s  rho:   full %.3f -> trailing 12m %.3f   (%+.3f)",
               nmA, mean(rho, na.rm = TRUE), mean(rho[rec], na.rm = TRUE),
               mean(rho[rec], na.rm = TRUE) - mean(rho, na.rm = TRUE)))
  sayx(sprintf("  %s  ratio: full %.3f -> trailing 12m %.3f   (%+.3f)",
               nmA, mean(rat, na.rm = TRUE), mean(rat[rec], na.rm = TRUE),
               mean(rat[rec], na.rm = TRUE) - mean(rat, na.rm = TRUE)))
}
sayx("  [Read] Whichever component moved more is the one your Chapter 5 argument")
sayx("         must be built on. If it is the volatility ratio, be explicit that the")
sayx("         higher beta reflects a calm equity market as much as a hot crypto one.")


# -----------------------------------------------------------------------------
# 4.3  TABLE 6 - CALENDAR-YEAR BETAS
# -----------------------------------------------------------------------------
# The un-mineable split. Nobody can accuse you of choosing 2025 because it gave a
# big number: the boundary is 1 January. Report N per year - 2026 is a part-year
# and its standard error will be wider; say so rather than hoping nobody checks.

sayx("\n", strrep("=", 78))
sayx("TABLE 6  Equity beta by calendar year (same specification as Table 3)")
sayx(strrep("=", 78))
sayx(sprintf("  %-6s %-5s %6s %9s %9s %8s %9s", "Year", "Asset", "N", "beta", "HAC SE", "t", "p"))

D$yr <- as.integer(format(D$date, "%Y"))
t6 <- NULL
for (y in sort(unique(D$yr))) {
  Dy <- D[D$yr == y, ]
  if (nrow(Dy) < 40) next
  for (a in c("btc", "eth")) {
    f <- as.formula(sprintf("%s_ret ~ spx_ret + dvix + %s_lag", a, a))
    m <- ols_hac(f, Dy)
    sayx(sprintf("  %-6d %-5s %6d %9.4f %9.4f %8.3f %9.4f %s", y, toupper(a), m$n,
                 m$coef[["spx_ret"]], m$se[["spx_ret"]], m$t[["spx_ret"]],
                 m$p[["spx_ret"]], stars(m$p[["spx_ret"]])))
    t6 <- rbind(t6, data.frame(Year = y, Asset = toupper(a), N = m$n,
                               Beta = round(m$coef[["spx_ret"]], 4),
                               HAC_SE = round(m$se[["spx_ret"]], 4),
                               t_stat = round(m$t[["spx_ret"]], 3),
                               p_value = round(m$p[["spx_ret"]], 4),
                               Sig = stars(m$p[["spx_ret"]]),
                               dVIX = round(m$coef[["dvix"]], 5),
                               Adj_R2 = round(m$adj_r2, 4), stringsAsFactors = FALSE))
  }
}
write.csv(t6, file.path(TAB_DIR, "table6_annual_betas.csv"), row.names = FALSE)
sayx("\n  [Read] Read DOWN the beta column. A monotone climb is the strongest single")
sayx("         piece of evidence you can put in front of the FPC. A jagged path with")
sayx("         one high year is an episode, not a trend - and you must say which.")


# -----------------------------------------------------------------------------
# 4.4  TABLE 7 - DOWNSIDE ASYMMETRY  (the highest-value four lines in this script)
# -----------------------------------------------------------------------------
#     r_i,t = a + b1 r_SPX,t + b_dn (Dneg_t x r_SPX,t) + c Dneg_t + b2 dVIX_t + g r_i,t-1
#     Dneg_t = 1 if r_SPX,t < 0
# Beta on UP days   = b1
# Beta on DOWN days = b1 + b_dn
#
# WHY IT MATTERS MORE THAN ANYTHING ELSE HERE: the FPC does not care about average
# co-movement. It cares about co-movement CONDITIONAL ON STRESS - because that is
# when a leveraged holder is forced to sell, and when correlations that were
# comfortable become fatal. An average beta of 0.70 that becomes 1.20 on down days
# is a fundamentally more dangerous asset than a constant beta of 0.85, even though
# the unconditional statistics look almost identical. This single coefficient is
# the difference between a merit and a distinction in Chapter 5.

D$neg <- as.integer(D$spx_ret < 0)
sayx("\n", strrep("=", 78))
sayx("TABLE 7  Downside asymmetry: is the equity beta larger when equities FALL?")
sayx(strrep("=", 78))
sayx(sprintf("  Down days: %d of %d (%.1f%%)", sum(D$neg), nrow(D), 100 * mean(D$neg)))

t7 <- NULL
for (a in c("btc", "eth")) {
  nmA <- toupper(a)
  f <- as.formula(sprintf(
    "%s_ret ~ spx_ret + I(neg*spx_ret) + neg + dvix + %s_lag", a, a))
  m  <- ols_hac(f, D)
  bu <- m$coef[["spx_ret"]]; bd <- m$coef[["I(neg * spx_ret)"]]
  print_model(m, sprintf("%s with downside interaction", nmA))
  sayx(sprintf("  -> beta on UP days   = %.3f", bu))
  sayx(sprintf("  -> beta on DOWN days = %.3f  (difference %+.3f, HAC t = %.2f, p = %.4f %s)",
               bu + bd, bd, m$t[["I(neg * spx_ret)"]], m$p[["I(neg * spx_ret)"]],
               stars(m$p[["I(neg * spx_ret)"]])))
  t7 <- rbind(t7, tidy_model(m, paste0("Table 7 - ", nmA)))
}
write.csv(t7, file.path(TAB_DIR, "table7_downside_asymmetry.csv"), row.names = FALSE)
sayx("\n  [Read] A POSITIVE, significant interaction = crypto falls harder with equities")
sayx("         than it rises with them. That is asymmetric contagion, and it maps")
sayx("         straight onto FPC Channel 1 (systemic institutions) and Channel 2")
sayx("         (core markets): losses concentrate exactly when balance sheets are")
sayx("         least able to absorb them. If it is insignificant, that is ALSO a")
sayx("         finding - it means the risk is symmetric and the average beta is")
sayx("         sufficient for supervisory purposes. Either way, you must report it.")


# -----------------------------------------------------------------------------
# 4.5  TABLE 8 - EXTENDED REGIMES: THE ETF ERA, SPLIT
# -----------------------------------------------------------------------------
# Table 4's single ETF dummy covers Jan-2024 to now - long enough to average away
# any trend inside it. Here the ETF era is broken into calendar years (the same
# un-mineable rule as Table 6) and the joint Wald re-run. If the delta on the most
# recent year is large and significant while the earlier ones are not, then the
# Part 3 conclusion ("integration exists but is stable") was an artefact of coarse
# binning, and Table 8 - not Table 4 - is your headline. Report BOTH and explain
# why they differ. Showing the examiner the reconciliation is worth more marks
# than quietly deleting the table that disagrees with you.

pb_years <- sort(unique(as.integer(format(D$date[D$date >= ETF_S], "%Y"))))
D$RG_TIGHT2 <- as.integer(D$date >= TIGHT_S & D$date < ETF_S)
for (y in pb_years) {
  D[[paste0("RG_E", y)]] <- as.integer(D$date >= ETF_S &
                                       as.integer(format(D$date, "%Y")) == y)
}
EYR   <- paste0("RG_E", pb_years)
EINT  <- paste0("I(", EYR, " * spx_ret)")
ALLI  <- c("I(RG_COVID * spx_ret)", "I(RG_TIGHT2 * spx_ret)", EINT)

f8 <- function(y, yl) as.formula(paste0(
  y, " ~ spx_ret + dvix + ", yl,
  " + I(RG_COVID*spx_ret) + I(RG_TIGHT2*spx_ret) + ",
  paste(sprintf("I(%s*spx_ret)", EYR), collapse = " + "),
  " + RG_COVID + RG_TIGHT2 + ", paste(EYR, collapse = " + ")))

sayx("\n", strrep("=", 78))
sayx("TABLE 8  Regime interactions with the ETF era split by calendar year")
sayx("         Baseline (omitted) = 2020-01-01 to 2022-03-15, ex-COVID.")
sayx(strrep("=", 78))

t8 <- NULL
for (a in c("btc", "eth")) {
  nmA <- toupper(a)
  m   <- ols_hac(f8(paste0(a, "_ret"), paste0(a, "_lag")), D)
  print_model(m, sprintf("%s, extended regimes", nmA))
  b1 <- m$coef[["spx_ret"]]
  sayx("  implied beta by regime:")
  sayx(sprintf("    baseline      %.3f", b1))
  sayx(sprintf("    COVID         %.3f", b1 + m$coef[["I(RG_COVID * spx_ret)"]]))
  sayx(sprintf("    tightening    %.3f", b1 + m$coef[["I(RG_TIGHT2 * spx_ret)"]]))
  for (i in seq_along(pb_years))
    sayx(sprintf("    ETF era %d  %.3f", pb_years[i], b1 + m$coef[[EINT[i]]]))
  w <- wald_hac(m, ALLI)
  sayx(sprintf("  Joint Wald, H0: all deltas = 0 -> W = %.3f (df = %d), p = %.4f  %s",
               w[["W"]], w[["df"]], w[["p"]],
               ifelse(w[["p"]] < .05, "REJECT constant beta (H3 SUPPORTED)",
                      "cannot reject constant beta")))
  # Is the most recent year different from the FIRST ETF year? The trend test.
  if (length(EINT) >= 2) {
    b_first <- m$coef[[EINT[1]]]; b_last <- m$coef[[EINT[length(EINT)]]]
    idx <- c(EINT[1], EINT[length(EINT)])
    Vs  <- m$vcov[idx, idx]
    dif <- b_last - b_first
    sed <- sqrt(Vs[1, 1] + Vs[2, 2] - 2 * Vs[1, 2])
    td  <- dif / sed
    sayx(sprintf("  Trend test, beta(%d) - beta(%d) = %+.3f, HAC t = %.2f, p = %.4f %s",
                 pb_years[length(pb_years)], pb_years[1], dif, td,
                 2 * pnorm(-abs(td)), stars(2 * pnorm(-abs(td)))))
  }
  t8 <- rbind(t8, tidy_model(m, paste0("Table 8 - ", nmA)))
}
write.csv(t8, file.path(TAB_DIR, "table8_extended_regimes.csv"), row.names = FALSE)


# -----------------------------------------------------------------------------
# 4.6  TABLE 9 - THE DIAGNOSTICS AN EXAMINER WILL ASK FOR
# -----------------------------------------------------------------------------

sayx("\n", strrep("=", 78))
sayx("TABLE 9  Diagnostics")
sayx(strrep("=", 78))

## (i) ADF stationarity ------------------------------------------------------
# Augmented Dickey-Fuller with drift:  dx_t = a + rho*x_{t-1} + sum_j c_j dx_{t-j}
# H0: rho = 0 (a unit root). The test statistic is the t-ratio on x_{t-1}, but it
# does NOT have a t distribution, so it is compared with MacKinnon critical values.
# Lag order is chosen by AIC ON A FIXED SAMPLE (all candidate p estimated on the
# same rows) - comparing AICs across different sample sizes is a common and silent
# error. Returns are stationary by construction; this table exists so the examiner
# does not have to take that on trust.
adf_drift <- function(x, max_lag = NULL) {
  x <- as.numeric(x[!is.na(x)]); n <- length(x)
  if (is.null(max_lag)) max_lag <- floor(12 * (n / 100)^0.25)
  dx <- diff(x); xl <- head(x, -1); m <- length(dx)
  Lg <- sapply(seq_len(max_lag), function(j) c(rep(NA, j), head(dx, m - j)))
  full <- data.frame(dx = dx, xl = xl, Lg)
  keep <- complete.cases(full); full <- full[keep, ]      # fixed sample, all p
  best <- NULL
  for (p in 0:max_lag) {
    dfm <- full[, c("dx", "xl", if (p > 0) paste0("X", seq_len(p)))]
    fit <- lm(dx ~ ., data = dfm)
    a   <- AIC(fit)
    if (is.null(best) || a < best$aic) best <- list(aic = a, fit = fit, p = p)
  }
  tau <- summary(best$fit)$coefficients["xl", "t value"]
  cv  <- c(`1%` = -3.43, `5%` = -2.86, `10%` = -2.57)     # MacKinnon, constant, no trend
  c(tau = tau, lags = best$p, cv5 = cv[["5%"]],
    reject5 = as.numeric(tau < cv[["5%"]]))
}
sayx("\n(i) ADF unit-root tests (constant, no trend; AIC lag choice on a fixed sample)")
sayx(sprintf("    %-28s %9s %6s %9s  %s", "Series", "ADF tau", "lags", "5% crit", "Verdict"))
adf_set <- list("BTC return"    = D$btc_ret, "ETH return" = D$eth_ret,
                "S&P 500 return" = D$spx_ret, "dVIX"      = D$dvix,
                "VIX level (contrast)" = D$vix_close)
t9a <- NULL
for (nm in names(adf_set)) {
  r <- adf_drift(adf_set[[nm]])
  v <- if (r[["reject5"]] == 1) "reject unit root -> stationary" else "CANNOT reject unit root"
  sayx(sprintf("    %-28s %9.3f %6d %9.2f  %s", nm, r[["tau"]], r[["lags"]], r[["cv5"]], v))
  t9a <- rbind(t9a, data.frame(Test = "ADF (drift)", Series = nm,
                               Statistic = round(r[["tau"]], 4), Lags = r[["lags"]],
                               Crit_5pct = r[["cv5"]], Verdict = v, stringsAsFactors = FALSE))
}
sayx("    [Read] All four regression variables should reject. The VIX LEVEL row is")
sayx("           deliberately included as a contrast: if it fails to reject, that is")
sayx("           precisely why the model uses dVIX and not VIX. Say that in Ch. 3.")

## (ii) VIF -------------------------------------------------------------------
# r_SPX and dVIX correlate at about -0.79 (Table 2), which LOOKS alarming. VIF
# turns the worry into a number: VIF = 1/(1-R2_j) from regressing each regressor on
# the others. Rule of thumb: >10 is a problem, >5 warrants comment. Anything near
# 2.6 is a non-issue - but the examiner does not know that until you show them, and
# an unanswered collinearity question in a viva is a bad place to be.
vif_calc <- function(fit) {
  X <- model.matrix(fit)[, -1, drop = FALSE]
  sapply(colnames(X), function(v) {
    o <- setdiff(colnames(X), v)
    1 / (1 - summary(lm(X[, v] ~ X[, o, drop = FALSE]))$r.squared)
  })
}
sayx("\n(ii) Variance inflation factors, Table 3 specification")
t9b <- NULL
for (a in c("btc", "eth")) {
  m <- if (a == "btc") m3_btc else m3_eth
  v <- vif_calc(m$fit)
  for (k in names(v)) {
    sayx(sprintf("     %-5s %-12s VIF = %5.2f  %s", toupper(a), k, v[[k]],
                 ifelse(v[[k]] > 10, "<- SEVERE", ifelse(v[[k]] > 5, "<- comment on this", "ok"))))
    t9b <- rbind(t9b, data.frame(Test = "VIF", Series = paste(toupper(a), k),
                                 Statistic = round(v[[k]], 4), Lags = NA,
                                 Crit_5pct = NA,
                                 Verdict = ifelse(v[[k]] > 5, "investigate", "no collinearity problem"),
                                 stringsAsFactors = FALSE))
  }
}

## (iii) Drop dVIX ------------------------------------------------------------
# The collinearity answer that actually persuades: if beta barely moves when the
# collinear regressor is removed, collinearity was never inflating anything.
sayx("\n(iii) Beta with dVIX removed (the direct collinearity check)")
t9c <- NULL
for (a in c("btc", "eth")) {
  m_with <- if (a == "btc") m3_btc else m3_eth
  m_wo   <- ols_hac(as.formula(sprintf("%s_ret ~ spx_ret + %s_lag", a, a)), D)
  sayx(sprintf("     %-5s beta with dVIX = %.4f (SE %.4f) | without dVIX = %.4f (SE %.4f) | change %+.4f",
               toupper(a), m_with$coef[["spx_ret"]], m_with$se[["spx_ret"]],
               m_wo$coef[["spx_ret"]], m_wo$se[["spx_ret"]],
               m_wo$coef[["spx_ret"]] - m_with$coef[["spx_ret"]]))
  t9c <- rbind(t9c, tidy_model(m_wo, paste0("No-dVIX - ", toupper(a))))
}
write.csv(t9c, file.path(TAB_DIR, "table9c_no_dvix_spec.csv"), row.names = FALSE)

## (iv) Newey-West lag sensitivity -------------------------------------------
# The HAC lag is a choice, and any choice is an attack surface. Show that the
# inference does not depend on it. The coefficient CANNOT change (HAC touches only
# the standard errors) - so this table is about the t-statistics, and that is
# exactly the point to make in the text.
sayx("\n(iv) Newey-West lag sensitivity (the coefficient cannot move; only the SE can)")
Lstar <- m3_btc$lag
Lset  <- sort(unique(pmax(0, c(Lstar - 2, Lstar, Lstar + 2, 10, 20))))
sayx(sprintf("     Rule-of-thumb lag L* = %d", Lstar))
sayx(sprintf("     %-5s %5s %10s %10s %9s", "Asset", "L", "beta", "HAC SE", "t"))
t9d <- NULL
for (a in c("btc", "eth")) {
  f <- as.formula(sprintf("%s_ret ~ spx_ret + dvix + %s_lag", a, a))
  for (L in Lset) {
    m <- ols_hac(f, D, L = L)
    sayx(sprintf("     %-5s %5d %10.4f %10.4f %9.3f %s", toupper(a), L,
                 m$coef[["spx_ret"]], m$se[["spx_ret"]], m$t[["spx_ret"]],
                 stars(m$p[["spx_ret"]])))
    t9d <- rbind(t9d, data.frame(Asset = toupper(a), NW_lag = L,
                                 Beta = round(m$coef[["spx_ret"]], 4),
                                 HAC_SE = round(m$se[["spx_ret"]], 4),
                                 t_stat = round(m$t[["spx_ret"]], 3),
                                 p_value = round(m$p[["spx_ret"]], 4),
                                 stringsAsFactors = FALSE))
  }
}
write.csv(rbind(t9a, t9b), file.path(TAB_DIR, "table9_stationarity_vif.csv"), row.names = FALSE)
write.csv(t9d, file.path(TAB_DIR, "table9d_nw_lag_sensitivity.csv"), row.names = FALSE)

## (v) The joint-vs-individual tension in Table 4 ------------------------------
# Do not let the examiner find this before you do. In Table 4 the BTC tightening
# interaction is individually significant while the 3-restriction joint Wald is
# not. That is not a contradiction: a joint test spreads its power across all
# restrictions, so when only one binds, it can fail to reject while the single
# coefficient rejects easily. State it, explain it, move on. Ignoring it looks
# like you did not notice; explaining it looks like you know what a Wald test is.
sayx("\n(v) Reconciling the joint and individual tests in Table 4")
for (nm in c("BTC", "ETH")) {
  m <- if (nm == "BTC") m4_btc else m4_eth
  w <- wald_hac(m, INTER)
  sig <- names(which(m$p[INTER] < 0.05))
  sayx(sprintf("     %s: joint Wald p = %.4f; individually significant interactions: %s",
               nm, w[["p"]], if (length(sig)) paste(sig, collapse = ", ") else "none"))
}
sayx("     [Read] If a single interaction is significant but the joint test is not,")
sayx("            the joint test is simply under-powered against a one-regime shift.")
sayx("            Report both. Never report only the one that suits the argument.")

writeLines(EXT, file.path(TAB_DIR, "extensions_report.txt"))

# --- Preview -----------------------------------------------------------------
graphics.off(); invisible(gc())
for (p in list(fig6, fig7)) try(print(p), silent = TRUE)

sayx("\n", strrep("=", 78))
sayx("PART 4 COMPLETE")
sayx(strrep("=", 78))
sayx("  figures/figure6_rolling_beta.*         <- the exhibit Chapter 4 is built on")
sayx("  figures/figure7_beta_decomposition.*   <- proves the beta move is economic, not arithmetic")
sayx("  tables/table6_annual_betas.csv         <- the un-mineable split")
sayx("  tables/table7_downside_asymmetry.csv   <- the FPC's actual question")
sayx("  tables/table8_extended_regimes.csv     <- Table 4, done properly")
sayx("  tables/table9*.csv                     <- ADF, VIF, NW-lag, no-dVIX")
sayx("  tables/extensions_report.txt           <- READ THIS ONE FIRST")
writeLines(EXT, file.path(TAB_DIR, "extensions_report.txt"))

} else if (RUN_EXTENSIONS) {
  cat("\nPART 4 skipped: it needs PART 3 in the same session.",
      "\n  Set RUN_REGRESSIONS <- TRUE and re-source.\n")
} else cat("\nPART 4 skipped (RUN_EXTENSIONS = FALSE).\n")


# =============================================================================
# DONE
# =============================================================================
cat("\n", strrep("=", 78), "\n", sep = "")
cat("FINISHED.\n\n")
cat("  Figures  -> the Plots pane (use the arrows), and figures/\n")
cat("  Tables   -> tables/  (regression_report.txt and extensions_report.txt are the readable ones)\n")
cat("  Data     -> data/clean/\n\n")
cat("READ THESE THREE FILES BEFORE YOU WRITE ANYTHING:\n")
cat("  1. data/clean/reconciliation_report.txt - the lead-lag scan MUST peak at\n")
cat("     lag 0 for both coins. If it does not, every correlation and beta above\n")
cat("     is shifted by a day and the results are worthless. Tell your supervisor.\n")
cat("  2. tables/regression_report.txt - Tables 1-5. The AVERAGE picture.\n")
cat("  3. tables/extensions_report.txt - Tables 6-9 and Figures 6-7. The TRAJECTORY,\n")
cat("     the downside asymmetry, and the diagnostics. This is where the 85% lives.\n\n")
cat("Every date marked [verify] in PART 0 still needs a primary source.\n")
cat(strrep("=", 78), "\n")
