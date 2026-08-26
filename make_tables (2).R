# =============================================================================
#   TABLES — reproduces the nine dissertation exhibits in R
#   Requires the master pipeline sourced first (uses ols_hac and D).
#   Writes tables/table1.csv ... table9.csv, ready to paste into Word.
#   Each table also prints to console so you can eyeball it.
# =============================================================================

TAB_DIR <- "tables"
if (!dir.exists(TAB_DIR)) dir.create(TAB_DIR)

fmt  <- function(x, d = 3) formatC(x, format = "f", digits = d)
star <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", "")))
wr   <- function(df, n) { write.csv(df, file.path(TAB_DIR, n), row.names = FALSE)
                          cat("\n===", n, "===\n"); print(df, row.names = FALSE) }

# helper: pull a coef row as "coef (se), t=.., p"
cell <- function(m, term) sprintf("%s (%s), t=%s%s",
          fmt(m$coef[[term]]), fmt(m$se[[term]]), fmt(m$t[[term]], 2), star(m$p[[term]]))

# -----------------------------------------------------------------------------
# TABLE 1 — Descriptive statistics (both panels)
# -----------------------------------------------------------------------------
# Assumes the pipeline created panel_a (all-days) and D (Panel B aligned).
# If your all-days frame has another name, change PA below.
PA <- if (exists("panel_a")) panel_a else NULL
descr <- function(x, ann) {
  x <- x[is.finite(x)]
  n <- length(x); m <- mean(x); s <- sd(x)
  sk <- mean((x - m)^3)/s^3; ku <- mean((x - m)^4)/s^4 - 3
  jb <- n/6 * (sk^2 + (ku^2)/4)
  data.frame(N = n, Mean = fmt(m,4), SD = fmt(s,4),
             Ann_vol = fmt(s*sqrt(ann),2), Skew = fmt(sk,3),
             Exc_kurt = fmt(ku,2), Jarque_Bera = fmt(jb,0))
}
t1 <- rbind(
  cbind(Series = "BTC (Panel B, aligned)", descr(D$btc_ret, 252)),
  cbind(Series = "ETH (Panel B, aligned)", descr(D$eth_ret, 252)),
  cbind(Series = "S&P 500",                descr(D$spx_ret, 252)),
  cbind(Series = "Nasdaq",                 descr(D$ndx_ret, 252)))
if (!is.null(PA)) {
  t1 <- rbind(
    cbind(Series = "BTC (Panel A, 7-day)", descr(PA$btc_ret, 365)),
    cbind(Series = "ETH (Panel A, 7-day)", descr(PA$eth_ret, 365)),
    t1)
}
wr(t1, "table1_descriptives.csv")

# -----------------------------------------------------------------------------
# TABLE 2 — Correlation matrix (Panel B daily returns)
# -----------------------------------------------------------------------------
cm <- cor(D[, c("btc_ret","eth_ret","spx_ret","ndx_ret","dvix")], use = "complete.obs")
t2 <- as.data.frame(round(cm, 3)); t2 <- cbind(Series = rownames(t2), t2)
wr(t2, "table2_correlations.csv")

# -----------------------------------------------------------------------------
# TABLE 3 — Primary OLS pricing regression + total beta
# -----------------------------------------------------------------------------
t3 <- data.frame()
for (a in c("btc","eth")) {
  m  <- ols_hac(as.formula(sprintf("%s_ret ~ spx_ret + dvix + %s_lag", a, a)), D)
  mt <- ols_hac(as.formula(sprintf("%s_ret ~ spx_ret + %s_lag", a, a)), D)
  t3 <- rbind(t3, data.frame(
    Asset = toupper(a),
    Partial_beta_SPX = cell(m, "spx_ret"),
    dVIX             = cell(m, "dvix"),
    Lagged_return    = cell(m, paste0(a, "_lag")),
    Adj_R2           = fmt(m$adj_r2, 3),
    Total_beta       = sprintf("%s (SE %s)", fmt(mt$coef[["spx_ret"]]), fmt(mt$se[["spx_ret"]]))))
}
wr(t3, "table3_primary.csv")

# -----------------------------------------------------------------------------
# TABLE 4 — Sub-period total betas
# -----------------------------------------------------------------------------
subs <- list(c("2020-01-01","2022-01-01","2020-2021"),
             c("2022-01-01","2024-01-01","2022-2023"),
             c("2024-01-01","2027-01-01","2024-2026"))
t4 <- data.frame()
for (a in c("btc","eth")) {
  row <- list(Asset = toupper(a))
  for (s in subs) {
    Dp <- D[D$date >= as.Date(s[1]) & D$date < as.Date(s[2]), ]
    m <- ols_hac(as.formula(sprintf("%s_ret ~ spx_ret + %s_lag", a, a)), Dp)
    row[[s[3]]] <- sprintf("%s (t=%s)", fmt(m$coef[["spx_ret"]],2), fmt(m$t[["spx_ret"]],2))
  }
  t4 <- rbind(t4, as.data.frame(row, stringsAsFactors = FALSE))
}
wr(t4, "table4_subperiod_betas.csv")

# -----------------------------------------------------------------------------
# TABLE 5 — Trend test with CIs and minimum detectable effect
# -----------------------------------------------------------------------------
D$tyr <- as.numeric(D$date - min(D$date)) / 365.25
span <- max(D$tyr)
t5 <- data.frame()
for (a in c("btc","eth")) {
  D$tX <- D$tyr * D$spx_ret
  m <- ols_hac(as.formula(sprintf("%s_ret ~ spx_ret + tX + tyr + %s_lag", a, a)), D)
  th <- m$coef[["tX"]]; se <- m$se[["tX"]]
  t5 <- rbind(t5, data.frame(
    Asset = toupper(a),
    Trend_per_yr = sprintf("%s (t=%s)", fmt(th,3), fmt(m$t[["tX"]],2)),
    CI95 = sprintf("[%s, %s]", fmt(th-1.96*se,3), fmt(th+1.96*se,3)),
    Cumulative_CI = sprintf("[%s, %s]", fmt((th-1.96*se)*span,2), fmt((th+1.96*se)*span,2)),
    Min_detectable_per_yr = fmt(2.8*se, 3)))
}
D$tX <- NULL
wr(t5, "table5_trend.csv")

# -----------------------------------------------------------------------------
# TABLE 6 — VIX-median state split + threshold sweep
# -----------------------------------------------------------------------------
sweeps <- list(list("median", function(v) v > median(v)),
               list("VIX>20", function(v) v > 20),
               list("VIX>25", function(v) v > 25),
               list("VIX>30", function(v) v > 30),
               list("top quintile", function(v) v > quantile(v,.8)),
               list("top decile", function(v) v > quantile(v,.9)))
t6 <- data.frame()
for (a in c("btc","eth")) {
  for (s in sweeps) {
    D$h <- as.integer(s[[2]](D$vix_close))
    if (sum(D$h) < 30 || sum(D$h) > nrow(D)-30) next
    m <- ols_hac(as.formula(sprintf("%s_ret ~ spx_ret + I(h*spx_ret) + h + %s_lag", a, a)), D)
    b <- m$coef[["spx_ret"]]; d <- m$coef[["I(h * spx_ret)"]]
    t6 <- rbind(t6, data.frame(Asset = toupper(a), Threshold = s[[1]],
      N_hi = sum(D$h), Beta_calm = fmt(b), Beta_stressed = fmt(b+d),
      Delta = fmt(d), t = fmt(m$t[["I(h * spx_ret)"]],2),
      p = fmt(m$p[["I(h * spx_ret)"]],4), Sig = star(m$p[["I(h * spx_ret)"]])))
  }
}
D$h <- NULL
wr(t6, "table6_state_sweep.csv")

# -----------------------------------------------------------------------------
# TABLE 7 — Quartile betas + Q3-vs-Q4 attenuation test
# -----------------------------------------------------------------------------
D$vq <- cut(D$vix_close, breaks = quantile(D$vix_close, c(0,.25,.5,.75,1)),
            labels = c("Q1","Q2","Q3","Q4"), include.lowest = TRUE)
t7 <- data.frame()
for (a in c("btc","eth")) {
  row <- list(Asset = toupper(a))
  for (q in c("Q1","Q2","Q3","Q4")) {
    Dq <- D[D$vq == q, ]
    m <- ols_hac(as.formula(sprintf("%s_ret ~ spx_ret + %s_lag", a, a)), Dq)
    row[[q]] <- fmt(m$coef[["spx_ret"]], 2)
  }
  D$q34 <- ifelse(D$vq=="Q4",1,ifelse(D$vq=="Q3",0,NA)); Ds <- D[!is.na(D$q34),]
  m <- ols_hac(as.formula(sprintf("%s_ret ~ spx_ret + I(q34*spx_ret) + q34 + %s_lag", a, a)), Ds)
  row[["Q3_Q4_attenuation"]] <- sprintf("%s (t=%s, p=%s)",
    fmt(m$coef[["I(q34 * spx_ret)"]]), fmt(m$t[["I(q34 * spx_ret)"]],2), fmt(m$p[["I(q34 * spx_ret)"]],3))
  t7 <- rbind(t7, as.data.frame(row, stringsAsFactors = FALSE))
}
D$q34 <- NULL
wr(t7, "table7_quartiles.csv")

# -----------------------------------------------------------------------------
# TABLE 8 — Holdout + cross-asset restriction
# -----------------------------------------------------------------------------
split <- as.Date("2024-01-01"); Dtr <- D[D$date < split,]; Dte <- D[D$date >= split,]
thr <- median(Dtr$vix_close)
holdout_sets <- list(train = Dtr, test = Dte)
t8a <- data.frame()
for (a in c("btc","eth")) {
  for (partname in names(holdout_sets)) {
    Dp <- holdout_sets[[partname]]; Dp$h <- as.integer(Dp$vix_close > thr)
    m <- ols_hac(as.formula(sprintf("%s_ret ~ spx_ret + I(h*spx_ret) + h + %s_lag", a, a)), Dp)
    t8a <- rbind(t8a, data.frame(Asset = toupper(a), Sample = partname,
      Delta = fmt(m$coef[["I(h * spx_ret)"]]), t = fmt(m$t[["I(h * spx_ret)"]],2),
      p = fmt(m$p[["I(h * spx_ret)"]],4)))
  }
}
wr(t8a, "table8_holdout.csv")
# cross-asset stacked test (delta_BTC = delta_ETH)
st <- rbind(
  data.frame(y=D$btc_ret, spx=D$spx_ret, hi=as.integer(D$vix_close>median(D$vix_close)), lag=D$btc_lag, eth=0, date=D$date),
  data.frame(y=D$eth_ret, spx=D$spx_ret, hi=as.integer(D$vix_close>median(D$vix_close)), lag=D$eth_lag, eth=1, date=D$date))
st <- st[order(st$date),]
mS <- ols_hac(y ~ spx + I(hi*spx) + hi + lag + eth + I(eth*spx) + I(eth*hi*spx) + I(eth*hi) + I(eth*lag), st)
t8b <- data.frame(Test = "H0: delta_BTC = delta_ETH",
  Delta_BTC = fmt(mS$coef[["I(hi * spx)"]]),
  Diff_ETH_minus_BTC = fmt(mS$coef[["I(eth * hi * spx)"]]),
  t = fmt(mS$t[["I(eth * hi * spx)"]],2),
  p = fmt(mS$p[["I(eth * hi * spx)"]],3),
  Verdict = ifelse(mS$p[["I(eth * hi * spx)"]] < .05, "reject equality", "cannot reject equality"))
wr(t8b, "table8b_crossasset.csv")

# -----------------------------------------------------------------------------
# TABLE 9 — Robustness wall
# -----------------------------------------------------------------------------
D$dlvix <- c(NA, diff(log(D$vix_close)))
t9 <- data.frame()
specs <- list(
  list("Baseline (S&P)",  "%s_ret ~ spx_ret + dvix + %s_lag", "spx_ret", D),
  list("Nasdaq factor",   "%s_ret ~ ndx_ret + dvix + %s_lag", "ndx_ret", D),
  list("dlog(VIX)",       "%s_ret ~ spx_ret + dlvix + %s_lag","spx_ret", D[!is.na(D$dlvix),]))
for (a in c("btc","eth")) {
  for (sp in specs) {
    m <- ols_hac(as.formula(sprintf(sp[[2]], a, a)), sp[[4]])
    t9 <- rbind(t9, data.frame(Asset = toupper(a), Spec = sp[[1]],
      Beta = fmt(m$coef[[sp[[3]]]]), t = fmt(m$t[[sp[[3]]]],2),
      p = fmt(m$p[[sp[[3]]]],4), N = m$n))
  }
}
wr(t9, "table9_robustness.csv")

cat("\n\nAll tables written to", TAB_DIR, "/  — nine CSVs ready to paste into Word.\n")
cat("Spot-check against embedded numbers: Table 3 total betas should be ~1.164 / 1.610;\n")
cat("Table 7 quartiles BTC 0.58/0.86/1.61/1.15; Table 6 median deltas +0.46 / +0.31.\n")
