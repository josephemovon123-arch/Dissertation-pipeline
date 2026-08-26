# =============================================================================
#
#   PART 5 - THE STATE SPECIFICATION, DONE PROPERLY
#   Folds the headline result into the master pipeline and hardens it.
#
#   Requires PART 3 in the same session (reuses ols_hac, hac_vcov, wald_hac,
#   tidy_model, stars, say, and the estimation sample D). Source the main
#   pipeline first with RUN_REGRESSIONS <- TRUE, then source this.
#
#   WHAT IT ADDS
#     5.1  The VIX-median state spec        <- your actual Finding 3, in the pipeline
#     5.2  The explicit trend test          <- H3a, partial AND total beta, + tightening control
#     5.3  Threshold sweep                  <- turns one p=0.038 into a dose-response
#     5.4  Lagged state (VIX at t-1)        <- fixes endogeneity, makes the policy claim operational
#     5.5  Cross-asset test d_BTC = d_ETH   <- the ETH differentiation claim, tested for real
#     5.6  Holdout: fit 2020-23, test 2024-26 <- the answer to the specification-search charge
# =============================================================================

RUN_PART5 <- TRUE

if (RUN_PART5 && exists("ols_hac") && exists("D")) {

P5 <- character(0)
say5 <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); P5 <<- c(P5, m) }

say5("\n", strrep("=", 78))
say5("PART 5  STATE-CONTINGENCY: THE HEADLINE SPECIFICATION AND ITS STRESS TESTS")
say5(strrep("=", 78))

# -----------------------------------------------------------------------------
# 5.1  THE VIX-MEDIAN STATE SPECIFICATION  (Finding 3, now reproducible)
# -----------------------------------------------------------------------------
#     r_i,t = a + b1 r_SPX,t + d (Dhigh_t x r_SPX,t) + c Dhigh_t + g r_i,t-1 + e
#     Dhigh_t = 1 if VIX_t > median(VIX)
# Calm-market beta   = b1
# Stressed-market beta = b1 + d
#
# dVIX is deliberately EXCLUDED here: r_SPX and dVIX correlate at -0.79, so
# holding one dVIX coefficient across both regimes would force regime-varying
# co-movement into the interaction term. State this in the write-up.
#
# NOTE ON THE STATE VARIABLE: this uses the contemporaneous VIX (VIX_t), matching
# how the result was originally produced. Section 5.4 re-runs it with VIX_{t-1};
# report BOTH and lead the policy claim on the lagged version.

D$vix_hi <- as.integer(D$vix_close > median(D$vix_close))

say5("\n--- 5.1  VIX-median state specification (contemporaneous) ---")
say5(sprintf("    Median VIX = %.2f; stressed days = %d of %d (%.1f%%)",
             median(D$vix_close), sum(D$vix_hi), nrow(D), 100 * mean(D$vix_hi)))
t5_state <- NULL
for (a in c("btc", "eth")) {
  nmA <- toupper(a)
  m <- ols_hac(as.formula(sprintf(
    "%s_ret ~ spx_ret + I(vix_hi*spx_ret) + vix_hi + %s_lag", a, a)), D)
  b <- m$coef[["spx_ret"]]; d <- m$coef[["I(vix_hi * spx_ret)"]]
  td <- m$t[["I(vix_hi * spx_ret)"]]; pd <- m$p[["I(vix_hi * spx_ret)"]]
  say5(sprintf("  %s: calm beta = %.3f | stressed beta = %.3f | difference = %+.3f (HAC t = %.2f, p = %.4f %s)",
               nmA, b, b + d, d, td, pd, stars(pd)))
  t5_state <- rbind(t5_state, data.frame(Asset = nmA,
    Beta_calm = round(b, 4), Beta_stressed = round(b + d, 4),
    Delta = round(d, 4), HAC_t = round(td, 3), p_value = round(pd, 4),
    Sig = stars(pd), N = m$n, stringsAsFactors = FALSE))
}
write.csv(t5_state, file.path(TAB_DIR, "table5_1_vix_state.csv"), row.names = FALSE)
say5("\n  [Read] This is the number the dissertation turns on. It must now be")
say5("         reproducible from the pipeline, not from a one-off snippet.")

# -----------------------------------------------------------------------------
# 5.2  THE EXPLICIT TREND TEST  (H3a - partial, total, and tightening-controlled)
# -----------------------------------------------------------------------------
#   Trend model:  r_i,t = a + b1 r_SPX,t + theta (t x r_SPX,t) + lambda t
#                          [+ b2 dVIX_t]  + g r_i,t-1 + e
#   t = years since sample start. H0: theta = 0 (no drift in beta).
# Reported three ways so the write-up can cite t=0.50 and t=-0.02 honestly:
#   (i)  total-beta form (no dVIX)                <- the supervisory quantity
#   (ii) partial-beta form (dVIX in)              <- shows the apparent trend
#   (iii) total-beta form + tightening control    <- drift evaporates
D$tyr <- as.numeric(D$date - min(D$date)) / 365.25

trend_row <- function(a, spec) {
  yl <- paste0(a, "_lag")
  f <- switch(spec,
    total   = sprintf("%s_ret ~ spx_ret + I(tyr*spx_ret) + tyr + %s", a, yl),
    partial = sprintf("%s_ret ~ spx_ret + I(tyr*spx_ret) + tyr + dvix + %s", a, yl),
    control = sprintf("%s_ret ~ spx_ret + I(tyr*spx_ret) + tyr + I(RG_TIGHT*spx_ret) + RG_TIGHT + %s", a, yl))
  m <- ols_hac(as.formula(f), D)
  th <- m$coef[["I(tyr * spx_ret)"]]; tt <- m$t[["I(tyr * spx_ret)"]]; pp <- m$p[["I(tyr * spx_ret)"]]
  data.frame(Asset = toupper(a), Spec = spec,
             Theta_per_year = round(th, 4), HAC_t = round(tt, 3),
             p_value = round(pp, 4), Sig = stars(pp), stringsAsFactors = FALSE)
}
say5("\n--- 5.2  Trend-in-beta test (H3a): theta on (t x r_SPX) ---")
t5_trend <- NULL
for (a in c("btc", "eth")) for (spec in c("total", "partial", "control"))
  t5_trend <- rbind(t5_trend, trend_row(a, spec))
for (i in seq_len(nrow(t5_trend))) say5(sprintf(
  "  %-4s %-8s theta = %+.4f/yr  (HAC t = %6.2f, p = %.4f) %s",
  t5_trend$Asset[i], t5_trend$Spec[i], t5_trend$Theta_per_year[i],
  t5_trend$HAC_t[i], t5_trend$p_value[i], t5_trend$Sig[i]))
write.csv(t5_trend, file.path(TAB_DIR, "table5_2_trend_test.csv"), row.names = FALSE)
say5("\n  [Read] The contrast IS the finding: total-beta theta is ~0 (t~0.5), the")
say5("         partial-beta theta looks like a trend, and the total-beta theta")
say5("         collapses further once tightening gets its own coefficient.")
say5("         Report all three; the partial-vs-total gap is diagnostic, not noise.")

# -----------------------------------------------------------------------------
# 5.3  THRESHOLD SWEEP  (a median split is not 'stress' - show a dose-response)
# -----------------------------------------------------------------------------
# The VIX median over 2020-2026 is ~18, which is a normal day. If state-contingency
# is real, the calm/stressed gap should WIDEN as the threshold rises toward
# genuine stress. Reporting the whole sweep is not p-hacking - it is the opposite:
# it exposes whether the median result is a knife-edge or a robust pattern.
say5("\n--- 5.3  Threshold sweep: does the gap widen with the threshold? ---")
say5(sprintf("  %-5s %-16s %6s %9s %9s %9s %8s %8s", "Asset", "Threshold", "N_hi",
             "beta_calm", "beta_str", "delta", "t", "p"))
sweep_defs <- list(
  list(lab = "median",     fn = function(v) v > median(v)),
  list(lab = "VIX > 20",   fn = function(v) v > 20),
  list(lab = "VIX > 25",   fn = function(v) v > 25),
  list(lab = "VIX > 30",   fn = function(v) v > 30),
  list(lab = "top quintile", fn = function(v) v > quantile(v, 0.80)),
  list(lab = "top decile",   fn = function(v) v > quantile(v, 0.90)))
t5_sweep <- NULL
for (a in c("btc", "eth")) {
  for (s in sweep_defs) {
    D$hi_tmp <- as.integer(s$fn(D$vix_close))
    if (sum(D$hi_tmp) < 30 || sum(D$hi_tmp) > nrow(D) - 30) next
    m <- ols_hac(as.formula(sprintf(
      "%s_ret ~ spx_ret + I(hi_tmp*spx_ret) + hi_tmp + %s_lag", a, a)), D)
    b <- m$coef[["spx_ret"]]; d <- m$coef[["I(hi_tmp * spx_ret)"]]
    say5(sprintf("  %-5s %-16s %6d %9.3f %9.3f %+9.3f %8.2f %8.4f %s",
                 toupper(a), s$lab, sum(D$hi_tmp), b, b + d, d,
                 m$t[["I(hi_tmp * spx_ret)"]], m$p[["I(hi_tmp * spx_ret)"]],
                 stars(m$p[["I(hi_tmp * spx_ret)"]])))
    t5_sweep <- rbind(t5_sweep, data.frame(Asset = toupper(a), Threshold = s$lab,
      N_hi = sum(D$hi_tmp), Beta_calm = round(b, 4), Beta_stressed = round(b + d, 4),
      Delta = round(d, 4), HAC_t = round(m$t[["I(hi_tmp * spx_ret)"]], 3),
      p_value = round(m$p[["I(hi_tmp * spx_ret)"]], 4), stringsAsFactors = FALSE))
  }
}
D$hi_tmp <- NULL
write.csv(t5_sweep, file.path(TAB_DIR, "table5_3_threshold_sweep.csv"), row.names = FALSE)
say5("\n  [Read] If delta rises monotonically down each asset's block, you have a")
say5("         dose-response - far stronger than any single p-value. If it does NOT,")
say5("         you need to know now, and the median result must be reported cautiously.")

# -----------------------------------------------------------------------------
# 5.4  LAGGED STATE  (VIX at t-1: fixes endogeneity, makes the claim operational)
# -----------------------------------------------------------------------------
# A supervisor cannot condition on a state observed simultaneously with the loss.
# Define the state on YESTERDAY'S VIX. If the switch survives, the policy claim
# ("watch the state") is earned; if it weakens, that is a material limitation.
D$vix_lag   <- c(NA_real_, head(D$vix_close, -1))
D$vix_hi_l  <- as.integer(D$vix_lag > median(D$vix_close, na.rm = TRUE))
Dl <- D[!is.na(D$vix_hi_l), ]
say5("\n--- 5.4  State defined on VIX_{t-1} (predetermined) ---")
t5_lag <- NULL
for (a in c("btc", "eth")) {
  m <- ols_hac(as.formula(sprintf(
    "%s_ret ~ spx_ret + I(vix_hi_l*spx_ret) + vix_hi_l + %s_lag", a, a)), Dl)
  b <- m$coef[["spx_ret"]]; d <- m$coef[["I(vix_hi_l * spx_ret)"]]
  say5(sprintf("  %s: calm beta = %.3f | stressed beta = %.3f | difference = %+.3f (HAC t = %.2f, p = %.4f %s)",
               toupper(a), b, b + d, d, m$t[["I(vix_hi_l * spx_ret)"]],
               m$p[["I(vix_hi_l * spx_ret)"]], stars(m$p[["I(vix_hi_l * spx_ret)"]])))
  t5_lag <- rbind(t5_lag, data.frame(Asset = toupper(a), State = "VIX_{t-1}",
    Beta_calm = round(b, 4), Beta_stressed = round(b + d, 4), Delta = round(d, 4),
    HAC_t = round(m$t[["I(vix_hi_l * spx_ret)"]], 3),
    p_value = round(m$p[["I(vix_hi_l * spx_ret)"]], 4), stringsAsFactors = FALSE))
}
write.csv(t5_lag, file.path(TAB_DIR, "table5_4_lagged_state.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# 5.5  CROSS-ASSET TEST  H0: d_BTC = d_ETH  (the differentiation claim, tested)
# -----------------------------------------------------------------------------
# The dissertation argues BTC and ETH need different prudential treatment because
# BTC switches and ETH does not. But "ETH's delta is insignificant" does NOT show
# the two deltas differ. Stack both assets and test the restriction directly.
# The asset returns share time periods, so errors are correlated across the two
# equations - a seemingly-unrelated / stacked GMM structure. Implemented here as a
# stacked OLS with a full HAC covariance and an asset dummy fully interacted, so
# the d_BTC = d_ETH restriction is one linear hypothesis on the stacked model.
say5("\n--- 5.5  Cross-asset test of equal switching: H0: delta_BTC = delta_ETH ---")
stackL <- rbind(
  data.frame(y = D$btc_ret, spx = D$spx_ret, hi = D$vix_hi, lag = D$btc_lag,
             eth = 0, date = D$date),
  data.frame(y = D$eth_ret, spx = D$spx_ret, hi = D$vix_hi, lag = D$eth_lag,
             eth = 1, date = D$date))
stackL <- stackL[order(stackL$date), ]   # order by time so HAC lags run within date
mS <- ols_hac(y ~ spx + I(hi*spx) + hi + lag +
                  eth + I(eth*spx) + I(eth*hi*spx) + I(eth*hi) + I(eth*lag),
              stackL)
# delta_BTC = coef on I(hi*spx); (delta_ETH - delta_BTC) = coef on I(eth*hi*spx).
# So H0: delta_BTC = delta_ETH  <=>  coef on I(eth*hi*spx) = 0.
diff_term <- "I(eth * hi * spx)"
if (diff_term %in% names(mS$coef)) {
  w <- wald_hac(mS, diff_term)
  say5(sprintf("  delta_BTC (I(hi*spx))            = %+.4f", mS$coef[["I(hi * spx)"]]))
  say5(sprintf("  delta_ETH - delta_BTC (interaction) = %+.4f (HAC t = %.2f, p = %.4f)",
               mS$coef[[diff_term]], mS$t[[diff_term]], mS$p[[diff_term]]))
  say5(sprintf("  Wald H0: delta_BTC = delta_ETH -> W = %.3f (df = 1), p = %.4f  %s",
               w[["W"]], w[["p"]],
               ifelse(w[["p"]] < .05,
                      "REJECT equality -> the assets DO switch differently (differentiation supported)",
                      "CANNOT reject equality -> no evidence the assets switch differently")))
  write.csv(tidy_model(mS, "Table 5.5 - stacked cross-asset"),
            file.path(TAB_DIR, "table5_5_cross_asset.csv"), row.names = FALSE)
}
say5("\n  [Read] This decides the Chapter 5 differentiation argument. If you CANNOT")
say5("         reject equality, the honest claim is differentiation by LEVEL")
say5("         (ETH calm beta 1.34 > BTC stressed beta 1.23), not by state-dependence.")

# -----------------------------------------------------------------------------
# 5.6  HOLDOUT  (fit on 2020-2023, test on 2024-2026)
# -----------------------------------------------------------------------------
# H3b was formulated after H3a failed - exploratory by your own declaration. The
# clean answer to "you searched specifications" is out-of-sample confirmation:
# estimate the state split on the early sample only, then ask whether the SAME
# calm/stressed gap appears, untouched, in the later sample.
split_date <- as.Date("2024-01-01")
Dtr <- D[D$date <  split_date, ]
Dte <- D[D$date >= split_date, ]
say5(sprintf("\n--- 5.6  Holdout: train %s..%s (N=%d) | test %s..%s (N=%d) ---",
             min(Dtr$date), max(Dtr$date), nrow(Dtr),
             min(Dte$date), max(Dte$date), nrow(Dte)))
# Freeze the calm/stressed threshold on the TRAINING sample only, then apply it
# unchanged to the test sample - no peeking at test data to set the split.
thr_train <- median(Dtr$vix_close)
say5(sprintf("    Threshold frozen on training data: median VIX = %.2f", thr_train))
t5_hold <- NULL
for (a in c("btc", "eth")) {
  for (part in c("train", "test")) {
    Dp <- if (part == "train") Dtr else Dte
    Dp$hi_f <- as.integer(Dp$vix_close > thr_train)
    if (sum(Dp$hi_f) < 20 || sum(Dp$hi_f) > nrow(Dp) - 20) {
      say5(sprintf("  %s %-5s: too few obs in one regime - skipped", toupper(a), part)); next }
    m <- ols_hac(as.formula(sprintf(
      "%s_ret ~ spx_ret + I(hi_f*spx_ret) + hi_f + %s_lag", a, a)), Dp)
    b <- m$coef[["spx_ret"]]; d <- m$coef[["I(hi_f * spx_ret)"]]
    say5(sprintf("  %s %-5s: calm = %.3f | stressed = %.3f | delta = %+.3f (t = %.2f, p = %.4f %s)",
                 toupper(a), part, b, b + d, d, m$t[["I(hi_f * spx_ret)"]],
                 m$p[["I(hi_f * spx_ret)"]], stars(m$p[["I(hi_f * spx_ret)"]])))
    t5_hold <- rbind(t5_hold, data.frame(Asset = toupper(a), Sample = part,
      Beta_calm = round(b, 4), Beta_stressed = round(b + d, 4), Delta = round(d, 4),
      HAC_t = round(m$t[["I(hi_f * spx_ret)"]], 3),
      p_value = round(m$p[["I(hi_f * spx_ret)"]], 4), N = m$n, stringsAsFactors = FALSE))
  }
}
write.csv(t5_hold, file.path(TAB_DIR, "table5_6_holdout.csv"), row.names = FALSE)
say5("\n  [Read] If the delta has the same sign and rough magnitude in the TEST")
say5("         sample - data never used to form the hypothesis - the specification-")
say5("         search objection is answered as fully as this design allows.")

writeLines(P5, file.path(TAB_DIR, "part5_report.txt"))
say5("\n", strrep("=", 78))
say5("PART 5 COMPLETE - see tables/part5_report.txt and table5_*.csv")
say5(strrep("=", 78))

} else if (RUN_PART5) {
  cat("\nPART 5 skipped: it needs PART 3 in the same session.",
      "\n  Set RUN_REGRESSIONS <- TRUE, source the main pipeline, then source this.\n")
} else cat("\nPART 5 skipped (RUN_PART5 = FALSE).\n")
