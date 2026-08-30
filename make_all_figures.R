# =============================================================================
#   ALL FIVE DISSERTATION FIGURES — one script
#   Requires the master pipeline sourced first (uses ols_hac and D).
#   install.packages("ggplot2") once if needed.
#   Writes 5 PNGs into figures/ with correct, consistent numbering.
#
#     Figure 1 = rolling volatility        (Section 4.1)
#     Figure 2 = rolling equity beta        (Section 4.3)
#     Figure 3 = threshold sweep            (Section 4.4)
#     Figure 4 = in/out-of-sample collapse  (Section 4.4)
#     Figure 5 = inverted-U quartiles       (Section 4.5)
# =============================================================================

library(ggplot2)
FIG_DIR <- "figures"; if (!dir.exists(FIG_DIR)) dir.create(FIG_DIR)
NAVY <- "#1F3A5F"; ORANGE <- "#E8833A"; GREY <- "#8895A7"; LBLUE <- "#5B8DB8"; RED <- "#C0392B"

base_theme <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "#555555", size = 9),
        legend.position = "top", legend.title = element_blank())

# =============================================================================
# FIGURE 1 — 30-day rolling annualised volatility   [Section 4.1]
# =============================================================================
roll_sd <- function(x, w = 30, ann = 252) {
  n <- length(x); out <- rep(NA_real_, n)
  for (i in w:n) out[i] <- sd(x[(i-w+1):i]) * sqrt(ann)
  out
}
voldf <- data.frame(
  date = rep(D$date, 3),
  vol  = c(roll_sd(D$btc_ret), roll_sd(D$eth_ret), roll_sd(D$spx_ret)),
  Asset = rep(c("Bitcoin","Ethereum","S&P 500"), each = nrow(D)))
voldf <- voldf[!is.na(voldf$vol), ]

p1 <- ggplot(voldf, aes(date, vol, color = Asset)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c(Bitcoin = ORANGE, Ethereum = NAVY, "S&P 500" = GREY)) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%")) +
  labs(title = "Figure 1  Crypto volatility towers over equities across the sample",
       subtitle = "30-day rolling annualised volatility, 2020-2026",
       x = NULL, y = "Annualised volatility") +
  base_theme
ggsave(file.path(FIG_DIR, "figure1_rolling_vol.png"), p1, width = 7.6, height = 4.2, dpi = 200)

# =============================================================================
# FIGURE 2 — 90-day rolling equity beta with Newey-West band   [Section 4.3]
# =============================================================================
roll_beta <- function(a, w = 90) {
  n <- nrow(D); b <- se <- rep(NA_real_, n)
  for (i in w:n) {
    Dw <- D[(i-w+1):i, ]
    m <- tryCatch(ols_hac(as.formula(sprintf("%s_ret ~ spx_ret + %s_lag", a, a)), Dw),
                  error = function(e) NULL)
    if (!is.null(m)) { b[i] <- m$coef[["spx_ret"]]; se[i] <- m$se[["spx_ret"]] }
  }
  data.frame(date = D$date, beta = b, se = se, Asset = toupper(a))
}
bdf <- rbind(roll_beta("btc"), roll_beta("eth"))
bdf <- bdf[!is.na(bdf$beta), ]
bdf$Asset <- ifelse(bdf$Asset == "BTC", "Bitcoin", "Ethereum")

p2 <- ggplot(bdf, aes(date, beta, color = Asset, fill = Asset)) +
  geom_ribbon(aes(ymin = beta - 1.96*se, ymax = beta + 1.96*se), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = 1, linetype = "dashed", color = GREY) +
  scale_color_manual(values = c(Bitcoin = ORANGE, Ethereum = NAVY)) +
  scale_fill_manual(values  = c(Bitcoin = ORANGE, Ethereum = NAVY)) +
  labs(title = "Figure 2  The equity beta oscillates between regimes; it does not trend",
       subtitle = "90-day rolling total equity beta with Newey-West 95% band, 2020-2026",
       x = NULL, y = "Rolling equity beta") +
  base_theme
ggsave(file.path(FIG_DIR, "figure2_rolling_beta.png"), p2, width = 7.6, height = 4.2, dpi = 200)

# =============================================================================
# FIGURE 3 — threshold sweep (the switch dissolving)   [Section 4.4]
# =============================================================================
sweeps <- list(list("median (18.9)", function(v) v > median(v)),
               list("VIX>20", function(v) v > 20),
               list("VIX>25", function(v) v > 25),
               list("VIX>30", function(v) v > 30),
               list("top quintile", function(v) v > quantile(v, .8)),
               list("top decile", function(v) v > quantile(v, .9)))
f_sweep <- data.frame()
for (a in c("btc", "eth")) {
  for (i in seq_along(sweeps)) {
    D$h <- as.integer(sweeps[[i]][[2]](D$vix_close))
    m <- ols_hac(as.formula(sprintf("%s_ret ~ spx_ret + I(h*spx_ret) + h + %s_lag", a, a)), D)
    f_sweep <- rbind(f_sweep, data.frame(asset = toupper(a), thr = i, lab = sweeps[[i]][[1]],
                                         delta = m$coef[["I(h * spx_ret)"]],
                                         se = m$se[["I(h * spx_ret)"]]))
  }
}
D$h <- NULL
f_sweep$Asset <- ifelse(f_sweep$asset == "BTC", "Bitcoin", "Ethereum")
f_sweep$lab <- factor(f_sweep$lab, levels = sapply(sweeps, `[[`, 1))

p3 <- ggplot(f_sweep, aes(lab, delta, fill = Asset)) +
  geom_col(position = position_dodge(.8), width = .7) +
  geom_errorbar(aes(ymin = delta - 1.96*se, ymax = delta + 1.96*se),
                position = position_dodge(.8), width = .2, linewidth = .4) +
  geom_hline(yintercept = 0, color = "#333333", linewidth = .5) +
  scale_fill_manual(values = c(Bitcoin = ORANGE, Ethereum = NAVY)) +
  labs(title = "Figure 3  The apparent stress-switch dissolves as the threshold tightens",
       subtitle = "A positive gap at the median reverses to zero or negative once the threshold reaches genuine stress",
       x = "Stress threshold (loose to strict)", y = "Calm-to-stressed beta gap") +
  base_theme
ggsave(file.path(FIG_DIR, "figure3_threshold_sweep.png"), p3, width = 7.2, height = 4.4, dpi = 200)

# =============================================================================
# FIGURE 4 — in-sample vs out-of-sample collapse   [Section 4.4]
# =============================================================================
split <- as.Date("2024-01-01")
Dtr <- D[D$date < split, ]; Dte <- D[D$date >= split, ]
thr <- median(Dtr$vix_close)
state_delta <- function(Dp, a) {
  Dp$h <- as.integer(Dp$vix_close > thr)
  m <- ols_hac(as.formula(sprintf("%s_ret ~ spx_ret + I(h*spx_ret) + h + %s_lag", a, a)), Dp)
  m$coef[["I(h * spx_ret)"]]
}
sign_delta <- function(Dp, a) {
  Dp$n <- as.integer(Dp$spx_ret < 0)
  m <- ols_hac(as.formula(sprintf("%s_ret ~ spx_ret + I(n*spx_ret) + n + dvix + %s_lag", a, a)), Dp)
  m$coef[["I(n * spx_ret)"]]
}
f_oos <- data.frame(
  test = factor(rep(c("BTC\nVIX-state","ETH\nVIX-state","BTC\ndownside","ETH\ndownside"), 2),
                levels = c("BTC\nVIX-state","ETH\nVIX-state","BTC\ndownside","ETH\ndownside")),
  sample = rep(c("In-sample (train <2024)", "Out-of-sample (test >=2024)"), each = 4),
  delta = c(state_delta(Dtr,"btc"), state_delta(Dtr,"eth"), sign_delta(Dtr,"btc"), sign_delta(Dtr,"eth"),
            state_delta(Dte,"btc"), state_delta(Dte,"eth"), sign_delta(Dte,"btc"), sign_delta(Dte,"eth")))

p4 <- ggplot(f_oos, aes(test, delta, fill = sample)) +
  geom_col(position = position_dodge(.8), width = .7) +
  geom_hline(yintercept = 0, color = "#333333", linewidth = .5) +
  scale_fill_manual(values = c("In-sample (train <2024)" = LBLUE, "Out-of-sample (test >=2024)" = RED)) +
  labs(title = "Figure 4  Effects that look robust in-sample vanish or reverse out-of-sample",
       subtitle = "Three interaction tests agree in-sample; none survives the 2024-2026 holdout",
       x = NULL, y = "Estimated stress/downside beta gap") +
  base_theme
ggsave(file.path(FIG_DIR, "figure4_oos_collapse.png"), p4, width = 7.2, height = 4.4, dpi = 200)

# =============================================================================
# FIGURE 5 — the inverted-U (quartile betas with HAC 95% CIs)   [Section 4.5]
# =============================================================================
D$vq <- cut(D$vix_close, breaks = quantile(D$vix_close, c(0, .25, .5, .75, 1)),
            labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)
cuts <- quantile(D$vix_close, c(0, .25, .5, .75, 1))
qlab <- sprintf("%s\nVIX %.0f-%.0f", c("Q1","Q2","Q3","Q4"), cuts[1:4], cuts[2:5])

f_q <- data.frame()
for (a in c("btc", "eth")) {
  for (i in seq_along(levels(D$vq))) {
    q <- levels(D$vq)[i]; Dq <- D[D$vq == q, ]
    m <- ols_hac(as.formula(sprintf("%s_ret ~ spx_ret + %s_lag", a, a)), Dq)
    f_q <- rbind(f_q, data.frame(asset = toupper(a), q = i,
                                 beta = m$coef[["spx_ret"]], se = m$se[["spx_ret"]]))
  }
}
f_q$Asset <- ifelse(f_q$asset == "BTC", "Bitcoin", "Ethereum")

p5 <- ggplot(f_q, aes(q, beta, color = Asset, shape = Asset)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = GREY) +
  geom_errorbar(aes(ymin = beta - 1.96*se, ymax = beta + 1.96*se), width = .12, linewidth = .7) +
  geom_line(linewidth = 1) + geom_point(size = 3) +
  scale_color_manual(values = c(Bitcoin = ORANGE, Ethereum = NAVY)) +
  scale_x_continuous(breaks = 1:4, labels = qlab) +
  labs(title = "Figure 5  Crypto-equity integration is non-monotonic in market stress",
       subtitle = "Beta rises with volatility, peaks in the elevated-but-orderly band, then attenuates in the extreme tail",
       x = "VIX quartile (calm to stressed)", y = "Total equity beta (HAC 95% CI)") +
  base_theme
ggsave(file.path(FIG_DIR, "figure5_inverted_u.png"), p5, width = 7.2, height = 4.4, dpi = 200)

# =============================================================================
cat("\nAll five figures written to", FIG_DIR, "/\n")
cat("  figure1_rolling_vol.png       (Section 4.1)\n")
cat("  figure2_rolling_beta.png      (Section 4.3)\n")
cat("  figure3_threshold_sweep.png   (Section 4.4)\n")
cat("  figure4_oos_collapse.png      (Section 4.4)\n")
cat("  figure5_inverted_u.png        (Section 4.5)\n")
cat("\nQuartile betas (should match BTC 0.58/0.86/1.61/1.15, ETH 0.81/1.65/2.21/1.55):\n")
print(f_q[, c("Asset","q","beta")])
