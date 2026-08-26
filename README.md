# Volatility and Pricing Dynamics of Cryptocurrencies

Reproducibility code for the MSc dissertation *Volatility and Pricing Dynamics of Cryptocurrencies: Crypto–Equity Integration as a Standing, Non-Monotonic Exposure* (MSc Financial Technology and Innovation, Bayes Business School, 2026).

## Overview

This repository contains the full analysis pipeline used in the dissertation. All tables, figures, and coefficient estimates reported in the dissertation are reproducible from the scripts and data here. The empirical work estimates HAC-robust OLS pricing regressions of Bitcoin and Ethereum daily returns on the S&P 500, testing whether crypto–equity integration follows a secular trend, a stress-activated switch, or a non-monotonic structure.

## Contents

| File | Description |
|------|-------------|
| `panel_b_aligned.csv` | The estimation dataset: 1,638 daily observations of BTC, ETH, S&P 500, Nasdaq, FTSE 100 and VIX returns, aligned to the US equity trading calendar (2 Jan 2020 – 10 Jul 2026). |
| `dissertation_pipeline.R` | Master pipeline: data construction, the hand-rolled Newey–West HAC estimator (`ols_hac`), and the primary regressions. |
| `part5_state_and_trend.R` | The state, trend, threshold-sweep, lagged-state, cross-asset, and out-of-sample holdout specifications (Chapter 4). |
| `make_tables.R` | Generates the nine result tables as CSV files. |
| `make_all_figures.R` | Generates the five figures. |

## How to reproduce

1. Install [R](https://www.r-project.org/) (version 4.0 or later) and the `ggplot2` package: `install.packages("ggplot2")`.
2. Clone or download this repository.
3. In R, set the working directory to the repository folder.
4. Source the master pipeline: `source("dissertation_pipeline.R")` (this builds the estimation sample `D` and the `ols_hac` function). Confirm `nrow(D)` returns 1638.
5. In the same session, source the additional scripts as needed:
   - `source("part5_state_and_trend.R")` — the state, trend and robustness tests
   - `source("make_tables.R")` — writes the tables to `tables/`
   - `source("make_all_figures.R")` — writes the figures to `figures/`

## Method note

The HAC estimator uses a Bartlett kernel with automatic lag length L = ⌊4(T/100)^(2/9)⌋ = 7 and a finite-sample n/(n−k) adjustment, and is cross-validated against R's `sandwich::NeweyWest` to machine precision.

## Author

Osaetin Emovon, MSc Financial Technology and Innovation, Bayes Business School (City St George's, University of London).
