########################################################################
# LP-IV SUBSAMPLE COMPARISON - Time-cluster Block Bootstrap
# PARSIMONIOUS SPECIFICATION:
# - No currency controls
# - Crisis dummy lags reduced from 4 to 2
# - Block bootstrap (block length = 12 months)
# Pre-financialization (1994-2007) vs Post-financialization (2010-2025)
########################################################################

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sandwich)
library(lmtest)
library(purrr)
library(lubridate)

input  <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\02 Data\\DTA"
output <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\04 Output - figures and tables\\04 bootstrapping\\"

if (!dir.exists(output)) dir.create(output, recursive = TRUE)

########################################################################
# LOAD DATA
########################################################################

df <- read_dta(paste0(input, "\\master_panel_gs1.dta"))

df <- df %>%
  mutate(
    date    = as.Date("1960-01-01") %m+% months(as.integer(date)),
    date_ym = format(date, "%Y-%m")
  )

if (!"d_gs1" %in% names(df)) stop("d_gs1 not found")
cat("d_gs1 found. Non-NA obs:", sum(!is.na(df$d_gs1)), "\n")
cat("Unique commodities:", paste(unique(df$commodity), collapse = ", "), "\n")
cat("Date range:", format(min(df$date)), "to", format(max(df$date)), "\n")

df <- df %>%
  mutate(
    gfc     = as.integer(date >= as.Date("2008-09-01") &
                           date <= as.Date("2009-06-01")),
    covid   = as.integer(date >= as.Date("2020-03-01") &
                           date <= as.Date("2021-06-01")),
    ukraine = as.integer(date >= as.Date("2022-02-01") &
                           date <= as.Date("2022-12-01"))
  )

########################################################################
# PARAMETERS
########################################################################

lags         <- 4       # kept at 4 for comparability with baseline
crisis_lags  <- 2       # reduced from 4
block_length <- 12      # block bootstrap block length in months
breps        <- 500
horizon      <- 24

commodities <- c("Coffee", "Copper", "Gold", "Oil", "Soybeans", "Wheat")

# No currency controls in parsimonious specification
crisis_vars <- list(
  Coffee   = c("gfc", "covid"),
  Copper   = c("gfc", "covid"),
  Gold     = c("gfc", "covid"),
  Oil      = c("gfc", "covid", "ukraine"),
  Soybeans = c("gfc", "covid", "ukraine"),
  Wheat    = c("gfc", "covid", "ukraine")
)

pre_start  <- as.Date("1994-01-01")
pre_end    <- as.Date("2007-12-01")
post_start <- as.Date("2010-01-01")
post_end   <- as.Date("2025-12-01")

########################################################################
# PRE-COMPUTE LAGS
# Currency lags removed — only price, macro, and crisis lags needed
########################################################################

cat("=== Pre-computing lags ===\n")

df <- df %>%
  arrange(commodity, date) %>%
  group_by(commodity) %>%
  mutate(
    lag1_dlp       = dplyr::lag(d_log_price, 1),
    lag2_dlp       = dplyr::lag(d_log_price, 2),
    lag3_dlp       = dplyr::lag(d_log_price, 3),
    lag4_dlp       = dplyr::lag(d_log_price, 4),
    lag1_ip_growth = dplyr::lag(ip_growth,   1),
    lag2_ip_growth = dplyr::lag(ip_growth,   2),
    lag3_ip_growth = dplyr::lag(ip_growth,   3),
    lag4_ip_growth = dplyr::lag(ip_growth,   4),
    lag1_inflation = dplyr::lag(inflation,   1),
    lag2_inflation = dplyr::lag(inflation,   2),
    lag3_inflation = dplyr::lag(inflation,   3),
    lag4_inflation = dplyr::lag(inflation,   4),
    # Crisis dummies — only 2 lags
    lag0_gfc       = gfc,
    lag1_gfc       = dplyr::lag(gfc,         1),
    lag2_gfc       = dplyr::lag(gfc,         2),
    lag0_covid     = covid,
    lag1_covid     = dplyr::lag(covid,       1),
    lag2_covid     = dplyr::lag(covid,       2),
    lag0_ukraine   = ukraine,
    lag1_ukraine   = dplyr::lag(ukraine,     1),
    lag2_ukraine   = dplyr::lag(ukraine,     2)
  ) %>%
  ungroup()

cat("Pre-computation done. Columns:", ncol(df), "\n")

########################################################################
# FIRST STAGE
########################################################################

cat("=== Running first stages ===\n")

macro_controls <- c(
  paste0("lag", 1:lags, "_ip_growth"),
  paste0("lag", 1:lags, "_inflation")
)

fs_formula <- paste(
  "d_gs1 ~ shock +",
  paste(macro_controls, collapse = " + ")
)

run_first_stage <- function(data, start, end, label) {
  
  d_ts <- data %>%
    filter(date >= start, date <= end) %>%
    arrange(date) %>%
    group_by(date) %>%
    slice(1) %>%
    ungroup() %>%
    filter(!is.na(d_gs1), !is.na(shock)) %>%
    drop_na(any_of(macro_controls))
  
  if (nrow(d_ts) < 10) stop(paste("Too few time-series obs for", label))
  
  fit <- lm(as.formula(fs_formula), data = d_ts)
  
  cat("\n--- First Stage:", label, "---\n")
  cat("N:", nrow(d_ts), "\n")
  cat("F-statistic:", round(summary(fit)$fstatistic[1], 2), "\n")
  cat("R-squared:",   round(summary(fit)$r.squared, 4), "\n")
  cat("shock coef:",  round(coef(fit)["shock"], 4),
      "| t-stat:", round(summary(fit)$coefficients["shock", "t value"], 2), "\n")
  
  d_ts$d_gs1_hat <- fitted(fit)
  return(d_ts %>% select(date, d_gs1_hat))
}

fs_pre  <- run_first_stage(df, pre_start,  pre_end,  "Pre  (1994-2007)")
fs_post <- run_first_stage(df, post_start, post_end, "Post (2010-2025)")

df <- df %>%
  left_join(fs_pre  %>% rename(d_gs1_hat_pre  = d_gs1_hat), by = "date") %>%
  left_join(fs_post %>% rename(d_gs1_hat_post = d_gs1_hat), by = "date")

cat("\nFirst stage fitted values merged.\n")

########################################################################
# SECOND STAGE HELPER
# Parsimonious: no currency controls, crisis_lags = 2
########################################################################

run_second_stage <- function(data, commodity_name, h,
                             crisis_v, sub_start, sub_end,
                             fitted_col) {
  
  d <- data %>%
    filter(commodity == commodity_name,
           date >= sub_start,
           date <= sub_end) %>%
    arrange(date)
  
  dep_var <- paste0("dep_h", h)
  if (!dep_var %in% names(d))        return(NULL)
  if (nrow(d) < 10)                  return(NULL)
  if (!fitted_col %in% names(d))     return(NULL)
  if (all(is.na(d[[fitted_col]])))   return(NULL)
  
  d$d_gs1_hat <- d[[fitted_col]]
  
  # Core controls — no currency
  core_controls <- c(
    paste0("lag", 1:lags, "_dlp"),
    paste0("lag", 1:lags, "_ip_growth"),
    paste0("lag", 1:lags, "_inflation")
  )
  
  # Crisis dummies with reduced lags
  crisis_controls <- c()
  for (cv in crisis_v) {
    crisis_controls <- c(crisis_controls,
                         paste0("lag", 0:crisis_lags, "_", cv))
  }
  
  full_controls <- c(core_controls, crisis_controls)
  
  missing_cols <- setdiff(
    c(dep_var, "d_gs1_hat", full_controls),
    names(d)
  )
  if (length(missing_cols) > 0) {
    cat("Missing columns for", commodity_name, "h=", h, ":",
        paste(missing_cols, collapse = ", "), "\n")
    return(NULL)
  }
  
  d <- d %>% drop_na(any_of(c(dep_var, "d_gs1_hat", full_controls)))
  if (nrow(d) < 10) return(NULL)
  
  ss_formula <- paste(
    dep_var, "~",
    paste(c("d_gs1_hat", full_controls), collapse = " + ")
  )
  
  tryCatch({
    fit <- lm(as.formula(ss_formula), data = d)
    bw  <- max(1, h)
    nw  <- NeweyWest(fit, lag = bw, prewhite = FALSE)
    ct  <- coeftest(fit, vcov = nw)
    return(list(beta = ct["d_gs1_hat", "Estimate"], n = nrow(d)))
  }, error = function(e) NULL)
}

########################################################################
# STEP 1: POINT ESTIMATES
########################################################################

cat("\n=== Computing point estimates ===\n")

point_estimates <- list()

for (c in commodities) {
  cat("  Commodity:", c, "\n")
  for (h in 0:horizon) {
    
    res_pre <- run_second_stage(
      data           = df,
      commodity_name = c,
      h              = h,
      crisis_v       = crisis_vars[[c]],
      sub_start      = pre_start,
      sub_end        = pre_end,
      fitted_col     = "d_gs1_hat_pre"
    )
    
    res_post <- run_second_stage(
      data           = df,
      commodity_name = c,
      h              = h,
      crisis_v       = crisis_vars[[c]],
      sub_start      = post_start,
      sub_end        = post_end,
      fitted_col     = "d_gs1_hat_post"
    )
    
    if (!is.null(res_pre) & !is.null(res_post)) {
      point_estimates[[paste(c, h, sep = "_")]] <- data.frame(
        commodity = c,
        horizon   = h,
        beta_pre  = res_pre$beta,
        beta_post = res_post$beta,
        beta_diff = res_post$beta - res_pre$beta,
        n_pre     = res_pre$n,
        n_post    = res_post$n
      )
    }
  }
}

point_df <- bind_rows(point_estimates)
cat("Point estimates computed:", nrow(point_df), "rows\n")
print(head(point_df))

########################################################################
# STEP 2: BLOCK BOOTSTRAP
# Resample contiguous blocks of months separately for each subsample
########################################################################

cat("\n=== Running block bootstrap (B =", breps,
    ", block =", block_length, "months) ===\n")

dates_pre  <- df %>%
  filter(date >= pre_start,  date <= pre_end)  %>%
  pull(date) %>% unique() %>% sort()

dates_post <- df %>%
  filter(date >= post_start, date <= post_end) %>%
  pull(date) %>% unique() %>% sort()

nT_pre  <- length(dates_pre)
nT_post <- length(dates_post)

cat("Pre-period unique dates:",  nT_pre,  "\n")
cat("Post-period unique dates:", nT_post, "\n")

# ---- Block resampling helper ----
resample_blocks <- function(dates, n_obs, block_len) {
  
  # Starting indices for valid blocks
  valid_starts <- 1:(n_obs - block_len + 1)
  n_blocks     <- ceiling(n_obs / block_len)
  
  # Sample block starting indices
  chosen_starts <- sample(valid_starts, n_blocks, replace = TRUE)
  
  # Expand each block into its constituent dates
  boot_dates <- map(chosen_starts, \(s) {
    end_idx <- min(s + block_len - 1, n_obs)
    dates[s:end_idx]
  }) %>%
    unlist() %>%
    as.Date(origin = "1970-01-01") %>%
    head(n_obs)   # trim to exact length
  
  boot_dates
}

boot_results <- list()

for (b in 1:breps) {
  
  if (b %% 50 == 0) cat("  Replication", b, "of", breps, "\n")
  
  # Resample blocks for each subsample
  boot_dates_pre  <- resample_blocks(dates_pre,  nT_pre,  block_length)
  boot_dates_post <- resample_blocks(dates_post, nT_post, block_length)
  
  # Build bootstrap datasets
  boot_df_pre <- map_dfr(seq_along(boot_dates_pre), \(i) {
    df %>%
      filter(date == boot_dates_pre[i]) %>%
      mutate(draw_id = i)
  })
  
  boot_df_post <- map_dfr(seq_along(boot_dates_post), \(i) {
    df %>%
      filter(date == boot_dates_post[i]) %>%
      mutate(draw_id = i)
  })
  
  for (c in commodities) {
    for (h in 0:horizon) {
      
      res_pre <- run_second_stage(
        data           = boot_df_pre,
        commodity_name = c,
        h              = h,
        crisis_v       = crisis_vars[[c]],
        sub_start      = pre_start,
        sub_end        = pre_end,
        fitted_col     = "d_gs1_hat_pre"
      )
      
      res_post <- run_second_stage(
        data           = boot_df_post,
        commodity_name = c,
        h              = h,
        crisis_v       = crisis_vars[[c]],
        sub_start      = post_start,
        sub_end        = post_end,
        fitted_col     = "d_gs1_hat_post"
      )
      
      if (!is.null(res_pre) & !is.null(res_post)) {
        boot_results[[length(boot_results) + 1]] <- data.frame(
          commodity   = c,
          horizon     = h,
          rep         = b,
          beta_pre_b  = res_pre$beta,
          beta_post_b = res_post$beta,
          beta_diff_b = res_post$beta - res_pre$beta
        )
      }
    }
  }
}

boot_df_results <- bind_rows(boot_results)
cat("Bootstrap results:", nrow(boot_df_results), "rows\n")

if (nrow(boot_df_results) == 0) {
  stop("Bootstrap produced no results")
}

########################################################################
# STEP 3: CONFIDENCE BANDS
########################################################################

cat("=== Computing confidence bands ===\n")

boot_bands <- boot_df_results %>%
  group_by(commodity, horizon) %>%
  summarise(
    nvalid       = n(),
    lower_diff90 = quantile(beta_diff_b, 0.05,  na.rm = TRUE),
    lower_diff68 = quantile(beta_diff_b, 0.16,  na.rm = TRUE),
    upper_diff68 = quantile(beta_diff_b, 0.84,  na.rm = TRUE),
    upper_diff90 = quantile(beta_diff_b, 0.95,  na.rm = TRUE),
    lower_pre90  = quantile(beta_pre_b,  0.05,  na.rm = TRUE),
    lower_pre68  = quantile(beta_pre_b,  0.16,  na.rm = TRUE),
    upper_pre68  = quantile(beta_pre_b,  0.84,  na.rm = TRUE),
    upper_pre90  = quantile(beta_pre_b,  0.95,  na.rm = TRUE),
    lower_post90 = quantile(beta_post_b, 0.05,  na.rm = TRUE),
    lower_post68 = quantile(beta_post_b, 0.16,  na.rm = TRUE),
    upper_post68 = quantile(beta_post_b, 0.84,  na.rm = TRUE),
    upper_post90 = quantile(beta_post_b, 0.95,  na.rm = TRUE),
    .groups = "drop"
  )

########################################################################
# STEP 4: MERGE AND SAVE
########################################################################

final_df <- point_df %>%
  left_join(boot_bands, by = c("commodity", "horizon"))

write.csv(
  final_df,
  paste0(input, "\\lp_iv_subsample_bootstrap_parsimonious.csv"),
  row.names = FALSE
)

cat("=== Saved lp_iv_subsample_bootstrap_parsimonious.csv ===\n")

########################################################################
# STEP 5: SAMPLE SIZE REPORT
# Check degrees of freedom gained vs original specification
########################################################################

cat("\n=== Sample size and regressor count check ===\n")

for (c in commodities) {
  n_crisis  <- length(crisis_vars[[c]]) * (crisis_lags + 1)
  n_core    <- lags * 3          # dlp + ip_growth + inflation
  n_total   <- 1 + n_core + n_crisis + 1   # intercept + core + crisis + d_gs1_hat
  cat(c, "— regressors:", n_total,
      "| pre obs at h=24 approx:", nT_pre - 24 - lags,
      "| post obs at h=24 approx:", nT_post - 24 - lags, "\n")
}

cat("\n=== Done ===\n")

########### PLOT

# ---- Load new parsimonious results ----
boot <- read.csv(paste0(input, "\\lp_iv_subsample_bootstrap_parsimonious.csv"))

# ---- Reuse existing plotting code ----

# Difference patched figure (all commodities)
y_min_diff <- boot %>%
  filter(commodity %in% commodities_boot) %>%
  pull(lower_diff90) %>% min(na.rm = TRUE)

y_max_diff <- boot %>%
  filter(commodity %in% commodities_boot) %>%
  pull(upper_diff90) %>% max(na.rm = TRUE)

y_pad_diff <- (y_max_diff - y_min_diff) * 0.05
y_lim_diff <- c(y_min_diff - y_pad_diff, y_max_diff + y_pad_diff)

p_gold_d     <- make_boot_panel(boot, "Gold",     type = "diff",
                                show_y_label = TRUE,  y_lim = y_lim_diff) +
  labs(subtitle = "Gold")
p_copper_d   <- make_boot_panel(boot, "Copper",   type = "diff",
                                show_y_label = FALSE, y_lim = y_lim_diff) +
  labs(subtitle = "Copper")
p_oil_d      <- make_boot_panel(boot, "Oil",      type = "diff",
                                show_y_label = FALSE, y_lim = y_lim_diff) +
  labs(subtitle = "Oil")
p_soybeans_d <- make_boot_panel(boot, "Soybeans", type = "diff",
                                show_y_label = TRUE,  y_lim = y_lim_diff) +
  labs(subtitle = "Soybeans")
p_wheat_d    <- make_boot_panel(boot, "Wheat",    type = "diff",
                                show_y_label = FALSE, y_lim = y_lim_diff) +
  labs(subtitle = "Wheat")

combined_diff <- (p_gold_d | p_copper_d | p_oil_d) /
  (p_soybeans_d | p_wheat_d | plot_spacer()) +
  plot_annotation(
    title    = "Change in Monetary Policy Transmission: Post minus Pre Financialization",
    subtitle = "Negative values indicate stronger negative price response post-financialization",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 13,
                                   margin = margin(b = 4)),
      plot.subtitle = element_text(size = 10, color = "grey40",
                                   margin = margin(b = 10))
    )
  )

ggsave(
  filename = paste0(fig_path_boot, "diff only\\irf_boot_diff_patched_parsimonious.png"),
  plot     = combined_diff,
  width    = 15,
  height   = 10,
  dpi      = 300
)

# Individual triple plots (pre | post | diff)
walk(commodities_boot, \(c) {
  p <- make_boot_triple(boot, c)
  if (!is.null(p)) {
    ggsave(
      filename = paste0(fig_path_boot, "irf_boot_", c,
                        "_triple_parsimonious.png"),
      plot     = p,
      width    = 15,
      height   = 5,
      dpi      = 300
    )
    cat("Saved:", c, "\n")
  }
})

cat("=== Parsimonious bootstrap plots saved ===\n")