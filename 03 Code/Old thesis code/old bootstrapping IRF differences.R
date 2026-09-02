########################################################################
# LP SUBSAMPLE COMPARISON - Time-cluster Bootstrap
# Pre-financialization (1994-2003) vs Post-financialization (2010-2025)
#08-04-2026
########################################################################

# Load packages
library(haven)        # read .dta files
library(dplyr)        # data manipulation
library(tidyr)        # reshaping
library(ggplot2)      # plotting
library(sandwich)     # Newey-West SE
library(lmtest)       # coeftest
library(purrr)        # map functions
library(lubridate) 

# Set paths
input  <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\02 Data\\DTA"
output <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\04 Output - figures and tables\\"
########################################################################
# LOAD DATA
########################################################################

df <- read_dta(paste0(input, "\\master_panel.dta"))

# Convert Stata monthly date to R date
# Stata monthly date = months since January 1960
df <- df %>%
  mutate(
    date = as.Date("1960-01-01") %m+% months(as.integer(date)),
    date_ym = format(date, "%Y-%m")
  )

# Define subsamples
df <- df %>%
  mutate(
    subsample_use = case_when(
      date < as.Date("2004-01-01") ~ 1,   # pre
      date >= as.Date("2010-01-01") ~ 2,  # post
      TRUE ~ 0                             # transition — excluded
    ),
    post       = as.integer(date >= as.Date("2010-01-01")),
    shock_post = shock * post,
    gfc        = as.integer(date >= as.Date("2008-09-01") & 
                              date <= as.Date("2009-06-01")),
    covid      = as.integer(date >= as.Date("2020-03-01") & 
                              date <= as.Date("2021-06-01")),
    ukraine    = as.integer(date >= as.Date("2022-02-01") & 
                              date <= as.Date("2022-12-01"))
  )

########################################################################
# PARAMETERS
########################################################################

lags     <- 4
horizon  <- 24
breps    <- 500
commodities <- c("Coffee", "Copper", "Gold", "Oil", "Soybeans", "Wheat")

curr_controls <- list(
  Coffee   = "d_brl",
  Copper   = "d_clp",
  Gold     = "d_aud",
  Oil      = NULL,
  Soybeans = "d_brl",
  Wheat    = NULL
)

crisis_vars <- list(
  Coffee   = c("gfc", "covid"),
  Copper   = c("gfc", "covid"),
  Gold     = c("gfc", "covid"),
  Oil      = c("gfc", "covid", "ukraine"),
  Soybeans = c("gfc", "covid", "ukraine"),
  Wheat    = c("gfc", "covid", "ukraine")
)

########################################################################
# HELPER FUNCTION: build formula and run newey-west regression
########################################################################

run_lp <- function(data, commodity_name, h, lags, 
                   curr_control, crisis_v) {
  
  # Filter to commodity and estimation sample
  d <- data %>%
    filter(commodity == commodity_name, subsample_use != 0) %>%
    arrange(date)
  
  # Dependent variable
  dep_var <- paste0("dep_h", h)
  
  if (!dep_var %in% names(d)) return(NULL)
  
  # Build regressors
  # Lags of d_log_price
  for (l in 1:lags) {
    d[[paste0("lag", l, "_dlp")]] <- dplyr::lag(d$d_log_price, l)
  }
  
  # Macro lags
  for (v in c("ip_growth", "inflation")) {
    for (l in 1:lags) {
      d[[paste0("lag", l, "_", v)]] <- dplyr::lag(d[[v]], l)
    }
  }
  
  # Currency lags
  if (!is.null(curr_control)) {
    for (l in 1:lags) {
      d[[paste0("lag", l, "_curr")]] <- dplyr::lag(d[[curr_control]], l)
    }
  }
  
  # Crisis lags (l = 0 to lags)
  for (cv in crisis_v) {
    for (l in 0:lags) {
      d[[paste0("lag", l, "_", cv)]] <- dplyr::lag(d[[cv]], l)
    }
  }
  
  # Drop NAs
  d <- d %>% drop_na()
  
  if (nrow(d) < 10) return(NULL)
  
  # Build formula
  rhs_vars <- c(
    "shock", "shock_post", "post",
    paste0("lag", 1:lags, "_dlp"),
    paste0("lag", 1:lags, "_ip_growth"),
    paste0("lag", 1:lags, "_inflation")
  )
  
  if (!is.null(curr_control)) {
    rhs_vars <- c(rhs_vars, paste0("lag", 1:lags, "_curr"))
  }
  
  for (cv in crisis_v) {
    rhs_vars <- c(rhs_vars, paste0("lag", 0:lags, "_", cv))
  }
  
  formula_str <- paste(dep_var, "~", paste(rhs_vars, collapse = " + "))
  
  # Run OLS
  tryCatch({
    fit <- lm(as.formula(formula_str), data = d)
    
    # Newey-West SE with bandwidth max(1,h)
    bw  <- max(1, h)
    nw  <- NeweyWest(fit, lag = bw, prewhite = FALSE)
    ct  <- coeftest(fit, vcov = nw)
    
    # Extract coefficients
    beta_pre  <- ct["shock", "Estimate"]
    beta_diff <- ct["shock_post", "Estimate"]
    beta_post <- beta_pre + beta_diff
    
    return(list(
      beta_pre  = beta_pre,
      beta_post = beta_post,
      beta_diff = beta_diff,
      n         = nrow(d)
    ))
  }, error = function(e) NULL)
}

########################################################################
# STEP 1: POINT ESTIMATES
########################################################################

cat("=== Computing point estimates ===\n")

point_estimates <- list()

for (c in commodities) {
  cat("  Commodity:", c, "\n")
  for (h in 0:horizon) {
    res <- run_lp(
      data          = df,
      commodity_name = c,
      h             = h,
      lags          = lags,
      curr_control  = curr_controls[[c]],
      crisis_v      = crisis_vars[[c]]
    )
    if (!is.null(res)) {
      point_estimates[[paste(c, h, sep = "_")]] <- data.frame(
        commodity = c,
        horizon   = h,
        beta_pre  = res$beta_pre,
        beta_post = res$beta_post,
        beta_diff = res$beta_diff,
        nobs      = res$n
      )
    }
  }
}

point_df <- bind_rows(point_estimates)

########################################################################
# STEP 2: TIME-CLUSTER BOOTSTRAP
########################################################################

cat("=== Running bootstrap (B =", breps, ") ===\n")

# Get unique dates in estimation sample
dates_pool <- df %>%
  filter(subsample_use != 0) %>%
  pull(date) %>%
  unique() %>%
  sort()

nT <- length(dates_pool)

boot_results <- list()

for (b in 1:breps) {
  
  if (b %% 50 == 0) cat("  Replication", b, "of", breps, "\n")
  
  # Draw dates with replacement
  boot_dates <- sample(dates_pool, nT, replace = TRUE)
  
  # Build bootstrap dataset — retain all commodities for each drawn date
  boot_df <- map_dfr(seq_along(boot_dates), function(i) {
    df %>%
      filter(date == boot_dates[i]) %>%
      mutate(draw_id = i)
  })
  
  # For each commodity and horizon
  for (c in commodities) {
    for (h in 0:horizon) {
      
      res <- run_lp(
        data           = boot_df,
        commodity_name = c,
        h              = h,
        lags           = lags,
        curr_control   = curr_controls[[c]],
        crisis_v       = crisis_vars[[c]]
      )
      
      if (!is.null(res)) {
        boot_results[[length(boot_results) + 1]] <- data.frame(
          commodity   = c,
          horizon     = h,
          rep         = b,
          beta_pre_b  = res$beta_pre,
          beta_post_b = res$beta_post,
          beta_diff_b = res$beta_diff
        )
      }
    }
  }
}

boot_df_results <- bind_rows(boot_results)

########################################################################
# STEP 3: COMPUTE BOOTSTRAP CONFIDENCE BANDS
########################################################################

cat("=== Computing confidence bands ===\n")

boot_bands <- boot_df_results %>%
  group_by(commodity, horizon) %>%
  summarise(
    nvalid       = n(),
    # Difference bands
    lower_diff90 = quantile(beta_diff_b, 0.05,  na.rm = TRUE),
    lower_diff68 = quantile(beta_diff_b, 0.16,  na.rm = TRUE),
    upper_diff68 = quantile(beta_diff_b, 0.84,  na.rm = TRUE),
    upper_diff90 = quantile(beta_diff_b, 0.95,  na.rm = TRUE),
    # Pre bands
    lower_pre90  = quantile(beta_pre_b,  0.05,  na.rm = TRUE),
    lower_pre68  = quantile(beta_pre_b,  0.16,  na.rm = TRUE),
    upper_pre68  = quantile(beta_pre_b,  0.84,  na.rm = TRUE),
    upper_pre90  = quantile(beta_pre_b,  0.95,  na.rm = TRUE),
    # Post bands
    lower_post90 = quantile(beta_post_b, 0.05,  na.rm = TRUE),
    lower_post68 = quantile(beta_post_b, 0.16,  na.rm = TRUE),
    upper_post68 = quantile(beta_post_b, 0.84,  na.rm = TRUE),
    upper_post90 = quantile(beta_post_b, 0.95,  na.rm = TRUE),
    .groups = "drop"
  )

########################################################################
# STEP 4: MERGE POINT ESTIMATES AND BANDS
########################################################################

final_df <- point_df %>%
  left_join(boot_bands, by = c("commodity", "horizon"))

write.csv(final_df, paste0(input, "lp_subsample_bootstrap.csv"), 
          row.names = FALSE)
cat("=== lp_subsample_bootstrap.csv saved ===\n")

########################################################################
# STEP 5: PLOT
########################################################################

cat("=== Plotting ===\n")

plot_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey85", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 13),
    legend.position  = "bottom",
    axis.title       = element_text(size = 11)
  )

for (c in commodities) {
  
  d <- final_df %>% filter(commodity == c)
  
  # Pre-financialization IRF
  p_pre <- ggplot(d, aes(x = horizon)) +
    geom_ribbon(aes(ymin = lower_pre90, ymax = upper_pre90),
                fill = "blue", alpha = 0.15) +
    geom_ribbon(aes(ymin = lower_pre68, ymax = upper_pre68),
                fill = "blue", alpha = 0.30) +
    geom_line(aes(y = beta_pre), color = "blue", linewidth = 0.8) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 24, 4)) +
    labs(
      title    = paste0(c, ": IRF Pre-Financialization (1994-2003)"),
      x        = "Months after shock",
      y        = "Cumulative log price change",
      caption  = "Time-cluster bootstrap, B=500\nBands = 16th-84th (68%) and 5th-95th (90%) percentiles"
    ) +
    plot_theme
  
  ggsave(paste0(output,"\\shock IRF bootstrapping\\irf_", c, "_pre_boot.png"), 
         p_pre, width = 10, height = 6, dpi = 200)
  
  # Post-financialization IRF
  p_post <- ggplot(d, aes(x = horizon)) +
    geom_ribbon(aes(ymin = lower_post90, ymax = upper_post90),
                fill = "red", alpha = 0.15) +
    geom_ribbon(aes(ymin = lower_post68, ymax = upper_post68),
                fill = "red", alpha = 0.30) +
    geom_line(aes(y = beta_post), color = "red", linewidth = 0.8) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 24, 4)) +
    labs(
      title    = paste0(c, ": IRF Post-Financialization (2010-2025)"),
      x        = "Months after shock",
      y        = "Cumulative log price change",
      caption  = "Time-cluster bootstrap, B=500\nBands = 16th-84th (68%) and 5th-95th (90%) percentiles"
    ) +
    plot_theme
  
  ggsave(paste0(output,"\\shock IRF bootstrapping\\irf_", c, "_post_boot.png"), 
         p_post, width = 10, height = 6, dpi = 200)
  
  # Difference IRF
  p_diff <- ggplot(d, aes(x = horizon)) +
    geom_ribbon(aes(ymin = lower_diff90, ymax = upper_diff90),
                fill = "darkgreen", alpha = 0.15) +
    geom_ribbon(aes(ymin = lower_diff68, ymax = upper_diff68),
                fill = "darkgreen", alpha = 0.30) +
    geom_line(aes(y = beta_diff), color = "darkgreen", linewidth = 0.8) +
    geom_hline(yintercept = 0, color = "black") +
    scale_x_continuous(breaks = seq(0, 24, 4)) +
    labs(
      title    = paste0(c, ": Difference in IRF (Post minus Pre)"),
      x        = "Months after shock",
      y        = "Difference in cumulative log price change",
      caption  = "Negative = stronger negative response post-financialization\nTime-cluster bootstrap, B=500, bands = 16th-84th (68%) and 5th-95th (90%) percentiles"
    ) +
    plot_theme
  
  ggsave(paste0(output,"\\shock IRF bootstrapping\\irf_", c, "_diff_boot.png"), 
         p_diff, width = 10, height = 6, dpi = 200)
  
  cat("  Saved:", c, "pre, post, diff plots\n")
}

cat("=== Done ===\n")