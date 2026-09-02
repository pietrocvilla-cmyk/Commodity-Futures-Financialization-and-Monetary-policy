############# Financialization measure #####################
## This code creates the financialization measures and their descriptive statistics display. 

library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(purrr)
library(zoo)
library(scales)
library(haven)

# Use simple relative path from project root
file_path  <- "Data/Financialization of commodities/Financialization measures 1990-2025.xlsx"
sp500_path <- "Data/S&P 500 time series.xlsx"
comm_path  <- "Data/Commodity prices data - LSEG.xlsx"
fig_path   <- "Output/00 descriptives"
output_path <- "\\DTA\\"

dir.create(fig_path,    showWarnings = FALSE, recursive = TRUE)
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

# Verify we're in the right place
if (!file.exists(file_path)) {
  stop("\n========================================\n",
       "ERROR: Cannot find data file!\n",
       "========================================\n",
       "Expected file: data/Dataset finale.xlsx\n",
       "Current working directory: ", getwd(), "\n\n",
       "SOLUTION:\n",
       "1. Close this script\n",
       "2. In your file explorer, find: .Rproj\n",
       "3. Double-click the .Rproj file to open the project\n",
       "4. Then open and run this script\n",
       "========================================\n")
}

cat("✓ Working directory:", getwd(), "\n")
cat("✓ Data file found:", file_path, "\n\n")
# ---- Sheet names ----
sheets      <- c("Coffee", "Gold", "Crude oil", "Soybean", "Wheat", "Copper")
comm_sheets <- c("Coffee", "Gold", "Oil", "Soybeans", "Wheat", "Copper")

# ---- Rolling window for S&P correlation (months) ----
roll_window <- 24

# ---- Weights (Gold = 0 throughout) ----
w_nc <- c(
  "Coffee"    = 0.20,
  "Gold"      = 0.00,
  "Crude oil" = 0.20,
  "Soybean"   = 0.20,
  "Wheat"     = 0.20,
  "Copper"    = 0.20
)

w_corr <- c(
  "Coffee"   = 0.20,
  "Gold"     = 0.00,
  "Oil"      = 0.20,
  "Soybeans" = 0.20,
  "Wheat"    = 0.20,
  "Copper"   = 0.20
)

# ---- Sample period for .dta export ----
sample_start <- as.Date("1992-01-01")
sample_end   <- as.Date("2025-12-31")

###############################################################################
# HELPERS
###############################################################################

clean_names_simple <- function(x) {
  x %>%
    str_trim() %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

pick <- function(df, candidates) {
  nms <- names(df)
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0) NA_character_ else hit[1]
}

###############################################################################
# SECTION 1 — NC GROSS SHARE (COT DATA)
###############################################################################

compute_from_sheet <- function(sheet_name) {
  df_raw <- read_excel(file_path, sheet = sheet_name)
  names(df_raw) <- clean_names_simple(names(df_raw))
  
  date_col <- pick(df_raw, c("report_date_as_yyyy_mm_dd", "report_date", "date",
                             "as_of_date_in_form_yyyy_mm_dd"))
  if (is.na(date_col)) stop(paste0("No date column found in sheet: ", sheet_name))
  
  pct_long  <- pick(df_raw, c("pct_of_oi_noncomm_long_all", "pct_of_oi_noncomm_long",
                              "noncommercial_long_pct_of_oi", "non_commercial_long_percent_of_oi",
                              "noncommercial_long_percent_of_oi"))
  pct_short <- pick(df_raw, c("pct_of_oi_noncomm_short_all", "pct_of_oi_noncomm_short",
                              "noncommercial_short_pct_of_oi", "non_commercial_short_percent_of_oi",
                              "noncommercial_short_percent_of_oi"))
  oi_col    <- pick(df_raw, c("open_interest_all", "open_interest", "open_interest_total",
                              "open_interest_oi"))
  nc_long   <- pick(df_raw, c("noncomm_positions_long_all", "noncomm_positions_long",
                              "noncommercial_long", "non_commercial_long", "noncommercial_long_all"))
  nc_short  <- pick(df_raw, c("noncomm_positions_short_all", "noncomm_positions_short",
                              "noncommercial_short", "non_commercial_short", "noncommercial_short_all"))
  
  df <- df_raw %>%
    mutate(
      report_date = suppressWarnings(ymd(.data[[date_col]])),
      report_date = if_else(is.na(report_date),
                            suppressWarnings(as.Date(.data[[date_col]])),
                            report_date)
    )
  
  if (!is.na(pct_long) && !is.na(pct_short)) {
    df <- df %>%
      mutate(
        nc_gross_share = ((as.numeric(.data[[pct_long]]) + as.numeric(.data[[pct_short]])) / 2) / 100
      )
  } else if (!is.na(oi_col) && !is.na(nc_long) && !is.na(nc_short)) {
    df <- df %>%
      mutate(
        oi  = as.numeric(.data[[oi_col]]),
        ncl = as.numeric(.data[[nc_long]]),
        ncs = as.numeric(.data[[nc_short]]),
        nc_gross_share = (ncl + ncs) / (2 * oi)
      )
  } else {
    stop(paste0("Can't compute gross share in sheet '", sheet_name, "'."))
  }
  
  df %>%
    filter(!is.na(report_date), !is.na(nc_gross_share)) %>%
    arrange(report_date) %>%
    mutate(month = floor_date(report_date, "month")) %>%
    group_by(month) %>%
    summarise(nc_gross_share = mean(nc_gross_share, na.rm = TRUE), .groups = "drop") %>%
    transmute(commodity = sheet_name, report_date = month, nc_gross_share)
}

# Build dataset
all_data <- map_dfr(sheets, compute_from_sheet)

# ---- Per-commodity: monthly + 12-month rolling average ----
roll_nc_plots <- all_data %>%
  split(.$commodity) %>%
  map(\(d) {
    d <- d %>%
      arrange(report_date) %>%
      mutate(
        nc_roll12 = rollapply(nc_gross_share, width = 12, FUN = mean,
                              fill = NA, align = "right")
      )
    ggplot(d, aes(x = report_date)) +
      geom_line(aes(y = nc_gross_share, color = "Monthly"),             alpha = 0.4, linewidth = 0.5) +
      geom_line(aes(y = nc_roll12,      color = "12-Month Rolling Avg"), linewidth = 0.9) +
      scale_color_manual(
        name   = NULL,
        values = c("Monthly" = "steelblue", "12-Month Rolling Avg" = "darkblue")
      ) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        title    = paste0(d$commodity[1], ": NC Gross Share of Open Interest"),
        subtitle = "Monthly data vs 12-month rolling average",
        x = NULL, y = "Gross share"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })

walk2(roll_nc_plots, names(roll_nc_plots), \(p, nm) {
  print(p)
  ggsave(
    filename = paste0(fig_path, "nc_gross_share_", str_replace_all(nm, " ", "_"), ".png"),
    plot = p, width = 10, height = 5, dpi = 300
  )
})

# ---- Weighted average NC gross share: monthly + 12-month rolling average ----
missing_w_nc <- setdiff(unique(all_data$commodity), names(w_nc))
if (length(missing_w_nc) > 0) stop(paste("No weight for:", paste(missing_w_nc, collapse = ", ")))
if (abs(sum(w_nc) - 1) > 1e-8) stop(paste("NC weights sum to", sum(w_nc), "not 1."))

weights_nc_df <- tibble(commodity = names(w_nc), weight = as.numeric(w_nc))

wa_nc <- all_data %>%
  inner_join(weights_nc_df, by = "commodity") %>%
  filter(weight > 0) %>%
  group_by(report_date) %>%
  summarise(
    nc_gross_share_wavg = sum(nc_gross_share * weight) / sum(weight),
    n_commodities = n(),
    .groups = "drop"
  ) %>%
  arrange(report_date) %>%
  mutate(
    nc_wavg_roll12 = rollapply(nc_gross_share_wavg, width = 12, FUN = mean,
                               fill = NA, align = "right")
  )

p_nc_wavg <- ggplot(wa_nc, aes(x = report_date)) +
  geom_line(aes(y = nc_gross_share_wavg, color = "Monthly"),             alpha = 0.4, linewidth = 0.5) +
  geom_line(aes(y = nc_wavg_roll12,      color = "12-Month Rolling Avg"), linewidth = 0.9) +
  scale_color_manual(
    name   = NULL,
    values = c("Monthly" = "steelblue", "12-Month Rolling Avg" = "darkblue")
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Weighted Average: NC Gross Share of Open Interest",
    subtitle = "Equal weights across Coffee, Crude Oil, Soybean, Wheat, Copper — monthly vs 12-month rolling avg",
    x = NULL, y = "Weighted average gross share"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_nc_wavg)
ggsave(
  filename = paste0(fig_path, "nc_gross_share_weighted_avg.png"),
  plot = p_nc_wavg, width = 10, height = 5, dpi = 300
)

###############################################################################
# SECTION 2 — S&P 500 ROLLING CORRELATION
###############################################################################

# ---- Load S&P 500 ----
sp_raw <- read_excel(sp500_path, sheet = "1990-2026")
names(sp_raw) <- clean_names_simple(names(sp_raw))

date_col_sp  <- pick(sp_raw, c("date", "report_date", "as_of_date", "time", "period"))
price_col_sp <- pick(sp_raw, c("close", "price", "adj_close", "last", "sp500", "value", "level"))

if (is.na(date_col_sp))  stop("No date column found in S&P 500 sheet.")
if (is.na(price_col_sp)) stop("No price column found in S&P 500 sheet.")

sp500 <- sp_raw %>%
  transmute(
    date  = suppressWarnings(ymd(.data[[date_col_sp]])),
    date  = if_else(is.na(date), as.Date(.data[[date_col_sp]]), date),
    price = as.numeric(.data[[price_col_sp]])
  ) %>%
  filter(!is.na(date), !is.na(price)) %>%
  arrange(date) %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  transmute(month, sp_ret = log(price / lag(price))) %>%
  filter(!is.na(sp_ret))

# ---- Load commodity prices and compute monthly returns ----
load_commodity <- function(sheet_name) {
  df_raw <- read_excel(comm_path, sheet = sheet_name)
  names(df_raw) <- clean_names_simple(names(df_raw))
  
  date_col  <- pick(df_raw, c("date", "report_date", "time", "period", "as_of_date"))
  price_col <- pick(df_raw, c("close", "price", "adj_close", "last", "settle",
                              "settlement", "value", "px_last"))
  
  if (is.na(date_col))  stop(paste0("No date column in sheet: ", sheet_name))
  if (is.na(price_col)) stop(paste0("No price column in sheet: ", sheet_name))
  
  df_raw %>%
    transmute(
      date  = suppressWarnings(ymd(.data[[date_col]])),
      date  = if_else(is.na(date), as.Date(.data[[date_col]]), date),
      price = as.numeric(.data[[price_col]])
    ) %>%
    filter(!is.na(date), !is.na(price)) %>%
    arrange(date) %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    transmute(month, comm_ret = log(price / lag(price)), commodity = sheet_name) %>%
    filter(!is.na(comm_ret))
}

# ---- Compute 24-month rolling correlation per commodity ----
compute_rolling_corr <- function(sheet_name) {
  comm <- load_commodity(sheet_name)
  merged <- inner_join(comm, sp500, by = "month") %>% arrange(month)
  merged %>%
    mutate(
      rolling_corr = rollapply(
        data      = select(., comm_ret, sp_ret),
        width     = roll_window,
        FUN       = function(m) cor(m[, 1], m[, 2], use = "complete.obs"),
        by.column = FALSE,
        fill      = NA,
        align     = "right"
      )
    ) %>%
    filter(!is.na(rolling_corr)) %>%
    transmute(commodity = sheet_name, month, rolling_corr)
}

all_corr <- map_dfr(comm_sheets, compute_rolling_corr)

# ---- Per-commodity correlation plots ----
corr_plots <- all_corr %>%
  split(.$commodity) %>%
  map(\(d) {
    ggplot(d, aes(x = month, y = rolling_corr)) +
      geom_line(color = "#2c7bb6") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      geom_vline(xintercept = as.Date("2009-01-01"),
                 linetype = "dotted", color = "red", linewidth = 0.7) +
      annotate("text", x = as.Date("2009-06-01"), y = -0.85,
               label = "2009", color = "red", size = 3, hjust = 0) +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(
        title = paste0(d$commodity[1], ": 24-Month Rolling Correlation with S&P 500"),
        x = NULL, y = "Rolling correlation"
      ) +
      theme_minimal()
  })

walk2(corr_plots, names(corr_plots), \(p, nm) {
  print(p)
  ggsave(
    filename = paste0(fig_path, "sp500_corr_", str_replace_all(nm, " ", "_"), ".png"),
    plot = p, width = 10, height = 5, dpi = 300
  )
})

# ---- Weighted average correlation ----
missing_w_corr <- setdiff(unique(all_corr$commodity), names(w_corr))
if (length(missing_w_corr) > 0) stop(paste("No weight for:", paste(missing_w_corr, collapse = ", ")))
if (abs(sum(w_corr) - 1) > 1e-8) stop(paste("Corr weights sum to", sum(w_corr), "not 1."))

weights_corr_df <- tibble(commodity = names(w_corr), weight = as.numeric(w_corr))

wa_corr <- all_corr %>%
  inner_join(weights_corr_df, by = "commodity") %>%
  filter(weight > 0) %>%
  group_by(month) %>%
  summarise(
    corr_wavg     = sum(rolling_corr * weight) / sum(weight),
    n_commodities = n(),
    .groups = "drop"
  ) %>%
  arrange(month)

p_corr_wavg <- ggplot(wa_corr, aes(x = month, y = corr_wavg)) +
  geom_line(color = "#2c7bb6", linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = as.Date("2008-07-01"),
             linetype = "dotted", color = "red", linewidth = 0.7) +
  annotate("text", x = as.Date("2008-10-01"), y = -0.85,
           label = "2008", color = "red", size = 3, hjust = 0) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(
    title    = "Weighted Average: 24-Month Rolling Correlation with S&P 500",
    subtitle = "Equal weights across Coffee, Crude Oil, Soybeans, Wheat, Copper",
    x = NULL, y = "Weighted average rolling correlation"
  ) +
  theme_minimal()

print(p_corr_wavg)
ggsave(
  filename = paste0(fig_path, "sp500_corr_weighted_avg.png"),
  plot = p_corr_wavg, width = 10, height = 5, dpi = 300
)

###############################################################################
# SECTION 3 — EXPORT TO .DTA FOR STATA
###############################################################################

# ---- NC gross share (all commodities, long format) ----
all_data %>%
  rename(date = report_date) %>%
  filter(date >= sample_start & date <= sample_end) %>%
  mutate(date = format(date, "%Y-%m-%d")) %>%
  write_dta(paste0(output_path, "nc_gross_share.dta"))

# ---- Weighted average NC gross share ----
wa_nc %>%
  rename(date = report_date) %>%
  filter(date >= sample_start & date <= sample_end) %>%
  mutate(date = format(date, "%Y-%m-%d")) %>%
  select(date, nc_gross_share_wavg) %>%
  write_dta(paste0(output_path, "nc_gross_share_wavg.dta"))

# ---- Rolling S&P 500 correlation (all commodities, long format) ----
all_corr %>%
  rename(date = month) %>%
  filter(date >= sample_start & date <= sample_end) %>%
  mutate(date = format(date, "%Y-%m-%d")) %>%
  write_dta(paste0(output_path, "sp500_corr.dta"))

# ---- Weighted average S&P 500 correlation ----
wa_corr %>%
  rename(date = month) %>%
  filter(date >= sample_start & date <= sample_end) %>%
  mutate(date = format(date, "%Y-%m-%d")) %>%
  select(date, corr_wavg) %>%
  write_dta(paste0(output_path, "sp500_corr_wavg.dta"))

cat("Done\n")
