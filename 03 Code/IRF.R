############# Plots #####################
#Plot IRFs for the thesis analysis
# Sheets: "Coffee", "Gold", "Crude oil", "Soybean", "Wheat", "Copper"
############# Interaction IRF Plots #####################

library(haven)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(patchwork)

# ---- Paths ----
data_path <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\02 Data\\DTA\\"
fig_path  <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\04 Output - figures and tables\\05 Nice R IRF graphs\\"

dir.create(fig_path, showWarnings = FALSE, recursive = TRUE)

# ---- Load results ----
interaction <- read_dta(paste0(data_path, "lp_iv_gs1_interaction_ma12.dta"))

# ---- Check variable names ----
cat("Variables in interaction file:\n")
print(names(interaction))

# ---- Settings ----
commodities <- c("Coffee", "Copper", "Gold", "Oil", "Soybeans", "Wheat")

fin_labels <- c(
  "nc_gs_ma12"   = "NC Gross Share 12-Month MA (CFTC)",
  "rolling_corr" = "S&P 500 Rolling Correlation (24M)"
)

# ---- Theme ----
theme_irf <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title        = element_text(face = "bold", size = 12,
                                       margin = margin(b = 8)),
      plot.subtitle     = element_text(size = 20, face = "bold.italic",
                                       color = "grey25", hjust = 0),
      axis.title        = element_text(size = 20),
      axis.text         = element_text(size = 20),
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(color = "grey92"),
      plot.margin       = margin(10, 15, 10, 10)
    )
}

# ---- Single panel builder ----
make_panel <- function(d, subtitle_text, show_y_label = TRUE, y_lim = NULL) {
  
  p <- ggplot(d, aes(x = horizon)) +
    
    # Vertical reference at horizon 0
    geom_vline(xintercept = 0,
               color = "grey70", linewidth = 0.3, linetype = "solid") +
    
    # 90% CI
    geom_ribbon(aes(ymin = lower_int90, ymax = upper_int90),
                fill = "#d73027", alpha = 0.15) +
    
    # 68% CI
    geom_ribbon(aes(ymin = lower_int68, ymax = upper_int68),
                fill = "#d73027", alpha = 0.30) +
    
    # IRF line — smoothed
    geom_smooth(aes(y = beta_int),
                method  = "loess",
                span    = 0.3,
                color   = "#d73027",
                linewidth = 0.9,
                se      = FALSE) +
    
    # Zero line
    geom_hline(yintercept = 0,
               linetype = "dashed", color = "black", linewidth = 0.4) +
    
    scale_x_continuous(breaks = seq(0, 24, by = 4)) +
    
    labs(
      subtitle = subtitle_text,
      x        = "Months after shock",
      y        = if (show_y_label) "Interaction coefficient" else NULL
    ) +
    
    theme_irf()
  
  # Apply shared y-axis limits if provided
  if (!is.null(y_lim)) {
    p <- p + coord_cartesian(ylim = y_lim)
  }
  
  # Remove y-axis label from right panel
  if (!show_y_label) {
    p <- p + theme(axis.title.y = element_blank())
  }
  
  p
}

# ---- Paired plot: two measures side by side for one commodity ----
make_paired_plot <- function(data, commodity_name) {
  
  d_nc <- data %>%
    filter(commodity == commodity_name, fin_measure == "nc_gs_ma12")
  
  d_corr <- data %>%
    filter(commodity == commodity_name, fin_measure == "rolling_corr")
  
  # Shared y-axis limits across both panels
  y_min <- min(d_nc$lower_int90, d_corr$lower_int90, na.rm = TRUE)
  y_max <- max(d_nc$upper_int90, d_corr$upper_int90, na.rm = TRUE)
  y_pad <- (y_max - y_min) * 0.05
  y_lim <- c(y_min - y_pad, y_max + y_pad)
  
  p_nc <- make_panel(
    d            = d_nc,
    subtitle_text = fin_labels["nc_gs_ma12"],
    show_y_label  = TRUE,
    y_lim         = y_lim
  )
  
  p_corr <- make_panel(
    d             = d_corr,
    subtitle_text = fin_labels["rolling_corr"],
    show_y_label  = FALSE,
    y_lim         = y_lim
  )
  
  # Combine
  combined <- (p_nc | p_corr) +
    plot_annotation(
      title = paste0(),
      theme = theme(
        plot.title = element_text(face = "bold", size = 12,
                                  margin = margin(b = 8))
      )
    )
  
  combined
}

# ---- Generate and save all paired plots ----
walk(commodities, \(c) {
  p <- make_paired_plot(interaction, c)
  ggsave(
    filename = paste0(fig_path, "irf_interaction_", c, "_paired.png"),
    plot     = p,
    width    = 14,
    height   = 6,
    dpi      = 300
  )
  cat("Saved:", c, "\n")
})

cat("=== All interaction IRF plots saved ===\n")

############# Capped Sample Interaction IRF Plots #####################
# ---- Load results ----
capped <- read_dta(paste0(data_path, "lp_iv_gs1_interaction_capped_agri.dta"))

# ---- Check variable names ----
cat("Variables in capped file:\n")
print(names(capped))
cat("\nCommodities:\n")
print(unique(capped$commodity))
cat("\nFinancialization measures:\n")
print(unique(capped$fin_measure))

# ---- Settings ----
commodities <- c("Coffee", "Soybeans", "Wheat")

fin_labels <- c(
  "nc_gs_ma12"   = "NC Gross Share 12-Month MA (CFTC)",
  "rolling_corr" = "S&P 500 Rolling Correlation (24M)"
)

# ---- Theme ----
theme_irf <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 12,
                                      margin = margin(b = 8)),
      plot.subtitle    = element_text(size = 10, face = "bold.italic",
                                      color = "grey25", hjust = 0),
      axis.title       = element_text(size = 9),
      axis.text        = element_text(size = 8),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92"),
      plot.margin      = margin(10, 15, 10, 10)
    )
}

# ---- Single panel builder ----
make_panel <- function(d, subtitle_text, show_y_label = TRUE, y_lim = NULL) {
  
  p <- ggplot(d, aes(x = horizon)) +
    
    # Vertical reference at horizon 0
    geom_vline(xintercept = 0,
               color = "grey70", linewidth = 0.3, linetype = "solid") +
    
    # 90% CI
    geom_ribbon(aes(ymin = lower_int90, ymax = upper_int90),
                fill = "#d73027", alpha = 0.15) +
    
    # 68% CI
    geom_ribbon(aes(ymin = lower_int68, ymax = upper_int68),
                fill = "#d73027", alpha = 0.30) +
    
    # IRF line — smoothed
    geom_smooth(aes(y = beta_int),
                method    = "loess",
                span      = 0.3,
                color     = "#d73027",
                linewidth = 0.9,
                se        = FALSE) +
    
    # Zero line
    geom_hline(yintercept = 0,
               linetype = "dashed", color = "black", linewidth = 0.4) +
    
    scale_x_continuous(breaks = seq(0, 24, by = 4)) +
    
    labs(
      subtitle = subtitle_text,
      x        = "Months after shock",
      y        = if (show_y_label) "Interaction coefficient" else NULL
    ) +
    
    theme_irf()
  
  if (!is.null(y_lim)) {
    p <- p + coord_cartesian(ylim = y_lim)
  }
  
  if (!show_y_label) {
    p <- p + theme(axis.title.y = element_blank())
  }
  
  p
}

# ---- Paired plot: two measures side by side for one commodity ----
make_paired_plot <- function(data, commodity_name) {
  
  d_nc <- data %>%
    filter(commodity == commodity_name, fin_measure == "nc_gs_ma12")
  
  d_corr <- data %>%
    filter(commodity == commodity_name, fin_measure == "rolling_corr")
  
  if (nrow(d_nc) == 0 | nrow(d_corr) == 0) {
    warning(paste("No data for", commodity_name))
    return(NULL)
  }
  
  # Shared y-axis limits
  y_min <- min(d_nc$lower_int90, d_corr$lower_int90, na.rm = TRUE)
  y_max <- max(d_nc$upper_int90, d_corr$upper_int90, na.rm = TRUE)
  y_pad <- (y_max - y_min) * 0.05
  y_lim <- c(y_min - y_pad, y_max + y_pad)
  
  p_nc <- make_panel(
    d             = d_nc,
    subtitle_text = fin_labels["nc_gs_ma12"],
    show_y_label  = TRUE,
    y_lim         = y_lim
  )
  
  p_corr <- make_panel(
    d             = d_corr,
    subtitle_text = fin_labels["rolling_corr"],
    show_y_label  = FALSE,
    y_lim         = y_lim
  )
  
  combined <- (p_nc | p_corr) +
    plot_annotation(
      title = paste0(commodity_name,
                     ": Interaction Effect of Financialization",
                     " — Capped Sample (1994\u20132017)"),
      theme = theme(
        plot.title = element_text(face = "bold", size = 12,
                                  margin = margin(b = 8))
      )
    )
  
  combined
}

# ---- Generate and save all paired plots ----
walk(commodities, \(c) {
  p <- make_paired_plot(capped, c)
  if (!is.null(p)) {
    ggsave(
      filename = paste0(fig_path, "irf_capped_", c, "_paired.png"),
      plot     = p,
      width    = 12,
      height   = 5,
      dpi      = 300
    )
    cat("Saved:", c, "\n")
  }
})

cat("=== All capped sample IRF plots saved ===\n")

############# Baseline IRF Plots #####################

# ---- Paths ----
baseline_path <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\02 Data\\DTA\\"
fig_path_base <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\04 Output - figures and tables\\05 Nice R IRF graphs\\01 baseline\\"

dir.create(fig_path_base, showWarnings = FALSE, recursive = TRUE)

# ---- Load results ----
baseline <- read_dta(paste0(baseline_path, "lp_iv_gs1_baseline.dta"))

# ---- Check ----
cat("Variables in baseline file:\n")
print(names(baseline))
cat("\nCommodities:\n")
print(unique(baseline$commodity))

# ---- Settings ----
commodities_base <- c("Coffee", "Copper", "Gold", "Oil", "Soybeans", "Wheat")

# ---- Single baseline IRF plot ----
make_baseline_plot <- function(data, commodity_name) {
  
  d <- data %>% filter(commodity == commodity_name)
  
  if (nrow(d) == 0) {
    warning(paste("No data for", commodity_name))
    return(NULL)
  }
  
  # y-axis limits with padding
  y_min <- min(d$lower90, na.rm = TRUE)
  y_max <- max(d$upper90, na.rm = TRUE)
  y_pad <- (y_max - y_min) * 0.05
  y_lim <- c(y_min - y_pad, y_max + y_pad)
  
  ggplot(d, aes(x = horizon)) +
    
    # Vertical reference at horizon 0
    geom_vline(xintercept = 0,
               color = "grey70", linewidth = 0.3, linetype = "solid") +
    
    # 90% CI
    geom_ribbon(aes(ymin = lower90, ymax = upper90),
                fill = "steelblue", alpha = 0.15) +
    
    # 68% CI
    geom_ribbon(aes(ymin = lower68, ymax = upper68),
                fill = "steelblue", alpha = 0.30) +
    
    # IRF line — smoothed
    geom_smooth(aes(y = beta),
                method    = "loess",
                span      = 0.3,
                color     = "steelblue",
                linewidth = 0.9,
                se        = FALSE) +
    
    # Zero line
    geom_hline(yintercept = 0,
               linetype = "dashed", color = "black", linewidth = 0.4) +
    
    scale_x_continuous(breaks = seq(0, 24, by = 4)) +
    coord_cartesian(ylim = y_lim) +
    
    labs(
      title = paste0(commodity_name,
                     ": Baseline IRF to Monetary Policy Shock"),
      x     = "Months after shock",
      y     = "Cumulative log price change"
    ) +
    
    theme_irf()
}

# ---- Generate and save ----
walk(commodities_base, \(c) {
  p <- make_baseline_plot(baseline, c)
  if (!is.null(p)) {
    ggsave(
      filename = paste0(fig_path_base, "irf_baseline_", c, ".png"),
      plot     = p,
      width    = 8,
      height   = 5,
      dpi      = 300
    )
    cat("Saved:", c, "\n")
  }
})

cat("=== All baseline IRF plots saved ===\n")

############# Baseline IRF Plots — Patched #####################

# ---- Paths ----
baseline_path <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\02 Data\\DTA\\"
fig_path_base <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\04 Output - figures and tables\\05 Nice R IRF graphs\\01 baseline\\"

dir.create(fig_path_base, showWarnings = FALSE, recursive = TRUE)

# ---- Load results ----
baseline <- read_dta(paste0(baseline_path, "lp_iv_gs1_baseline.dta"))

# ---- Settings ----
# Coffee excluded — 5 commodities in a 2x3 grid (or 3+2 layout)
commodities_base <- c("Gold", "Copper", "Oil", "Soybeans", "Wheat")

# ---- Single panel builder ----
make_baseline_panel <- function(data, commodity_name,
                                show_y_label = TRUE,
                                y_lim = NULL) {
  
  d <- data %>% filter(commodity == commodity_name)
  
  if (nrow(d) == 0) {
    warning(paste("No data for", commodity_name))
    return(NULL)
  }
  
  if (is.null(y_lim)) {
    y_min <- min(d$lower90, na.rm = TRUE)
    y_max <- max(d$upper90, na.rm = TRUE)
    y_pad <- (y_max - y_min) * 0.05
    y_lim <- c(y_min - y_pad, y_max + y_pad)
  }
  
  p <- ggplot(d, aes(x = horizon)) +
    
    geom_vline(xintercept = 0,
               color = "grey70", linewidth = 0.3, linetype = "solid") +
    
    geom_ribbon(aes(ymin = lower90, ymax = upper90),
                fill = "steelblue", alpha = 0.15) +
    
    geom_ribbon(aes(ymin = lower68, ymax = upper68),
                fill = "steelblue", alpha = 0.30) +
    
    geom_smooth(aes(y = beta),
                method    = "loess",
                span      = 0.3,
                color     = "steelblue",
                linewidth = 0.9,
                se        = FALSE) +
    
    geom_hline(yintercept = 0,
               linetype = "dashed", color = "black", linewidth = 0.4) +
    
    scale_x_continuous(breaks = seq(0, 24, by = 4)) +
    coord_cartesian(ylim = y_lim) +
    
    labs(
      subtitle = commodity_name,
      x        = "Months after shock",
      y        = if (show_y_label) "Cumulative log price change" else NULL
    ) +
    
    theme_irf() +
    theme(
      plot.subtitle = element_text(size = 20, face = "bold.italic",
                                   color = "grey25", hjust = 0)
    )
  
  if (!show_y_label) {
    p <- p + theme(axis.title.y = element_blank())
  }
  
  p
}

# ---- Shared y-axis limits across all five commodities ----
y_min_all <- baseline %>%
  filter(commodity %in% commodities_base) %>%
  pull(lower90) %>% min(na.rm = TRUE)

y_max_all <- baseline %>%
  filter(commodity %in% commodities_base) %>%
  pull(upper90) %>% max(na.rm = TRUE)

y_pad_all <- (y_max_all - y_min_all) * 0.05
y_lim_all <- c(y_min_all - y_pad_all, y_max_all + y_pad_all)

# ---- Build panels ----
# Row 1: Gold, Copper, Oil
# Row 2: Soybeans, Wheat (centred)
p_gold     <- make_baseline_panel(baseline, "Gold",     show_y_label = TRUE,  y_lim = y_lim_all)
p_copper   <- make_baseline_panel(baseline, "Copper",   show_y_label = FALSE, y_lim = y_lim_all)
p_oil      <- make_baseline_panel(baseline, "Oil",      show_y_label = FALSE, y_lim = y_lim_all)
p_soybeans <- make_baseline_panel(baseline, "Soybeans", show_y_label = TRUE,  y_lim = y_lim_all)
p_wheat    <- make_baseline_panel(baseline, "Wheat",    show_y_label = FALSE, y_lim = y_lim_all)

# ---- Combine with patchwork ----
combined <- (p_gold | p_copper | p_oil) /
  (p_soybeans | p_wheat | plot_spacer()) +
  plot_annotation(
    title = "Baseline IRF to Monetary Policy Shock",
    theme = theme(
      plot.title = element_text(face = "bold", size = 13,
                                margin = margin(b = 10))
    )
  )

# ---- Save ----
ggsave(
  filename = paste0(fig_path_base, "irf_baseline_patched.png"),
  plot     = combined,
  width    = 15,
  height   = 10,
  dpi      = 300
)

cat

############# Baseline IRF Plots — Split by Commodity Group #####################

# ---- Paths ----
baseline_path <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\02 Data\\DTA\\"
fig_path_base <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\04 Output - figures and tables\\05 Nice R IRF graphs\\01 baseline\\"

dir.create(fig_path_base, showWarnings = FALSE, recursive = TRUE)

# ---- Load results ----
baseline <- read_dta(paste0(baseline_path, "lp_iv_gs1_baseline.dta"))

# ---- Override theme for larger text ----
theme_irf <- function() {
  theme_minimal(base_size = 13) +    # increased from 11
    theme(
      plot.subtitle    = element_text(size = 20, face = "bold.italic",
                                      color = "grey25", hjust = 0),
      axis.title       = element_text(size = 20),
      axis.text        = element_text(size = 20),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92"),
      plot.margin      = margin(10, 15, 10, 25)  # extra left margin
    )
}

# ---- Single panel builder ----
make_baseline_panel <- function(data, commodity_name,
                                show_y_label = TRUE,
                                y_lim = NULL) {
  
  d <- data %>% filter(commodity == commodity_name)
  
  if (nrow(d) == 0) {
    warning(paste("No data for", commodity_name))
    return(NULL)
  }
  
  if (is.null(y_lim)) {
    y_min <- min(d$lower90, na.rm = TRUE)
    y_max <- max(d$upper90, na.rm = TRUE)
    y_pad <- (y_max - y_min) * 0.05
    y_lim <- c(y_min - y_pad, y_max + y_pad)
  }
  
  p <- ggplot(d, aes(x = horizon)) +
    
    geom_vline(xintercept = 0,
               color = "grey70", linewidth = 0.3, linetype = "solid") +
    
    geom_ribbon(aes(ymin = lower90, ymax = upper90),
                fill = "steelblue", alpha = 0.15) +
    
    geom_ribbon(aes(ymin = lower68, ymax = upper68),
                fill = "steelblue", alpha = 0.30) +
    
    geom_smooth(aes(y = beta),
                method    = "loess",
                span      = 0.3,
                color     = "steelblue",
                linewidth = 0.9,
                se        = FALSE) +
    
    geom_hline(yintercept = 0,
               linetype = "dashed", color = "black", linewidth = 0.4) +
    
    scale_x_continuous(breaks = seq(0, 24, by = 4)) +
    coord_cartesian(ylim = y_lim) +
    
    labs(
      subtitle = commodity_name,
      x        = "Months after shock",
      y        = if (show_y_label) "Cumulative log price change" else NULL
    ) +
    
    theme_irf() +
    theme(
      plot.subtitle = element_text(size = 20, face = "bold.italic",
                                   color = "grey25", hjust = 0)
    )
  
  if (!show_y_label) {
    p <- p + theme(axis.title.y = element_blank())
  }
  
  p
}

########################################################################
# FIGURE 1 — INDUSTRIAL COMMODITIES: Copper + Oil
########################################################################

commodities_industrial <- c("Copper", "Oil")

y_min_ind <- baseline %>%
  filter(commodity %in% commodities_industrial) %>%
  pull(lower90) %>% min(na.rm = TRUE)

y_max_ind <- baseline %>%
  filter(commodity %in% commodities_industrial) %>%
  pull(upper90) %>% max(na.rm = TRUE)

y_pad_ind <- (y_max_ind - y_min_ind) * 0.05
y_lim_ind <- c(y_min_ind - y_pad_ind, y_max_ind + y_pad_ind)

p_copper <- make_baseline_panel(baseline, "Copper",
                                show_y_label = TRUE,
                                y_lim = y_lim_ind)

p_oil    <- make_baseline_panel(baseline, "Oil",
                                show_y_label = FALSE,
                                y_lim = y_lim_ind)

fig_industrial <- (p_copper | p_oil)

ggsave(
  filename = paste0(fig_path_base, "irf_baseline_industrial.png"),
  plot     = fig_industrial,
  width    = 12,
  height   = 5,
  dpi      = 300
)
cat("Saved: irf_baseline_industrial.png\n")

########################################################################
# FIGURE 2 — AGRICULTURAL COMMODITIES: Coffee + Soybeans + Wheat
########################################################################

commodities_agricultural <- c("Soybeans", "Wheat")

y_min_agri <- baseline %>%
  filter(commodity %in% commodities_agricultural) %>%
  pull(lower90) %>% min(na.rm = TRUE)

y_max_agri <- baseline %>%
  filter(commodity %in% commodities_agricultural) %>%
  pull(upper90) %>% max(na.rm = TRUE)

y_pad_agri <- (y_max_agri - y_min_agri) * 0.05
y_lim_agri <- c(y_min_agri - y_pad_agri, y_max_agri + y_pad_agri)

p_soybeans <- make_baseline_panel(baseline, "Soybeans",
                                  show_y_label = FALSE,
                                  y_lim = y_lim_agri)

p_wheat    <- make_baseline_panel(baseline, "Wheat",
                                  show_y_label = FALSE,
                                  y_lim = y_lim_agri)

fig_agricultural <- (p_soybeans | p_wheat)

ggsave(
  filename = paste0(fig_path_base, "irf_baseline_agricultural.png"),
  plot     = fig_agricultural,
  width    = 15,
  height   = 5,
  dpi      = 300
)
cat("Saved: irf_baseline_agricultural.png\n")

########################################################################
# FIGURE 3 — GOLD (standalone)
########################################################################

y_min_gold <- baseline %>%
  filter(commodity == "Gold") %>%
  pull(lower90) %>% min(na.rm = TRUE)

y_max_gold <- baseline %>%
  filter(commodity == "Gold") %>%
  pull(upper90) %>% max(na.rm = TRUE)

y_pad_gold <- (y_max_gold - y_min_gold) * 0.05
y_lim_gold <- c(y_min_gold - y_pad_gold, y_max_gold + y_pad_gold)

p_gold <- make_baseline_panel(baseline, "Gold",
                              show_y_label = TRUE,
                              y_lim = y_lim_gold)

ggsave(
  filename = paste0(fig_path_base, "irf_baseline_gold.png"),
  plot     = p_gold,
  width    = 7,
  height   = 5,
  dpi      = 300
)
cat("Saved: irf_baseline_gold.png\n")

cat("=== All baseline IRF plots saved (split by group) ===\n")

# ---- Display in RStudio plot pane ----
print(fig_industrial)
print(fig_agricultural)
print(p_gold)


############# Bootstrap Subsample IRF Plots #####################

#---- Define theme ----
  theme_irf <- function() {
    theme_minimal(base_size = 11) +
      theme(
        plot.title       = element_text(face = "bold", size = 12,
                                        margin = margin(b = 8)),
        plot.subtitle    = element_text(size = 10, face = "bold.italic",
                                        color = "grey25", hjust = 0),
        axis.title       = element_text(size = 9),
        axis.text        = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey92"),
        plot.margin      = margin(10, 15, 10, 10)
      )
  }

# ---- Paths ----
boot_path      <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\02 Data\\DTA\\"
fig_path_boot  <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\04 Output - figures and tables\\05 Nice R IRF graphs\\04 bootstrap\\"

dir.create(fig_path_boot, showWarnings = FALSE, recursive = TRUE)

# ---- Load results ----
boot <- read.csv(paste0(boot_path, "lp_iv_subsample_bootstrap_parsimonious.csv"))

# ---- Check ----
cat("Variables in bootstrap file:\n")
print(names(boot))
cat("\nCommodities:\n")
print(unique(boot$commodity))

# ---- Settings ----
commodities_boot <- c("Gold", "Copper", "Oil", "Soybeans", "Wheat")

# ---- Panel builder ----
# type = "pre", "post", or "diff"
make_boot_panel <- function(data, commodity_name,
                            type        = "pre",
                            show_y_label = TRUE,
                            y_lim       = NULL) {
  
  d <- data %>% filter(commodity == commodity_name)
  if (nrow(d) == 0) { warning(paste("No data for", commodity_name)); return(NULL) }
  
  # Select correct columns based on type
  if (type == "pre") {
    beta_col  <- "beta_pre"
    lo90_col  <- "lower_pre90"
    hi90_col  <- "upper_pre90"
    lo68_col  <- "lower_pre68"
    hi68_col  <- "upper_pre68"
    fill_col  <- "#2166ac"   # blue
    y_label   <- "Cumulative log price change"
    sub_label <- "Pre-financialization (1994\u20132007)"
  } else if (type == "post") {
    beta_col  <- "beta_post"
    lo90_col  <- "lower_post90"
    hi90_col  <- "upper_post90"
    lo68_col  <- "lower_post68"
    hi68_col  <- "upper_post68"
    fill_col  <- "#d73027"   # red
    y_label   <- "Cumulative log price change"
    sub_label <- "Post-financialization (2010\u20132025)"
  } else {
    beta_col  <- "beta_diff"
    lo90_col  <- "lower_diff90"
    hi90_col  <- "upper_diff90"
    lo68_col  <- "lower_diff68"
    hi68_col  <- "upper_diff68"
    fill_col  <- "#1a9641"   # green
    y_label   <- "Difference in cumulative log price change"
    sub_label <- "Difference: Post minus Pre"
  }
  
  if (is.null(y_lim)) {
    y_min <- min(d[[lo90_col]], na.rm = TRUE)
    y_max <- max(d[[hi90_col]], na.rm = TRUE)
    y_pad <- (y_max - y_min) * 0.05
    y_lim <- c(y_min - y_pad, y_max + y_pad)
  }
  
  p <- ggplot(d, aes(x = horizon)) +
    
    geom_vline(xintercept = 0,
               color = "grey70", linewidth = 0.3, linetype = "solid") +
    
    geom_ribbon(aes(ymin = .data[[lo90_col]], ymax = .data[[hi90_col]]),
                fill = fill_col, alpha = 0.15) +
    
    geom_ribbon(aes(ymin = .data[[lo68_col]], ymax = .data[[hi68_col]]),
                fill = fill_col, alpha = 0.30) +
    
    geom_smooth(aes(y = .data[[beta_col]]),
                method    = "loess",
                span      = 0.3,
                color     = fill_col,
                linewidth = 0.9,
                se        = FALSE) +
    
    geom_hline(yintercept = 0,
               linetype = "dashed", color = "black", linewidth = 0.4) +
    
    scale_x_continuous(breaks = seq(0, 24, by = 4)) +
    coord_cartesian(ylim = y_lim) +
    
    labs(
      subtitle = sub_label,
      x        = "Months after shock",
      y        = if (show_y_label) y_label else NULL
    ) +
    
    theme_irf() +
    theme(
      plot.subtitle = element_text(size = 10, face = "bold.italic",
                                   color = "grey25", hjust = 0)
    )
  
  if (!show_y_label) {
    p <- p + theme(axis.title.y = element_blank())
  }
  
  p
}

# ---- Paired figure builder: pre | post | diff for one commodity ----
make_boot_triple <- function(data, commodity_name) {
  
  d <- data %>% filter(commodity == commodity_name)
  if (nrow(d) == 0) { warning(paste("No data for", commodity_name)); return(NULL) }
  
  # Shared y-axis for pre and post panels only
  # Diff gets its own scale since units differ conceptually
  y_min_pp <- min(d$lower_pre90, d$lower_post90, na.rm = TRUE)
  y_max_pp <- max(d$upper_pre90, d$upper_post90, na.rm = TRUE)
  y_pad_pp <- (y_max_pp - y_min_pp) * 0.05
  y_lim_pp <- c(y_min_pp - y_pad_pp, y_max_pp + y_pad_pp)
  
  y_min_d  <- min(d$lower_diff90, na.rm = TRUE)
  y_max_d  <- max(d$upper_diff90, na.rm = TRUE)
  y_pad_d  <- (y_max_d - y_min_d) * 0.05
  y_lim_d  <- c(y_min_d - y_pad_d, y_max_d + y_pad_d)
  
  p_pre  <- make_boot_panel(data, commodity_name,
                            type = "pre",  show_y_label = TRUE,
                            y_lim = y_lim_pp)
  p_post <- make_boot_panel(data, commodity_name,
                            type = "post", show_y_label = FALSE,
                            y_lim = y_lim_pp)
  p_diff <- make_boot_panel(data, commodity_name,
                            type = "diff", show_y_label = FALSE,
                            y_lim = y_lim_d)
  
  combined <- (p_pre | p_post | p_diff) +
    plot_annotation(
      theme = theme(
        plot.title = element_text(face = "bold", size = 12,
                                  margin = margin(b = 8))
      )
    )
  
  combined
}

# ---- Generate and save one figure per commodity ----
walk(commodities_boot, \(c) {
  p <- make_boot_triple(boot, c)
  if (!is.null(p)) {
    ggsave(
      filename = paste0(fig_path_boot, "irf_boot_", c, "_triple.png"),
      plot     = p,
      width    = 15,
      height   = 5,
      dpi      = 300
    )
    cat("Saved:", c, "\n")
  }
})

cat("=== All bootstrap IRF plots saved ===\n")

############# Bootstrap Difference IRF Plots #####################

# ---- Paths ----
fig_path_boot_diff <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\04 Output - figures and tables\\05 Nice R IRF graphs\\04 bootstrap\\diff only\\"

dir.create(fig_path_boot_diff, showWarnings = FALSE, recursive = TRUE)

# ---- Settings ----
# boot data already loaded above

# ---- Shared y-axis across all commodities for comparability ----
y_min_diff <- boot %>%
  filter(commodity %in% commodities_boot) %>%
  pull(lower_diff90) %>% min(na.rm = TRUE)

y_max_diff <- boot %>%
  filter(commodity %in% commodities_boot) %>%
  pull(upper_diff90) %>% max(na.rm = TRUE)

y_pad_diff <- (y_max_diff - y_min_diff) * 0.05
y_lim_diff <- c(y_min_diff - y_pad_diff, y_max_diff + y_pad_diff)

# ---- Build panels ----
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
# ---- Combine ----
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

# ---- Save ----
ggsave(
  filename = paste0(fig_path_boot_diff, "irf_boot_diff_patched.png"),
  plot     = combined_diff,
  width    = 15,
  height   = 10,
  dpi      = 300
)

cat("=== Bootstrap difference patched figure saved ===\n")

############# Bootstrap Difference IRF Plots — Split by Commodity Group #####################

# ---- Paths ----
boot_path      <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\02 Data\\DTA\\"
fig_path_boot  <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\04 Output - figures and tables\\05 Nice R IRF graphs\\04 bootstrap\\"

dir.create(fig_path_boot, showWarnings = FALSE, recursive = TRUE)

# ---- Load results ----
boot <- read.csv(paste0(boot_path, "lp_iv_subsample_bootstrap_pre2007.csv"))

cat("Variables in bootstrap file:\n")
print(names(boot))
cat("\nCommodities:\n")
print(unique(boot$commodity))

########################################################################
# THEME — large text for presentation
########################################################################

theme_irf_large <- function() {
  theme_minimal(base_size = 20) +
    theme(
      plot.subtitle    = element_text(size = 20, face = "bold.italic",
                                      color = "grey25", hjust = 0),
      axis.title       = element_text(size = 20),
      axis.text        = element_text(size = 20),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92"),
      plot.margin      = margin(10, 20, 10, 30)
    )
}

########################################################################
# PANEL BUILDER — difference only
########################################################################

make_diff_panel <- function(data, commodity_name,
                            show_y_label = TRUE,
                            y_lim = NULL) {
  
  d <- data %>% filter(commodity == commodity_name)
  if (nrow(d) == 0) {
    warning(paste("No data for", commodity_name))
    return(NULL)
  }
  
  if (is.null(y_lim)) {
    y_min <- min(d$lower_diff90, na.rm = TRUE)
    y_max <- max(d$upper_diff90, na.rm = TRUE)
    y_pad <- (y_max - y_min) * 0.05
    y_lim <- c(y_min - y_pad, y_max + y_pad)
  }
  
  fill_col <- "#1a9641"   # green for difference
  
  p <- ggplot(d, aes(x = horizon)) +
    
    geom_vline(xintercept = 0,
               color = "grey70", linewidth = 0.3, linetype = "solid") +
    
    geom_ribbon(aes(ymin = lower_diff90, ymax = upper_diff90),
                fill = fill_col, alpha = 0.15) +
    
    geom_ribbon(aes(ymin = lower_diff68, ymax = upper_diff68),
                fill = fill_col, alpha = 0.30) +
    
    geom_smooth(aes(y = beta_diff),
                method    = "loess",
                span      = 0.3,
                color     = fill_col,
                linewidth = 0.9,
                se        = FALSE) +
    
    geom_hline(yintercept = 0,
               linetype = "dashed", color = "black", linewidth = 0.4) +
    
    scale_x_continuous(breaks = seq(0, 24, by = 4)) +
    coord_cartesian(ylim = y_lim) +
    
    labs(
      subtitle = commodity_name,
      x        = "Months after shock",
      y        = if (show_y_label) "Difference in cumulative\nlog price change" else NULL
    ) +
    
    theme_irf_large()
  
  if (!show_y_label) {
    p <- p + theme(axis.title.y = element_blank())
  }
  
  p
}

########################################################################
# FIGURE 1 — INDUSTRIAL COMMODITIES: Copper + Oil
########################################################################

commodities_industrial <- c("Copper", "Oil")

y_min_ind <- boot %>%
  filter(commodity %in% commodities_industrial) %>%
  pull(lower_diff90) %>% min(na.rm = TRUE)

y_max_ind <- boot %>%
  filter(commodity %in% commodities_industrial) %>%
  pull(upper_diff90) %>% max(na.rm = TRUE)

y_pad_ind <- (y_max_ind - y_min_ind) * 0.05
y_lim_ind <- c(y_min_ind - y_pad_ind, y_max_ind + y_pad_ind)

p_copper_d <- make_diff_panel(boot, "Copper",
                              show_y_label = TRUE,
                              y_lim = y_lim_ind)

p_oil_d    <- make_diff_panel(boot, "Oil",
                              show_y_label = FALSE,
                              y_lim = y_lim_ind)

fig_diff_industrial <- (p_copper_d | p_oil_d)

ggsave(
  filename = paste0(fig_path_boot, "irf_boot_diff_industrial.png"),
  plot     = fig_diff_industrial,
  width    = 20,
  height   = 8,
  dpi      = 300
)
cat("Saved: irf_boot_diff_industrial.png\n")

########################################################################
# FIGURE 2 — AGRICULTURAL COMMODITIES: Soybeans + Wheat
########################################################################

commodities_agricultural <- c("Soybeans", "Wheat")

y_min_agri <- boot %>%
  filter(commodity %in% commodities_agricultural) %>%
  pull(lower_diff90) %>% min(na.rm = TRUE)

y_max_agri <- boot %>%
  filter(commodity %in% commodities_agricultural) %>%
  pull(upper_diff90) %>% max(na.rm = TRUE)

y_pad_agri <- (y_max_agri - y_min_agri) * 0.05
y_lim_agri <- c(y_min_agri - y_pad_agri, y_max_agri + y_pad_agri)

p_soybeans_d <- make_diff_panel(boot, "Soybeans",
                                show_y_label = TRUE,
                                y_lim = y_lim_agri)

p_wheat_d    <- make_diff_panel(boot, "Wheat",
                                show_y_label = FALSE,
                                y_lim = y_lim_agri)

fig_diff_agricultural <- (p_soybeans_d | p_wheat_d)

ggsave(
  filename = paste0(fig_path_boot, "irf_boot_diff_agricultural.png"),
  plot     = fig_diff_agricultural,
  width    = 20,
  height   = 8,
  dpi      = 300
)
cat("Saved: irf_boot_diff_agricultural.png\n")

########################################################################
# FIGURE 3 — GOLD (standalone)
########################################################################

y_min_gold <- boot %>%
  filter(commodity == "Gold") %>%
  pull(lower_diff90) %>% min(na.rm = TRUE)

y_max_gold <- boot %>%
  filter(commodity == "Gold") %>%
  pull(upper_diff90) %>% max(na.rm = TRUE)

y_pad_gold <- (y_max_gold - y_min_gold) * 0.05
y_lim_gold <- c(y_min_gold - y_pad_gold, y_max_gold + y_pad_gold)

p_gold_d <- make_diff_panel(boot, "Gold",
                            show_y_label = TRUE,
                            y_lim = y_lim_gold)

ggsave(
  filename = paste0(fig_path_boot, "irf_boot_diff_gold.png"),
  plot     = p_gold_d,
  width    = 12,
  height   = 8,
  dpi      = 300
)
cat("Saved: irf_boot_diff_gold.png\n")

cat("=== All bootstrap difference IRF plots saved (split by group) ===\n")

# ---- Display in RStudio plot pane ----
print(fig_diff_industrial)
print(fig_diff_agricultural)
print(p_gold_d)

########################################################################
# DESCRIPTIVE FINANCIALIZATION GRAPHS
# Uses pre-computed .dta files from Financialization_data_analysis.R
#
# nc_gross_share.dta  — long format, monthly NC gross share per commodity
# sp500_corr.dta      — long format, monthly 24M rolling corr per commodity
#
# Two figures:
#   Figure 1: NC gross share (12M MA) — industrial vs agricultural
#   Figure 2: S&P 500 rolling correlation (24M) — industrial vs agricultural
#
# Industrial:   Copper + Oil     (equal weights, Gold excluded)
# Agricultural: Soybeans + Wheat (equal weights, Coffee excluded)
########################################################################

library(haven)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(scales)
library(patchwork)

# ---- Paths ----
input    <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\02 Data\\DTA"
fig_path <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\04 Output - figures and tables\\00 descriptives\\"

dir.create(fig_path, showWarnings = FALSE, recursive = TRUE)

########################################################################
# LOAD DATA
########################################################################

nc   <- read_dta(paste0(input, "\\nc_gross_share.dta")) %>%
  mutate(date = as.Date(date))

corr <- read_dta(paste0(input, "\\sp500_corr.dta")) %>%
  mutate(date = as.Date(date))

# Check commodity names in each file
cat("NC gross share commodities:", paste(unique(nc$commodity),   collapse = ", "), "\n")
cat("Rolling corr commodities:",   paste(unique(corr$commodity), collapse = ", "), "\n")

########################################################################
# SECTION 1 — NC GROSS SHARE
# Compute 12-month rolling average then weighted group averages
########################################################################

# ---- 12-month rolling average per commodity ----
nc <- nc %>%
  arrange(commodity, date) %>%
  group_by(commodity) %>%
  mutate(
    nc_gs_ma12 = rollapply(nc_gross_share, width = 12, FUN = mean,
                           fill = NA, align = "right")
  ) %>%
  ungroup()

# ---- Industrial average: Copper + Oil ----
# Check your commodity names from the cat() output above and adjust if needed
# COT sheet was "Crude oil" so commodity column may read "Crude oil"
nc_industrial <- nc %>%
  filter(commodity %in% c("Copper", "Crude oil")) %>%
  group_by(date) %>%
  filter(n() == 2) %>%
  summarise(
    nc_gs_ma12_wavg = mean(nc_gs_ma12, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Agricultural average: Soybeans + Wheat ----
# COT sheet was "Soybean" so commodity column may read "Soybean"
nc_agricultural <- nc %>%
  filter(commodity %in% c("Soybean", "Wheat")) %>%
  group_by(date) %>%
  filter(n() == 2) %>%
  summarise(
    nc_gs_ma12_wavg = mean(nc_gs_ma12, na.rm = TRUE),
    .groups = "drop"
  )

cat("NC industrial obs:",   nrow(nc_industrial),   "\n")
cat("NC agricultural obs:", nrow(nc_agricultural), "\n")

########################################################################
# SECTION 2 — S&P 500 ROLLING CORRELATION
# Already 24-month rolling correlation — just compute group averages
########################################################################

# ---- Industrial average: Copper + Oil ----
corr_industrial <- corr %>%
  filter(commodity %in% c("Copper", "Oil")) %>%
  group_by(date) %>%
  filter(n() == 2) %>%
  summarise(
    corr_wavg = mean(rolling_corr, na.rm = TRUE),
    .groups   = "drop"
  )

# ---- Agricultural average: Soybeans + Wheat ----
corr_agricultural <- corr %>%
  filter(commodity %in% c("Soybeans", "Wheat")) %>%
  group_by(date) %>%
  filter(n() == 2) %>%
  summarise(
    corr_wavg = mean(rolling_corr, na.rm = TRUE),
    .groups   = "drop"
  )

cat("Corr industrial obs:",   nrow(corr_industrial),   "\n")
cat("Corr agricultural obs:", nrow(corr_agricultural), "\n")

########################################################################
# THEME AND COLOURS
########################################################################

col_ind  <- "#2166ac"   # blue  — industrial
col_agri <- "#1a9641"   # green — agricultural

theme_fin <- function() {
  theme_minimal(base_size = 20) +
    theme(
      plot.title       = element_text(face = "bold", size = 11,
                                      margin = margin(b = 6)),
      plot.subtitle    = element_text(size = 20, color = "grey30",
                                      margin = margin(b = 8)),
      axis.title       = element_text(size = 20),
      axis.text        = element_text(size = 20),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92"),
      plot.margin      = margin(10, 15, 10, 10)
    )
}

########################################################################
# FIGURE 1 — NC GROSS SHARE (12-MONTH MA)
########################################################################

# ---- Shared y-axis limits for both panels ----
nc_ymax <- max(
  max(nc_industrial$nc_gs_ma12_wavg,   na.rm = TRUE),
  max(nc_agricultural$nc_gs_ma12_wavg, na.rm = TRUE)
) * 1.05   # 5% padding above maximum

nc_ylim <- c(0, nc_ymax)

# ---- Panel A: Industrial ----
p_nc_ind <- ggplot(nc_industrial,
                   aes(x = date, y = nc_gs_ma12_wavg)) +
  geom_line(color = col_ind, linewidth = 0.9) +
  geom_vline(xintercept = as.Date("2008-09-01"),
             linetype = "dotted", color = "red", linewidth = 0.6) +
  annotate("text", x = as.Date("2009-04-01"), y = nc_ymax * 0.05,
           label = "GFC", color = "red", size = 3, hjust = 0) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = nc_ylim) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(
    subtitle = "Industrial Commodities (Copper & Oil)",
    x        = NULL,
    y        = "NC gross share (12M MA)"
  ) +
  theme_fin()

# ---- Panel B: Agricultural ----
p_nc_agri <- ggplot(nc_agricultural,
                    aes(x = date, y = nc_gs_ma12_wavg)) +
  geom_line(color = col_agri, linewidth = 0.9) +
  geom_vline(xintercept = as.Date("2008-09-01"),
             linetype = "dotted", color = "red", linewidth = 0.6) +
  annotate("text", x = as.Date("2009-04-01"), y = nc_ymax * 0.05,
           label = "GFC", color = "red", size = 3, hjust = 0) +
  geom_vline(xintercept = as.Date("2015-01-01"),
             linetype = "dashed", color = "grey40", linewidth = 0.5) +
  annotate("text", x = as.Date("2015-06-01"), y = nc_ymax * 0.05,
           label = "2015", color = "grey40", size = 3, hjust = 0) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = nc_ylim) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(
    subtitle = "Agricultural Commodities (Soybeans & Wheat)",
    x        = NULL,
    y        = "NC gross share (12M MA)"
  ) +
  theme_fin()

# ---- Combine and save ----
fig_nc <- (p_nc_ind | p_nc_agri)

ggsave(
  filename = paste0(fig_path, "fin_descriptive_nc_gross_share.png"),
  plot     = fig_nc,
  width    = 14,
  height   = 5,
  dpi      = 300
)
cat("Saved: fin_descriptive_nc_gross_share.png\n")

########################################################################
# FIGURE 2 — S&P 500 ROLLING CORRELATION (24-MONTH)
########################################################################

# ---- Panel A: Industrial ----
p_corr_ind <- ggplot(corr_industrial,
                     aes(x = date, y = corr_wavg)) +
  geom_line(color = col_ind, linewidth = 0.9) +
  geom_hline(yintercept = 0,
             linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = as.Date("2008-09-01"),
             linetype = "dotted", color = "red", linewidth = 0.6) +
  annotate("text", x = as.Date("2009-04-01"), y = -0.85,
           label = "GFC", color = "red", size = 3, hjust = 0) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(
    subtitle = "Industrial Commodities (Copper & Oil)",
    x        = NULL,
    y        = "Rolling correlation with S&P 500"
  ) +
  theme_fin()

# ---- Panel B: Agricultural ----
p_corr_agri <- ggplot(corr_agricultural,
                      aes(x = date, y = corr_wavg)) +
  geom_line(color = col_agri, linewidth = 0.9) +
  geom_hline(yintercept = 0,
             linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = as.Date("2008-09-01"),
             linetype = "dotted", color = "red", linewidth = 0.6) +
  annotate("text", x = as.Date("2009-04-01"), y = -0.85,
           label = "GFC", color = "red", size = 3, hjust = 0) +
  geom_vline(xintercept = as.Date("2015-01-01"),
             linetype = "dashed", color = "grey40", linewidth = 0.5) +
  annotate("text", x = as.Date("2015-06-01"), y = -0.85,
           label = "2015", color = "grey40", size = 3, hjust = 0) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(
    subtitle = "Agricultural Commodities (Soybeans & Wheat)",
    x        = NULL,
    y        = "Rolling correlation with S&P 500"
  ) +
  theme_fin()

# ---- Combine and save ----
fig_corr <- (p_corr_ind | p_corr_agri)

ggsave(
  filename = paste0(fig_path, "fin_descriptive_rolling_corr.png"),
  plot     = fig_corr,
  width    = 14,
  height   = 5,
  dpi      = 300
)
cat("Saved: fin_descriptive_rolling_corr.png\n")

cat("=== Both descriptive financialization figures saved ===\n")

# ---- Display in RStudio plot pane ----
print(fig_nc)
print(fig_corr)

############# Capped Sample Interaction IRF Plots — No Titles, Large Text #####################

library(haven)
library(dplyr)
library(ggplot2)
library(purrr)
library(patchwork)

# ---- Paths ----
data_path <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\02 Data\\DTA\\"
fig_path  <- "C:\\Users\\pitvi\\OneDrive\\Documenti\\03 LSE\\03 Dissertation\\04 Output - figures and tables\\05 Nice R IRF graphs\\"

dir.create(fig_path, showWarnings = FALSE, recursive = TRUE)

# ---- Load results ----
capped <- read_dta(paste0(data_path, "lp_iv_gs1_interaction_capped_agri.dta"))

# ---- Settings ----
commodities <- c("Coffee", "Soybeans", "Wheat")

fin_labels <- c(
  "nc_gs_ma12"   = "NC Gross Share 12-Month MA (CFTC)",
  "rolling_corr" = "S&P 500 Rolling Correlation (24M)"
)

########################################################################
# THEME — large text, no title
########################################################################

theme_irf_large <- function() {
  theme_minimal(base_size = 20) +
    theme(
      plot.title       = element_blank(),
      plot.subtitle    = element_text(size = 20, face = "bold.italic",
                                      color = "grey25", hjust = 0),
      axis.title       = element_text(size = 20),
      axis.text        = element_text(size = 20),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92"),
      plot.margin      = margin(10, 20, 10, 30)
    )
}

########################################################################
# SINGLE PANEL BUILDER
########################################################################

make_panel_large <- function(d, subtitle_text,
                             show_y_label = TRUE,
                             y_lim = NULL) {
  
  p <- ggplot(d, aes(x = horizon)) +
    
    geom_vline(xintercept = 0,
               color = "grey70", linewidth = 0.3, linetype = "solid") +
    
    geom_ribbon(aes(ymin = lower_int90, ymax = upper_int90),
                fill = "#d73027", alpha = 0.15) +
    
    geom_ribbon(aes(ymin = lower_int68, ymax = upper_int68),
                fill = "#d73027", alpha = 0.30) +
    
    geom_smooth(aes(y = beta_int),
                method    = "loess",
                span      = 0.3,
                color     = "#d73027",
                linewidth = 0.9,
                se        = FALSE) +
    
    geom_hline(yintercept = 0,
               linetype = "dashed", color = "black", linewidth = 0.4) +
    
    scale_x_continuous(breaks = seq(0, 24, by = 4)) +
    
    labs(
      subtitle = subtitle_text,
      x        = "Months after shock",
      y        = if (show_y_label) "Interaction coefficient" else NULL
    ) +
    
    theme_irf_large()
  
  if (!is.null(y_lim)) {
    p <- p + coord_cartesian(ylim = y_lim)
  }
  
  if (!show_y_label) {
    p <- p + theme(axis.title.y = element_blank())
  }
  
  p
}

########################################################################
# PAIRED PLOT BUILDER — no title, large text
########################################################################

make_paired_plot_large <- function(data, commodity_name) {
  
  d_nc <- data %>%
    filter(commodity == commodity_name, fin_measure == "nc_gs_ma12")
  
  d_corr <- data %>%
    filter(commodity == commodity_name, fin_measure == "rolling_corr")
  
  if (nrow(d_nc) == 0 | nrow(d_corr) == 0) {
    warning(paste("No data for", commodity_name))
    return(NULL)
  }
  
  # Shared y-axis limits
  y_min <- min(d_nc$lower_int90, d_corr$lower_int90, na.rm = TRUE)
  y_max <- max(d_nc$upper_int90, d_corr$upper_int90, na.rm = TRUE)
  y_pad <- (y_max - y_min) * 0.05
  y_lim <- c(y_min - y_pad, y_max + y_pad)
  
  p_nc <- make_panel_large(
    d             = d_nc,
    subtitle_text = fin_labels["nc_gs_ma12"],
    show_y_label  = TRUE,
    y_lim         = y_lim
  )
  
  p_corr <- make_panel_large(
    d             = d_corr,
    subtitle_text = fin_labels["rolling_corr"],
    show_y_label  = FALSE,
    y_lim         = y_lim
  )
  
  # No plot_annotation — no title
  combined <- (p_nc | p_corr)
  
  combined
}

########################################################################
# GENERATE AND SAVE
########################################################################

walk(commodities, \(c) {
  p <- make_paired_plot_large(capped, c)
  if (!is.null(p)) {
    ggsave(
      filename = paste0(fig_path, "irf_capped_", c, "_paired_large.png"),
      plot     = p,
      width    = 16,
      height   = 6,
      dpi      = 300
    )
    cat("Saved:", c, "\n")
  }
})

cat("=== All capped sample IRF plots saved (large text, no titles) ===\n")

# ---- Display in RStudio plot pane ----
walk(commodities, \(c) {
  p <- make_paired_plot_large(capped, c)
  if (!is.null(p)) print(p)
})

