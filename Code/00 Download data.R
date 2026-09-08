###############################################################################
# DOWNLOAD DATA.R
#
#Purpose: download the raw data needed for the analysis on commodity futures financialization and monetary policy
#
#This script relies on the .Rproj file at the repository root to set the
#working directory automatically: open the .Rproj file first, then run
#this script. All paths below are relative to the repository root and
#match the existing folder names in the GitHub repository exactly.
#
Scope of this script:
#   - Downloads ONLY data available from FRED, restricted to the sample
#     period below.
#   - For data that cannot be retrieved this way (LSEG subscription data,
#     CFTC data, and data from other scholars' published work), this
#     script writes a short placeholder/instructions file instead, with
#     the exact source confirmed from the thesis draft.
#
# Packages needed: writexl
###############################################################################
 
library(writexl)
 
# ---------------------------------------------------------------------------
# Sample period used for the raw commodity/price data in this project
# (matches the thesis: "Daily settlement prices are collected over the
# sample period 01.01.1990 - 01.01.2026")
# ---------------------------------------------------------------------------
sample_start <- "1990-01-01"
sample_end   <- "2026-01-01"
sample_span_text <- "January 1990 - January 2026"
 
# ---------------------------------------------------------------------------
# Helper: download a FRED series directly as CSV and
# save it as an Excel file
# ---------------------------------------------------------------------------
get_fred_series <- function(series_id, folder, filename) {
  url <- paste0(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series_id,
    "&cosd=", sample_start, "&coed=", sample_end
  )
  df <- read.csv(url)
  write_xlsx(df, file.path(folder, paste0(filename, ".xlsx")))
  message("Downloaded ", series_id, " (", sample_span_text, ") -> ",
          file.path(folder, paste0(filename, ".xlsx")))
  # NOTE: if a series' earliest available observation is later than
  # sample_start, FRED simply returns data from its own earliest date
  # onward -- check df$DATE manually if exact start coverage matters.
}

# ---------------------------------------------------------------------------
# Helper: write a placeholder note explaining how to obtain data that
# cannot be downloaded automatically (LSEG, CFTC, or other scholars' data)
# ---------------------------------------------------------------------------
write_placeholder <- function(folder, filename, message) {
  writeLines(message, file.path(folder, paste0(filename, "_README.txt")))
  message("Placeholder written -> ", file.path(folder, paste0(filename, "_README.txt")))
}

###############################################################################
# 1. SP500 TIME SERIES  (FRED, no key needed)
#    Data/S&P 500 time series.xlsx
#    Pulled from FRED
###############################################################################
get_fred_series(
  series_id = "SP500",
  folder    = "Data",
  filename  = "S&P 500 time series"
)
 
###############################################################################
# 2. COMMODITY PRICES  (MANUAL - LSEG)
#    Data/Commodity prices/
###############################################################################
write_placeholder(
  folder   = "Data/Commodity prices",
  filename = "Commodity prices",
  message  = paste(
    "Commodity prices -- MANUAL DOWNLOAD REQUIRED",
    "Source: Refinitiv (London Stock Exchange Group, LSEG) Datastream.",
    "Citation: London Stock Exchange Group (LSEG) (2024). Refinitiv",
    "Datastream. Electronic database. Accessed: April 2024-April 2026.",
    paste("Required sample period:", sample_span_text),
    "",
    "Series needed: front-month futures daily settlement prices for",
    "crude oil, copper, wheat, soybeans, and gold (the 5 commodities used",
    "in the thesis), plus their respective main exchanges (see Table 2A",
    "in the thesis appendix, e.g. WTI/NYMEX, Copper/COMEX, etc.).",
    "The last available daily settlement price of each month is retained",
    "to construct the monthly series used in the analysis.",
    "",
    "To reproduce:",
    "1. Log into LSEG Workspace/Eikon with an institutional license.",
    "2. Pull daily settlement prices for the front-month futures contract",
    "   of each commodity, for the sample period above.",
    "3. Save the file(s) in this folder, matching the names referenced",
    "   in code/Data cleaning.do.",
    sep = "\n"
  )
)
 
###############################################################################
# 3. FINANCIALIZATION OF COMMODITIES  (MANUAL - CFTC / LSEG / FRED-derived)
#    Data/Financialization of commodities/
#    Two measures used in the thesis:
#    (a) Non-commercial gross share (extensive margin) -- from CFTC
#        Commitments of Traders reports. Citation: Commodity Futures
#        Trading Commission (2024). Commitments of traders: Futures only
#        reports, historical compressed. Monthly, end-of-month, downloaded
#        1994-2026 for each commodity.
#        NOTE: in code/Financialization data analysis.R, the exported
#        sample window is 1992-01-01 to 2025-12-31 (narrower than
#        1990-2026), a deliberate choice due to CFTC data availability --
#        keep as is.
#    (b) 24-month rolling correlation with S&P 500 -- built from FRED
#        SP500 (already downloaded in step 1) and the LSEG commodity
#        futures prices (step 2). No separate manual file needed beyond
#        those two.
###############################################################################
write_placeholder(
  folder   = "Data/Financialization of commodities",
  filename = "Financialization measures 1990-2025",
  message  = paste(
    "Financialization measures -- MANUAL DOWNLOAD REQUIRED",
    "",
    "(a) Non-commercial gross share (extensive margin):",
    "Source: Commodity Futures Trading Commission (CFTC), Commitments of",
    "Traders: Futures Only Reports (historical compressed).",
    "URL: https://www.cftc.gov/MarketReports/CommitmentsofTraders/HistoricalCompressed/index.htm",
    "Citation: Commodity Futures Trading Commission (2024). Commitments",
    "of traders: Futures only reports, historical compressed. Monthly,",
    "end-of-month data downloaded 1994-2026 for each commodity, then",
    "smoothed with a 12-month rolling average.",
    "NOTE: the analysis code (Financialization data analysis.R) exports",
    "this measure over 1992-01-01 to 2025-12-31, narrower than the",
    "1990-2026 window used elsewhere, due to CFTC data availability --",
    "this is intentional, not an error.",
    "",
    "(b) 24-month rolling correlation with S&P 500 (intensive margin):",
    "Built directly from the FRED SP500 series (already downloaded to",
    "Data/S&P 500 time series.xlsx by this script) and the LSEG commodity",
    "futures prices (Data/Commodity prices/) -- no separate manual",
    "download needed for this measure.",
    "",
    "To reproduce:",
    "1. Download the CFTC Commitments of Traders data from the URL above,",
    "   for each of the 5 commodities in the thesis.",
    "2. Save the raw file in this folder.",
    "3. Run code/Financialization measures.R to construct both measures.",
    sep = "\n"
  )
)
 
###############################################################################
# 4. MONETARY POLICY SURPRISES
#    Data/Monetary policy surprises/
###############################################################################
 
# --- 4a. Fed funds rate  (FRED, no key needed) ------------------------------
get_fred_series(
  series_id = "FEDFUNDS",
  folder    = "Data/Monetary policy surprises",
  filename  = "Fedfunds"
)
 
# --- 4b. Treasury yield  (FRED, no key needed) ------------------------------
# Confirmed from the thesis: 1-year Treasury constant maturity yield (DGS1),
# used as the endogenous variable in the first stage. Citation: Board of
# Governors of the Federal Reserve System (US) (2026). Market yield on
# U.S. treasury securities at 1-year constant maturity, quoted on an
# investment basis [DGS1]. FRED. Retrieved April 2026.
get_fred_series(
  series_id = "DGS1",
  folder    = "Data/Monetary policy surprises",
  filename  = "Treasury"
)
 
# --- 4c. Mps_data.xlsx  (MANUAL - other scholar's work) ---------------------
# CONFIRMED from the thesis draft: high-frequency monetary policy shock
# from Acosta et al. (2025), drawn from the U.S. Monetary Policy
# Event-Study Database (USMPD).
write_placeholder(
  folder   = "Data/Monetary policy surprises",
  filename = "Mps_data",
  message  = paste(
    "Mps_data.xlsx -- U.S. Monetary Policy Event-Study Database (USMPD)",
    "-- MANUAL DOWNLOAD REQUIRED",
    paste("Required sample period:", sample_span_text),
    "Source: Federal Reserve Bank of San Francisco.",
    "URL: https://www.frbsf.org/research-and-insights/data-and-indicators/us-monetary-policy-event-study-database/",
    "Citation: Acosta, M. et al. (2025). US monetary policy event study",
    "database. Federal Reserve Bank of San Francisco. / Federal Reserve",
    "Bank of San Francisco (2026). U.S. monetary policy event-study",
    "database (USMPD). Updated March 19, 2026.",
    "",
    "This is the high-frequency monetary policy surprise used as the",
    "excluded instrument in the LP-IV first stage.",
    "",
    "To reproduce:",
    "1. Go to the URL above.",
    "2. Download the USMPD monthly shock series.",
    "3. Save it as 'Mps_data.xlsx' in this folder.",
    sep = "\n"
  )
)
 
###############################################################################
# 5. CONTROLS  (Data/Controls)
#    Monthly industrial production (US), inflation (US), and exchange
#    rates: US dollar vs. Brazilian real, Australian dollar, Chilean peso.
#    Confirmed from the thesis: currency controls are the Brazilian Real
#    (d_brl) for Coffee and Soybeans, the Chilean Peso (d_clp) for Copper,
#    and the Australian Dollar (d_aud) for Gold. No currency control for
#    Oil or Wheat.
###############################################################################
 
# --- 5a. Indpro -- Industrial Production Index, US (FRED, no key needed) ---
get_fred_series(
  series_id = "INDPRO",
  folder    = "Data/Controls",
  filename  = "Indpro"
)
 
# --- 5b. Inflation -- CPI for All Urban Consumers, US (FRED, no key) -------
get_fred_series(
  series_id = "CPIAUCSL",
  folder    = "Data/Controls",
  filename  = "Inflation"
)
 
# --- 5c. Us-aus -- US Dollars to Australian Dollar, monthly (FRED) --------
# NOTE: FRED series EXUSAL only starts Oct 1997; earlier months in the
# 1990-2026 window will not be available from this series.
get_fred_series(
  series_id = "EXUSAL",
  folder    = "Data/Controls",
  filename  = "Us-aus"
)
 
# --- 5d. Us-brazil -- Brazilian Reals to US Dollar, monthly (FRED) --------
# NOTE: FRED series EXBZUS only starts Jan 1995; earlier months in the
# 1990-2026 window will not be available from this series.
get_fred_series(
  series_id = "EXBZUS",
  folder    = "Data/Controls",
  filename  = "Us-brazil"
)
 
# --- 5e. Us-chile -- Chilean Peso to US Dollar, monthly (FRED) ------------
get_fred_series(
  series_id = "CCUSMA02CLM618N",
  folder    = "Data/Controls",
  filename  = "Us-chile"
)
 
###############################################################################
# 6. KANZIG OIL SHOCKS  (MANUAL - other scholar's work)
#    Data/DTA/Kanzig oil shocks/  -- SKIPPED per instructions (DTA/ is
#    populated by the Stata code, not this script). Documented here only.
#
#    CONFIRMED from the thesis: Kanzig (2021) OPEC supply news shock,
#    included as a control (contemporaneous + 4 lags) for Oil only.
#    Source: Diego Kanzig's own data page.
#    URL: https://github.com/dkaenzig/oilsupplynews
#    (see also https://www.diegokaenzig.com/data)
#    Citation: Kanzig, D. R. (2021). The macroeconomic effects of oil
#    supply news: Evidence from OPEC announcements. American Economic
#    Review, 111(4):1092-1125.
###############################################################################
 
###############################################################################
# 7. GPR DATA  (MANUAL - other scholars' work)
#    Data/DTA/GPR data/  -- SKIPPED per instructions (DTA/ is populated by
#    the Stata code, not this script). Documented here only.
#
#    CONFIRMED: used in a more recent version of the thesis than the draft
#    reviewed above. Geopolitical Risk (GPR) Index.
#    Source: Matteo Iacoviello's data page.
#    URL: https://www.matteoiacoviello.com/gpr.htm
#    Citation: Caldara, D. and Iacoviello, M. (2022). Measuring
#    geopolitical risk. American Economic Review, 112(4):1194-1225.
#    Required sample period: January 1990 - January 2026 (monthly).
###############################################################################
 
###############################################################################
# DONE
###############################################################################
cat("\n========================================\n")
cat("Data download complete. Sample period:", sample_span_text, "\n")
cat("Downloaded directly from FRED (no API key needed): SP500, FEDFUNDS,\n")
cat("  DGS1, INDPRO, CPIAUCSL, EXUSAL, EXBZUS, CCUSMA02CLM618N.\n")
cat("Placeholder README.txt files written for manual-retrieval data:\n")
cat("  Commodity prices (LSEG), Financialization measures (CFTC + LSEG/FRED),\n")
cat("  Mps_data (USMPD / Acosta et al. 2025 - confirmed from thesis draft).\n")
cat("Kanzig oil shocks and GPR data (Caldara & Iacoviello 2022) documented\n")
cat("  in comments, sources confirmed. Neither is written to DTA/, which\n")
cat("  is populated by the Stata code, not this script.\n")
cat("========================================\n\n")
 
