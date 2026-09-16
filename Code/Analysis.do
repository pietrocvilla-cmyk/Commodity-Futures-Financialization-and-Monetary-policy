********************************************************************************************************************
********* Has Futures Markets Financialization Changed Monetary Policy Transmission to Commodity Prices?************
********************************************************************************************************************
********************************************** Analysis Code *******************************************************


*********** DATA CLEANING ************************

***MASTER DIRECTORY SETUP
*Edit ONLY the line below to match where you cloned this repository on
*your own computer. Everything else uses relative paths from there.

* global root "C:\Users\yourname\my-repo"

* Subfolders (relative to root) -- should not need editing
global input    "$root\Data"
global output  "$root\Output"
global dta     "$root\Data\DTA"

* Verify the setup worked before running anything else

cap confirm file "$input\S&P 500 time series.xlsx"
if _rc {
    di as error "ERROR: Cannot find expected data file."
    di as error "Current global root: $root"
    di as error "SOLUTION: edit the 'global root' line at the top of this"
    di as error "script to point to your local clone of the repository."
    exit 601
}
else {
    di as result "✓ Root directory correctly set to: $root"
}
cd "$main"


*** Date spine setting ***
*We build monthly date spine: full sample calendar used as master index
*All datasets are merged onto this spine so monthly dates align and
*missing observations can be filled consistently

clear
set obs 385  // 1994m1 to 2026m1 = 372 months
gen date = ym(1994,1) + _n - 1
format date %tm
label variable date "Monthly date"
save "$input\DTA\date_spine.dta", replace

*** Monetary policy shocks

import excel "$input\Mps_data.xlsx", firstrow clear

gen date = mofd(Date)
format date %tm

* We scale ME to 25bp units to standardize MP shocks interpretation 
destring ME, replace
gen shock = ME / 0.25

collapse (sum) shock, by(date)
merge 1:1 date using "date_spine.dta", nogenerate
replace shock = 0 if missing(shock)

sort date
save "$dta\mps_monthly.dta", replace

*** Commodity prices
local sheets "Oil Gold Copper Soybeans Wheat"
foreach s of local sheets {

    import excel "$input\Commodity prices.xlsx", ///
        sheet("`s'") cellrange(A2) clear
    drop A
    rename B date_raw
    rename C price

    destring price, replace force

    gen date = mofd(date_raw)
    format date %tm

* We drop obs outside sample period before any other operations
    keep if date >= ym(1994,1) & date <= ym(2025,12)

    * We take Log price level needed to construct LP dependent variables
    sort date
    gen log_price = log(price)

    gen d_log_price = log_price - log_price[_n-1]

    gen commodity = "`s'"

    keep date commodity price log_price d_log_price
/*
    * Quick check
    di "=== `s' ==="
    di "Observations: " _N
    di "Start: " %tm date[1]
    di "End:   " %tm date[_N]
    sum price, detail

    save "temp_`s'.dta", replace
*/
}

*** We merge all the commodity prices variables in one file 
use "temp_Oil.dta", clear
foreach s in Gold Copper Soybeans Wheat {
    append using "temp_`s'.dta"
}

sort commodity date
encode commodity, gen(commodity_id)
label list commodity_id
xtset commodity_id date

* Check
tabstat price log_price d_log_price, ///
    by(commodity) stat(n mean sd min max) nototal

table commodity, stat(min date) stat(max date) stat(count price)

sort commodity_id date
order commodity commodity_id date price log_price d_log_price
compress
save "$dta\prices_long.dta", replace

* Clean up temp files
local sheets "Oil Gold Copper Soybeans Wheat"
foreach s of local sheets {
    erase "temp_`s'.dta"
}

di "=== prices_long.dta saved ==="
di "Total observations: " _N
di "Commodities: 6"
di "Sample: 1994m1 to 2025m12"

*** Industrial output 
* Monthly, seasonally adjusted, already in growth terms
import excel "$input\indpro.xlsx", firstrow clear
describe
list in 1/5

*We rename columns to adjust if FRED used different headers
rename observation_date date_raw
rename INDPRO_PCH ip_growth

gen date = mofd(date_raw)
format date %tm
drop date_raw

* We keep slightly wider than sample for lag construction
keep if date >= ym(1993,1) & date <= ym(2025,12)

sort date

* Check
di "=== INDUSTRIAL PRODUCTION ==="
di "Observations: " _N
di "Start: " %tm date[1]
di "End:   " %tm date[_N]
sum ip_growth, detail

keep date ip_growth
sort date
save "$dta\ip.dta", replace

di "=== ip.dta saved ==="

*** Inflation
import excel "$input\inflation.xlsx", firstrow clear
describe
list in 1/5

rename observation_date date_raw
rename CPIAUCSL_PCH inflation

gen date = mofd(date_raw)
format date %tm
drop date_raw

keep if date >= ym(1993,1) & date <= ym(2025,12)

sort date

* Check
di "=== CPI INFLATION ==="
di "Observations: " _N
di "Start: " %tm date[1]
di "End:   " %tm date[_N]
sum inflation, detail

keep date inflation
sort date
save "$dta\cpi.dta", replace

di "=== cpi.dta saved ==="

*** Exchange rates
* Us-brazil.xlsx — BRL/USD (soybeans)
* Us-aus.xlsx    — AUD/USD (copper and gold)
* Us-chile.xlsx  — CLP/USD (copper)
* Save to $dta

* Define local with file and variable names
local fx_files  "Us-brazil Us-aus Us-chile"
local fx_names  "d_brl d_aud d_clp"
local fx_titles "BRAXUSAL DEXBZUS CCUSMA02CLM618N"

local n : word count `fx_files'

forvalues i = 1/`n' {

    local file  : word `i' of `fx_files'
    local vname : word `i' of `fx_names'
    local title : word `i' of `fx_titles'

    import excel "$input\\`file'.xlsx", firstrow clear

    * Check
    describe
    list in 1/5


    rename observation_date date_raw

    * We rename whatever the rate column is called to a temp name
    * We will figure out exact name from describe — placeholder below
    * rename DEXBZUS rate   // for BRL
    * rename DEXAUS  rate   // for AUD
    * rename DEXCHUS rate   // for CLP
    * For now rename the second variable generically:
    ds date_raw, not
    local ratecol `r(varlist)'
    rename `ratecol' rate

    gen date = mofd(date_raw)
    format date %tm
    drop date_raw

    sort date date
    by date: keep if _n == _N

    keep if date >= ym(1993,1) & date <= ym(2025,12)

    sort date

    gen `vname' = log(rate) - log(rate[_n-1])

    * Check
    di "=== `title' ==="
    di "Observations: " _N
    di "Start: " %tm date[1]
    di "End:   " %tm date[_N]
    sum rate `vname', detail

    
    keep date `vname'
    sort date
    save "$dta\\`file'.dta", replace

    di "=== `file'.dta saved ==="
}

*** Financialization measures
** NC gross share
use "$dta\nc_gross_share.dta", clear

* Verify that data already collapsed to monthly means in R: should be exactly 1 observation per commodity-month
duplicates report commodity date
* If duplicates = 0, data is clean

list commodity date nc_gross_share in 1/5
tabstat nc_gross_share, by(commodity) stat(n mean sd min max) nototal

sort commodity date
save "$dta\nc_gross_share_monthly.dta", replace
di "=== nc_gross_share_monthly.dta saved ==="

** SP500 Rolling Correlation
use "$dta\sp500_corr.dta", clear

describe
list in 1/5

tabstat rolling_corr, by(commodity) stat(n mean sd min max) nototal

table commodity, stat(min date) stat(max date) stat(count rolling_corr)

sort commodity date
save "$dta\sp500_corr_monthly.dta", replace
di "=== sp500_corr_monthly.dta saved ==="

*** Endogenous monetary policy 
import excel "$input\Treasury.xlsx", firstrow clear

gen date_stata = mofd(date)
format date_stata %tm
drop date
rename date_stata date
rename yield gs1

sort date
tsset date
gen d_gs1 = gs1 - L.gs1

save "$dta\gs1_prepared.dta", replace


**** Kanzig oil shock (for robustness checks) 

import excel "$input\oil supply shocks.xlsx", ///
    sheet("Monthly") ///
    firstrow ///
    clear

rename Date                date_str
rename Oilsupply           oil_supply_dummy
rename Oilsupplynewsshock  kanzig_shock

gen date = monthly(date_str, "YM")
format date %tm

list date_str date in 1/5

drop date_str

* Define the cutoff to exclude the dates we don't need
local cutoff = monthly("1994m1", "YM")

count if date < `cutoff'
di "Observations before January 1994 (to be dropped): " r(N)

count if date >= `cutoff'
di "Observations from January 1994 onward (to be kept): " r(N)

drop if date < `cutoff'

sum date
di "Date range after trim: " %tm `=r(min)' " to " %tm `=r(max)'

sort date
tempfile kanzig
save `kanzig'

*** Merge everything into master data panel
* prices_long.dta is the spine, so everything merges onto it

use "$dta\prices_long.dta", clear

merge m:1 date using "$dta\mps_monthly.dta",  keep(1 3) nogenerate
di "After MPS merge: " _N " obs"

merge m:1 date using "$dta\ip.dta",           keep(1 3) nogenerate
di "After IP merge: " _N " obs"

merge m:1 date using "$dta\cpi.dta",          keep(1 3) nogenerate
di "After CPI merge: " _N " obs"

merge m:1 date using "$dta\us_brazil.dta",          keep(1 3) nogenerate
di "After BRL merge: " _N " obs"

merge m:1 date using "$dta\us_aus.dta",          keep(1 3) nogenerate
di "After AUD merge: " _N " obs"

merge m:1 date using "$dta\us_chile.dta",     keep(1 3) nogenerate
di "After CLP merge: " _N " obs"

merge m:1 date using "$dta\gs1_prepared.dta",  keep(1 3) nogenerate
di "After  merge Treasuries merge: " _N " obs"

* Financialization measures: since these vary by commodity so merge on both keys, date and commodity
merge 1:1 commodity date using ///
    "$dta\nc_gross_share_monthly.dta", ///
    keep(1 3) nogenerate

merge 1:1 commodity date using ///
    "$dta\sp500_corr_monthly.dta", ///
    keep(1 3) nogenerate

merge m:1 date using `kanzig',    keep(1 3) nogenerate 

* We replace missing shocks (Months with no FOMC meeting) with 0s
replace shock = 0 if missing(shock)

sort commodity_id date
xtset commodity_id date

********************************************************************************
* LOCAL PROJECTION MODELS
********************************************************************************
*********** Local projections preparatory work *********************************

forvalues h = 0/24 {
    by commodity_id: gen dep_h`h' = log_price[_n+`h'] - log_price[_n-1]
    label variable dep_h`h' "Cumulative log price change horizon `h'"
}

**** Subsample time indicators 

* Binary post indicator
gen post = .
replace post = 0 if date >= ym(1994,1) & date <= ym(2003,12)
replace post = 1 if date >= ym(2010,1) & date <= ym(2025,12)
* 2004-2009 left as missing — excluded from subsample LPs but kept in dataset

* Subsample label
gen subsample = ""
replace subsample = "pre"  if date >= ym(1994,1) & date <= ym(2003,12)
replace subsample = "post" if date >= ym(2010,1) & date <= ym(2025,12)

label variable post "0=pre (1994-2003), 1=post (2010-2025), .=transition (2004-2009)"

di "=== MISSING VALUE CHECK ==="
foreach v of varlist shock ip_growth inflation nc_gross_share rolling_corr {
    quietly count if missing(`v')
    di "Missing in `v': " r(N)
}

tabstat shock ip_growth inflation nc_gross_share rolling_corr, ///
    by(commodity) stat(n mean) nototal

table commodity, stat(min date) stat(max date) stat(count dep_h0)

      price log_price d_log_price ///
      shock ///
      ip_growth inflation ///
      d_brl d_aud d_clp ///
      nc_gross_share rolling_corr ///
      dep_h* ///
      subsample post

compress
save "$dta\master_panel.dta", replace

di "=== master_panel.dta saved ==="
di "Total observations: " _N
di "Variables: " c(k)

* Save as new file — do NOT overwrite master_panel
save "$dta\master_panel_gs1.dta", replace
di "=== master_panel_gs1.dta saved ==="


**********************ANALYSIS 1 - BASELINE model (no interaction or subdivision)**********************

** Here we estimate the baseline local-projection model over the full sample period. 
* The specification includes crisis-period dummy variables to control for unusual economic conditions during major crises, 
* as well as the relevant macroeconomic and financialization controls. 
* This provides the main benchmark estimate of the response of commodity prices to monetary policy shocks.

*****LP-IV BASELINE - 1-Year Treasury (first difference)

use "$dta\master_panel_gs1.dta", clear
xtset commodity_id date

global lags    4
global horizon 24

local curr_Coffee   "d_brl"
local curr_Copper   "d_clp"
local curr_Gold     "d_aud"
local curr_Oil      ""
local curr_Soybeans "d_brl"
local curr_Wheat    ""

* Crisis dummies
gen gfc     = (date >= tm(2008m9)  & date <= tm(2009m6))
gen covid   = (date >= tm(2020m3)  & date <= tm(2021m6))
gen ukraine = (date >= tm(2022m2)  & date <= tm(2022m12))

local commodities "Copper Gold Oil Soybeans Wheat"

***** FIRST STAGE: get fitted values of d_gs1
** The first stage identifies the endogenous change in the 1-year Treasury yield
* using the high-frequency monetary-policy shock as the excluded instrument.
* regress d_gs1 shock. 


preserve
    duplicates drop date, force
    tsset date

    regress d_gs1 shock

    di ""
    di "=== First Stage: d_gs1 ~ shock ==="
    di "  Coefficient: " %7.4f _b[shock]
    di "  t-stat:      " %7.4f _b[shock]/_se[shock]
    di "  F-stat:      " %7.4f e(F)
    di "  R-squared:   " %7.4f e(r2)
    di "  N:           " e(N)

** We generate fitted values from the first stage. These predicted changes in the
* Treasury yield are used as the instrumented monetary-policy variable in the LP.

    predict d_gs1_hat, xb
    keep date d_gs1_hat
    sort date
    tempfile fittedvals
    save `fittedvals'
restore

merge m:1 date using `fittedvals', nogenerate
sort commodity_id date
xtset commodity_id date

**** LOOP
tempfile results
preserve
    clear
    save `results', emptyok replace
restore

foreach c of local commodities {

    local curr_control = "`curr_`c''"
    di "=== `c' ==="

    local crisis_vars "gfc covid"
    if inlist("`c'", "Oil", "Soybeans", "Wheat") local crisis_vars "gfc covid ukraine"

    forvalues h = 0/$horizon {

        local depvarlags ""
        forvalues l = 1/$lags {
            local depvarlags "`depvarlags' L`l'.d_log_price"
        }

        local macrolags ""
        foreach v in ip_growth inflation {
            forvalues l = 1/$lags {
                local macrolags "`macrolags' L`l'.`v'"
            }
        }

        local currlags ""
        if "`curr_control'" != "" {
            forvalues l = 1/$lags {
                local currlags "`currlags' L`l'.`curr_control'"
            }
        }

        local crisislags ""
        foreach d of local crisis_vars {
            forvalues l = 0/$lags {
                local crisislags "`crisislags' L`l'.`d'"
            }
        }

        local bw = max(1, `h')

** We estimate cumulative impulse responses for horizons 0 to 24 months.
* The Newey-West bandwidth increases with the horizon to account for
* serial correlation in the local-projection residuals.

        quietly newey dep_h`h' d_gs1_hat ///
            `depvarlags' `macrolags' `currlags' `crisislags' ///
            if commodity == "`c'", lag(`bw')

        local beta    = _b[d_gs1_hat]
        local se      = _se[d_gs1_hat]
        local upper90 = `beta' + 1.645 * `se'
        local lower90 = `beta' - 1.645 * `se'
        local upper68 = `beta' + 1.000 * `se'
        local lower68 = `beta' - 1.000 * `se'
        local obs     = e(N)

        preserve
            clear
            set obs 1
            gen str14 commodity = "`c'"
            gen horizon         = `h'
            gen beta            = `beta'
            gen se              = `se'
            gen upper90         = `upper90'
            gen lower90         = `lower90'
            gen upper68         = `upper68'
            gen lower68         = `lower68'
            gen nobs            = `obs'
            append using `results'
            sort commodity horizon
            save `results', replace
        restore
    }
}

use `results', clear
sort commodity horizon

save "$dta\lp_iv_gs1_baseline.dta", replace
di "=== lp_iv_gs1_baseline.dta saved ==="
di "Total rows: " _N
list in 1/5

*** We now plot the results 

use "$dta\lp_iv_gs1_baseline.dta", clear

local commodities "Coffee Copper Gold Oil Soybeans Wheat"

foreach c of local commodities {

    twoway ///
        (rarea upper90 lower90 horizon if commodity == "`c'", ///
            color(blue%15) lwidth(none)) ///
        (rarea upper68 lower68 horizon if commodity == "`c'", ///
            color(blue%30) lwidth(none)) ///
        (line beta horizon if commodity == "`c'", ///
            lcolor(blue) lwidth(medium)) ///
        , ///
        yline(0, lcolor(black) lpattern(solid)) ///
        title("`c': LP-IV IRF to Monetary Policy Shock" ///
              "(1-Year Treasury, First Difference)") ///
        xtitle("Months after shock") ///
        ytitle("Cumulative log price change") ///
        xlabel(0(4)24) ///
        legend(order(1 "90% CI" 2 "68% CI" 3 "IRF") ///
               rows(1) size(small)) ///
        note("Endogenous variable: Change in 1-Year Treasury yield" ///
             "Instrument: Acosta et al. (2025) shock" ///
             "Shaded areas = 68% and 90% CI, Newey-West SE")

    local outpath `"$output\01 baseline"'
    graph export `"`outpath'\irf_`c'_lpiv_gs1.png"', replace width(2000)
    di "Saved: 01 baseline\irf_`c'_lpiv_gs1.png"
}


******* VERIFICATION CHECKS BEFORE INTERACTION MODEL **************************

use "$dta\master_panel_gs1.dta", clear
xtset commodity_id date

* CHECK 1: NC gross share is monthly mean (one obs per commodity-month)


di "=== CHECK 1: NC Gross Share — one obs per commodity-month ==="
duplicates report commodity date
* Should show 0 duplicates

* Summary by commodity — check mean and SD look reasonable
tabstat nc_gross_share, by(commodity) stat(n mean sd min max) nototal

* Check first few obs per commodity
foreach c in Coffee Copper Gold Oil Soybeans Wheat {
    di "--- `c' ---"
    list date nc_gross_share if commodity == "`c'" in 1/5
}

* CHECK 2: Rolling correlation coverage by commodity

di "=== CHECK 2: Rolling Correlation Coverage ==="
tabstat rolling_corr, by(commodity) stat(n mean sd min max) nototal

* Check start date per commodity — should be ~24 months after price data starts
table commodity, stat(min date) stat(max date) stat(count rolling_corr)

* Flag commodities with missing rolling_corr
foreach c in Coffee Copper Gold Oil Soybeans Wheat {
    quietly count if missing(rolling_corr) & commodity == "`c'"
    di "`c': missing rolling_corr = " r(N)
}

* Visual check — is missingness concentrated at start of sample?
foreach c in Coffee Copper Gold Oil Soybeans Wheat {
    di "--- `c': first non-missing rolling_corr ---"
    list date rolling_corr if commodity == "`c'" & !missing(rolling_corr) ///
        in 1/3
}

* CHECK 4: Compare sample sizes between the two financialization measures
* After applying L1 (one additional lag) as in the interaction model

di "=== CHECK 4: Effective sample sizes after L1 lag ==="

foreach c in Coffee Copper Gold Oil Soybeans Wheat {

    * nc_gs_ma12 lagged one period
    quietly count if !missing(L1.nc_gs_ma12) & commodity == "`c'"
    local n_nc = r(N)

    * rolling_corr lagged one period
    quietly count if !missing(L1.rolling_corr) & commodity == "`c'"
    local n_corr = r(N)

    di "`c': L1.nc_gs_ma12 non-missing = `n_nc' | L1.rolling_corr non-missing = `n_corr'"
    
    if `n_nc' != `n_corr' {
        di as error "  WARNING: sample size differs between measures for `c'"
    }
}

* CHECK 5: Standardization preview
* Show mean and SD that will be used in the interaction loop

di "=== CHECK 5: Standardization preview ==="

foreach c in Coffee Copper Gold Oil Soybeans Wheat {
    di "--- `c' ---"
    foreach fm in nc_gs_ma12 rolling_corr {
        quietly sum L1.`fm' if commodity == "`c'"
        di "  L1.`fm': mean=" %6.4f r(mean) ///
           " sd=" %6.4f r(sd) ///
           " N=" r(N)
    }
}

di "=== All checks complete — safe to proceed with interaction model ==="

******************************* ANALYSIS 2 - INTERACTION MODEL **********************************************************
* Financialization is standardized separately within each commodity.
* The Oil specification includes contemporaneous and four lagged Känzig oil-supply shocks as additional controls.
* All other commodities use the standard specification without these controls.
* The endogenous variable, d_gs1, is instrumented using the Acosta monetary-policy shock.

use "$dta\master_panel_gs1.dta", clear
xtset commodity_id date

global lags    4
global horizon 24

local curr_Coffee   "d_brl"
local curr_Copper   "d_clp"
local curr_Gold     "d_aud"
local curr_Oil      ""
local curr_Soybeans "d_brl"
local curr_Wheat    ""

local commodities "Copper Gold Oil Soybeans Wheat"
local fin_measures "nc_gs_ma12 rolling_corr"
local fin_labels   `""NC Gross Share 12M MA (CFTC)" "SP500 Rolling Correlation (24M)""'


***Preparatory work: dummy variables setting and financialization measure smoothing
** We generate smoothed NC gross share and we drop first to avoid conflict
capture drop nc_gross_share_ma12
gen nc_gross_share_ma12 = (nc_gross_share + ///
    L1.nc_gross_share  + L2.nc_gross_share  + ///
    L3.nc_gross_share  + L4.nc_gross_share  + ///
    L5.nc_gross_share  + L6.nc_gross_share  + ///
    L7.nc_gross_share  + L8.nc_gross_share  + ///
    L9.nc_gross_share  + L10.nc_gross_share + ///
    L11.nc_gross_share) / 12

capture drop nc_gs_ma12
gen nc_gs_ma12 = nc_gross_share_ma12

** Crisis dummies
capture drop gfc covid ukraine
gen gfc     = (date >= tm(2008m9)  & date <= tm(2009m6))
gen covid   = (date >= tm(2020m3)  & date <= tm(2021m6))
gen ukraine = (date >= tm(2022m2)  & date <= tm(2022m12))

****IV FIRST STAGE 
* d_gs1 (US 10-year treasury) is instrumented by Acosta shock to clean from endogenous component of monetary policy

preserve
    duplicates drop date, force
    tsset date

    regress d_gs1 shock

    di ""
    di "=== First Stage: d_gs1 ~ shock ==="
    di "  Coefficient: " %7.4f _b[shock]
    di "  t-stat:      " %7.4f _b[shock]/_se[shock]
    di "  F-stat:      " %7.4f e(F)
    di "  R-squared:   " %7.4f e(r2)
    di "  N:           " e(N)

    * Warn if instrument is weak
    if e(F) < 10 {
        di as error "WARNING: First stage F-stat below 10 — weak instrument"
    }

    predict d_gs1_hat, xb
    keep date d_gs1_hat
    sort date
    tempfile fittedvals
    save `fittedvals'
restore

merge m:1 date using `fittedvals', nogenerate
sort commodity_id date
xtset commodity_id date

tempfile results
preserve
    clear
    save `results', emptyok replace
restore


**** MAIN REGRESSION LOOP
**We regress commodity prices first differences on instrumented MP shock and controls in standard IV-LP setting. 

foreach c of local commodities {
    foreach fm of local fin_measures {

        local curr_control = "`curr_`c''"

        * Crisis vars — Ukraine added for Oil Soybeans Wheat
        local crisis_vars "gfc covid"
        if inlist("`c'", "Oil", "Soybeans", "Wheat") ///
            local crisis_vars "gfc covid ukraine"

        * Känzig control — Oil only
        local kanzigcontrols ""
        if "`c'" == "Oil" {
            forvalues l = 0/$lags {
                local kanzigcontrols "`kanzigcontrols' L`l'.kanzig_shock"
            }
            di "=== Oil: Känzig controls included: `kanzigcontrols' ==="
        }

        di ""
        di "=== `c' — LP-IV interaction with `fm' (t-1) ==="
        if "`c'" == "Oil" di "    [Känzig oil supply shock control included]"

        * Standardize using THIS commodity's own distribution only
        quietly sum L1.`fm' if commodity == "`c'"
        local mean_fin = r(mean)
        local sd_fin   = r(sd)

        di "    `fm': mean=" %6.4f `mean_fin' ///
           " sd=" %6.4f `sd_fin' ///
           " N=" r(N) " (commodity-specific)"

        * Generate commodity-specific standardized variable and interaction
        * Drop first to avoid conflicts across iterations
        capture drop fin_std_temp
        capture drop d_gs1_x_fin_temp

        gen fin_std_temp = (L1.`fm' - `mean_fin') / `sd_fin' ///
            if commodity == "`c'"

        gen d_gs1_x_fin_temp = d_gs1_hat * fin_std_temp ///
            if commodity == "`c'"

        forvalues h = 0/$horizon {

            * Dependent variable lags
            local depvarlags ""
            forvalues l = 1/$lags {
                local depvarlags "`depvarlags' L`l'.d_log_price"
            }

            * Macro lags
            local macrolags ""
            foreach v in ip_growth inflation {
                forvalues l = 1/$lags {
                    local macrolags "`macrolags' L`l'.`v'"
                }
            }

            * Currency lags — commodity specific
            local currlags ""
            if "`curr_control'" != "" {
                forvalues l = 1/$lags {
                    local currlags "`currlags' L`l'.`curr_control'"
                }
            }

            * Crisis dummy lags
            local crisislags ""
            foreach d of local crisis_vars {
                forvalues l = 0/$lags {
                    local crisislags "`crisislags' L`l'.`d'"
                }
            }

            local bw = max(1, `h')

            * Second stage regression
            * kanzigcontrols is empty string for non-Oil commodities
            * so including it is harmless — Stata ignores empty locals
            quietly newey dep_h`h' ///
                d_gs1_hat d_gs1_x_fin_temp fin_std_temp ///
                `depvarlags' `macrolags' `currlags' ///
                `crisislags' `kanzigcontrols' ///
                if commodity == "`c'", lag(`bw')

            * Extract coefficients
            local beta_shock  = _b[d_gs1_hat]
            local se_shock    = _se[d_gs1_hat]
            local beta_int    = _b[d_gs1_x_fin_temp]
            local se_int      = _se[d_gs1_x_fin_temp]
            local upper_int90 = `beta_int' + 1.645 * `se_int'
            local lower_int90 = `beta_int' - 1.645 * `se_int'
            local upper_int68 = `beta_int' + 1.000 * `se_int'
            local lower_int68 = `beta_int' - 1.000 * `se_int'
            local obs         = e(N)

            * Diagnostic output at selected horizons
            if (`h' == 0 | `h' == 12 | `h' == 24) & "`c'" == "Oil" {
                di "  Oil h=`h' | beta_int=" %7.4f `beta_int' ///
                   " se=" %7.4f `se_int' ///
                   " N=" `obs'
                if "`fm'" == "nc_gs_ma12" {
                    di "  Känzig L0=" %7.4f _b[L0.kanzig_shock] ///
                       " L1=" %7.4f _b[L1.kanzig_shock] ///
                       " L2=" %7.4f _b[L2.kanzig_shock]
                }
            }

            * Save results
            preserve
                clear
                set obs 1
                gen str20 commodity    = "`c'"
                gen str20 fin_measure  = "`fm'"
                gen horizon            = `h'
                gen beta_shock         = `beta_shock'
                gen se_shock           = `se_shock'
                gen beta_int           = `beta_int'
                gen se_int             = `se_int'
                gen upper_int90        = `upper_int90'
                gen lower_int90        = `lower_int90'
                gen upper_int68        = `upper_int68'
                gen lower_int68        = `lower_int68'
                gen nobs               = `obs'
                * Flag whether Känzig control was included
                gen kanzig_included    = ("`c'" == "Oil")
                append using `results'
                sort commodity fin_measure horizon
                save `results', replace
            restore
        }

        * Clean up temporary variables before next iteration
        capture drop fin_std_temp
        capture drop d_gs1_x_fin_temp

        di "=== `c' `fm' complete ==="
    }
}

********************************************************************************
* SAVE RESULTS
********************************************************************************

use `results', clear
sort commodity fin_measure horizon

save "$input\DTA\lp_iv_gs1_interaction_ma12.dta", replace

di ""
di "======================================================"
di "=== lp_iv_gs1_interaction_ma12.dta saved         ==="
di "=== Total rows: " _N
di "=== Commodities: Coffee Copper Gold Oil Soybeans Wheat"
di "=== Oil includes Känzig supply shock control     ==="
di "======================================================"

list commodity fin_measure horizon beta_int kanzig_included ///
    in 1/10

********************************************************************************
* PLOT
********************************************************************************

use "$input\DTA\lp_iv_gs1_interaction_ma12.dta", clear

local commodities "Coffee Copper Gold Oil Soybeans Wheat"
local fm_list     "nc_gs_ma12 rolling_corr"
local fin_labels  `""NC Gross Share 12M MA (CFTC)" "SP500 Rolling Correlation (24M)""'

local n : word count `fm_list'

foreach c of local commodities {
    forvalues i = 1/`n' {

        local fm    : word `i' of `fm_list'
        local fmlbl : word `i' of `fin_labels'

        * Build note — add Känzig note for Oil
        local note_line1 "Negative = higher financialization amplifies"
        local note_line2 "negative price response to monetary tightening"
        local note_line3 "Endogenous: Change in 1-Year Treasury, instrumented by Acosta shock"
        local note_line4 "Financialization standardized within commodity | Lagged 1 period"
        local note_line5 "Shaded areas = 68% and 90% CI, Newey-West SE"

        local kanzig_note ""
        if "`c'" == "Oil" {
            local kanzig_note "Känzig oil supply shock included as control (lag 0-4)"
        }

        twoway ///
            (rarea upper_int90 lower_int90 horizon ///
                if commodity == "`c'" & fin_measure == "`fm'", ///
                color(red%15) lwidth(none)) ///
            (rarea upper_int68 lower_int68 horizon ///
                if commodity == "`c'" & fin_measure == "`fm'", ///
                color(red%30) lwidth(none)) ///
            (line beta_int horizon ///
                if commodity == "`c'" & fin_measure == "`fm'", ///
                lcolor(red) lwidth(medium)) ///
            , ///
            yline(0, lcolor(black) lpattern(solid)) ///
            title("`c': LP-IV Interaction Effect of Financialization (t-1)" ///
                  "Measure: `fmlbl'") ///
            xtitle("Months after shock") ///
            ytitle("Interaction coefficient") ///
            xlabel(0(4)24) ///
            legend(order(1 "90% CI" 2 "68% CI" 3 "Interaction coeff") ///
                   rows(1) size(small)) ///
            note("`note_line1'" ///
                 "`note_line2'" ///
                 "`note_line3'" ///
                 "`note_line4'" ///
                 "`note_line5'" ///
                 "`kanzig_note'")

        local outpath `"$output\02 interaction"'
        graph export ///
            `"`outpath'\irf_`c'_lpiv_interaction_`fm'.png"', ///
            replace width(2000)
        di "Saved: `c' — `fm'"
    }
}

di ""
di "=== Analysis 2 complete ==="
di "=== All IRF plots saved to $output\02 interaction ==="

********************************************************************************
* ANALYSIS 3 - INTERACTION MODEL — CAPPED SAMPLE
* Coffee, Soybeans, Wheat only — capped at 2017m12
* Financialization standardized within each commodity separately
* using capped sample distribution
********************************************************************************

use "$input\DTA\master_panel_gs1.dta", clear
xtset commodity_id date

global lags    4
global horizon 24

local curr_Coffee   "d_brl"
local curr_Soybeans "d_brl"
local curr_Wheat    ""

local commodities "Coffee Soybeans Wheat"
local fin_measures "nc_gs_ma12 rolling_corr"
local fin_labels   `""NC Gross Share 12M MA (CFTC)" "SP500 Rolling Correlation (24M)""'

* Generate smoothed measure
capture drop nc_gross_share_ma12
gen nc_gross_share_ma12 = (nc_gross_share + ///
    L1.nc_gross_share  + L2.nc_gross_share  + ///
    L3.nc_gross_share  + L4.nc_gross_share  + ///
    L5.nc_gross_share  + L6.nc_gross_share  + ///
    L7.nc_gross_share  + L8.nc_gross_share  + ///
    L9.nc_gross_share  + L10.nc_gross_share + ///
    L11.nc_gross_share) / 12

capture drop nc_gs_ma12
gen nc_gs_ma12 = nc_gross_share_ma12

* Crisis dummies
capture drop gfc covid ukraine
gen gfc     = (date >= tm(2008m9)  & date <= tm(2009m6))
gen covid   = (date >= tm(2020m3)  & date <= tm(2021m6))
gen ukraine = (date >= tm(2022m2)  & date <= tm(2022m12))

* Sample cap — all three commodities capped at 2017m12
gen byte sample_cap = 1
replace sample_cap = 0 if date > tm(2017m12)

* Verify cap
di "=== Sample cap verification ==="
tab commodity sample_cap if ///
    inlist(commodity, "Coffee", "Soybeans", "Wheat")

********************************************************************************
* FIRST STAGE — full time series
* Note: first stage uses all available dates for maximum precision
* Cap applied only in second stage
********************************************************************************

preserve
    duplicates drop date, force
    tsset date

    regress d_gs1 shock

    di ""
    di "=== First Stage: d_gs1 ~ shock ==="
    di "  Coefficient: " %7.4f _b[shock]
    di "  t-stat:      " %7.4f _b[shock]/_se[shock]
    di "  F-stat:      " %7.4f e(F)
    di "  R-squared:   " %7.4f e(r2)
    di "  N:           " e(N)

    if e(F) < 10 {
        di as error "WARNING: weak instrument — F-stat below 10"
    }

    predict d_gs1_hat, xb
    keep date d_gs1_hat
    sort date
    tempfile fittedvals
    save `fittedvals'
restore

merge m:1 date using `fittedvals', nogenerate
sort commodity_id date
xtset commodity_id date

* Create results file
tempfile results
preserve
    clear
    save `results', emptyok replace
restore

********************************************************************************
* MAIN LOOP — Coffee Soybeans Wheat only
* Standardization uses each commodity's own capped sample distribution
********************************************************************************

foreach c of local commodities {
    foreach fm of local fin_measures {

        local curr_control = "`curr_`c''"

        * Crisis vars — ukraine included for Soybeans and Wheat
        * but not for Coffee
        local crisis_vars "gfc covid"
        if inlist("`c'", "Soybeans", "Wheat") ///
            local crisis_vars "gfc covid ukraine"

        di ""
        di "=== `c' (capped 1994-2017) — LP-IV interaction with `fm' ==="

        * Standardize using THIS commodity's own capped distribution
        * Both conditions: own commodity AND within cap
        quietly sum L1.`fm' ///
        if commodity == "`c'"
        local mean_fin = r(mean)
        local sd_fin   = r(sd)

        di "    `fm': mean=" %6.4f `mean_fin' ///
           " sd=" %6.4f `sd_fin' " N=" r(N)

        * Generate commodity-specific variables
        capture drop fin_std_temp
        capture drop d_gs1_x_fin_temp

        gen fin_std_temp = (L1.`fm' - `mean_fin') / `sd_fin' ///
        if commodity == "`c'"

         gen d_gs1_x_fin_temp = d_gs1_hat * fin_std_temp ///
         if commodity == "`c'"

        forvalues h = 0/$horizon {

            * Dependent variable lags
            local depvarlags ""
            forvalues l = 1/$lags {
                local depvarlags "`depvarlags' L`l'.d_log_price"
            }

            * Macro lags
            local macrolags ""
            foreach v in ip_growth inflation {
                forvalues l = 1/$lags {
                    local macrolags "`macrolags' L`l'.`v'"
                }
            }

            * Currency lags
            local currlags ""
            if "`curr_control'" != "" {
                forvalues l = 1/$lags {
                    local currlags "`currlags' L`l'.`curr_control'"
                }
            }

            * Crisis dummy lags
            local crisislags ""
            foreach d of local crisis_vars {
                forvalues l = 0/$lags {
                    local crisislags "`crisislags' L`l'.`d'"
                }
            }

            local bw = max(1, `h')

            * Second stage — restricted to capped sample
            quietly newey dep_h`h' ///
                d_gs1_hat d_gs1_x_fin_temp fin_std_temp ///
                `depvarlags' `macrolags' `currlags' `crisislags' ///
                if commodity == "`c'" & sample_cap == 1, lag(`bw')

            * Extract coefficients
            local beta_shock  = _b[d_gs1_hat]
            local se_shock    = _se[d_gs1_hat]
            local beta_int    = _b[d_gs1_x_fin_temp]
            local se_int      = _se[d_gs1_x_fin_temp]
            local upper_int90 = `beta_int' + 1.645 * `se_int'
            local lower_int90 = `beta_int' - 1.645 * `se_int'
            local upper_int68 = `beta_int' + 1.000 * `se_int'
            local lower_int68 = `beta_int' - 1.000 * `se_int'
            local obs         = e(N)

            preserve
                clear
                set obs 1
                gen str20 commodity   = "`c'"
                gen str20 fin_measure = "`fm'"
                gen horizon           = `h'
                gen beta_shock        = `beta_shock'
                gen se_shock          = `se_shock'
                gen beta_int          = `beta_int'
                gen se_int            = `se_int'
                gen upper_int90       = `upper_int90'
                gen lower_int90       = `lower_int90'
                gen upper_int68       = `upper_int68'
                gen lower_int68       = `lower_int68'
                gen nobs              = `obs'
                append using `results'
                sort commodity fin_measure horizon
                save `results', replace
            restore
        }

        capture drop fin_std_temp
        capture drop d_gs1_x_fin_temp

        di "=== `c' `fm' complete ==="
    }
}

********************************************************************************
* SAVE
********************************************************************************

use `results', clear
sort commodity fin_measure horizon

save "$input\DTA\lp_iv_gs1_interaction_capped_agri.dta", replace

di ""
di "======================================================"
di "=== lp_iv_gs1_interaction_capped_agri.dta saved  ==="
di "=== Commodities: Coffee Soybeans Wheat            ==="
di "=== Sample: 1994-2017 (capped)                   ==="
di "=== Total rows: " _N
di "======================================================"

list commodity fin_measure horizon beta_int nobs in 1/6

********************************************************************************
* PLOT
********************************************************************************

use "$input\DTA\lp_iv_gs1_interaction_capped_agri.dta", clear

local commodities "Coffee Soybeans Wheat"
local fm_list     "nc_gs_ma12 rolling_corr"
local fin_labels  `""NC Gross Share 12M MA (CFTC)" "SP500 Rolling Correlation (24M)""'

local n : word count `fm_list'

foreach c of local commodities {
    forvalues i = 1/`n' {

        local fm    : word `i' of `fm_list'
        local fmlbl : word `i' of `fin_labels'

        twoway ///
            (rarea upper_int90 lower_int90 horizon ///
                if commodity == "`c'" & fin_measure == "`fm'", ///
                color(red%15) lwidth(none)) ///
            (rarea upper_int68 lower_int68 horizon ///
                if commodity == "`c'" & fin_measure == "`fm'", ///
                color(red%30) lwidth(none)) ///
            (line beta_int horizon ///
                if commodity == "`c'" & fin_measure == "`fm'", ///
                lcolor(red) lwidth(medium)) ///
            , ///
            yline(0, lcolor(black) lpattern(solid)) ///
            title("`c': LP-IV Interaction Effect of Financialization (t-1)" ///
                  "Measure: `fmlbl'") ///
            xtitle("Months after shock") ///
            ytitle("Interaction coefficient") ///
            xlabel(0(4)24) ///
            legend(order(1 "90% CI" 2 "68% CI" 3 "Interaction coeff") ///
                   rows(1) size(small)) ///
            note("Negative = higher financialization amplifies" ///
                 "negative price response to monetary tightening" ///
                 "Capped sample 1994-2017" ///
                 "Endogenous: Change in 1-Year Treasury, instrumented by Acosta shock" ///
                 "Financialization standardized within commodity | Lagged 1 period" ///
                 "Shaded areas = 68% and 90% CI, Newey-West SE")

        local outpath `"$output\03 capped agriculture"'
        graph export ///
            `"`outpath'\irf_`c'_lpiv_interaction_`fm'_capped.png"', ///
            replace width(2000)
        di "Saved: `c' `fm' capped"
    }
}

di "=== Analysis 3 capped agricultural complete ==="

********************************************************************************
* ANALYSIS 3b - INTERACTION MODEL — POSITIVE ROLLING CORRELATION
* Coffee, Soybeans, Wheat
* Two sample versions:
*   1. Full sample 1994-2025
*   2. Capped sample 1994-2017
* rolling_corr_pos = max(rolling_corr, 0)
* Financialization standardized within each commodity and sample
********************************************************************************

use "$input\DTA\master_panel_gs1.dta", clear
xtset commodity_id date

global lags    4
global horizon 24

local curr_Coffee   "d_brl"
local curr_Soybeans "d_brl"
local curr_Wheat    ""

local commodities "Coffee Soybeans Wheat"

********************************************************************************
* PREPARE VARIABLES
********************************************************************************

capture drop nc_gross_share_ma12
gen nc_gross_share_ma12 = (nc_gross_share + ///
    L1.nc_gross_share  + L2.nc_gross_share  + ///
    L3.nc_gross_share  + L4.nc_gross_share  + ///
    L5.nc_gross_share  + L6.nc_gross_share  + ///
    L7.nc_gross_share  + L8.nc_gross_share  + ///
    L9.nc_gross_share  + L10.nc_gross_share + ///
    L11.nc_gross_share) / 12

capture drop nc_gs_ma12
gen nc_gs_ma12 = nc_gross_share_ma12

* Positive rolling correlation
capture drop rolling_corr_pos
gen rolling_corr_pos = max(rolling_corr, 0)

* Crisis dummies
capture drop gfc covid ukraine
gen gfc     = (date >= tm(2008m9)  & date <= tm(2009m6))
gen covid   = (date >= tm(2020m3)  & date <= tm(2021m6))
gen ukraine = (date >= tm(2022m2)  & date <= tm(2022m12))

* Both sample caps defined as separate variables
* Full sample: all obs = 1
* Capped: post 2017m12 = 0
gen byte sample_full   = 1
gen byte sample_capped = (date <= tm(2017m12))

* Verify
di "=== Sample sizes ==="
foreach c in Coffee Soybeans Wheat {
    count if commodity == "`c'" & sample_full == 1
    di "`c' full sample: " r(N)
    count if commodity == "`c'" & sample_capped == 1
    di "`c' capped sample: " r(N)
}

local fin_measures "nc_gs_ma12 rolling_corr_pos"
local fin_labels   `""NC Gross Share 12M MA (CFTC)" "SP500 Rolling Corr (Positive Only)""'

********************************************************************************
* FIRST STAGE
********************************************************************************

preserve
    duplicates drop date, force
    tsset date

    regress d_gs1 shock

    di ""
    di "=== First Stage: d_gs1 ~ shock ==="
    di "  Coefficient: " %7.4f _b[shock]
    di "  t-stat:      " %7.4f _b[shock]/_se[shock]
    di "  F-stat:      " %7.4f e(F)
    di "  R-squared:   " %7.4f e(r2)
    di "  N:           " e(N)

    predict d_gs1_hat, xb
    keep date d_gs1_hat
    sort date
    tempfile fittedvals
    save `fittedvals'
restore

merge m:1 date using `fittedvals', nogenerate
sort commodity_id date
xtset commodity_id date

********************************************************************************
* OUTER LOOP OVER SAMPLE DEFINITIONS
********************************************************************************

local sample_names   "full capped"
local slabel_full    "Full sample 1994-2025"
local slabel_capped  "Capped sample 1994-2017"

local fin_measures          "nc_gs_ma12 rolling_corr_pos"
local fmlbl_nc_gs_ma12      "NC Gross Share 12M MA (CFTC)"
local fmlbl_rolling_corr_pos "SP500 Rolling Corr (Positive Only)"

foreach sname of local sample_names {

    local slabel = "`slabel_`sname''"

    di ""
    di "======================================================"
    di "=== SAMPLE: `slabel'"
    di "======================================================"

    tempfile results_`sname'
    preserve
        clear
        save `results_`sname'', emptyok replace
    restore

    foreach c of local commodities {
        foreach fm of local fin_measures {

            local curr_control = "`curr_`c''"
            local fmlbl = "`fmlbl_`fm''"

            local crisis_vars "gfc covid"
            if inlist("`c'", "Soybeans", "Wheat") ///
                local crisis_vars "gfc covid ukraine"

            di ""
            di "=== `c' (`slabel') — `fm' ==="

            quietly sum L1.`fm' ///
                if commodity == "`c'" & sample_`sname' == 1
            local mean_fin = r(mean)
            local sd_fin   = r(sd)

            di "    mean=" %6.4f `mean_fin' ///
               " sd=" %6.4f `sd_fin' " N=" r(N)

            capture drop fin_std_temp
            capture drop d_gs1_x_fin_temp

            gen fin_std_temp = (L1.`fm' - `mean_fin') / `sd_fin' ///
                if commodity == "`c'"

            gen d_gs1_x_fin_temp = d_gs1_hat * fin_std_temp ///
                if commodity == "`c'"

            forvalues h = 0/$horizon {

                local depvarlags ""
                forvalues l = 1/$lags {
                    local depvarlags "`depvarlags' L`l'.d_log_price"
                }

                local macrolags ""
                foreach v in ip_growth inflation {
                    forvalues l = 1/$lags {
                        local macrolags "`macrolags' L`l'.`v'"
                    }
                }

                local currlags ""
                if "`curr_control'" != "" {
                    forvalues l = 1/$lags {
                        local currlags "`currlags' L`l'.`curr_control'"
                    }
                }

                local crisislags ""
                foreach d of local crisis_vars {
                    forvalues l = 0/$lags {
                        local crisislags "`crisislags' L`l'.`d'"
                    }
                }

                local bw = max(1, `h')

                quietly newey dep_h`h' ///
                    d_gs1_hat d_gs1_x_fin_temp fin_std_temp ///
                    `depvarlags' `macrolags' `currlags' `crisislags' ///
                    if commodity == "`c'" & sample_`sname' == 1, ///
                    lag(`bw')

                local beta_shock  = _b[d_gs1_hat]
                local se_shock    = _se[d_gs1_hat]
                local beta_int    = _b[d_gs1_x_fin_temp]
                local se_int      = _se[d_gs1_x_fin_temp]
                local upper_int90 = `beta_int' + 1.645 * `se_int'
                local lower_int90 = `beta_int' - 1.645 * `se_int'
                local upper_int68 = `beta_int' + 1.000 * `se_int'
                local lower_int68 = `beta_int' - 1.000 * `se_int'
                local obs         = e(N)

                preserve
                    clear
                    set obs 1
                    gen str20 commodity   = "`c'"
                    gen str20 fin_measure = "`fm'"
                    gen str20 sample      = "`sname'"
                    gen horizon           = `h'
                    gen beta_shock        = `beta_shock'
                    gen se_shock          = `se_shock'
                    gen beta_int          = `beta_int'
                    gen se_int            = `se_int'
                    gen upper_int90       = `upper_int90'
                    gen lower_int90       = `lower_int90'
                    gen upper_int68       = `upper_int68'
                    gen lower_int68       = `lower_int68'
                    gen nobs              = `obs'
                    append using `results_`sname''
                    sort commodity fin_measure horizon
                    save `results_`sname'', replace
                restore
            }

            capture drop fin_std_temp
            capture drop d_gs1_x_fin_temp

            di "=== `c' `fm' `sname' complete ==="
        }
    }
}

********************************************************************************
* COMBINE AND SAVE
********************************************************************************

use `results_full', clear
append using `results_capped'
sort sample commodity fin_measure horizon

save "$input\DTA\lp_iv_gs1_interaction_agri_pos.dta", replace

di ""
di "======================================================"
di "=== lp_iv_gs1_interaction_agri_pos.dta saved     ==="
di "=== Total rows: " _N
di "======================================================"

tab sample commodity

********************************************************************************
* PLOT
********************************************************************************

use "$input\DTA\lp_iv_gs1_interaction_agri_pos.dta", clear

local commodities  "Coffee Soybeans Wheat"
local fin_measures "nc_gs_ma12 rolling_corr_pos"
local sample_names "full capped"

local fmlbl_nc_gs_ma12       "NC Gross Share 12M MA (CFTC)"
local fmlbl_rolling_corr_pos "SP500 Rolling Corr (Positive Only)"
local slabel_full            "Full sample 1994-2025"
local slabel_capped          "Capped sample 1994-2017"

foreach c of local commodities {
    foreach fm of local fin_measures {
        foreach sname of local sample_names {

            local fmlbl  = "`fmlbl_`fm''"
            local slabel = "`slabel_`sname''"

            local measure_note ""
            if "`fm'" == "rolling_corr_pos" {
                local measure_note ///
                    "Negative correlation episodes set to zero"
            }

            twoway ///
                (rarea upper_int90 lower_int90 horizon ///
                    if commodity == "`c'" & ///
                       fin_measure == "`fm'" & ///
                       sample == "`sname'", ///
                    color(red%15) lwidth(none)) ///
                (rarea upper_int68 lower_int68 horizon ///
                    if commodity == "`c'" & ///
                       fin_measure == "`fm'" & ///
                       sample == "`sname'", ///
                    color(red%30) lwidth(none)) ///
                (line beta_int horizon ///
                    if commodity == "`c'" & ///
                       fin_measure == "`fm'" & ///
                       sample == "`sname'", ///
                    lcolor(red) lwidth(medium)) ///
                , ///
                yline(0, lcolor(black) lpattern(solid)) ///
                title("`c': LP-IV Interaction Effect of Financialization (t-1)" ///
                      "Measure: `fmlbl'") ///
                xtitle("Months after shock") ///
                ytitle("Interaction coefficient") ///
                xlabel(0(4)24) ///
                legend(order(1 "90% CI" 2 "68% CI" 3 "Interaction coeff") ///
                       rows(1) size(small)) ///
                note("Negative = higher financialization amplifies" ///
                     "negative price response to monetary tightening" ///
                     "`slabel'" ///
                     "Endogenous: Change in 1-Year Treasury, instrumented by Acosta shock" ///
                     "Financialization standardized within commodity | Lagged 1 period" ///
                     "`measure_note'" ///
                     "Shaded areas = 68% and 90% CI, Newey-West SE")

            local outpath `"$output\03 capped agriculture"'
            graph export ///
                `"`outpath'\irf_`c'_lpiv_interaction_`fm'_`sname'.png"', ///
                replace width(2000)
            di "Saved: `c' `fm' `sname'"
        }
    }
}

di "=== Analysis 3b complete ==="

********************************************************************************
* ANALYSIS 4 - PLACEBO TEST
* Does financialization (rolling_corr / nc_gs_ma12) amplify the response to a
* shock that should NOT operate through risk appetite (Kanzig OPEC supply shock)?
* If F amplifies monetary shocks but not this placebo shock, that is evidence
* against the "mechanical encoding" critique of the equity-correlation measure.
* Commodity: Oil only (Kanzig shock is oil-specific)
* Monetary shock (d_gs1_hat) is included as a control throughout, so the
* interaction coefficient is not picking up a confounded monetary channel.
********************************************************************************

use "$input\DTA\master_panel_gs1.dta", clear
xtset commodity_id date

global lags    4
global horizon 24

local commodities "Oil"
local fin_measures "nc_gs_ma12 rolling_corr"
local fin_labels   `""NC Gross Share 12M MA (CFTC)" "SP500 Rolling Correlation (24M)""'

********************************************************************************
* PREPARE VARIABLES
********************************************************************************

capture drop nc_gross_share_ma12
gen nc_gross_share_ma12 = (nc_gross_share + ///
    L1.nc_gross_share  + L2.nc_gross_share  + ///
    L3.nc_gross_share  + L4.nc_gross_share  + ///
    L5.nc_gross_share  + L6.nc_gross_share  + ///
    L7.nc_gross_share  + L8.nc_gross_share  + ///
    L9.nc_gross_share  + L10.nc_gross_share + ///
    L11.nc_gross_share) / 12

capture drop nc_gs_ma12
gen nc_gs_ma12 = nc_gross_share_ma12

capture drop gfc covid ukraine
gen gfc     = (date >= tm(2008m9)  & date <= tm(2009m6))
gen covid   = (date >= tm(2020m3)  & date <= tm(2021m6))
gen ukraine = (date >= tm(2022m2)  & date <= tm(2022m12))

* Confirm Kanzig shock present
capture confirm variable kanzig_shock
if _rc {
    di as error "ERROR: kanzig_shock not found — run Kanzig merge step first"
    exit 111
}

********************************************************************************
* FIRST STAGE — same as Analysis 2, needed to control for the monetary channel
********************************************************************************

preserve
    duplicates drop date, force
    tsset date
    regress d_gs1 shock
    di "=== First Stage: d_gs1 ~ shock === F-stat: " %7.4f e(F)
    predict d_gs1_hat, xb
    keep date d_gs1_hat
    sort date
    tempfile fittedvals
    save `fittedvals'
restore

merge m:1 date using `fittedvals', nogenerate
sort commodity_id date
xtset commodity_id date

* Create results file
tempfile results_placebo
preserve
    clear
    save `results_placebo', emptyok replace
restore

********************************************************************************
* MAIN LOOP — Oil only, Kanzig shock as the "treatment"
********************************************************************************

foreach c of local commodities {
    foreach fm of local fin_measures {

        di ""
        di "=== PLACEBO: `c' — Kanzig shock interacted with `fm' (t-1) ==="

        * Standardize financialization measure — same as Analysis 2
        quietly sum L1.`fm' if commodity == "`c'"
        local mean_fin = r(mean)
        local sd_fin   = r(sd)

        di "    `fm': mean=" %6.4f `mean_fin' " sd=" %6.4f `sd_fin'

        capture drop fin_std_temp
        capture drop kanzig_x_fin_temp

        gen fin_std_temp = (L1.`fm' - `mean_fin') / `sd_fin' ///
            if commodity == "`c'"

        * Interaction: Kanzig shock (lag 0) x standardized financialization
        gen kanzig_x_fin_temp = kanzig_shock * fin_std_temp ///
            if commodity == "`c'"

        forvalues h = 0/$horizon {

            local depvarlags ""
            forvalues l = 1/$lags {
                local depvarlags "`depvarlags' L`l'.d_log_price"
            }

            local macrolags ""
            foreach v in ip_growth inflation {
                forvalues l = 1/$lags {
                    local macrolags "`macrolags' L`l'.`v'"
                }
            }

            local crisislags ""
            foreach d in gfc covid ukraine {
                forvalues l = 0/$lags {
                    local crisislags "`crisislags' L`l'.`d'"
                }
            }

            * Additional Kanzig lags 1-4 as controls (lag 0 is the main regressor)
            local kanziglags ""
            forvalues l = 1/$lags {
                local kanziglags "`kanziglags' L`l'.kanzig_shock"
            }

            local bw = max(1, `h')

            * Second stage: control for d_gs1_hat (monetary shock) throughout
            quietly newey dep_h`h' ///
                kanzig_shock kanzig_x_fin_temp fin_std_temp ///
                d_gs1_hat ///
                `depvarlags' `macrolags' `crisislags' `kanziglags' ///
                if commodity == "`c'", lag(`bw')

            local beta_shock  = _b[kanzig_shock]
            local se_shock    = _se[kanzig_shock]
            local beta_int    = _b[kanzig_x_fin_temp]
            local se_int      = _se[kanzig_x_fin_temp]
            local upper_int90 = `beta_int' + 1.645 * `se_int'
            local lower_int90 = `beta_int' - 1.645 * `se_int'
            local upper_int68 = `beta_int' + 1.000 * `se_int'
            local lower_int68 = `beta_int' - 1.000 * `se_int'
            local obs         = e(N)

            preserve
                clear
                set obs 1
                gen str20 commodity   = "`c'"
                gen str20 fin_measure = "`fm'"
                gen horizon           = `h'
                gen beta_shock        = `beta_shock'
                gen se_shock          = `se_shock'
                gen beta_int          = `beta_int'
                gen se_int            = `se_int'
                gen upper_int90       = `upper_int90'
                gen lower_int90       = `lower_int90'
                gen upper_int68       = `upper_int68'
                gen lower_int68       = `lower_int68'
                gen nobs              = `obs'
                append using `results_placebo'
                sort commodity fin_measure horizon
                save `results_placebo', replace
            restore
        }

        capture drop fin_std_temp
        capture drop kanzig_x_fin_temp

        di "=== `c' `fm' placebo complete ==="
    }
}

********************************************************************************
* SAVE
********************************************************************************

use `results_placebo', clear
sort commodity fin_measure horizon

save "$input\DTA\lp_iv_placebo_kanzig_interaction.dta", replace

di ""
di "======================================================"
di "=== lp_iv_placebo_kanzig_interaction.dta saved   ==="
di "=== If beta_int ~ 0 and CIs cover 0 throughout,  ==="
di "=== this supports the equity-correlation measure ==="
di "=== NOT mechanically amplifying any/every shock. ==="
di "======================================================"

list commodity fin_measure horizon beta_int se_int in 1/10

********************************************************************************
* PLOT
********************************************************************************

use "$input\DTA\lp_iv_placebo_kanzig_interaction.dta", clear

local fm_list     "nc_gs_ma12 rolling_corr"
local fin_labels  `""NC Gross Share 12M MA (CFTC)" "SP500 Rolling Correlation (24M)""'
local n : word count `fm_list'

forvalues i = 1/`n' {

    local fm    : word `i' of `fm_list'
    local fmlbl : word `i' of `fin_labels'

    twoway ///
        (rarea upper_int90 lower_int90 horizon ///
            if fin_measure == "`fm'", color(gray%15) lwidth(none)) ///
        (rarea upper_int68 lower_int68 horizon ///
            if fin_measure == "`fm'", color(gray%30) lwidth(none)) ///
        (line beta_int horizon ///
            if fin_measure == "`fm'", lcolor(black) lwidth(medium)) ///
        , ///
        yline(0, lcolor(red) lpattern(solid)) ///
        title("PLACEBO: Oil — Kanzig OPEC Shock x Financialization" ///
              "Measure: `fmlbl'") ///
        xtitle("Months after shock") ///
        ytitle("Interaction coefficient") ///
        xlabel(0(4)24) ///
        legend(order(1 "90% CI" 2 "68% CI" 3 "Interaction coeff") ///
               rows(1) size(small)) ///
        note("If this interaction is null while the monetary-shock" ///
             "interaction (Analysis 2) is not, financialization" ///
             "amplifies monetary transmission specifically -" ///
             "not any shock indiscriminately." ///
             "Monetary shock (d_gs1_hat) controlled for throughout" ///
             "Shaded areas = 68% and 90% CI, Newey-West SE")

    local outpath `"$output\06 Robustness checks"'
    graph export ///
        `"`outpath'\irf_Oil_placebo_kanzig_`fm'.png"', ///
        replace width(2000)
    di "Saved: placebo Oil `fm'"
}

di "=== Analysis 4 (placebo) complete ==="

********************************************************************************
* IMPORT GPR (GEOPOLITICAL RISK) SHOCK — Caldara & Iacoviello
* Source file: data_gpr_export.xls
* "month" is already a Stata %td daily date on import — no offset needed
********************************************************************************

import excel "$input\data_gpr_export.xls", sheet("Sheet1") firstrow clear

keep month GPR
rename GPR gpr_level

* month is already a Stata daily date (%td) — just convert to monthly
gen date = mofd(month)
format date %tm

* Keep sample period
keep if date >= ym(1993,1) & date <= ym(2025,12)

sort date

* Check for duplicates / gaps
duplicates report date
* Should be 0 duplicates — one obs per month

* Check coverage
sum date
di "GPR coverage: " %tm `=r(min)' " to " %tm `=r(max)'

* Shock = monthly log change in the index
gen gpr_shock = log(gpr_level) - log(gpr_level[_n-1])

* Quick visual check
twoway line gpr_shock date, ///
    title("GPR Index: Monthly Log Change") ///
    xline(`=ym(2004,1)', lcolor(red) lpattern(dash)) ///
    xtitle("") ytitle("Log change") ///
    yline(0, lcolor(black) lpattern(solid)) ///
    note("Red dashed line = 2004 financialization break")
graph export "$output\gpr_shock_check.png", replace width(2000)

keep date gpr_level gpr_shock
sort date
save "$input\DTA\gpr_shock.dta", replace

di "=== gpr_shock.dta saved ==="
sum gpr_level gpr_shock, detail

use "$input\DTA\master_panel_gs1.dta", clear

merge m:1 date using "$input\DTA\gpr_shock.dta", keep(1 3) nogenerate
di "After GPR merge: " _N " obs"
count if missing(gpr_shock)
di "Missing gpr_shock: " r(N)

save "$input\DTA\master_panel_gs1.dta", replace

di "=== master_panel_gs1.dta updated with gpr_shock ==="


********************************************************************************
* ANALYSIS 5 - RISK-SENTIMENT (GPR) PLACEBO TEST
* Does financialization amplify the response to a geopolitical risk shock
* (a risk-sentiment event, but not a monetary policy shock)?
* All six commodities. Monetary shock (d_gs1_hat) controlled for throughout
* as a confound control — NOT interacted, since it is a separate treatment,
* not a mediator of the GPR effect.
********************************************************************************

use "$input\DTA\master_panel_gs1.dta", clear
xtset commodity_id date

global lags    4
global horizon 24

local curr_Coffee   "d_brl"
local curr_Copper   "d_clp"
local curr_Gold     "d_aud"
local curr_Oil      ""
local curr_Soybeans "d_brl"
local curr_Wheat    ""

local commodities "Coffee Copper Gold Oil Soybeans Wheat"
local fin_measures "nc_gs_ma12 rolling_corr"

********************************************************************************
* PREPARE VARIABLES
********************************************************************************

capture drop nc_gross_share_ma12
gen nc_gross_share_ma12 = (nc_gross_share + ///
    L1.nc_gross_share + L2.nc_gross_share + L3.nc_gross_share + ///
    L4.nc_gross_share + L5.nc_gross_share + L6.nc_gross_share + ///
    L7.nc_gross_share + L8.nc_gross_share + L9.nc_gross_share + ///
    L10.nc_gross_share + L11.nc_gross_share) / 12
capture drop nc_gs_ma12
gen nc_gs_ma12 = nc_gross_share_ma12

capture drop gfc covid ukraine
gen gfc     = (date >= tm(2008m9)  & date <= tm(2009m6))
gen covid   = (date >= tm(2020m3)  & date <= tm(2021m6))
gen ukraine = (date >= tm(2022m2)  & date <= tm(2022m12))

capture confirm variable gpr_shock
if _rc {
    di as error "ERROR: gpr_shock not found — run GPR import/merge step first"
    exit 111
}

count if missing(gpr_shock)
di "=== Missing gpr_shock obs: " r(N) " (should be small given full-sample GPR coverage) ==="

********************************************************************************
* FIRST STAGE — same as Analysis 2/4
********************************************************************************

preserve
    duplicates drop date, force
    tsset date
    regress d_gs1 shock
    di ""
    di "=== First Stage: d_gs1 ~ shock ==="
    di "  F-stat: " %7.4f e(F)
    if e(F) < 10 di as error "WARNING: weak instrument"
    predict d_gs1_hat, xb
    keep date d_gs1_hat
    sort date
    tempfile fittedvals
    save `fittedvals'
restore

merge m:1 date using `fittedvals', nogenerate
sort commodity_id date
xtset commodity_id date

tempfile results_gpr
preserve
    clear
    save `results_gpr', emptyok replace
restore

********************************************************************************
* MAIN LOOP — GPR shock interacted with financialization, all commodities
********************************************************************************

foreach c of local commodities {
    foreach fm of local fin_measures {

        local curr_control = "`curr_`c''"
        local crisis_vars "gfc covid"
        if inlist("`c'", "Oil", "Soybeans", "Wheat") ///
            local crisis_vars "gfc covid ukraine"

        di ""
        di "=== GPR PLACEBO: `c' — interaction with `fm' (t-1) ==="

        quietly sum L1.`fm' if commodity == "`c'"
        local mean_fin = r(mean)
        local sd_fin   = r(sd)

        di "    `fm': mean=" %6.4f `mean_fin' " sd=" %6.4f `sd_fin' ///
           " N=" r(N)

        capture drop fin_std_temp
        capture drop gpr_x_fin_temp

        gen fin_std_temp = (L1.`fm' - `mean_fin') / `sd_fin' ///
            if commodity == "`c'"

        gen gpr_x_fin_temp = gpr_shock * fin_std_temp ///
            if commodity == "`c'"

        forvalues h = 0/$horizon {

            local depvarlags ""
            forvalues l = 1/$lags {
                local depvarlags "`depvarlags' L`l'.d_log_price"
            }

            local macrolags ""
            foreach v in ip_growth inflation {
                forvalues l = 1/$lags {
                    local macrolags "`macrolags' L`l'.`v'"
                }
            }

            local currlags ""
            if "`curr_control'" != "" {
                forvalues l = 1/$lags {
                    local currlags "`currlags' L`l'.`curr_control'"
                }
            }

            local crisislags ""
            foreach d of local crisis_vars {
                forvalues l = 0/$lags {
                    local crisislags "`crisislags' L`l'.`d'"
                }
            }

            local bw = max(1, `h')

            * d_gs1_hat included as flat control — separate treatment, not
            * interacted, avoids the bad-control/mediator problem
            quietly newey dep_h`h' ///
                gpr_shock gpr_x_fin_temp fin_std_temp d_gs1_hat ///
                `depvarlags' `macrolags' `currlags' `crisislags' ///
                if commodity == "`c'", lag(`bw')

            local beta_shock  = _b[gpr_shock]
            local se_shock    = _se[gpr_shock]
            local beta_int    = _b[gpr_x_fin_temp]
            local se_int      = _se[gpr_x_fin_temp]
            local upper_int90 = `beta_int' + 1.645 * `se_int'
            local lower_int90 = `beta_int' - 1.645 * `se_int'
            local upper_int68 = `beta_int' + 1.000 * `se_int'
            local lower_int68 = `beta_int' - 1.000 * `se_int'
            local obs         = e(N)

            preserve
                clear
                set obs 1
                gen str20 commodity   = "`c'"
                gen str20 fin_measure = "`fm'"
                gen horizon           = `h'
                gen beta_shock        = `beta_shock'
                gen se_shock          = `se_shock'
                gen beta_int          = `beta_int'
                gen se_int            = `se_int'
                gen upper_int90       = `upper_int90'
                gen lower_int90       = `lower_int90'
                gen upper_int68       = `upper_int68'
                gen lower_int68       = `lower_int68'
                gen nobs              = `obs'
                append using `results_gpr'
                sort commodity fin_measure horizon
                save `results_gpr', replace
            restore
        }

        capture drop fin_std_temp
        capture drop gpr_x_fin_temp
        di "=== `c' `fm' GPR placebo complete ==="
    }
}

********************************************************************************
* SAVE
********************************************************************************

use `results_gpr', clear
sort commodity fin_measure horizon
save "$input\DTA\lp_iv_placebo_gpr_interaction.dta", replace

di ""
di "======================================================"
di "=== lp_iv_placebo_gpr_interaction.dta saved      ==="
di "=== Total rows: " _N
di "======================================================"

list commodity fin_measure horizon beta_int se_int in 1/12

********************************************************************************
* PLOT
********************************************************************************

use "$input\DTA\lp_iv_placebo_gpr_interaction.dta", clear

local commodities "Coffee Copper Gold Oil Soybeans Wheat"
local fm_list     "nc_gs_ma12 rolling_corr"
local fin_labels  `""NC Gross Share 12M MA (CFTC)" "SP500 Rolling Correlation (24M)""'
local n : word count `fm_list'

capture mkdir `"$output\06 Robustnness checks"'

foreach c of local commodities {
    forvalues i = 1/`n' {

        local fm    : word `i' of `fm_list'
        local fmlbl : word `i' of `fin_labels'

        twoway ///
            (rarea upper_int90 lower_int90 horizon ///
                if commodity == "`c'" & fin_measure == "`fm'", ///
                color(green%15) lwidth(none)) ///
            (rarea upper_int68 lower_int68 horizon ///
                if commodity == "`c'" & fin_measure == "`fm'", ///
                color(green%30) lwidth(none)) ///
            (line beta_int horizon ///
                if commodity == "`c'" & fin_measure == "`fm'", ///
                lcolor(green) lwidth(medium)) ///
            , ///
            yline(0, lcolor(red) lpattern(solid)) ///
            title("PLACEBO: `c' — GPR Shock x Financialization" ///
                  "Measure: `fmlbl'") ///
            xtitle("Months after shock") ///
            ytitle("Interaction coefficient") ///
            xlabel(0(4)24) ///
            legend(order(1 "90% CI" 2 "68% CI" 3 "Interaction coeff") ///
                   rows(1) size(small)) ///
            note("Monetary shock (d_gs1_hat) controlled for, not interacted" ///
                 "Shaded areas = 68% and 90% CI, Newey-West SE")

        local outpath `"$output\06 Robustness checks"'
        graph export ///
            `"`outpath'\irf_`c'_placebo_gpr_`fm'.png"', ///
            replace width(2000)
        di "Saved: GPR placebo `c' `fm'"
    }
}

di "=== Analysis 5 (GPR placebo) complete ==="
