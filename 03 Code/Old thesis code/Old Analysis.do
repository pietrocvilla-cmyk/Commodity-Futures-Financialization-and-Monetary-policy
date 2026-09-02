********* Master's thesis empirical analysis - OLD ANALYSIS - Monetary economics************
/* 

Author: Pietro Villa 

University: London School of Economics and Political Science
Degree: MSc Economics 
Date: 03-03-2025 
Description: 
This script performs the empirical work for the Master's final thesis on the effects of monetary policy shocks on commodity (futures) prices and the role of financialization - OBSOLETE. IV with fed rate and no IV at all


*/

*********** DATA CLEANING **************************************

* ---- 0. Set working directory ----
global main "C:\Users\pitvi\OneDrive\Documenti\03 LSE\03 Dissertation"
global input "$main\02 Data"
global output "$main\04 Output - figures and tables"

cd "$main"

********************************************************************************
use "$input\DTA\master_panel.dta", clear
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
local subsamples  "pre post"

* Save master panel to tempfile so we can reload after each iteration
tempfile master results
save `master', replace

* Create empty results file
clear
save `results', emptyok replace

foreach c of local commodities {
    foreach s of local subsamples {

        if "`s'" == "pre"  local cond "post == 0"
        if "`s'" == "post" local cond "post == 1"

        local curr_control = "`curr_`c''"

        di "=== `c' — `s' === currency: `curr_control'"

        forvalues h = 0/$horizon {

            * Reload master panel at each horizon
            use `master', clear
            xtset commodity_id date

            * Build lag controls
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

            local bw = max(1, `h')

            * Run LP
            quietly newey dep_h`h' shock ///
                `depvarlags' `macrolags' `currlags' ///
                if commodity == "`c'" & `cond', ///
                lag(`bw')

            * Store results
            local beta  = _b[shock]
            local se    = _se[shock]
            local upper = `beta' + 1.96 * `se'
            local lower = `beta' - 1.96 * `se'
            local obs   = e(N)

            * Save to results file
            clear
            set obs 1
            gen commodity  = "`c'"
            gen subsample  = "`s'"
            gen horizon    = `h'
            gen beta       = `beta'
            gen se         = `se'
            gen upper      = `upper'
            gen lower      = `lower'
            gen nobs       = `obs'
            append using `results'
            save `results', replace
        }
    }
}

* Load and save final results
use `results', clear
sort commodity subsample horizon

save "$input\DTA\lp_results_baseline.dta", replace
di "=== lp_results_baseline.dta saved ==="
di "Total results rows: " _N
list in 1/10

********************************************************************************
* PLOT IRFs
* One graph per commodity showing pre vs post subsample
********************************************************************************

use "$input\DTA\lp_results_baseline.dta", clear

local commodities "Coffee Copper Gold Oil Soybeans Wheat"

foreach c of local commodities {

    twoway ///
        (rarea upper lower horizon ///
            if commodity == "`c'" & subsample == "pre", ///
            color(blue%20) lwidth(none)) ///
        (line beta horizon ///
            if commodity == "`c'" & subsample == "pre", ///
            lcolor(blue) lwidth(medium)) ///
        (rarea upper lower horizon ///
            if commodity == "`c'" & subsample == "post", ///
            color(red%20) lwidth(none)) ///
        (line beta horizon ///
            if commodity == "`c'" & subsample == "post", ///
            lcolor(red) lwidth(medium)) ///
        , ///
        yline(0, lcolor(black) lpattern(solid)) ///
        legend(order(2 "Pre-financialization (1994-2003)" ///
                     4 "Post-financialization (2010-2025)") ///
               position(6) rows(1)) ///
        title("`c': IRF to 25bp Monetary Policy Tightening") ///
        xtitle("Months after shock") ///
        ytitle("Cumulative log price change") ///
        xlabel(0(4)24) ///
        note("Shaded areas = 95% confidence bands" ///
             "Newey-West SE, bandwidth = horizon")

    graph export "$output\irf_`c'_baseline.png", replace width(2000)
    di "Saved: irf_`c'_baseline.png"
}

********************************************************************************
* COMBINED GRAPH — all six commodities
********************************************************************************

graph combine ///
    "$output\irf_Coffee_baseline.png" ///
    "$output\irf_Copper_baseline.png" ///
    "$output\irf_Gold_baseline.png" ///
    "$output\irf_Oil_baseline.png" ///
    "$output\irf_Soybeans_baseline.png" ///
    "$output\irf_Wheat_baseline.png" ///
    , ///
    title("IRFs to 25bp Monetary Policy Tightening") ///
    note("Blue = pre-financialization (1994-2003)" ///
         "Red = post-financialization (2010-2025)" ///
         "Shaded areas = 95% confidence bands") ///
    cols(2) ///
    xsize(10) ysize(14)

graph export "$output\irf_all_baseline.png", replace width(3000)
di "=== Combined graph saved ==="

*************************
* Alternative post indicator excluding 2022-2025

use "$input\DTA\master_panel.dta", clear
xtset commodity_id date

* Check date is there
describe date
list date in 1/3
gen post_alt = .
replace post_alt = 0 if date >= ym(1994,1) & date <= ym(2003,12)
replace post_alt = 1 if date >= ym(2010,1) & date <= ym(2021,12)
* 2004-2009 and 2022-2025 excluded

* Save to tempfile
tempfile master results
save `master', replace

clear
save `results', emptyok replace

local subsamples "pre post"

foreach s of local subsamples {

    if "`s'" == "pre"  local cond "post_alt == 0"
    if "`s'" == "post" local cond "post_alt == 1"

    di "=== Oil — `s' (alternative) ==="

    forvalues h = 0/$horizon {

        use `master', clear
        xtset commodity_id date

        * Lags of dependent variable
        local depvarlags ""
        forvalues l = 1/$lags {
            local depvarlags "`depvarlags' L`l'.d_log_price"
        }

        * Lags of macro controls
        local macrolags ""
        foreach v in ip_growth inflation {
            forvalues l = 1/$lags {
                local macrolags "`macrolags' L`l'.`v'"
            }
        }

        * No currency control for oil
        local bw = max(1, `h')

        quietly newey dep_h`h' shock ///
            `depvarlags' `macrolags' ///
            if commodity == "Oil" & `cond', ///
            lag(`bw')

        local beta  = _b[shock]
        local se    = _se[shock]
        local upper = `beta' + 1.96 * `se'
        local lower = `beta' - 1.96 * `se'
        local obs   = e(N)

        clear
        set obs 1
        gen commodity  = "Oil"
        gen subsample  = "`s'"
        gen horizon    = `h'
        gen beta       = `beta'
        gen se         = `se'
        gen upper      = `upper'
        gen lower      = `lower'
        gen nobs       = `obs'
        append using `results'
        save `results', replace
    }
}

* Load results
use `results', clear
sort subsample horizon

save "$input\DTA\lp_results_oil_alt.dta", replace
di "=== lp_results_oil_alt.dta saved ==="

********************************************************************************
* PLOT — Oil alternative vs baseline comparison
* Panel A: alternative (2010-2021)
* Panel B: original (2010-2025) from baseline results
********************************************************************************

* First plot: alternative post period only
twoway ///
    (rarea upper lower horizon ///
        if subsample == "pre", ///
        color(blue%20) lwidth(none)) ///
    (line beta horizon ///
        if subsample == "pre", ///
        lcolor(blue) lwidth(medium)) ///
    (rarea upper lower horizon ///
        if subsample == "post", ///
        color(red%20) lwidth(none)) ///
    (line beta horizon ///
        if subsample == "post", ///
        lcolor(red) lwidth(medium)) ///
    , ///
    yline(0, lcolor(black) lpattern(solid)) ///
    legend(order(2 "Pre-financialization (1994-2003)" ///
                 4 "Post-financialization (2010-2021)") ///
           position(6) rows(1)) ///
    title("Oil: IRF to 25bp Monetary Policy Tightening" ///
          "(Post period excludes 2022-2025)") ///
    xtitle("Months after shock") ///
    ytitle("Cumulative log price change") ///
    xlabel(0(4)24) ///
    note("Shaded areas = 95% confidence bands" ///
         "Newey-West SE, bandwidth = horizon")

graph export "$output\irf_Oil_alt2021.png", replace width(2000)
di "Saved: irf_Oil_alt2021.png"


********************************************************************************
* MAIN MODEL — INTERACTION SPECIFICATION
* Uses preserve/restore instead of reloading tempfile each iteration
********************************************************************************

********************************************************************************
* FIX COMMODITY NAMES IN BOTH FINANCIALIZATION FILES
********************************************************************************

* ---- NC Gross Share ----
use "$input\DTA\nc_gross_share_monthly.dta", clear

replace commodity = "Oil"      if commodity == "Crude oil"
replace commodity = "Soybeans" if commodity == "Soybean"

tab commodity
save "$input\DTA\nc_gross_share_monthly.dta", replace
di "nc_gross_share names fixed"

* ---- SP500 Correlation ----
use "$input\DTA\sp500_corr_monthly.dta", clear

* Check names first
tab commodity

replace commodity = "Oil"      if commodity == "Crude oil" | commodity == "Oil"
replace commodity = "Soybeans" if commodity == "Soybean"

tab commodity
save "$input\DTA\sp500_corr_monthly.dta", replace
di "sp500_corr names fixed"

********************************************************************************
* RE-MERGE BOTH INTO MASTER PANEL
********************************************************************************

use "$input\DTA\master_panel.dta", clear

* Drop broken variables
drop nc_gross_share rolling_corr

* Re-merge both
merge 1:1 commodity date using ///
    "$input\DTA\nc_gross_share_monthly.dta", ///
    keep(1 3) nogenerate

merge 1:1 commodity date using ///
    "$input\DTA\sp500_corr_monthly.dta", ///
    keep(1 3) nogenerate

* Check all commodities now have values
di "=== NC GROSS SHARE ==="
tabstat nc_gross_share, by(commodity) stat(n mean) nototal col(stat)

di "=== ROLLING CORR ==="
tabstat rolling_corr, by(commodity) stat(n mean) nototal col(stat)

* Resave
sort commodity_id date
xtset commodity_id date
save "$input\DTA\master_panel.dta", replace
di "Master panel updated"

********LP INTERACTION - Financiarization at t

use "$input\DTA\master_panel.dta", clear
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
local fin_measures "nc_gross_share rolling_corr"

* Standardise and generate interaction terms once
foreach v of local fin_measures {
    quietly sum `v'
    gen `v'_std = (`v' - r(mean)) / r(sd)
    gen shock_`v' = shock * `v'_std
}

* Create results file
tempfile results
preserve
    clear
    save `results', emptyok replace
restore

********************************************************************************
* LOOP
********************************************************************************

foreach c of local commodities {
    foreach fm of local fin_measures {

        local curr_control = "`curr_`c''"
        di "=== `c' — interaction with `fm' ==="

        forvalues h = 0/$horizon {

            * Build lag lists
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

            local bw = max(1, `h')

            * Run regression — master panel stays in memory
            quietly newey dep_h`h' shock shock_`fm' `fm'_std ///
                `depvarlags' `macrolags' `currlags' ///
                if commodity == "`c'", ///
                lag(`bw')

            * Store results using preserve/restore
            local beta_shock  = _b[shock]
            local se_shock    = _se[shock]
            local beta_int    = _b[shock_`fm']
            local se_int      = _se[shock_`fm']
            local upper_shock = `beta_shock' + 1.96 * `se_shock'
            local lower_shock = `beta_shock' - 1.96 * `se_shock'
            local upper_int   = `beta_int'   + 1.96 * `se_int'
            local lower_int   = `beta_int'   - 1.96 * `se_int'
            local obs         = e(N)

            preserve
                clear
                set obs 1
                gen str14 commodity   = "`c'"
                gen str14 fin_measure = "`fm'"
                gen horizon           = `h'
                gen beta_shock        = `beta_shock'
                gen se_shock          = `se_shock'
                gen upper_shock       = `upper_shock'
                gen lower_shock       = `lower_shock'
                gen beta_int          = `beta_int'
                gen se_int            = `se_int'
                gen upper_int         = `upper_int'
                gen lower_int         = `lower_int'
                gen nobs              = `obs'
                append using `results'
                save `results', replace
            restore
        }
    }
}

* Load and save final results
use `results', clear
sort commodity fin_measure horizon

save "$input\DTA\lp_results_interaction.dta", replace
di "=== lp_results_interaction.dta saved ==="
di "Total results rows: " _N
list in 1/5
********************************************************************************
* PLOT INTERACTION COEFFICIENTS
********************************************************************************

use "$input\DTA\lp_results_interaction.dta", clear

local commodities "Coffee Copper Gold Oil Soybeans Wheat"
local fm_list     "nc_gross_share rolling_corr"
local fin_labels  "NC Gross Share (CFTC)" "SP500 Rolling Correlation"

local n : word count `fm_list'

foreach c of local commodities {
    forvalues i = 1/`n' {

        local fm    : word `i' of `fm_list'
        local fmlbl : word `i' of `fin_labels'

        twoway ///
            (rarea upper_int lower_int horizon ///
                if commodity == "`c'" & fin_measure == "`fm'", ///
                color(red%20) lwidth(none)) ///
            (line beta_int horizon ///
                if commodity == "`c'" & fin_measure == "`fm'", ///
                lcolor(red) lwidth(medium)) ///
            , ///
            yline(0, lcolor(black) lpattern(solid)) ///
            title("`c': Interaction Effect of Financialization" ///
                  "Measure: `fmlbl'") ///
            xtitle("Months after shock") ///
            ytitle("Interaction coefficient") ///
            xlabel(0(4)24) ///
            note("Negative = higher financialization amplifies" ///
                 "negative price response to monetary tightening" ///
                 "Shaded area = 95% CI, Newey-West SE")

        graph export "$output\irf_`c'_interaction_`fm'.png", ///
            replace width(2000)
        di "Saved: irf_`c'_interaction_`fm'.png"
    }
}

********LP INTERACTION - Financialization at t-1, without DUMMIES

use "$input\DTA\master_panel.dta", clear
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
local fin_measures "nc_gross_share rolling_corr"

* Standardise using L1 and generate interaction terms once
foreach v of local fin_measures {
    quietly sum L1.`v'
    gen `v'_std = (L1.`v' - r(mean)) / r(sd)
    gen shock_`v' = shock * `v'_std
}

* Create results file
tempfile results
preserve
    clear
    save `results', emptyok replace
restore

********************************************************************************
* LOOP
********************************************************************************

foreach c of local commodities {
    foreach fm of local fin_measures {

        local curr_control = "`curr_`c''"
        di "=== `c' — interaction with `fm' (t-1) ==="

        forvalues h = 0/$horizon {

            * Build lag lists
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

            local bw = max(1, `h')

            * Run regression — master panel stays in memory
            quietly newey dep_h`h' shock shock_`fm' `fm'_std ///
                `depvarlags' `macrolags' `currlags' ///
                if commodity == "`c'", ///
                lag(`bw')

            * Store results using preserve/restore
            local beta_shock  = _b[shock]
            local se_shock    = _se[shock]
            local beta_int    = _b[shock_`fm']
            local se_int      = _se[shock_`fm']
            local upper_shock = `beta_shock' + 1.96 * `se_shock'
            local lower_shock = `beta_shock' - 1.96 * `se_shock'
            local upper_int   = `beta_int'   + 1.96 * `se_int'
            local lower_int   = `beta_int'   - 1.96 * `se_int'
            local obs         = e(N)

            preserve
                clear
                set obs 1
                gen str14 commodity   = "`c'"
                gen str14 fin_measure = "`fm'"
                gen horizon           = `h'
                gen beta_shock        = `beta_shock'
                gen se_shock          = `se_shock'
                gen upper_shock       = `upper_shock'
                gen lower_shock       = `lower_shock'
                gen beta_int          = `beta_int'
                gen se_int            = `se_int'
                gen upper_int         = `upper_int'
                gen lower_int         = `lower_int'
                gen nobs              = `obs'
                append using `results'
                save `results', replace
            restore
        }
    }
}

* Load and save final results
use `results', clear
sort commodity fin_measure horizon

save "$input\DTA\lp_results_interaction_L1.dta", replace
di "=== lp_results_interaction_L1.dta saved ==="
di "Total results rows: " _N
list in 1/5

********************************************************************************
* PLOT INTERACTION COEFFICIENTS
********************************************************************************

use "$input\DTA\lp_results_interaction_L1.dta", clear

local commodities "Coffee Copper Gold Oil Soybeans Wheat"
local fm_list     "nc_gross_share rolling_corr"
local fin_labels  "NC Gross Share (CFTC)" "SP500 Rolling Correlation"

local n : word count `fm_list'

foreach c of local commodities {
    forvalues i = 1/`n' {

        local fm    : word `i' of `fm_list'
        local fmlbl : word `i' of `fin_labels'

        twoway ///
            (rarea upper_int lower_int horizon ///
                if commodity == "`c'" & fin_measure == "`fm'", ///
                color(red%20) lwidth(none)) ///
            (line beta_int horizon ///
                if commodity == "`c'" & fin_measure == "`fm'", ///
                lcolor(red) lwidth(medium)) ///
            , ///
            yline(0, lcolor(black) lpattern(solid)) ///
            title("`c': Interaction Effect of Financialization (t-1)" ///
                  "Measure: `fmlbl'") ///
            xtitle("Months after shock") ///
            ytitle("Interaction coefficient") ///
            xlabel(0(4)24) ///
            note("Negative = higher financialization amplifies" ///
                 "negative price response to monetary tightening" ///
                 "Shaded area = 95% CI, Newey-West SE")

        graph export "$output\irf_`c'_interaction_`fm'_L1.png", ///
            replace width(2000)
        di "Saved: irf_`c'_interaction_`fm'_L1.png"
    }
}


********LP, NO INTERACTION, With CRISES DUMMIES

use "$input\DTA\master_panel.dta", clear
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

local commodities "Coffee Copper Gold Oil Soybeans Wheat"

* Create results file
tempfile results
preserve
    clear
    save `results', emptyok replace
restore

********************************************************************************
* LOOP
********************************************************************************

foreach c of local commodities {

    local curr_control = "`curr_`c''"
    di "=== `c' ==="

    * Crisis controls: GFC + COVID for all; Ukraine for Oil, Soybeans, Wheat
    local crisis "gfc covid"
    if inlist("`c'", "Oil", "Soybeans", "Wheat") local crisis "gfc covid ukraine"

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

        * Crisis dummy lags
        local crisislags ""
        foreach d of local crisis {
            forvalues l = 0/$lags {
                local crisislags "`crisislags' L`l'.`d'"
            }
        }

        local bw = max(1, `h')

        quietly newey dep_h`h' shock `depvarlags' `macrolags' `currlags' `crisislags' if commodity == "`c'", lag(`bw')

        local beta     = _b[shock]
        local se       = _se[shock]
        local upper90  = `beta' + 1.645 * `se'
        local lower90  = `beta' - 1.645 * `se'
        local upper68  = `beta' + 1.000 * `se'
        local lower68  = `beta' - 1.000 * `se'
        local obs      = e(N)

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
            save `results', replace
        restore
    }
}

********************************************************************************
* SAVE
********************************************************************************

use `results', clear
sort commodity horizon

save "$input\DTA\lp_results_crisis.dta", replace
di "=== lp_results_crisis.dta saved ==="
di "Total rows: " _N
list in 1/5

********************************************************************************
* PLOT
********************************************************************************

use "$input\DTA\lp_results_crisis.dta", clear

local commodities "Coffee Copper Gold Oil Soybeans Wheat"

foreach c of local commodities {

    twoway ///
        (rarea upper90 lower90 horizon if commodity == "`c'", color(blue%15) lwidth(none)) ///
        (rarea upper68 lower68 horizon if commodity == "`c'", color(blue%30) lwidth(none)) ///
        (line beta horizon if commodity == "`c'", lcolor(blue) lwidth(medium)) ///
        , ///
        yline(0, lcolor(black) lpattern(solid)) ///
        title("`c': IRF to Monetary Policy Shock" "(with crisis dummies)") ///
        xtitle("Months after shock") ///
        ytitle("Cumulative log price change") ///
        xlabel(0(4)24) ///
        legend(order(1 "90% CI" 2 "68% CI" 3 "IRF") rows(1) size(small)) ///
        note("Shaded areas = 68% and 90% CI, Newey-West SE" ///
             "Ukraine dummy included for Oil, Soybeans, Wheat")

    graph export "$output\irf_`c'_crisis.png", replace width(2000)
    di "Saved: irf_`c'_crisis.png"
}

moses
********LP INTERACTION - Financiarization at t-1 - WITH CRISIS DUMMIES

use "$input\DTA\master_panel.dta", clear
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
local fin_measures "nc_gross_share rolling_corr"

* Crisis dummies
gen gfc     = (date >= tm(2008m9)  & date <= tm(2009m6))
gen covid   = (date >= tm(2020m3)  & date <= tm(2021m6))
gen ukraine = (date >= tm(2022m2)  & date <= tm(2022m12))

* Standardise using L1 and generate interaction terms once
foreach v of local fin_measures {
    quietly sum L1.`v'
    gen `v'_std   = (L1.`v' - r(mean)) / r(sd)
    gen shock_`v' = shock * `v'_std
}

* Create results file
tempfile results
preserve
    clear
    save `results', emptyok replace
restore

********************************************************************************
* LOOP
********************************************************************************

foreach c of local commodities {
    foreach fm of local fin_measures {

        local curr_control = "`curr_`c''"

        * Ukraine for Oil, Soybeans and Wheat
        local crisis_vars "gfc covid"
        if inlist("`c'", "Oil", "Soybeans", "Wheat") local crisis_vars "gfc covid ukraine"

        di "=== `c' — interaction with `fm' (t-1, crisis dummies) ==="

        forvalues h = 0/$horizon {

            * Build lag lists
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

            * Build crisis dummy lags
            local crisislags ""
            foreach d of local crisis_vars {
                forvalues l = 0/$lags {
                    local crisislags "`crisislags' L`l'.`d'"
                }
            }

            local bw = max(1, `h')

            * Run regression — master panel stays in memory
            quietly newey dep_h`h' shock shock_`fm' `fm'_std ///
                `depvarlags' `macrolags' `currlags' `crisislags' ///
                if commodity == "`c'", ///
                lag(`bw')

            * Store results
            local beta_shock  = _b[shock]
            local se_shock    = _se[shock]
            local beta_int    = _b[shock_`fm']
            local se_int      = _se[shock_`fm']
            local upper_int90 = `beta_int' + 1.645 * `se_int'
            local lower_int90 = `beta_int' - 1.645 * `se_int'
            local upper_int68 = `beta_int' + 1.000 * `se_int'
            local lower_int68 = `beta_int' - 1.000 * `se_int'
            local obs         = e(N)

            preserve
                clear
                set obs 1
                gen str14 commodity   = "`c'"
                gen str14 fin_measure = "`fm'"
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
                save `results', replace
            restore
        }
    }
}

* Load and save final results
use `results', clear
sort commodity fin_measure horizon

save "$input\DTA\lp_results_interaction_crises_L1.dta", replace
di "=== lp_results_interaction_crises_L1.dta saved ==="
di "Total results rows: " _N
list in 1/5

********************************************************************************
* PLOT INTERACTION COEFFICIENTS
********************************************************************************

use "$input\DTA\lp_results_interaction_crises_L1.dta", clear

local commodities "Coffee Copper Gold Oil Soybeans Wheat"
local fm_list     "nc_gross_share rolling_corr"
local fin_labels  "NC Gross Share (CFTC)" "SP500 Rolling Correlation"

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
            title("`c': Interaction Effect of Financialization (t-1)" ///
                  "Measure: `fmlbl'") ///
            xtitle("Months after shock") ///
            ytitle("Interaction coefficient") ///
            xlabel(0(4)24) ///
            legend(order(1 "90% CI" 2 "68% CI" 3 "Interaction coeff") ///
                   rows(1) size(small)) ///
            note("Negative = higher financialization amplifies" ///
                 "negative price response to monetary tightening" ///
                 "Shaded areas = 68% and 90% CI, Newey-West SE")

        graph export "$output\irf_`c'_interaction_`fm'_crises_L1.png", ///
            replace width(2000)
        di "Saved: irf_`c'_interaction_`fm'_crises_L1.png"
    }
}
moses 
********BIC LAG SELECTION

use "$input\DTA\master_panel.dta", clear
xtset commodity_id date

local curr_Coffee   "d_brl"
local curr_Copper   "d_clp"
local curr_Gold     "d_aud"
local curr_Oil      ""
local curr_Soybeans "d_brl"
local curr_Wheat    ""

local commodities "Coffee Copper Gold Oil Soybeans Wheat"
local max_lags 8

tempfile bic_results
preserve
    clear
    save `bic_results', emptyok replace
restore

********************************************************************************
* LOOP
********************************************************************************

foreach c of local commodities {
    di "=== BIC Lag Selection: `c' ==="

    local curr_control = "`curr_`c''"

    forvalues p = 1/`max_lags' {

        local depvarlags ""
        forvalues l = 1/`p' {
            local depvarlags "`depvarlags' L`l'.d_log_price"
        }

        local macrolags ""
        foreach v in ip_growth inflation {
            forvalues l = 1/`p' {
                local macrolags "`macrolags' L`l'.`v'"
            }
        }

        local currlags ""
        if "`curr_control'" != "" {
            forvalues l = 1/`p' {
                local currlags "`currlags' L`l'.`curr_control'"
            }
        }

        quietly regress dep_h0 shock ///
            `depvarlags' `macrolags' `currlags' ///
            if commodity == "`c'"

        * Capture ALL scalars immediately — before any preserve/restore
        local N     = e(N)
        local k     = e(df_model) + 1
        local rss   = e(rss)
        local bic_v = `N' * ln(`rss'/`N') + `k' * ln(`N')

        di "  Lags = `p' | N = `N' | k = `k' | BIC = " %9.4f `bic_v'

        preserve
            clear
            set obs 1
            gen str14 commodity = "`c'"
            gen lags            = `p'
            gen nobs            = `N'
            gen nparam          = `k'
            gen bic             = `bic_v'
            append using `bic_results'
            save `bic_results', replace
        restore
    }
}

********************************************************************************
* SUMMARISE
********************************************************************************

use `bic_results', clear
sort commodity lags

bysort commodity (bic): gen optimal = (_n == 1)

di ""
di "=== OPTIMAL LAG LENGTH BY COMMODITY (BIC) ==="
list commodity lags bic nobs nparam if optimal == 1, noobs sep(0)

save "$input\DTA\bic_lag_selection.dta", replace
di "=== bic_lag_selection.dta saved ==="



********BIC LAG SELECTION

use "$input\DTA\master_panel.dta", clear
xtset commodity_id date

local curr_Coffee   "d_brl"
local curr_Copper   "d_clp"
local curr_Gold     "d_aud"
local curr_Oil      ""
local curr_Soybeans "d_brl"
local curr_Wheat    ""

local commodities "Coffee Copper Gold Oil Soybeans Wheat"
local max_lags 8

tempfile bic_results
preserve
    clear
    save `bic_results', emptyok replace
restore

********************************************************************************
* LOOP
********************************************************************************

foreach c of local commodities {
    di "=== BIC Lag Selection: `c' ==="

    local curr_control = "`curr_`c''"

    forvalues p = 1/`max_lags' {

        local depvarlags ""
        forvalues l = 1/`p' {
            local depvarlags "`depvarlags' L`l'.d_log_price"
        }

        local macrolags ""
        foreach v in ip_growth inflation {
            forvalues l = 1/`p' {
                local macrolags "`macrolags' L`l'.`v'"
            }
        }

        local currlags ""
        if "`curr_control'" != "" {
            forvalues l = 1/`p' {
                local currlags "`currlags' L`l'.`curr_control'"
            }
        }

        * No line continuation (///) — keep if condition on same command line
        quietly regress dep_h0 shock `depvarlags' `macrolags' `currlags' if commodity == "`c'"

        local N     = e(N)
        local k     = e(df_m) + 1
        local rss   = e(rss)
        local bic_v = `N' * ln(`rss'/`N') + `k' * ln(`N')

        di "  Lags = `p' | N = `N' | k = `k' | BIC = " %9.4f `bic_v'

        preserve
            clear
            set obs 1
            gen str14 commodity = "`c'"
            gen lags            = `p'
            gen nobs            = `N'
            gen nparam          = `k'
            gen bic             = `bic_v'
            append using `bic_results'
            save `bic_results', replace
        restore
    }
}

********************************************************************************
* SUMMARISE
********************************************************************************

use `bic_results', clear
sort commodity lags

bysort commodity (bic): gen optimal = (_n == 1)

di ""
di "=== OPTIMAL LAG LENGTH BY COMMODITY (BIC) ==="
list commodity lags bic nobs nparam if optimal == 1, noobs sep(0)

save "$input\DTA\bic_lag_selection.dta", replace
di "=== bic_lag_selection.dta saved ==="
moses

********AIC LAG SELECTION

use "$input\DTA\master_panel.dta", clear
xtset commodity_id date

local curr_Coffee   "d_brl"
local curr_Copper   "d_clp"
local curr_Gold     "d_aud"
local curr_Oil      ""
local curr_Soybeans "d_brl"
local curr_Wheat    ""

local commodities "Coffee Copper Gold Oil Soybeans Wheat"
local max_lags 8

tempfile aic_results
preserve
    clear
    save `aic_results', emptyok replace
restore

********************************************************************************
* LOOP
********************************************************************************

foreach c of local commodities {
    di "=== AIC Lag Selection: `c' ==="

    local curr_control = "`curr_`c''"

    forvalues p = 1/`max_lags' {

        local depvarlags ""
        forvalues l = 1/`p' {
            local depvarlags "`depvarlags' L`l'.d_log_price"
        }

        local macrolags ""
        foreach v in ip_growth inflation {
            forvalues l = 1/`p' {
                local macrolags "`macrolags' L`l'.`v'"
            }
        }

        local currlags ""
        if "`curr_control'" != "" {
            forvalues l = 1/`p' {
                local currlags "`currlags' L`l'.`curr_control'"
            }
        }

        quietly regress dep_h0 shock `depvarlags' `macrolags' `currlags' if commodity == "`c'"

        local N     = e(N)
        local k     = e(df_m) + 1
        local rss   = e(rss)
        local aic_v = `N' * ln(`rss'/`N') + 2 * `k'

        di "  Lags = `p' | N = `N' | k = `k' | AIC = " %9.4f `aic_v'

        preserve
            clear
            set obs 1
            gen str14 commodity = "`c'"
            gen lags            = `p'
            gen nobs            = `N'
            gen nparam          = `k'
            gen aic             = `aic_v'
            append using `aic_results'
            save `aic_results', replace
        restore
    }
}

********************************************************************************
* SUMMARISE
********************************************************************************

use `aic_results', clear
sort commodity lags

bysort commodity (aic): gen optimal = (_n == 1)

di ""
di "=== OPTIMAL LAG LENGTH BY COMMODITY (AIC) ==="
list commodity lags aic nobs nparam if optimal == 1, noobs sep(0)

save "$input\DTA\aic_lag_selection.dta", replace
di "=== aic_lag_selection.dta saved ==="

moses

*************** LP-IV Alternative **********************************
*1. Cleaning*

********PREPARE FEDFUNDS DATA

* Import Excel file
import excel "$input\fedfunds.xlsx", firstrow clear

* Check what date looks like
describe
list in 1/5

* Convert Excel numeric date to Stata date
gen date_stata = mofd(date)
format date_stata %tm
drop date
rename date_stata date

* Check
list in 1/5
sum fedfunds

* Save prepared file
save "$input\DTA\fedfunds_prepared.dta", replace
di "=== fedfunds_prepared.dta saved ==="

********MERGE FEDFUNDS INTO MASTER PANEL

use "$input\DTA\master_panel.dta", clear

* Merge on date (m:1 since fedfunds is common across commodities)
merge m:1 date using "$input\DTA\fedfunds_prepared.dta", ///
    keep(master match) nogenerate

* Check
sum fedfunds
xtset commodity_id date

save "$input\DTA\master_panel.dta", replace
di "=== master_panel updated with fedfunds ==="

********FIRST STAGE - LP-IV: INSTRUMENT STRENGTH

use "$input\DTA\master_panel.dta", clear
xtset commodity_id date

global lags 4

* Crisis dummies
gen gfc   = (date >= tm(2008m9)  & date <= tm(2009m6))
gen covid = (date >= tm(2020m3)  & date <= tm(2021m6))

* Keep one observation per date (FFR is aggregate, not commodity-specific)
keep if commodity == "Coffee"

* Lag lists
local ffrlags ""
forvalues l = 1/$lags {
    local ffrlags "`ffrlags' L`l'.fedfunds"
}

local macrolags ""
foreach v in ip_growth inflation {
    forvalues l = 1/$lags {
        local macrolags "`macrolags' L`l'.`v'"
    }
}

local crisislags ""
foreach d in gfc covid {
    forvalues l = 0/$lags {
        local crisislags "`crisislags' L`l'.`d'"
    }
}

********************************************************************************
*2. Analysis 

********FIRST STAGE - LP-IV: INSTRUMENT STRENGTH

use "$input\DTA\master_panel.dta", clear

* Keep one observation per date
duplicates drop date, force
tsset date

* Generate FFR change
gen d_fedfunds = fedfunds - L.fedfunds

********************************************************************************
* FIRST STAGE
********************************************************************************

di "=== First Stage: d_fedfunds ~ shock ==="

regress d_fedfunds shock

local fstat    = e(F)
local r2       = e(r2)
local N        = e(N)
local b_shock  = _b[shock]
local se_shock = _se[shock]
local t_shock  = `b_shock' / `se_shock'

di ""
di "  Coefficient on shock: " %7.4f `b_shock' " (SE = " %7.4f `se_shock' ")"
di "  t-statistic:          " %7.4f `t_shock'
di "  F-statistic:          " %7.4f `fstat'
di "  R-squared:            " %7.4f `r2'
di "  N:                    " `N'
di ""
di "  Stock-Yogo threshold:          F > 10"
di "  Montiel-Pflueger threshold:    F > 23.1"
if `fstat' > 23.1 {
    di "  --> STRONG instrument (Montiel-Pflueger)"
}
else if `fstat' > 10 {
    di "  --> STRONG instrument (Stock-Yogo) but check Montiel-Pflueger"
}
else {
    di "  --> WEAK instrument"
}


moses

********LP-IV SECOND STAGE - Baseline

use "$input\DTA\master_panel.dta", clear

xtset commodity_id date

global lags    4
global horizon 24

local curr_Coffee   "d_brl"
local curr_Copper   "d_clp"
local curr_Gold     "d_aud"
local curr_Oil      ""
local curr_Soybeans "d_brl"
local curr_Wheat    ""

* Generate FFR change
gen d_fedfunds = fedfunds - L.fedfunds

* Crisis dummies
gen gfc     = (date >= tm(2008m9)  & date <= tm(2009m6))
gen covid   = (date >= tm(2020m3)  & date <= tm(2021m6))
gen ukraine = (date >= tm(2022m2)  & date <= tm(2022m12))

********************************************************************************
* FIRST STAGE: get fitted values of d_fedfunds
********************************************************************************

preserve
    keep if commodity == "Coffee"
    tsset date
    regress d_fedfunds shock
    predict d_fedfunds_hat, xb
    keep date d_fedfunds_hat
    sort date
    tempfile fittedvals
    save `fittedvals'
restore

* Merge fitted values back into panel
merge m:1 date using `fittedvals', nogenerate

* Restore sort order after merge
sort commodity_id date
xtset commodity_id date

local commodities "Coffee Copper Gold Oil Soybeans Wheat"

* Create results file
tempfile results
preserve
    clear
    save `results', emptyok replace
restore

********************************************************************************
* LOOP
********************************************************************************

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

        quietly newey dep_h`h' d_fedfunds_hat ///
            `depvarlags' `macrolags' `currlags' `crisislags' ///
            if commodity == "`c'", lag(`bw')

        local beta    = _b[d_fedfunds_hat]
        local se      = _se[d_fedfunds_hat]
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

********************************************************************************
* SAVE
********************************************************************************

use `results', clear
sort commodity horizon

save "$input\DTA\lp_iv_results.dta", replace
di "=== lp_iv_results.dta saved ==="
di "Total rows: " _N
list in 1/5

********************************************************************************
* PLOT
********************************************************************************

use "$input\DTA\lp_iv_results.dta", clear

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
        title("`c': LP-IV IRF to Monetary Policy Shock") ///
        xtitle("Months after shock") ///
        ytitle("Cumulative log price change") ///
        xlabel(0(4)24) ///
        legend(order(1 "90% CI" 2 "68% CI" 3 "IRF") ///
               rows(1) size(small)) ///
        note("Endogenous variable: FFR change" ///
             "Instrument: Acosta et al. (2025) shock" ///
             "Shaded areas = 68% and 90% CI, Newey-West SE")

    graph export "$output\LP-IV_IRFs\irf_`c'_lpiv.png", replace width(2000)
    di "Saved: irf_`c'_lpiv.png"
}

moses

********LP-IV INTERACTION - Financialization at t-1 - WITH CRISIS DUMMIES

use "$input\DTA\master_panel.dta", clear
xtset commodity_id date

global lags    4
global horizon 24

local curr_Coffee   "d_brl"
local curr_Copper   "d_clp"
local curr_Gold     "d_aud"
local curr_Oil      ""
local curr_Soybeans "d_brl"
local curr_Wheat    ""

* Generate FFR change
gen d_fedfunds = fedfunds - L.fedfunds

* Crisis dummies
gen gfc     = (date >= tm(2008m9)  & date <= tm(2009m6))
gen covid   = (date >= tm(2020m3)  & date <= tm(2021m6))
gen ukraine = (date >= tm(2022m2)  & date <= tm(2022m12))

local commodities "Coffee Copper Gold Oil Soybeans Wheat"
local fin_measures "nc_gross_share rolling_corr"

* Standardise financialization at t-1
foreach v of local fin_measures {
    quietly sum L1.`v'
    gen `v'_std = (L1.`v' - r(mean)) / r(sd)
}

********************************************************************************
* FIRST STAGE: instrument d_fedfunds with shock only
********************************************************************************

preserve
    keep if commodity == "Coffee"
    tsset date

    regress d_fedfunds shock
    predict d_fedfunds_hat, xb
    keep date d_fedfunds_hat
    sort date
    tempfile fittedvals
    save `fittedvals'
restore

* Merge fitted values back into panel
merge m:1 date using `fittedvals', nogenerate
sort commodity_id date
xtset commodity_id date

* Generate interaction of fitted FFR with financialization measures
foreach v of local fin_measures {
    gen d_ff_x_`v' = d_fedfunds_hat * `v'_std
}

* Create results file
tempfile results
preserve
    clear
    save `results', emptyok replace
restore

********************************************************************************
* LOOP
********************************************************************************

foreach c of local commodities {
    foreach fm of local fin_measures {

        local curr_control = "`curr_`c''"

        local crisis_vars "gfc covid"
        if inlist("`c'", "Oil", "Soybeans", "Wheat") local crisis_vars "gfc covid ukraine"

        di "=== `c' — LP-IV interaction with `fm' ==="

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

            * Second stage: fitted FFR, interaction, and F_std level control
            quietly newey dep_h`h' d_fedfunds_hat d_ff_x_`fm' `fm'_std ///
                `depvarlags' `macrolags' `currlags' `crisislags' ///
                if commodity == "`c'", lag(`bw')

            local beta_shock  = _b[d_fedfunds_hat]
            local se_shock    = _se[d_fedfunds_hat]
            local beta_int    = _b[d_ff_x_`fm']
            local se_int      = _se[d_ff_x_`fm']
            local upper_int90 = `beta_int' + 1.645 * `se_int'
            local lower_int90 = `beta_int' - 1.645 * `se_int'
            local upper_int68 = `beta_int' + 1.000 * `se_int'
            local lower_int68 = `beta_int' - 1.000 * `se_int'
            local obs         = e(N)

            preserve
                clear
                set obs 1
                gen str14 commodity   = "`c'"
                gen str14 fin_measure = "`fm'"
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
    }
}

********************************************************************************
* SAVE
********************************************************************************

use `results', clear
sort commodity fin_measure horizon

save "$input\DTA\lp_iv_results_interaction.dta", replace
di "=== lp_iv_results_interaction.dta saved ==="
di "Total rows: " _N
list in 1/5

********************************************************************************
* PLOT
********************************************************************************

use "$input\DTA\lp_iv_results_interaction.dta", clear

local commodities "Coffee Copper Gold Oil Soybeans Wheat"
local fm_list     "nc_gross_share rolling_corr"
local fin_labels  "NC Gross Share (CFTC)" "SP500 Rolling Correlation"

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
                 "Instrument: Acosta et al. (2025) shock" ///
                 "Shaded areas = 68% and 90% CI, Newey-West SE")

        graph export "$output\LP-IV_IRFs\irf_`c'_lpiv_interaction_`fm'.png", ///
            replace width(2000)
        di "Saved: irf_`c'_lpiv_interaction_`fm'.png"
    }
}
moses 

********LP INTERACTION - Financialization at t-1 - CAPPED SAMPLE
* Coffee, Soybeans, Wheat: capped at 2017m12
* Copper, Gold, Oil: full sample
* WITH CRISIS DUMMIES

use "$input\DTA\master_panel.dta", clear
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
local fin_measures "nc_gross_share rolling_corr"

* Crisis dummies
gen gfc     = (date >= tm(2008m9)  & date <= tm(2009m6))
gen covid   = (date >= tm(2020m3)  & date <= tm(2021m6))
gen ukraine = (date >= tm(2022m2)  & date <= tm(2022m12))

* Sample cap — commodity specific
* Coffee, Soybeans, Wheat capped at 2017m12
* Copper, Gold, Oil full sample
gen byte sample_cap = 1
replace sample_cap = 0 if inlist(commodity, "Coffee", "Soybeans", "Wheat") ///
    & date > tm(2017m12)

* Standardise using L1 and generate interaction terms once
foreach v of local fin_measures {
    quietly sum L1.`v'
    gen `v'_std   = (L1.`v' - r(mean)) / r(sd)
    gen shock_`v' = shock * `v'_std
}

* Create results file
tempfile results
preserve
    clear
    save `results', emptyok replace
restore

********************************************************************************
* LOOP
********************************************************************************

foreach c of local commodities {
    foreach fm of local fin_measures {

        local curr_control = "`curr_`c''"

        * Ukraine for Oil, Soybeans and Wheat
        local crisis_vars "gfc covid"
        if inlist("`c'", "Oil", "Soybeans", "Wheat") ///
            local crisis_vars "gfc covid ukraine"

        di "=== `c' — interaction with `fm' (t-1, capped) ==="

        forvalues h = 0/$horizon {

            * Build lag lists
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

            * Build crisis dummy lags
            local crisislags ""
            foreach d of local crisis_vars {
                forvalues l = 0/$lags {
                    local crisislags "`crisislags' L`l'.`d'"
                }
            }

            local bw = max(1, `h')

            * Run regression — sample_cap restricts Coffee/Soybeans/Wheat
            quietly newey dep_h`h' shock shock_`fm' `fm'_std ///
                `depvarlags' `macrolags' `currlags' `crisislags' ///
                if commodity == "`c'" & sample_cap == 1, ///
                lag(`bw')

            * Store results
            local beta_shock  = _b[shock]
            local se_shock    = _se[shock]
            local beta_int    = _b[shock_`fm']
            local se_int      = _se[shock_`fm']
            local upper_int90 = `beta_int' + 1.645 * `se_int'
            local lower_int90 = `beta_int' - 1.645 * `se_int'
            local upper_int68 = `beta_int' + 1.000 * `se_int'
            local lower_int68 = `beta_int' - 1.000 * `se_int'
            local obs         = e(N)

            preserve
                clear
                set obs 1
                gen str14 commodity   = "`c'"
                gen str14 fin_measure = "`fm'"
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
    }
}

********************************************************************************
* SAVE
********************************************************************************

use `results', clear
sort commodity fin_measure horizon

save "$input\DTA\lp_results_interaction_capped.dta", replace
di "=== lp_results_interaction_capped.dta saved ==="
di "Total rows: " _N
list in 1/5

********************************************************************************
* PLOT
********************************************************************************

use "$input\DTA\lp_results_interaction_capped.dta", clear

local commodities "Coffee Copper Gold Oil Soybeans Wheat"
local fm_list     "nc_gross_share rolling_corr"
local fin_labels  "NC Gross Share (CFTC)" "SP500 Rolling Correlation"

local n : word count `fm_list'

foreach c of local commodities {
    forvalues i = 1/`n' {

        local fm    : word `i' of `fm_list'
        local fmlbl : word `i' of `fin_labels'

        * Sample note for title
        local sample_note "Full sample 1994-2025"
        if inlist("`c'", "Coffee", "Soybeans", "Wheat") ///
            local sample_note "Capped sample 1994-2017"

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
            title("`c': Interaction Effect of Financialization (t-1)" ///
                  "Measure: `fmlbl'") ///
            xtitle("Months after shock") ///
            ytitle("Interaction coefficient") ///
            xlabel(0(4)24) ///
            legend(order(1 "90% CI" 2 "68% CI" 3 "Interaction coeff") ///
                   rows(1) size(small)) ///
            note("Negative = higher financialization amplifies" ///
                 "negative price response to monetary tightening" ///
                 "`sample_note'" ///
                 "Shaded areas = 68% and 90% CI, Newey-West SE")

        graph export "$output\capped agricultural test\irf_`c'_interaction_`fm'_capped.png", ///
            replace width(2000)
        di "Saved: irf_`c'_interaction_`fm'_capped.png"
    }
}