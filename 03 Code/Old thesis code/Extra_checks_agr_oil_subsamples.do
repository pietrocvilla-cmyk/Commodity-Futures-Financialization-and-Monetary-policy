********* Master's thesis empirical analysis - Monetary economics************
/* 

Author: Pietro Villa 

University: London School of Economics and Political Science
Degree: MSc Economics 
Date: 03-03-2025 
Description: 
This script performs the EXTRA Checks for agricultural commodities and oil for 2 subsamples analysis for the Master's final thesis on the effects of monetary policy shocks on commodity (futures) prices and the role of financialization

*/

*********** DATA CLEANING **************************************

* ---- 0. Set working directory ----
global main "C:\Users\pitvi\OneDrive\Documenti\03 LSE\03 Dissertation"
global input "$main\02 Data"
global output "$main\04 Output - figures and tables"

cd "$main"






********************************************************************************
* ADDITIONAL ANALYSIS: WHEAT AND SOYBEANS 2009-2014
* Peak financialization period for agricultural commodities
********************************************************************************

use "$input\DTA\master_panel.dta", clear
xtset commodity_id date

global lags    4
global horizon 24

* Currency control for both
local curr_Soybeans "d_brl"
local curr_Wheat    ""

local commodities "Soybeans Wheat"

* Three subsamples for agricultural commodities
* pre:    1994-2003 (pre-financialization baseline)
* peak:   2009-2014 (peak financialization)
* post:   2010-2025 (full post — for comparison)

tempfile master results
save `master', replace

preserve
    clear
    save `results', emptyok replace
restore

foreach c of local commodities {

    * Three subsamples
    local subsamples  "pre peak post"

    foreach s of local subsamples {

        if "`s'" == "pre"  local cond "date >= ym(1994,1) & date <= ym(2003,12)"
        if "`s'" == "peak" local cond "date >= ym(2009,1) & date <= ym(2014,12)"
        if "`s'" == "post" local cond "date >= ym(2010,1) & date <= ym(2025,12)"

        local curr_control = "`curr_`c''"

        di "=== `c' — `s' ==="

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

            local bw = max(1, `h')

            quietly newey dep_h`h' shock ///
                `depvarlags' `macrolags' `currlags' ///
                if commodity == "`c'" & `cond', ///
                lag(`bw')

            local beta  = _b[shock]
            local se    = _se[shock]
            local upper = `beta' + 1.96 * `se'
            local lower = `beta' - 1.96 * `se'
            local obs   = e(N)

            preserve
                clear
                set obs 1
                gen str9  commodity = "`c'"
                gen str5  subsample = "`s'"
                gen horizon         = `h'
                gen beta            = `beta'
                gen se              = `se'
                gen upper           = `upper'
                gen lower           = `lower'
                gen nobs            = `obs'
                append using `results'
                save `results', replace
            restore
        }
    }
}

* Load and save
use `results', clear
sort commodity subsample horizon
save "$input\DTA\lp_results_agri_peak.dta", replace
di "=== lp_results_agri_peak.dta saved ==="

********************************************************************************
* PLOT — three lines: pre, peak, post
********************************************************************************

use "$input\DTA\lp_results_agri_peak.dta", clear

local commodities "Soybeans Wheat"

foreach c of local commodities {

    twoway ///
        (rarea upper lower horizon ///
            if commodity == "`c'" & subsample == "pre", ///
            color(blue%20) lwidth(none)) ///
        (line beta horizon ///
            if commodity == "`c'" & subsample == "pre", ///
            lcolor(blue) lwidth(medium)) ///
        (rarea upper lower horizon ///
            if commodity == "`c'" & subsample == "peak", ///
            color(green%20) lwidth(none)) ///
        (line beta horizon ///
            if commodity == "`c'" & subsample == "peak", ///
            lcolor(green) lwidth(medium)) ///
        (rarea upper lower horizon ///
            if commodity == "`c'" & subsample == "post", ///
            color(red%20) lwidth(none)) ///
        (line beta horizon ///
            if commodity == "`c'" & subsample == "post", ///
            lcolor(red) lwidth(medium) lpattern(dash)) ///
        , ///
        yline(0, lcolor(black) lpattern(solid)) ///
        legend(order(2 "Pre (1994-2003)" ///
                     4 "Peak financialization (2009-2014)" ///
                     6 "Full post (2010-2025)") ///
               position(6) rows(1)) ///
        title("`c': IRF to 25bp Monetary Policy Tightening") ///
        xtitle("Months after shock") ///
        ytitle("Cumulative log price change") ///
        xlabel(0(4)24) ///
        note("Green = peak agricultural financialization (2009-2014)" ///
             "Shaded areas = 95% confidence bands" ///
             "Newey-West SE, bandwidth = horizon")

    graph export "$output\irf_`c'_peak.png", replace width(2000)
    di "Saved: irf_`c'_peak.png"
}

* Combined
graph combine ///
    "$output\irf_Soybeans_peak.png" ///
    "$output\irf_Wheat_peak.png" ///
    , cols(2) ///
    title("Agricultural Commodities: Peak Financialization Period") ///
    note("Blue = pre (1994-2003)" ///
         "Green = peak financialization (2009-2014)" ///
         "Red dashed = full post (2010-2025)")



graph export "$output\irf_agri_peak.png", replace width(2000)
di "=== Agricultural peak graph saved ==="


********************* OIL WITHOUT COVID AND UKRAINE WAR ************
********************************************************************************
* OIL — ALTERNATIVE POST PERIOD: 2010-2019
* Excludes COVID (2020-2021), Ukraine war (2022-2025)
* Post period: February 2010 to February 2020 (pre-COVID only)
********************************************************************************

use "$input\DTA\master_panel.dta", clear
xtset commodity_id date

global lags    4
global horizon 24

* Alternative post: 2010m1 to 2020m2 (just before COVID hit markets)
gen post_2019 = .
replace post_2019 = 0 if date >= ym(1994,1)  & date <= ym(2003,12)
replace post_2019 = 1 if date >= ym(2010,1)  & date <= ym(2020,2)
* Everything else (2004-2009 transition, 2020m3 onwards) left as missing

* Check
tab post_2019, missing
di "Pre obs (oil):  "
count if post_2019 == 0 & commodity == "Oil"
di "Post obs (oil): "
count if post_2019 == 1 & commodity == "Oil"

tempfile master results
save `master', replace

preserve
    clear
    save `results', emptyok replace
restore

local subsamples "pre post"

foreach s of local subsamples {

    if "`s'" == "pre"  local cond "post_2019 == 0"
    if "`s'" == "post" local cond "post_2019 == 1"

    di "=== Oil — `s' (2010-2020m2) ==="

    forvalues h = 0/$horizon {

        * Lags of dependent variable
        local depvarlags ""
        forvalues l = 1/$lags {
            local depvarlags "`depvarlags' L`l'.d_log_price"
        }

        * Lags of macro controls (no currency for oil)
        local macrolags ""
        foreach v in ip_growth inflation {
            forvalues l = 1/$lags {
                local macrolags "`macrolags' L`l'.`v'"
            }
        }

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

        preserve
            clear
            set obs 1
            gen str5  commodity = "Oil"
            gen str5  subsample = "`s'"
            gen horizon         = `h'
            gen beta            = `beta'
            gen se              = `se'
            gen upper           = `upper'
            gen lower           = `lower'
            gen nobs            = `obs'
            append using `results'
            save `results', replace
        restore
    }
}

* Save results
use `results', clear
sort subsample horizon
save "$input\DTA\lp_results_oil_2019.dta", replace
di "=== lp_results_oil_2019.dta saved ==="

********************************************************************************
* PLOT — compare three post definitions side by side
* pre, post 2010-2019, post 2010-2021, post 2010-2025 (baseline)
********************************************************************************

* Load all three results sets and combine
use "$input\DTA\lp_results_oil_2019.dta", clear
gen version = "2010-2020m2"
tempfile all
save `all', replace

use "$input\DTA\lp_results_oil_alt.dta", clear
gen version = "2010-2021"
append using `all'
save `all', replace

use "$input\DTA\lp_results_baseline.dta", clear
keep if commodity == "Oil"
gen version = "2010-2025"
append using `all'
save `all', replace

use `all', clear
sort version subsample horizon

********************************************************************************
* PLOT 1 — pre-COVID post only (cleanest)
********************************************************************************

use "$input\DTA\lp_results_oil_2019.dta", clear

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
                 4 "Post-financialization (2010-2020m2)") ///
           position(6) rows(1)) ///
    title("Oil: IRF to 25bp Monetary Policy Tightening" ///
          "(Post excludes COVID and Ukraine)") ///
    xtitle("Months after shock") ///
    ytitle("Cumulative log price change") ///
    xlabel(0(4)24) ///
    note("Shaded areas = 95% confidence bands" ///
         "Newey-West SE, bandwidth = horizon" ///
         "Post period: 2010m1-2020m2 only")

graph export "$output\irf_Oil_2019.png", replace width(2000)
di "Saved: irf_Oil_2019.png"

********************************************************************************
* PLOT 2 — three-way comparison across post definitions
********************************************************************************

use `all', clear

twoway ///
    (rarea upper lower horizon ///
        if subsample == "pre" & version == "2010-2025", ///
        color(blue%20) lwidth(none)) ///
    (line beta horizon ///
        if subsample == "pre" & version == "2010-2025", ///
        lcolor(blue) lwidth(medium)) ///
    (line beta horizon ///
        if subsample == "post" & version == "2010-2025", ///
        lcolor(red) lwidth(medium) lpattern(dash)) ///
    (line beta horizon ///
        if subsample == "post" & version == "2010-2021", ///
        lcolor(orange) lwidth(medium) lpattern(shortdash)) ///
    (line beta horizon ///
        if subsample == "post" & version == "2010-2020m2", ///
        lcolor(green) lwidth(medium)) ///
    , ///
    yline(0, lcolor(black) lpattern(solid)) ///
    legend(order(2 "Pre (1994-2003)" ///
                 3 "Post: full (2010-2025)" ///
                 4 "Post: excl. Ukraine (2010-2021)" ///
                 5 "Post: excl. COVID+Ukraine (2010-2020m2)") ///
           position(6) rows(2)) ///
    title("Oil: Sensitivity to Post-Period Definition") ///
    xtitle("Months after shock") ///
    ytitle("Cumulative log price change") ///
    xlabel(0(4)24) ///
    note("Point estimates only for post variants" ///
         "Shaded area = 95% CI for pre period only" ///
         "Newey-West SE, bandwidth = horizon")

graph export "$output\irf_Oil_comparison_all.png", replace width(2000)
di "Saved: irf_Oil_comparison_all.png"
MOSES