********* Master's thesis empirical analysis - FAILED EXPERIMENTS  - Monetary economics************
/* 

Author: Pietro Villa 

University: London School of Economics and Political Science
Degree: MSc Economics 
Date: 03-03-2025 
Description: 
This script collects all the failed experiments in the empirical work for the Master's final thesis on the effects of monetary policy shocks on commodity (futures) prices and the role of financialization

*/
********LP INTERACTION - Strategy 1: Pre/Post 2004 Average Financialization - Failed because it gave very weird IRFs: after an initial decline, coefficients exploded: probably due to the reduced variability in the pre and post scenario interaction variable. 

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

********************************************************************************
* COMPUTE PRE/POST 2004 AVERAGES PER COMMODITY AND STANDARDISE
********************************************************************************

foreach v of local fin_measures {

    gen `v'_avg = .

    foreach c of local commodities {

        * Pre-2004 average
        quietly sum `v' if commodity == "`c'" & date < tm(2004m1)
        local avg_pre = r(mean)

        * Post-2004 average
        quietly sum `v' if commodity == "`c'" & date >= tm(2004m1)
        local avg_post = r(mean)

        * Assign average based on period
        replace `v'_avg = `avg_pre'  if commodity == "`c'" & date <  tm(2004m1)
        replace `v'_avg = `avg_post' if commodity == "`c'" & date >= tm(2004m1)
    }

    * Standardise across full sample
    quietly sum `v'_avg
    gen `v'_avg_std   = (`v'_avg - r(mean)) / r(sd)
    gen shock_`v'_avg = shock * `v'_avg_std
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

        di "=== `c' — interaction with `fm' avg (Strategy 1) ==="

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

            quietly newey dep_h`h' shock shock_`fm'_avg `fm'_avg_std ///
                `depvarlags' `macrolags' `currlags' `crisislags' ///
                if commodity == "`c'", ///
                lag(`bw')

            local beta_shock  = _b[shock]
            local se_shock    = _se[shock]
            local beta_int    = _b[shock_`fm'_avg]
            local se_int      = _se[shock_`fm'_avg]
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

********************************************************************************
* SAVE
********************************************************************************

use `results', clear
sort commodity fin_measure horizon

save "$input\DTA\lp_results_interaction_avg.dta", replace
di "=== lp_results_interaction_avg.dta saved ==="
di "Total rows: " _N
list in 1/5


********************************************************************************
* PLOT
********************************************************************************

use "$input\DTA\lp_results_interaction_avg.dta", clear

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
                color(green%15) lwidth(none)) ///
            (rarea upper_int68 lower_int68 horizon ///
                if commodity == "`c'" & fin_measure == "`fm'", ///
                color(green%30) lwidth(none)) ///
            (line beta_int horizon ///
                if commodity == "`c'" & fin_measure == "`fm'", ///
                lcolor(green) lwidth(medium)) ///
            , ///
            yline(0, lcolor(black) lpattern(solid)) ///
            title("`c': Interaction Effect — Pre/Post 2004 Average" ///
                  "Measure: `fmlbl'") ///
            xtitle("Months after shock") ///
            ytitle("Interaction coefficient") ///
            xlabel(0(4)24) ///
            legend(order(1 "90% CI" 2 "68% CI" 3 "Interaction coeff") ///
                   rows(1) size(small)) ///
            note("Negative = higher financialization amplifies" ///
                 "negative price response to monetary tightening" ///
                 "Shaded areas = 68% and 90% CI, Newey-West SE")

        graph export "$output\IRF_interaction_average\irf_`c'_interaction_`fm'_avg.png", ///
            replace width(2000)
        di "Saved: irf_`c'_interaction_`fm'_avg.png"
    }
}



********LP INTERACTION - Strategy 2: Post-2004 Dummy - Failed because the dummy captures everything that changed post 2004, without any specific reference to financialization. 

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

* Crisis dummies
gen gfc     = (date >= tm(2008m9)  & date <= tm(2009m6))
gen covid   = (date >= tm(2020m3)  & date <= tm(2021m6))
gen ukraine = (date >= tm(2022m2)  & date <= tm(2022m12))

* Post-2004 dummy and interaction
gen post2004       = (date >= tm(2004m1))
gen shock_post2004 = shock * post2004

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

    * Ukraine for Oil, Soybeans and Wheat
    local crisis_vars "gfc covid"
    if inlist("`c'", "Oil", "Soybeans", "Wheat") local crisis_vars "gfc covid ukraine"

    di "=== `c' — Post-2004 dummy interaction ==="

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

        quietly newey dep_h`h' shock shock_post2004 post2004 ///
            `depvarlags' `macrolags' `currlags' `crisislags' ///
            if commodity == "`c'", ///
            lag(`bw')

        local beta_shock  = _b[shock]
        local se_shock    = _se[shock]
        local beta_int    = _b[shock_post2004]
        local se_int      = _se[shock_post2004]
        local upper_int90 = `beta_int' + 1.645 * `se_int'
        local lower_int90 = `beta_int' - 1.645 * `se_int'
        local upper_int68 = `beta_int' + 1.000 * `se_int'
        local lower_int68 = `beta_int' - 1.000 * `se_int'
        local obs         = e(N)

        preserve
            clear
            set obs 1
            gen str14 commodity   = "`c'"
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

********************************************************************************
* SAVE
********************************************************************************

use `results', clear
sort commodity horizon

save "$input\DTA\lp_results_interaction_post2004.dta", replace
di "=== lp_results_interaction_post2004.dta saved ==="
di "Total rows: " _N
list in 1/5

********************************************************************************
* PLOT
********************************************************************************

use "$input\DTA\lp_results_interaction_post2004.dta", clear

local commodities "Coffee Copper Gold Oil Soybeans Wheat"

foreach c of local commodities {

    twoway ///
        (rarea upper_int90 lower_int90 horizon ///
            if commodity == "`c'", ///
            color(green%15) lwidth(none)) ///
        (rarea upper_int68 lower_int68 horizon ///
            if commodity == "`c'", ///
            color(green%30) lwidth(none)) ///
        (line beta_int horizon ///
            if commodity == "`c'", ///
            lcolor(green) lwidth(medium)) ///
        , ///
        yline(0, lcolor(black) lpattern(solid)) ///
        title("`c': Interaction Effect — Post-2004 Dummy") ///
        xtitle("Months after shock") ///
        ytitle("Interaction coefficient") ///
        xlabel(0(4)24) ///
        legend(order(1 "90% CI" 2 "68% CI" 3 "Interaction coeff") ///
               rows(1) size(small)) ///
        note("Positive = monetary transmission strengthened post-2004" ///
             "Negative = monetary transmission weakened post-2004" ///
             "Shaded areas = 68% and 90% CI, Newey-West SE")

    graph export "$output\IRF_interaction_dummy\irf_`c'_interaction_post2004.png", ///
        replace width(2000)
    di "Saved: irf_`c'_interaction_post2004.png"
}
