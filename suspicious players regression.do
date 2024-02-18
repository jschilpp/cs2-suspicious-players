* Suspicious players regression
* Requires the reghdfe package

* housekeeping
frame reset

* cd
cd "XXX" // insert your working directory with the dta file here

* import data
use "Data\match_infos.dta"

keep if period != 3 // remove treatment period such that same number of pre- and post-treatment periods are present
reghdfe hs_pct treated##after rank, absorb(period player_name) vce(robust) /// run fixed effects (controlling for day and player-specific effects) difference-in-differences regression, the interaction term "treated##after" is the DiD coefficient, showing us the significance and magnitude of the ban wave's effect on Premier players headshot percentage