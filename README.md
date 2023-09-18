# output-elasticities
Code for cleaning manufacturing data and estimate production functions. It outputs estimates for the variable input's (labor in this care) output elasticity estimated using q data.

# Some comments
* The production value in IVP data (which has quantity data) has about 0.5 correlation with the production value in the FEK data (which has input data). To resolve this, I calculate a price at the firm level using IVP data, merge this with the FEK data, and then define a quantity in the FEK data based on the firm level price and the production value in FEK.
* Output elasticities are estimated for groups with at least 50 observations, meaning many groups don't have an estimate. I provide cost share estimates for all groups, but these may not be a great proxy.
* The output elasticities are estimated at the (`sni2`, `sni_regime`) level. See the function `create_snigroups()` in `load_data.R`for how these are defined based on the evolving variable `sni`.
