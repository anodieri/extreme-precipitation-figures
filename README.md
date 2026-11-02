# Extreme precipitation figures
Code to estimate return levels from the EURO-SUPREME dataset [1], which contains annual maxima of precipitation from the EURO-CORDEX 0.11° climate models (CMIP5 generation). The plots in [2] are made using this repository. 

One folder, `_produce_return_levels` contains scripts to calculate return levels from the annual maxima for historical simulations in six countries: 
Belgium, UK, Germany, Finland, Denmark and the Netherlands. Furthermore it contains a script to create the masks on which the calculation of return levels
depends, a script to add the elevation to the dataframes and a script to create the summaries containing e.g. the bias which is used in the figures.
There is no need to use this folder except if something should be changed about the return level calculation.
To compute return levels for the full domain, without applying any mask, the script `RL_full_domain.R` in the `full_domain` folder can be used.

The remaining folders are designated to a specific type of figure and contain the data needed to produce it, the scripts to produce this data and the plotting
script itself. Sometimes they also contain some plots as examples.

## References
[1] Van de Vyver, Hans; Van Schaeybroeck, Bert; De Cruz, Lesley (2024). Subdaily Precipitation Extremes in the EURO-CORDEX 0.11° Ensemble. World Data Center for Climate (WDCC) at DKRZ. https://doi.org/10.26050/WDCC/EURCORDEX_prec


[2] Dierickx, A., Dewettinck, W., Van Schaeybroeck, B., De Cruz, L., Caluwaerts, S., Termonia, P., and Van de Vyver, H (2025). EURO-SUPREME: sub-daily precipitation extremes in the EURO-CORDEX ensemble, Earth Syst. Sci. Data, 17, 6747–6762, https://doi.org/10.5194/essd-17-6747-2025. 
