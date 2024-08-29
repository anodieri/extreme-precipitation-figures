# Extreme precipitation figures
Code to create the figures on extreme precipitation in EURO-CORDEX 0.11°.

The folders are designated to a specific type of figure and contain the data needed to produce it, the scripts to produce this data and the plotting
script itself. Sometimes they also contain some plots as examples.

One folder, `_produce_return_levels` contains scripts to calculate return levels from the annual maxima for historical simulations in six countries: 
Belgium, UK, Germany, Finland, Denmark and the Netherlands. Furthermore it contains a script to create the masks on which the calculation of return levels
depends, a script to add the elevation to the dataframes and a script to create the summaries containing e.g. the bias which is used in the figures.
There is no need to use this folder except if something should be changed about the return level calculation.
