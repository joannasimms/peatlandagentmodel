# peatlandagentmodel
A agent based model to analyse the effect of peatland subsides on GHG emissions in Lapland.
Written for a thesis at Aalto University. 
Further background, assumptions and planned improvements are explained in the accoupaning thesis.
To be updated when approval is granted for my thesis.

The model at this point contains functions to
- mimmic the Lapland farming conditions
- calculate the cost in emissions and euros for land packages of 2ha amended by a choice of subsidies
- an agent based decision process between subsidies, based on the SAGA model (van Duinen, 2016)
- the beginnings of a govenement optimisation function that would give the impact of these choices on a national level.

The data, unless otherwise stated, is amalgamated data from Luonnonvarakeskus.
Emissions data is from FOA, IPCC, Karki 2015, Huttunen 2003

Plans to
- generate more localised data from CoupModel and CMIP6 climate predictions to create a more detailed model.
- rewrite some of the hidden code in C/C++ to improve the speed.

How to download
The model is open source and can be downloaded as an R package.
Although the model function isn't currently included in the package and is run from the file labled model.R
See download instructutions for R Packages here: https://cran.r-project.org/web/packages/githubinstall/vignettes/githubinstall.html


# References

Huttunen, Jari T., et al. "Methane emissions from natural peatlands in the northern boreal zone in Finland, Fennoscandia." Atmospheric environment 37.1 (2003): 147-151

Karki, Sandhya, Lars Elsgaard, and Poul Erik Lærke. "Effect of reed canary grass cultivation on greenhouse gas emission from peat soil at controlled rewetting." Biogeosciences 12.2 (2015): 595-606.

van Duinen, Rianne, et al. "Going beyond perfect rationality: drought risk, economic choices and the influence of social networks." The Annals of Regional Science 57.2 (2016): 335-369.

