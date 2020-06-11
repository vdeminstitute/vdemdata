
An R package to load, explore and work with the most recent V-Dem (Varieties of Democracy) dataset (v10).

## V-Dem: Global Standards, Local Knowledge ##

Varieties of Democracy (V-Dem) is a new approach to conceptualizing and measuring democracy. We provide a multidimensional and disaggregated dataset that reflects the complexity of the concept of democracy as a system of rule that goes beyond the simple presence of elections. The V-Dem project distinguishes between five high-level principles of democracy: electoral, liberal, participatory, deliberative, and egalitarian, and collects data to measure these principles. 

We are a team of over 50 social scientists on six continents. We work with more than 3,000 country experts and a truly global International Advisory Board. [Read more about the work we do here](https://www.v-dem.net/en/).


## The vdemdata R package ##

This package contains the most recent V-Dem dataset (v10) and provides some additional functions.

#### Basics: ####
* `vdem`: Load the dataset (for non-R users, please access the V-Dem dataset [here](https://www.v-dem.net/en/data/data-version-10/))
* `var_info`: Print to the console basic information on a specific variable as given in the codebook
* `find_var`: Search variables via keywords
* `fill_vars`: Fill election-specific variables 

#### Graphics: ####
* `plot_indicator`: Plot V-Dem indicators for exploratory data analysis.
* `plot_episode`: Plot Episodes of Regime Transitions (ERT) over time.

#### Data on Episodes of Regime Transitions (ERT, for details see also the [ERT Codebook](https://github.com/vdeminstitute/vdemdata/blob/master/inst/ERT_codebook.pdf)): ####
* NOTE: for non-R users we provide [the ERT dataset here as csv. file](https://github.com/vdeminstitute/ERT) - however, we recommend using our vdemdata R package since one huge advantage of the package is that it allows to flexibly set parameters for generating the episodes.

* `get_eps`: Identify episodes of regime transitions (autocratization, democratization) in the most recent V-dem data set. Autocratization is defined as any movement towards autocracy which starts within democracies or autocracies [(cf. Lührmann and Lindberg, Democratization, 2019)](https://www.tandfonline.com/doi/full/10.1080/13510347.2019.1582029). Democratization is defined as any movement towards democracy which starts in autocracies or democracies [(cf. Wilson et al., 2020)](https://www.v-dem.net/en/news-publications/working-papers/)
* `find_overlap`: Find potential overlaps between episodes of democratization and autocratization which may occur depending on how the thresholds are set.
* `fix_overlap`: Interactive function to fix potential overlaps between episodes of democratization and autocratization which may occur depending on how the thresholds are set.

## Installation ##

```
# Install the development version of the vdemdata package 
# (since this package is still an ongoing project, 
# keep checking for updates, new functions, etc.!)

# First, you need to have the devtools package installed
install.packages("devtools")
# now, install the vdemdata package directly from GitHub
devtools::install_github("vdeminstitute/vdemdata")

# NOTE: make sure you have an updated R version (> 3.5) and
# - since the package is still a development version - 
# an updated version of xcode (Mac), rtools (Windows), r-base-dev (Linux)
# installed. If you have troubles with the installation 
# write to the package maintainer seraphine.maerz@v-dem.net
```


