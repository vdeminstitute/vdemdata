
An R package to load, explore and work with the most recent [V-Dem (Varieties of Democracy)](https://www.v-dem.net/en/data/data/v-dem-dataset/) and [V-Party](https://www.v-dem.net/en/data/data/v-party-dataset/) datasets.

## V-Dem: Global Standards, Local Knowledge ##

Varieties of Democracy (V-Dem) is a new approach to conceptualizing and measuring democracy. We provide a multidimensional and disaggregated dataset that reflects the complexity of the concept of democracy as a system of rule that goes beyond the simple presence of elections. The V-Dem project distinguishes between five high-level principles of democracy: electoral, liberal, participatory, deliberative, and egalitarian, and collects data to measure these principles. 

We are a team of over 50 social scientists on six continents. We work with more than 3,000 country experts and a truly global International Advisory Board. [Read more about the work we do here](https://www.v-dem.net/en/).


## The vdemdata R package ##

This package contains the most recent V-Dem and V-Party datasets and provides some additional functions.

#### Basics: ####
* `vdem`: Load the V-Dem dataset (for non-R users, please access the V-Dem dataset [here](https://www.v-dem.net/en/data/data/v-dem-dataset/))
* `vparty`: Load the V-Party dataset (for non-R users, please access the V-Party dataset [here](https://www.v-dem.net/en/data/data/v-party-dataset/))
* `var_info`: Print to the console basic information on a specific variable as given in the codebook of the V-Dem dataset
* `find_var`: Search variables via keywords in the V-Dem dataset
* `fill_vars`: Fill election-specific variables in the V-Dem dataset

#### Graphics: ####
* `plot_indicator`: Plot V-Dem indicators for exploratory data analysis.


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


