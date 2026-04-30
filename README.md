
An R package to load, explore and work with the most recent [V-Dem (Varieties of Democracy)](https://www.v-dem.net/vdemds.html) and [V-Party](https://www.v-dem.net/vpartyds.html) datasets. 

## V-Dem: Global Standards, Local Knowledge ##

Varieties of Democracy (V-Dem) conceptualizes and measures democracy. We provide a multidimensional and disaggregated dataset that reflects the complexity of the concept of democracy as a system of rule that goes beyond the simple presence of elections. The V-Dem project distinguishes between five high-level principles of democracy: electoral, liberal, participatory, deliberative, and egalitarian, and collects data to measure these principles. 

We are a team of over 50 social scientists on six continents. We work with more than 5,000 country experts. Read more about the work we do [here](https://www.v-dem.net/).


## The vdemdata R package ##

This package contains the most recent V-Dem (Country-Year: V-Dem Full+Others) and V-Party datasets and provides some additional functions. Other versions of the V-Dem dataset can be downloaded from our [website](https://v-dem.net/data/the-v-dem-dataset/).

#### Basics: ####
* `vdem`: Load the V-Dem dataset (for non-R users, please access the V-Dem dataset [here](https://www.v-dem.net/vdemds.html))
* `vparty`: Load the V-Party dataset (for non-R users, please access the V-Party dataset [here](https://www.v-dem.net/vpartyds.html))
* `var_info`: Print to the console basic information on a specific variable as given in the codebook of the V-Dem dataset
* `find_var`: Search variables via keywords in the V-Dem dataset
* `fill_vars`: Fill election-specific variables in the V-Dem dataset

#### Graphics: ####
* `plot_indicator`: Plot V-Dem indicators for exploratory data analysis.


## Installation ##

```
# Install the development version of the vdemdata package 
# (the package is an ongoing project, 
# keep checking for updates)

# The easiest way is using the `pak` package:
# Install pak if you haven't already
install.packages("pak")

# Install vdemdata
pak::pak("vdeminstitute/vdemdata")

# NOTE: make sure you have an updated R version and
# - since the package is a development version - 
# an updated version of rlang, xcode (Mac), rtools (Windows), r-base-dev (Linux)
# installed. If you have troubles with the installation 
# write to contact@v-dem.net at the V-Dem Institute.

# NOTE: a tar.gz is also provide if you encounter any 
# issues in the installation processes.
```

## Suggested Citations ##

Please remember to cite this package and the underlying datasets when using them for published work. 

#### Package ####

Maerz, Seraphine F., Amanda B. Edgell, Sebastian Hellemeier, Nina Illchenko, and Linnea Fox. 2026. vdemdata: An R package to load, explore and work with the most recent V-Dem (Varieties of Democracy) dataset. https://github.com/vdeminstitute/vdemdata


#### V-Dem Country-Year+Others Dataset ####

Coppedge, Michael, John Gerring, Carl Henrik Knutsen, Staffan I. Lindberg, Jan Teorell, David Altman, Fabio Angiolillo, Michael Bernhard, Agnes Cornell, M. Steven Fish, Linnea Fox, Lisa Gastaldi, Haakon Gjerløw, Adam Glynn, Ana Good God, Allen Hicken, Katrin Kinzelbach, Joshua Krusell, Kyle L. Marquardt, Kelly McMann, Valeriya Mechkova, Juraj Medzihorsky, Anja Neundorf, Pamela Paxton, Daniel Pemstein, Josefine Pernes, Johannes von Römer, Brigitte Seim, Rachel Sigman, Svend-Erik Skaaning, Jeffrey Staton, Aksel Sundström, Marcus Tannenberg, Eitan Tzelgov, Yi-ting Wang, Tore Wig, Steven Wilson and Daniel Ziblatt. 2026. "V-Dem Country-Year Dataset v16" Varieties of Democracy (V-Dem) Project. https://doi.org/10.23696/vdemds26.


#### V-Party Dataset ####

Lindberg, Staffan I., Nils Düpont, Masaaki Higashijima, Yaman Berker Kavasoglu, Kyle L. Marquardt, Michael Bernhard, Holger Döring, Allen Hicken, Melis Laebens, Juraj Medzihorsky, Anja Neundorf, Ora John Reuter, Saskia Ruth–Lovell, Keith R. Weghorst, Nina Wiesehomeier, Joseph Wright, Nazifa Alizada, Paul Bederke, Lisa Gastaldi, Sandra Grahn, Garry Hindle, Nina Ilchenko, Johannes von Römer, Steven Wilson, Daniel Pemstein, Brigitte Seim. 2022. Varieties of Party Identity and Organization (V–Party) Dataset V2. Varieties of Democracy (V–Dem) Project. https://www.v-dem.net/data/v-party-dataset/

