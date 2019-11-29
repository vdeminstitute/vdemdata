---
title: "vdemdata"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

A package to load, explore and work with the most recent (v9) V-Dem (Varieties of Democracy) dataset.

## V-Dem: Global Standards, Local Knowledge

Varieties of Democracy (V-Dem) is a new approach to conceptualizing and measuring democracy. We provide a multidimensional and disaggregated dataset that reflects the complexity of the concept of democracy as a system of rule that goes beyond the simple presence of elections. The V-Dem project distinguishes between five high-level principles of democracy: electoral, liberal, participatory, deliberative, and egalitarian, and collects data to measure these principles. 

We are a team of over 50 social scientists on six continents. We work with more than 3,000 country experts and a truly global International Advisory Board. [Read more about the work we do here](https://www.v-dem.net/en/).


## The vdemdata R package

This package contains the most recent V-Dem dataset (v9) and provides some additional functions:

* `vdem`: Load the dataset
* `var_info`: Print to the console basic information on a specific variable as given in the codebook

## Installation

```
# Install the development version from GitHub (since this package is still an ongoing project, keep checking for updates, new functions, etc. here!)
devtools::install_github("vdeminstitute/vdemdata")
```
output: md_document

