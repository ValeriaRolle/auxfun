# auxfun (AUXiliary FUNctions)
This package is a compilation of diverse and useful functions and wrappers designed to make my life (and the lives of some colleagues) easier.

## Functions included 
Updated: 2023-06-26


* ggtoppt() wrapper to quickly export ggplot2 and base plots to an editable powerpoint. This is the main function and purpose of the package.
* fptable() n and % of all categories in a variable with nice html and pdf rendering.
* ssummary() is the same as base::summary() but with the SD and nice html and pdf rendering.
* vcol() shows the names of all columns in a data frame and their order number.
* plot_coeffs1() is just a jtools::plot_coeffs() personalization.
* urlobs() reads a file path from the clipboard an changes the format to an Obsidian-friendly one.
* tabla2() descriptive table from a data frame.
* tabla3() same as table 2 but grouped and with p values.
* ortable() prints OR or HR, 95% CI and p values from logistic or survival models.

More details available in each function's help page.

## Installation

```r
require(devtools)
devtools::install_github("ValeriaRolle/auxfun")
```

