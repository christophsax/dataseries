R tools for [dataseries.org](http://www.dataseries.org)
===========================================================

A very preliminary test version to import data series from
[dataseries.org](http://www.dataseries.org) into R. It currently only contains
one function `ds` to download series as `xts` objects.

To install:

    if (!require("devtools")) install.packages("devtools")
    devtools::install_github("christophsax/dataseries")

Usage:

    dataseries::ds(c("CCI.AIK", "CCI.ASSS"))

