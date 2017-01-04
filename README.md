R Tools to Work With [www.dataseries.org](www.dataseries.org)
=============================================================

A very preliminary test version to import data series from
[www.dataseries.org](www.dataseries.org) into R. It currently only only contains
one function, `ds`, that imports series as `xts` objects.

To install:

    if (!require("devtools")) install.packages("devtools")
    devtools::install_github("christophsax/dataseries")

Usage:

    dataseries::ds(c("CCI.AIK", "CCI.ASSS"))

