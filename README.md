dataseries: Switzerland's Data Series in One Place
==================================================

Tools to download and import time series from
[www.dataseries.org](http://www.dataseries.org).

The packages contains a single function, `ds`, which downloads series from
[www.dataseries.org](http://www.dataseries.org) as a `data.frame` or `xts`.

To install:

    install.packages("dataseries")

Usage:

    dataseries::ds(c("CCI.AIK", "CCI.ASSS"))
    dataseries::ds(c("CCI.AIK", "CCI.ASSS"), class = "xts")

