
<!-- README.md is generated from README.Rmd. Please edit that file -->
**Licensing**

All my work is licensed under MIT (see LICENSE) while some files that come with the package (and do most of the heavy lifting) are authored as follows (and to my understanding all allow redistribution as long as leaving their notices intact):

-   **inst/helpers/jquery.min.js** : jQuery v1.9.1 | (c) 2005, 2012 jQuery Foundation, Inc. | jquery.org/license // @ sourceMappingURL=jquery.min.map
-   **inst/helpers/stickyTableHeaders.js** : Copyright (c) 2011 by Jonas Mosbech - <https://github.com/jmosbech/StickyTableHeaders> MIT license info: <https://github.com/jmosbech/StickyTableHeaders/blob/master/license.txt>

**Description**

This R package is for rendering (standalone) tables from R in HTML with sticky table headers which I find handy for interactive data inspection as well as for simple reporting.

``` r
# devtools::install_github("petermeissner/htab")    # install

library(htab)                                     # load package

?htab                                             # find help

htab( 
  airquality, 
  file="htab_example.html", 
  color   = "#000000", 
  bgcolor = scale_blues(airquality$Ozone, 5) , 
  standalone = TRUE, 
  html="
<h1>This is an example of htab package</h1> 

<p><a href='https://github.com/petermeissner/htab'>LINK</a></p>

<p>
The package renders (standalone) R data.frames as HTML tables and allows for sticky table headers thanks to <a href='https://github.com/jmosbech/StickyTableHeaders'>Jonas Mosbech</a>
</p>
")
## Writing output to file: htab_example.html
```

[Click here to see the results](http://pmeissner.com/downloads/htab_example.html)
