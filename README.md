
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RRmorph

<!-- badges: start -->
<!-- [![R-CMD-check](https://github.com/pedrocoutinhosilva/hexFinder/workflows/R-CMD-check/badge.svg)](https://CRAN.R-project.org/package=hexFinder) -->

[![cranlogs](https://www.r-pkg.org/badges/version/RRmorph)](https://CRAN.R-project.org/package=RRmorph)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/RRmorph)](https://CRAN.R-project.org/package=RRmorph)
<!--[![cranlogs](https://cranlogs.r-pkg.org/badges/RRmorph)](https://CRAN.R-project.org/package=RRmorph)-->

<!-- badges: end -->

------------------------------------------------------------------------

The goal of `RRmorph` is to provide tools to map evolutionary rate and
patterns directly on three-dimensional shapes. The main computational
functions of this package are `rate.map` and `conv.map`, which are still
also available as a part of the
[`RRphylo`](https://cran.r-project.org/web/packages/RRphylo/index.html)
R package (where they were first published). The reason we decided to
move these functions in a brand new package is that we made some heavy
changes to both of them and, in doing so, produced some additional tools
we think might be useful to everyone deals with 3d meshes and surfaces.
In `RRmorph`, `rate.map` and `conv.map` are able to plot evolutionary
rates/convergence patterns on real 3d surfaces. This is done by means of
a new function, `interpolMesh`, which transfers values related to the
reconstructed meshes (the output of `RRphylo::rate.map` and
`RRphylo::conv.map`) to the real 3D surfaces they were derived from.

## Installation from CRAN

The package is not yet available on CRAN.

## Installation from github

You can install RRmorph from github by using the R package devtools:

``` r
install.packages("devtools")

devtools::install_github("pasraia/RRmorph",dependencies=TRUE)
```
