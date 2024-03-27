# Run the following code before a new release to pre-compile vignettes that
# depend on online services like zenodo.org.
# Inspired by https://ropensci.org/blog/2019/12/08/precompute-vignettes/
knitr::knit("vignettes/frictionless.Rmd.orig", "vignettes/frictionless.Rmd")
