<!-- README.md is generated from README.Rmd. Please edit that file -->

---
output:
  md_document:
    variant: markdown_github
---

## unitedR

The R package **unitedR** is an easy to use tool for the assessment of lineups
and formations in United.

## Installation

You can install unitedR from [CRAN](http://cran.rstudio.com/package=unitedR):

```r
install.packages('unitedR', dependencies = TRUE)
```

## Motivation

Until now, there is no package for the United available.

## Usage

```r
library(unitedR)
?unitedR
vignette("unitedR")

home <- formation(10,10,10,10,30)
away <- formation(10,10,10,10,30)
unitedSim(home, away)
```

## License
The package is free and open source software, licensed under GPL 3.

