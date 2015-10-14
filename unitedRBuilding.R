
# setwd("D:\\David_lokal\\Github\\United")


library(knitr)
library(devtools)
library(testthat)

# For new Packages use this
# create("unitedR")


current.code <- as.package("unitedR")
# devtools::build_vignettes()
# devtools::use_vignette("unitedR.Rnw")
# in case something was deleted or renamed, run (twice)
# load_all(current.code, recompile = TRUE)
document(current.code)
load_all(current.code)
#test(current.code)
#load_all(current.code)
#run_examples(current.code)
check(current.code)

build(current.code)

# generate manual
# if (file.exists("./unitedR.pdf")) file.remove("./unitedR.pdf")
# system(paste('R CMD Rd2pdf ',  'unitedR'))

install.packages("./unitedR_0.1.9000.tar.gz", repos = NULL, type = "source")


# require(randomizeR)
# vignette(package = "unitedR")
# vignette()
