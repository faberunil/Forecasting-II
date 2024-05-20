##########################################################
## The following uses renv to help with reproducibility ##
##########################################################

# Uncomment the codes in this section if you would like to use it with your
# Check if renv is installed and load it
# if (!require("renv", character.only = TRUE)) {
#   install.packages("renv")
#   library("renv", character.only = TRUE)
# }

# Initialize renv and restore packages from the lockfile
# renv::init()
# renv::restore()

#############################################
## The following loads the needed packages ##
#############################################

# load your virtual environment for the project

# load the required packages
packages <- c(
  "here", # for the project's organization
  "tidyverse", "lubridate", # for wrangling
  "patchwork", "maps", "scales", # for plotting
  "knitr", "kableExtra", "rmarkdown", # for the report
  # "reticulate", # for using python
  "caret" # for the modelling part
)

# Install missing packages and load all the required libraries
purrr::walk(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
})

########################################
## The following sets the python path ##
########################################
# reticulate::use_condaenv("NAME_OF_YOUR_ENVIRONMENT")

######################################################
## The following sets a few option for nice reports ##
######################################################
# You can use this if you wanted to change the rendered output

# general options
# options(
#   digits = 3,
#   str = strOptions(strict.width = "cut"),
#   width = 69,
#   tibble.width = 69,
#   cli.unicode = FALSE
# )

# ggplot options for the report
theme_set(theme_light())

# knitr options 
# opts_chunk$set(
#   comment = "#>",
#   collapse = TRUE,
#   # cache = TRUE,
#   fig.retina = 0.8, # figures are either vectors or 300 dpi diagrams
#   dpi = 300,
#   out.width = "70%",
#   fig.align = "center",
#   fig.width = 6,
#   fig.asp = 0.618,
#   fig.show = "hold",
#   message = FALSE,
#   echo = FALSE
# )