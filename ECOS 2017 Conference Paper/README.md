# ECOS Conference Paper

This file contains the code used within the submission to the "30th INTERNATIONAL CONFERENCE on Efficiency, Cost, Optimisation, Simulation and Environmental Impact of Energy Systems."

## About Code

The code is written in R, using the R Markdown format

# Packages

The following packages are used within the analysis:

```r

# Packages used in analysis

install.packages("bestglm" # Finding the best fitting regression model
                 "caret" # Used for testing regression models
                 "extrafont", # used for changing fonts of graphics
                 "ggplot2", # producing graphs
                 "Hmisc",
                 )             

```

To avoid potential confusion or conflict with other installed packages, any package specific functions are always called with their package name e.g. `packagename::function`.
