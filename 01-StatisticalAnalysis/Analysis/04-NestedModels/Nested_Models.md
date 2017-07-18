Nested Models
================
Michael Harper
2017-02-09

The development of the model so far has been focussed on a single model only. This analysis looks therefore was interested in understanding whether the model could be split into seperate models based on a category, and for each model to be a more representative fit for the underlying data. Primarily, this interest was focussed on the Country of the application (England, Scotland or Wales):

``` r
knitr::kable(TwoWayFrequency("Country", "Status.Summary", "Approved", 
    TurbineData))
```

|          |  Approved|  Refused/Abandoned|  Total|  Percentage\_Approved|
|----------|---------:|------------------:|------:|---------------------:|
| England  |       281|                365|    646|                    43|
| Scotland |       362|                336|    698|                    52|
| Wales    |        66|                 66|    132|                    50|

Parameter List
--------------

A subset of parameters was derived from the full model of the most influential parameters. This was required as there are insufficient observations to segment the model with the full set of 30 parameters.

``` r
ModelSubset <- c("No..of.Turbines", "Urbanregions", "Nationalparks", 
    "Ramsar", "Spa", "Qual_PercentL4", "Age_Mean", "Lab_share", 
    "LD_share", "Windspeed45")

# Includes the dataset for Scottish National Party to be used
# only with the Scottish model
ModelSubsetSNP <- c("No..of.Turbines", "Urbanregions", "Nationalparks", 
    "Ramsar", "Spa", "Qual_PercentL4", "Age_Mean", "Lab_share", 
    "LD_share", "SNP_PC_share", "Windspeed45")
```

Segmenting by Country
---------------------

``` r
# Define Custom Plotting Function
Segmented_OddsPlotGroupedCustom <- function(OddsTables, linebreak){
  # Plots a faceted odds ratio plot for a list of segmented odds tables
  # 
  # Args:
  #   OddsTables: a list of odds tables as produced from the function "OddsTableSegmented"
  #
  suppressWarnings(library(ggplot2))
  
  Combined <- NULL # Blank table for results
  
  # Load reference names
  names <- read.csv("https://raw.githubusercontent.com/mikey-harper/WindStatisticalAnalysis/master/ECOS%202017%20Conference%20Paper/01-Data/Input/VariableDisplayNames.csv")
  
  for (i in 1:length(OddsTables)){ # Loop combines models into a single table
    datasetname <- (names(OddsTables[i])) # Extract name of the submodel
    ModelDataset <- OddsTables[[datasetname]] # Extract the data from the list
    
    # Rename parameters
    ModelDataset$term <- names[match(x = ModelDataset$term, table = names$Label, nomatch = ""), 2]
    Combined <- rbind(ModelDataset, Combined)
  }
  
  Terms <- ModelDataset$term
  linebrk <-linebreak # spacing between markers
  
  # Determine Max vales for axes
  Ymax <- 1.4
  Ymin <- 0.6
  
  # Crop error bars if they exceed the limits of the graph. Otherwise the scale has to be massive to get them on
  Combined$ci_lower[Combined$ci_lower <= Ymin] <- Ymin
  Combined$ci_upper[Combined$ci_upper >= Ymax] <- Ymax
  
  # axis values produces labels which always return 1
  axisValues <- c( -rev(seq(0, abs(Ymin), linebrk)),  seq(linebrk, Ymax, by = linebrk))
  offset <- 1 # Defines where barplots start from
  spacing <- 0.7
  windowsFonts(Times=windowsFont("TT Times New Roman"))
  
  plotOdds <- ggplot(Combined, aes(x = term, y = odds - offset, fill = Facet, width = spacing)) +
    geom_hline(yintercept = 0) +
    geom_bar(position=position_dodge(), stat="identity",colour = "black") + 
    geom_errorbar(aes(ymin = ci_lower - offset, ymax = ci_upper - offset),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(spacing),
                  size = 0.4,
                  colour = "grey20",
                  linetype = 1
    ) +
    
    
    scale_fill_manual(values = c("England" = "burlywood4", "Scotland" = "burlywood", "Wales" = "lightyellow"), guide = guide_legend(title = "Country")) +
    # Axes
    labs(y = "Odds Ratio") +
    scale_y_continuous(labels = seq( from = Ymin, to =  Ymax, by = linebreak),
                       breaks = seq(from = Ymin - offset, to = Ymax - offset, by = linebreak),
                       limits = c(Ymin, Ymax) - offset,
                       expand=c(0,0)
    ) +
    scale_x_discrete(limits = rev(Terms)) +
    
    # Theme
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major = element_line(colour = "grey46",  size = 0.2),
          axis.ticks = element_line(colour = "grey46",  size = 0.2),
          axis.title.y=element_blank(),
          panel.background = element_rect(fill = "white", colour = "grey46"),
          text = element_text(family="Times", size=12, colour = "black"),
          axis.text = element_text(family="Times", size=11, colour = "black"),
          plot.margin=unit(c(0.2,0.5,0.2,0.2),"cm")
          
    ) +
    coord_flip()
  
  return(plotOdds)
}
```

``` r
# Building Graph
a <- Segmented_Dataset(TurbineData, by = "Country")
b <- Segmented_LogisticModels(a, ModelSubset, outcome = "Status.Summary")
c <- Segmented_OddsTable(LogisticModelList = b)
SegmentedPlot <- Segmented_OddsPlotGroupedCustom(c, 0.1)
SegmentedPlot
```

![](Nested_Models_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
jpeg(filename = "Results/SegmentedCountry.jpg", width = 17, height = 11, 
    units = "cm", res = 600)
SegmentedPlot
dev.off()
```

    ## png 
    ##   2

Add SNP for Scotland
====================

Following code is slightly messy but designed to plot graphic

``` r
# Recreate the log model for Scotland including the SNP data
b$Scotland <- LogisticModel(PredictorVariables = c(ModelSubset, 
    "SNP_PC_share"), a$Scotland)
c <- Segmented_OddsTable(LogisticModelList = b)

# The england and wales models do not have odds ratios for
# the SNP value. Therefore we add blank values to make sure
# the variable plots
blankSNP <- c("SNP_PC_share", as.numeric(rep(1, 9)))
c$England <- rbind(c$England, blankSNP)
c$Wales <- rbind(c$Wales, blankSNP)

# Repair row names
row.names(c$England) <- c$England[, 1]
row.names(c$Wales) <- c$England[, 1]

# Update group name
c$Wales$Facet <- "Wales"
c$England$Facet <- "England"

# Repair columns as numeric
cols <- c(2:8)
c$England[, cols] = apply(c$England[, cols], 2, function(x) as.numeric(as.character(x)))
c$Wales[, cols] = apply(c$Wales[, cols], 2, function(x) as.numeric(as.character(x)))

# Reorder dataframe
c$Wales <- c$Wales[match(ModelSubsetSNP, row.names(c$Wales)), 
    ]
c$England <- c$England[match(ModelSubsetSNP, row.names(c$England)), 
    ]
c$Scotland <- c$Scotland[match(ModelSubsetSNP, row.names(c$Scotland)), 
    ]

# Produce Odds plot
(SegmentedPlot <- Segmented_OddsPlotGroupedCustom(OddsTables = c, 
    linebreak = 0.1))
```

![](Nested_Models_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
# Save the results
jpeg(filename = "Results/SegmentedCountrySNP.jpg", width = 17, 
    height = 11, units = "cm", res = 600)
SegmentedPlot
dev.off()
```

    ## png 
    ##   2

Detailed Results of Models
==========================

``` r
SegmentedDatasetsCountry <- Segmented_Dataset(TurbineData, by = "Country")

SegmentedDatasetsCountry$full <- TurbineData
SegmentedLogisticModelsCountry <- Segmented_LogisticModels(SegmentedDatasetsCountry, 
    ModelSubset, "Status.Summary")

SegmentedLogisticModelsCountry$Scotland <- b$Scotland

lapply(SegmentedLogisticModelsCountry, LogisticPseudoR2s)
```

    ## Pseudo R^2 for logistic regression
    ## Hosmer and Lemeshow R^2   0.058 
    ## Cox and Snell R^2         0.076 
    ## Nagelkerke R^2            0.102 
    ##  
    ## Pseudo R^2 for logistic regression
    ## Hosmer and Lemeshow R^2   0.093 
    ## Cox and Snell R^2         0.12 
    ## Nagelkerke R^2            0.16 
    ##  
    ## Pseudo R^2 for logistic regression
    ## Hosmer and Lemeshow R^2   0.14 
    ## Cox and Snell R^2         0.176 
    ## Nagelkerke R^2            0.235 
    ##  
    ## Pseudo R^2 for logistic regression
    ## Hosmer and Lemeshow R^2   0.055 
    ## Cox and Snell R^2         0.073 
    ## Nagelkerke R^2            0.098 
    ## 

    ## $England
    ## NULL
    ## 
    ## $Scotland
    ## NULL
    ## 
    ## $Wales
    ## NULL
    ## 
    ## $full
    ## NULL

``` r
lapply(SegmentedLogisticModelsCountry, ChiSquared)
```

    ## Chi Squared Test 
    ## Chi Squared               51.174 
    ## Df                        10 
    ## Chi Squared p             0 
    ##  
    ## Chi Squared Test 
    ## Chi Squared               89.298 
    ## Df                        11 
    ## Chi Squared p             0 
    ##  
    ## Chi Squared Test 
    ## Chi Squared               25.579 
    ## Df                        10 
    ## Chi Squared p             0.004 
    ##  
    ## Chi Squared Test 
    ## Chi Squared               112.206 
    ## Df                        10 
    ## Chi Squared p             0 
    ## 

    ## $England
    ## NULL
    ## 
    ## $Scotland
    ## NULL
    ## 
    ## $Wales
    ## NULL
    ## 
    ## $full
    ## NULL
