Data Preprocessing
================
Michael Harper
2017-02-09

Loading Data
------------

The turbine data is laoded into the file. This contains a number of variables:

``` r
TurbineData <- read.csv("Input/FullInfo_AllTurbines.csv")
names(TurbineData)
```

    ##  [1] "Turbine_ID"                       "X.coordinate"                    
    ##  [3] "Y.coordinate"                     "Site.Name"                       
    ##  [5] "No..of.Turbines"                  "Turbine.Capacity..MW."           
    ##  [7] "Capacity"                         "Status.Summary"                  
    ##  [9] "Old.Ref.ID"                       "Record.Last.Updated..dd.mm.yyyy."
    ## [11] "County"                           "Region"                          
    ## [13] "Country"                          "Planning.Application.Submitted"  
    ## [15] "Under.Construction"               "Operational"                     
    ## [17] "Size"                             "year"                            
    ## [19] "Censusboundaries"                 "Airports"                        
    ## [21] "Aonb"                             "Aroads"                          
    ## [23] "Broads"                           "Hcoast"                          
    ## [25] "Localunitaryauthority"            "Minroads"                        
    ## [27] "Motorways"                        "Nationalparks"                   
    ## [29] "Nnr"                              "Powerlines"                      
    ## [31] "Primaryroads"                     "Railway"                         
    ## [33] "Ramsar"                           "Sacs"                            
    ## [35] "Spa"                              "Sssi"                            
    ## [37] "Ukelevation"                      "Urbanregions"                    
    ## [39] "Windspeed45"                      "Code"                            
    ## [41] "Type"                             "Council_size"                    
    ## [43] "Con"                              "Lab"                             
    ## [45] "LD"                               "Other"                           
    ## [47] "SNP_PC"                           "Control"                         
    ## [49] "Majority"                         "Con_share"                       
    ## [51] "Lab_share"                        "LD_share"                        
    ## [53] "Oth_share"                        "SNP_PC_share"                    
    ## [55] "Age_Total"                        "Age_Mean"                        
    ## [57] "Age_Median"                       "Qual_Total"                      
    ## [59] "Qual_Level4"                      "Qual_PercentL4"                  
    ## [61] "SocialGradeTotal"                 "SocialGradePercentAB"            
    ## [63] "Tenure_Total"                     "Tenure_Owned"                    
    ## [65] "PercentOwner"

Data Transformation
-------------------

Planning status has been summarised to three variables:

1.  Accepted
2.  In Planning (Decision Pending)
3.  Rejected

For simplification, any turbines still in planning were removed:

``` r
# Removes "Awaiting Decision" Factor from list
TurbineData <- data.frame(TurbineData[TurbineData$Status.Summary != "Submitted", ])

# Relevel data to make the Reject 0 and Accept 1 
TurbineData$Status.Summary <- factor(TurbineData$Status.Summary) # relevel factor
TurbineData$Status.Summary <- relevel(TurbineData$Status.Summary, "Refused/Abandoned") 

# Add a numeric ID for planning status.
TurbineData$Planning_Status_Code <- TurbineData$Status.Summary  %in% "Approved"
TurbineData$Planning_Status_Code <- TurbineData$Planning_Status_Code * 1 # Convert TRUE FALSE to 1 and 0
```

Filtering Smaller Turbines
--------------------------

Finally, the analysis only deals with large wind turbines (projects which are greater than 1MW in capacity). The smaller turbines are therefore filtered from the database:

``` r
TurbineData <- TurbineData[TurbineData$Size == "Large", ]
```

The resulting dataset has a total of 1476 observations.

Saving Data
-----------

The resulting dataset is saved as a CSV to be used within the following stages of the analysis.

``` r
write.csv(x = TurbineData, file = "Preprocessed/TurbineDataset.csv", row.names = FALSE)
```
