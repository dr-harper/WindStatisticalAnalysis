# Name:       Functions
# Date:       20th January 2017  
# Author:     Michael Harper 

# Logistic Regression Functions -----------------------------------------------------------

#  --- Model Building

ParameterUpdate <- function(input, add = NULL, remove = NULL){
  # Update the list of parameters for the model building. Used in conjunction with the
  # "LogisticModel" and "LogsticModelInt" functions
  # 
  # Args:
  #   input: an existing list of input parameters
  #   add: a list of parameters to be added to the model (optional)
  #   remove: a list of values to be removed from the model (optional)
  #
  # Returns:
  #   An updateded model parameter list
  #
  ParList <- c(input, add)
  ParList <- ParList[!ParList %in% c(remove)]
  
  return(ParList)
}

LogisticModel <- function(PredictorVariables, Dataframe){
  # Function to build logistic model from a list of input parameters
  #
  # Args:
  #   PredictorVariables: a list of predictor variables
  #
  Formula <- formula(paste("Status.Summary ~ ", paste(PredictorVariables, collapse=" + ")))
  GlmModel <- glm(Formula , data = Dataframe, family = binomial())
  return(GlmModel)
}

# --- Diagnostics

LogisticDiagnostics <- function(Model){
  # Prints results for the Chi Squared, Psuedo R sqaured values, Variance Inflation 
  # Factors and Durbin Watson Test
  #
  # Args:
  #     Model: The statistical Model to be tested
  #
  ChiSquared(Model)
  LogisticPseudoR2s(Model)
  VIFcheck(Model)
  DurbinWatsonCheck(Model)
}

ChiSquared <- function(Model){
  # Calculates the Chi Squared statistics for a model and compares against the null
  # model. 
  #
  # Args:
  #   Model: The statistical Model to be tested
  #
  modelChi <- Model$null.deviance - Model$deviance
  chidf <- Model$df.null - Model$df.residual  # Degrees of Freedom 
  chisq.prob <- 1 - pchisq(modelChi, chidf) 
  Null.deviance <- cbind(modelChi, chidf, chisq.prob)
  
  cat("Chi Squared Test \n")
  cat("Chi Squared              ", round(modelChi, 3), "\n")
  cat("Df                       ", round(chidf, 3), "\n")
  cat("Chi Squared p            ", round(chisq.prob, 3), "\n \n")
  
}

LogisticPseudoR2s <- function(Model) {
  # Calculates the R squared values (Hosmer and Lemeshow, Cox and Snell and
  # Nagelkerke) values for a logistic regression model.
  # 
  # Args:
  #   Model: The statistical Model to be tested
  #
  dev <- Model$deviance
  nullDev <- Model$null.deviance
  modelN <-  length(Model$fitted.values)
  R.hl <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.hl, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n \n")  }


DurbinWatsonCheck <- function(Model){
  # Calculate the Durbin–Watson statistic to detect the presence of autocorrelation
  # in the residuals from a regression analysis.
  #
  # Args:
  #   Model: A linear model object
  #
  DW <- car::durbinWatsonTest(Model)
  # Check results
  b <-  (DW$p > 0.05)  # check bootstrapped p-values
  d <- (0 < DW$dw & DW$dw < 4)  # Check DurbinWatsonRange
  # Print Results
  cat("Durbin Watson Results:", "\n", "\n")
  print(DW)
  cat("\n","Is p-value greater than 0.05:            ", b)
  cat("\n","Is DW ~ 2 (range of 0 to 4 acceptable):  ", d) }


VIFcheck <- function(Model){
  # Calculates variance-inflation and generalized variance-inflation factors for 
  # linear and generalized linear models, and returns any which are above 10.
  #
  # Args: an object that responds to coef, vcov, and model.matrix, such as an lm or glm object
  # 
  vifresults <- car::vif(Model)
  # Check VIF for values above 10
  suspectvif <- vifresults[order(vifresults, decreasing = TRUE) & vifresults > 10]
  
  # Print Results
  cat("Variance Inflation Factors: \n")
  print(vifresults)
  cat("\n","Checking for potential issues: \n")
  
  if (length(suspectvif) > 1){  # Checks if empty
    return(suspectvif) }
  else
    return(cat("No apparent issues with collinearity"))
}

# --- Advanced Diagnostics: Checking Linearity & Collinearity

LogisticModelInt <- function(PredictorVariables, Outcome, dataframe){
  # Takes a list of variables, and  updates the list to include the log transformations (Int)
  # 
  # Args:
  #   variableList: a list of input model parameters
  #
  
  # Load Data
  fulldata <- dataframe[, c(Outcome, PredictorVariables)]
  
  # Calculation the logarithmic transformations
  for (i in PredictorVariables){
    data <- dataframe[ ,eval(i)]
    suppressWarnings(int <- log(data)* data)
    int <- as.data.frame(int)
    names(int) <- paste(i,"Int", sep="_")
    
    fulldata <- cbind(fulldata, int)
    rm(i, int)
  }
  
  # Formulate Logistic Regressio Model
  intNames <- paste(PredictorVariables, "Int", sep ="_")
  variableInt <- c(PredictorVariables, intNames)
  Formula <- formula(paste(Outcome,  "~", paste(variableInt, collapse=" + ")))
  GlmModel <- glm(Formula , data = fulldata, family = binomial())
  
  # Extract significance values
  Significance <- summary(GlmModel)$'coefficients'[, 4]
  Significance <- Significance[Significance < 0.05 ]
  
  # Return list of signficant parameters
  cat("Statistically significant parameters from Logarithmic Transformations: \n")
  cat(names(Significance), sep = ", ")
}

VariableCluster <- function(PredictorVariables, dataset){
  #  Divides a set of numeric variables into disjoint or hierarchical cluster
  # 
  Formula <- formula(paste("Status.Summary ~ ", paste(PredictorVariables, collapse=" + ")))
  ClusterPlot <- plot(Hmisc::varclus(Formula, data = dataset))
  return(ClusterPlot)
}

# --- Presenting Model Results

OddsTable <- function(Model, round = 3){
  # Creates an odds table for parameters within the regression model
  #
  # Args:
  #   Model: The statistical Model to be tested
  #
  odds <- exp(Model$coefficients)
  # Compute Confidence Intervals for Logistic Model parameters
  CI <- suppressMessages(exp(confint(Model)))
  results <- cbind(odds, CI) # Create Results Table
  results <- as.data.frame(cbind(odds, CI)) # Create Results Table
  row.names(results) <- matchNames(row.names(results))
  results <-  round(results, digits = round)
  return(results) 
}


LogisticResultsTable <- function(Model, round = 3){
  # Formats a logistic regression results table including significance indicators,
  # Odds ratios and confidence intervals
  # Base Table
  ResultsTable <- as.data.frame(summary(Model)$coefficients)
  ResultsTable <- round(ResultsTable, round)
  ResultsTable <- ResultsTable[,c(1,2,4)] 
  
  # Add Significance Stars
  pval <- ResultsTable[,3]
  symp <- symnum(pval, corr = FALSE,
                 cutpoints = c(0,  .001,.01,.05, .1, 1),
                 symbols = c("***","**","*","."," "))
  ResultsTable$Signif <- symp
  
  # Add Odd Ratio
  odds <- exp(Model$coefficients)
  CI <- suppressMessages(exp(confint(Model)))
  OddsResults <- cbind(odds, CI) # Create Results Table
  OddsResults <- round(OddsResults, round)
  ResultsTable <- cbind(ResultsTable, OddsResults)
  
  # Make row names a column
  ResultsNames <- row.names(ResultsTable)
  ResultsTable <- cbind(ResultsNames, ResultsTable)
  row.names(ResultsTable) <- NULL
  
  # Rename Columns
  names(ResultsTable) <- c("Variable", "Estimate", "Std. Error","Pr", "Sig.", "Odds Ratio", "OR 2.5% CI", "OR 97.5% CI")
  
  return(ResultsTable)
}

LogisticOddsPlot <- function(Model, Sort = FALSE, Title){
  
  a <- LogisticOddsTable(Model, Sort)
  b <- LogOddsPlotGraph(a, Title, Sort)
  return(b)
}

LogisticOddsTable <- function(Model, Sort = FALSE){
  # Produces an odds table with confidence intervals for a logistic model
  #
  ModelDF <-  broom::tidy(Model)
  ModelDF$odds <- exp(Model$coefficients)
  CI <- suppressMessages(exp(confint(Model)))
  CI <- as.data.frame(CI)
  names(CI) <- c("ci_lower", "ci_upper")
  ModelDF <- cbind(ModelDF, CI)
  ModelDF <- ModelDF[ModelDF$term != "(Intercept)", ]
  
  # Reorder values. Default order is as listed into model
  if (Sort == "Score"){ 
    ModelDF <- ModelDF[order(ModelDF$odds),]
    ModelDF$term <- factor(ModelDF$term, levels = ModelDF$term[order(ModelDF$odds)])
  }
  
  # Define whether the CI values are greater or less than one
  checkvalue <- function(x, y){
    if (x> 1 & y > 1){return("Positive")} 
    else if (x < 1 & y < 1) {return("Negative")}
    else return("Mixed")
  }
  
  # Apply function to data
  ModelDF$Relationship <- mapply(ModelDF$ci_lower, ModelDF$ci_upper, FUN = checkvalue)
  
  return(ModelDF)
}


LogOddsPlotGraph <- function(OddsTable, PlotTitle, Sort = FALSE){
  # Produces boxplots for estimated values from a regression model
  #
  # Args:
  #   OddsTable: a formatted odds table from the function "LogisticOddsTable"
  #   PlotTitle: the title of the resulting plot
  #   plotColour: the output colour of the boxplots
  #   Sort: reorder the plot by variable fit
  #
  ModelDF <- OddsTable
  
  # Rename Variables
  names <- read.csv("https://raw.githubusercontent.com/mikey-harper/WindStatisticalAnalysis/master/ECOS%202017%20Conference%20Paper/01-Data/Input/VariableDisplayNames.csv")
  ModelDF$term <- names[match(x = ModelDF$term, table = names$Label, nomatch = ""), 2]
  
  Terms <- ModelDF$term
  
  suppressWarnings(library(ggplot2))
  
  # Determine Max vales for axes
  Ymax <- max(c(ModelDF$ci_upper, ModelDF$ci_upper))
  Ymax <- round(Ymax + 0.5, 0)
  Ymin <- min(c(ModelDF$ci_upper, ModelDF$ci_upper))
  Ymin <- round(Ymin - 0.5, 0)
  
  offset = 1 # Defines where barplots start from
  linebreak = 0.1
  MixedColour <- "grey51"
  Negative <- "coral3"
  Positive <- "palegreen4"
  
  # Plot Graph
  plotlogodds <- ggplot(ModelDF, aes(x = term, y = odds - offset, fill = Relationship)) +
    # Add Data
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin = ci_lower - offset, ymax = ci_upper - offset),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)
    ) +
    geom_hline(yintercept = 0) +
    # Labels and axes
    labs(y = "Odds Ratio", x = "") +
    labs(title = PlotTitle) +
    scale_y_continuous(labels = seq(Ymin - offset - 0.1, Ymax + 0.1, linebreak) + offset,
                       breaks = seq(Ymin - offset- 0.1, Ymax + 0.1, linebreak)
    ) +
    scale_x_discrete(limits = rev(Terms)) +
    scale_fill_manual(values = c("Mixed" = MixedColour, "Negative" = Negative, "Positive" = Positive)) + 
    coord_flip() +
    # Theme Settings
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major.y = element_blank()
    )
  
  return(plotlogodds)
}



# Name Matching -----------------------------------------------------------

matchNames <- function(inputNames){
  # Matches variable names with their full descriptive name. Used when plotting or showing
  # results in a table
  #
  names <- read.csv("https://raw.githubusercontent.com/mikey-harper/WindStatisticalAnalysis/master/ECOS%202017%20Conference%20Paper/01-Data/Input/VariableDisplayNames.csv")
  return(names[match(x = inputNames, table = names$Label, nomatch = ""), 2])
}



# Model Internal Validation -----------------------------------------------------

# Name:       Cross Validation Functions
# Date:       16 December 2016  
# Author:     Michael Harper 

# Functions used to internally validate the logistic regression model

ModelAccuracy <- function(dataframe, outcomeVariable, predictorVariables, iterations = 300, foldSize = 0.05, fullstats = FALSE){
  
  reducedDataframe <- ReducedDataframe(dataframe, outcomeVariable, predictorVariables)
  modelAccuracy <- CrossValidation(reducedDataframe, iterations, foldSize, fullstats)
  
  return(modelAccuracy)
}

ReducedDataframe <- function(dataframe, outcomeVariable, inputVariables){
  # Reduces a dataframe to a list of parameters
  
  reducedDataFrame <- dataframe[, c(outcomeVariable, inputVariables)]
  reducedDataFrame <- reducedDataFrame[complete.cases(reducedDataFrame),]
  return(reducedDataFrame)    
}


percent <- function(x, digits = 0, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}


CrossValidation <- function(dataframe, iterations, foldSize, fullstats= FALSE){
  # Function for internally validating a glm model.
  #
  # Args:
  #   dataframe: the dataframe which contains the parameters to be modelled. The
  #              first column must contain the outcome variable
  #   iterations: the number of iterations for the function to run
  #   foldsize: the size of the test dataset expressed as a value between 0 and 1
  #   success: the probability at which the outcome is predicted to succeed
  #   failure: the probabiity at which the outcome is predicted to not succeed
  #
  # Create Blank Results
  fpr <- NULL  # False positive rate
  fnr <- NULL  # False negative rate
  acc <- NULL  # Accuracy
  
  k <- iterations # Number of iterations
  set.seed(123)
  OutcomeVariable <- names(dataframe)[1]
  
  for(i in 1:k)
  {
    # Train-test splitting
    smp_size <- floor((1- foldSize) * nrow(dataframe))
    index <- sample(seq_len(nrow(dataframe)), size=smp_size)
    train <- dataframe[index, ]  # 95% of samples -> fitting
    test <- dataframe[-index, ]  # 5% of samples -> testing
    
    # Fitting
    Formula <- formula(paste(OutcomeVariable, "~.", sep = ""))
    model <- glm(Formula, family = binomial, data = train)
    
    # Predict results
    results_prob <- predict.glm(model, subset(test, select = c(2:ncol(test))), type = 'response')
    
    results <- ifelse(results_prob > 0.5, 1,0)  # User defined cutoff points
    answers <- test[, 1] %in% "Approved" * 1  # Actual answers
    misClasificError <- mean(answers != results)  # Accuracy calculation
    acc[i] <- 1 - misClasificError  # Collecting results
    
    # Confusion matrix
    cm <- caret::confusionMatrix(data = results, reference = answers)
    fpr[i] <- cm$table[2] / (nrow(dataframe) - smp_size)
    fnr[i] <- cm$table[3] / (nrow(dataframe) - smp_size)
  }
  
  # Return Results
  results<- list(MeanAccuracy = percent(mean(acc)),
                 MeanFPR = mean(fpr),
                 MeanFNR = mean(fnr),
                 Accuracy  = round(acc, 3),
                 FPR = round(fpr, 3),
                 FNR = round(fnr, 3)
  )
  
  if (fullstats == FALSE){
    return(results$MeanAccuracy)
  } else {
    return(results)
  }
}


# Two Way Frequency Table -------------------------------------------------------------------

TwoWayFrequency <- function(Rows, Columns, SumPercentage, inputDataframe){
  # Creates a 2 way frequency table from a dataframe and calculates the percentage of
  # a specified category
  #
  # Args:
  #   Rows: the variable to counted in the rows
  #   Columns: the variable to be counted in the columns
  #   SumPercentage: the parameter to be summed in the column
  #   inputDataframe: the dataframe containing the row and column parameters
  #
  InputFormula <- paste("~ ",(paste(Rows, Columns, sep = " + ")))
  a <- xtabs( InputFormula , data = inputDataframe) # produces base count table
  
  # Create New columns
  Total <- rowSums(a)
  Percentage <- a[,SumPercentage] / Total
  Percentage <- round(Percentage, 2) * 100 # Convert into percentage
  
  # Merge and format data
  results <- cbind(a, Total, Percentage)
  totalcol <- ncol(a) + 1
  results  <- results[ results[,3] != 0, ] # removre any empty columns
  results <- as.data.frame(results)
  results <- plyr::rename(results, replace = c("Percentage" = paste("Percentage_", SumPercentage, sep="")))
  return(results)
}


# Segmented (Nested) Models -----------------------------------------------------------------------

Segmented_FullOddsPlot <- function(dataset, by, predictors, outcome, Title, linebreak){
  # Combines the stages outlined in the subfunctions below to produce a faceted box plot 
  # for the odds ratios of logistic regression models
  #
  #
  # Segment the dataset by the given category
  SegDatasets <- Segmented_Dataset(dataset, by)
  
  # Build Logistic Regression Models for the segmented models
  SegLogisticModels <- Segmented_LogisticModels(SegDatasets, predictors, outcome)
  
  # Build Odds Tables for the list of logistic regression models
  LogisticModelsOddsTable <- Segmented_OddsTable(SegLogisticModels)
  
  # Plot the Faceted Graph
  FacetedBoxPlot <- Segmented_OddsPlot(LogisticModelsOddsTable, Title, linebreak)
  
  return(FacetedBoxPlot)
}


# ---------------------------------------------------------------------------------------
# Sub Functions: 
# These functions are called by the master functions above or can be used individually
# ---------------------------------------------------------------------------------------

Segmented_Dataset <- function(dataset, by){
  # Creates data subsets for a full dataset based on a parameter within the dataset
  #
  # Args:
  #   dataset: the full datatable to be split
  #   by: the name of the column for the dataset to be split using
  #
  # Returns:
  #   A list of dataframes for each subset
  #
  columntoSplit <- dataset[[by]] 
  variableLevels <- levels(columntoSplit)
  SegmentedDatasets <- NULL # for results
  
  for (i in 1:length(variableLevels)){
    SegmentedDataset <- dataset[columntoSplit == variableLevels[i], ]
    SegmentedDatasets[[variableLevels[i]]] <- SegmentedDataset # Add dataset to results
  }
  return(SegmentedDatasets)
}


Segmented_LogisticModels <- function(SegmentedDatasets, predictors, outcome){
  # Combines the SplitDatasetbyVariable function and "Logstic Model" function to create
  # seperate logistic regression models
  # Args:
  #   dataset: the full dataframe to be split
  #   variableName: the name of the column for the dataset to be split using
  #
  # Returns:
  #   A list of logistic regression models
  #
  LogisticModels <- NULL # Empty dataframe for results
  
  for (i in 1:length(SegmentedDatasets)){
    datasetname <- (names(SegmentedDatasets[i]))
    ModelDataset <- SegmentedDatasets[[datasetname]]
    
    Formula <- formula(paste(outcome, "~ ", paste(predictors, collapse=" + ")))
    GlmModel <- glm(Formula , data = ModelDataset, family = binomial())
    LogisticModels[[datasetname]] <- GlmModel
    
  }
  return(LogisticModels)
}


Segmented_OddsTable <- function(LogisticModelList){
  # Splits the 
  #
  # Args:
  #   dataset: the input dataset to be split
  #   splitVariableName: the name of the parameter which will be used to segment the model
  #   predictorVariableList: the list of parameters to be used within the GLM
  #   outcomeVariable: the binary outcome variable 
  #
  # Returns: a single odds table which contains the statistics for the segmented model
  #
  # Split dataset into segments and build logistic models into a list
  OddsTables <- NULL
  
  for (i in 1:length(LogisticModelList)){
    datasetname <- (names(LogisticModelList[i]))
    ModelDataset <- LogisticModelList[[datasetname]]
    
    # Calculate Log odds
    ModelDF <- LogisticOddsTable(ModelDataset, Sort = FALSE)
    ModelDF$Facet <- datasetname
    OddsTables[[datasetname]] <- ModelDF
  }
  return(OddsTables)
}


Segmented_OddsPlot <- function(OddsTables, PlotTitle, linebreak){
  # Plots a faceted odds ratio plot for a list of segmented odds tables
  # 
  # Args:
  #   OddsTables: a list of odds tables as produced from the function "OddsTableSegmented"
  #
  suppressWarnings(library(ggplot2))
  
  Combined <- NULL # Blank table for results
  
  # Load reference names
  names <- read.csv(file.path(input.data.directory, "Tabular/VariableDisplayNames.csv"))
  
  for (i in 1:length(OddsTables)){ # Loop combines models into a single table
    datasetname <- (names(OddsTables[i])) # Extract name of the submodel
    ModelDataset <- OddsTables[[datasetname]] # Extract the data from the list
    # Rename parameters
    ModelDataset$term <- names[match(x = ModelDataset$term, table = names$Label, nomatch = ""), 2]
    Combined <- rbind(ModelDataset, Combined)
  }
  
  linebrk = linebreak # spacing between markers
  
  # Determine Max vales for axes
  Ymax <- max(c(Combined$ci_upper, Combined$ci_upper))
  Ymax <- round(Ymax + linebrk, 1)
  Ymin <- min(c(Combined$ci_upper, Combined$ci_upper))
  Ymin <- round(Ymin - linebrk, 1)
  
  # Produces axes labels starting from ymin and ending at Ymax.
  # Code structure forces the sequence to always stop at 0
  axisValues <- c( -rev(seq(0, abs(Ymin), linebrk)),  seq(linebrk, Ymax, by = linebrk))
  
  offset <- 1 # Defines where barplots start from
  MixedColour <- "grey51"
  Negative <- "coral3"
  Positive <- "palegreen4"
  
  # Plot Graph
  plotOdds <- ggplot(Combined, aes(x = term, y = odds - offset, fill = Relationship)) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin = ci_lower - offset, ymax = ci_upper - offset),
                  width=.2, position=position_dodge(.9)) +
    labs(y = "Odds Ratio", x = "Term") +
    labs(title = PlotTitle) +
    theme(plot.title = element_text(hjust = 0.5), panel.grid.major.y = element_blank(),
          axis.title.y=element_blank()) +
    scale_y_continuous(labels = axisValues + offset, breaks = axisValues) +
    scale_x_discrete(limits = rev(ModelDataset$term)) +
    scale_fill_manual(values = c("Mixed" = MixedColour, "Negative" = Negative,
                                 "Positive" = Positive)) + 
    geom_hline(yintercept = 0, size = 0.5) +
    facet_grid(. ~ Facet) +
    coord_flip() 
  
  return(plotOdds)
}

