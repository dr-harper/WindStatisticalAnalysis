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

VariableCluster <- function(PredictorVariables){
  #  Divides a set of numeric variables into disjoint or hierarchical cluster
  # 
  Formula <- formula(paste("Status.Summary ~ ", paste(PredictorVariables, collapse=" + ")))
  ClusterPlot <- plot(Hmisc::varclus(Formula, data = fulldata))
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
  names <- read.csv(file.path(input.data.directory, "Tabular/VariableDisplayNames.csv"))
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



# Logistic Regression Functions -----------------------------------------------------------


