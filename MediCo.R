### Loading libraries ----

library(dplyr)
library(ggcorrplot)
library(magrittr)
library(arsenal)
library(MASS)

#' Summarize a data frame.
#' 
#' @param df_var data frame to be processed.
#' @param nums_var columns to select for the  data frame (default is all).
#' @param by_var  indicates variable on which the analysis can be split (default is none).
#' @return a table summarizing descriptive statistics of the data frame.
#' @examples
#' library("gapminder")
#' descriptive_statistic(gapminder,)
descriptive_statistic <- function(df_var, nums_var = '.', by_var = ''){
  mycontrols  <- tableby.control(test=FALSE, total=FALSE,
                                 numeric.test="kwt", cat.test="chisq",
                                 numeric.stats=c("N","Nmiss", "meansd", "medianrange", "q1q3"),
                                 cat.stats=c("countpct"),
                                 stats.labels=list(N='Count', Nmiss = 'Missing Values', meansd='Mean (sd)', medianrange='Median, Min, Max', q1q3='Q1,Q3'))
  
  df_var_temp <- df_var[nums_var,]
  f <- as.formula(paste(by_var, '.', sep = " ~ "))
  tableone <- arsenal::tableby(f, data = df_var_temp, control = mycontrols)
  return(as.data.frame(summary(tableone, title = paste('Descriptive Statistics by', by_var, sep =' ')), text = TRUE))
  }


#TODO : example to be specified
#' Provide a univariate analysis given a data frame an a target variable.
#' 
#' @param df_var data frame to be processed.
#' @param by_var  indicates variable on which the univariate analysis will be done.
#' @return a table summarizing univariate analysis of the data frame with coefficient, sdev, t-stat, p-value, odds ratio and its 95% CI.
#' @examples
#' library("gapminder")
#' univariate_analysis(gapminder,)
univariate_analysis <- function(df_var, by_var){
  for (col in colnames(df_var)){
    f <- as.formula(paste(by_var, col, sep = " ~ "))
    model1 <- glm(f, data = df_var, family = 'binomial')
    model1_sum <- summary(model1)
    table_model1 <- as.data.frame(exp(cbind("Odds ratio" = coef(model1), confint.default(model1, level = 0.95))))
    table_model2 <- as.data.frame(model1_sum$coefficient)
    table_model1 <- cbind(table_model1, table_model2)
    table_model1$variable <- col
    df_results <- rbind(df_results, table_model1)
  }
  df_results$rownames = rownames(df_results)
  df_results <- df_results[!grepl("(Intercept)", df_results$rownames),]
  df_results %<>%
    dplyr::mutate(flag = ifelse(`97.5 %`>1 & `2.5 %` <1, 0,1))
  return(df_results[,c('variable','rownames','Estimate','Std. Error','z value','Pr(>|z|)','Odds ratio','2.5 %','97.5 %','flag')])
}


#' Provide a multivariate analysis given a data frame, a list specifying explanatory variables to do the regression and a target variable.
#' 
#' @param df_var data frame to be processed.
#' @param x_variables a list indicating the variables for multivariate analysis (can be flagged from univariate analysis).
#' @param by_var  indicates variable on which the multivariate analysis will be done.
#' @return a table summarizing univariate analysis of the data frame with coefficient, sdev, t-stat, p-value, odds ratio and its 95% CI.
#' @examples
#' library("gapminder")
#' univariate_analysis(gapminder,)
multivariate_analysis_model <- function(df_var, by_var, x_variables){
  f <- as.formula(paste(by_var, paste(x_variables, collapse = " + "), sep = " ~ "))
  model1 <- glm(f, data = df_var, family = 'binomial')
  stepw <- step(model,direction = "both")
  return(stepw)
}

#' Provides the coefficients estimates and statistics for a model (multivariate_analysis_model output) 
#' 
#' @param df_var data frame to be processed.
#' @param model model to be processed.
#' @return a table summarizing multivariate analysis of the data frame with coefficient, sd error, z value, p-value, odds ratio and its 95% CI.
#' @examples
#' library("gapminder")
#' univariate_analysis(gapminder,)
multivariate_analysis_summary <- function(df_var, model){
  stepw_summary <- summary(model)
  table_model1 <- as.data.frame(stepw_summary$coefficients)
  table_model2 <- as.data.frame(exp(cbind("Odds ratio" = coef(model), confint.default(stepw, level = 0.95))))
  table_model1 <- cbind(table_model1, table_model2)
  return(table_model1)
}

#' Provides the ROC curve for a model (multivariate_analysis_model output) 
#' 
#' @param df_var data frame to be processed.
#' @param model model to be processed.
#' @return a data frame with one column with different thresholds tested, an other column with sensitivity and an other with specificity
#' @examples
#' library("gapminder")
#' univariate_analysis(gapminder,)
multivariate_analysis_ROC <- function(df_var, model){
  threshold_list=seq(0, 1, by=0.01)
  table_roc <- data.frame(Threshold=numeric(),
                          specificity = numeric(), 
                          sensitivity = numeric()) 
  for (threshold in threshold_list){
    print(threshold)
    predicted_values<-ifelse(predict(model,type="response")>threshold,1,0)
    actual_values<-model$y
    TN <- sum(predicted_values*actual_values) #True negative
    TP <- sum((1-predicted_values)*(1-actual_values)) #True positives
    FN <- sum(predicted_values*(1-actual_values)) #False negatives
    FP <- sum((1-predicted_values)*actual_values) #False positives
    speci <- TN / (TN + FP)
    sensi <- TP / (TP + FN)
    table_roc <- rbind(table_roc,c(threshold, sensi, speci))
    colnames(table_roc) <- c('Threshold', 'Sensitivity', 'Specificity')
  }
  
  return(table_roc)
}
