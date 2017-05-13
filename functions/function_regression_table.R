
regression_table <- function(variables, outcome, covariates, model_type, data){

# Generate a "covariate_1 + covariate_2 ..." for the model if covariates are included. If covariates are missing, an unadjusted model is presented.
  
  covars <- ""
  if(!missing(covariates)){
    for(i in 1:length(covariates)){
      covars <- paste(covars, covariates[i], sep="+")
    }}

# For multiple adjusted models.
if(length(variables)>1){  

## For a continuous outcome.
if(model_type=="linear"){
  
  ### Creating an empty table
    tab <- data.frame(matrix(ncol = 5))
    colnames(tab) <- c("Predictors", "N", "Adj. beta", "95% CI", "P")
    if(missing(covariates)){
      colnames(tab)[3] <- "Unadj. beta"
    }

## Filling the table with adjusted results for each variable.
for(i in 1:length(variables)){
  model_text <- paste("lm","(",outcome,"~",variables[i],covars,",data=data",")", sep="")
  model <- eval(parse(text = model_text))
  n <- length(model$residuals)
  predictor <- variables[i] 
  coef <- matrix(summary(model)$coefficients, ncol=4)[2,]
  ci <- matrix(suppressMessages(confint(model)[2,]), ncol=2)
  ci_joined <- data.frame(CI = paste(round(ci[,1], 3), round(ci[,2], 3), sep=", "))
  lm_ci <- data.frame(predictor, n, beta = round(coef[1], 3), ci_joined, P = as.numeric(format(coef[4], digits=2)))
  colnames(lm_ci) <- c("Predictors", "N", "beta", "95% CI", "P")
  lm_ci$P <- as.character(lm_ci$P)
  tab <- rbind(tab, lm_ci)
}
    tab <- tab[-1,]
    rownames(tab) <- NULL
    if(missing(covariates)){
      colnames(tab)[3] <- "Unadj. beta"
    } else {
      colnames(tab)[3] <- "Adj. beta"
    }
    return(tab)
} 

## For a binary outcome (logistic regression).
if(model_type=="logistic"){
  
  tab <- data.frame(matrix(ncol = 5))
  colnames(tab) <- c("Predictors", "N", "OR", "95% CI", "P")
  
  for(i in 1:length(variables)){
    model_text <- paste("glm","(",outcome,"~",variables[i],covars,",data=data, family=binomial",")", sep="")
    model <- eval(parse(text = model_text))
    n <- length(model$residuals)
    predictor <- variables[i] 
    coef <- matrix(summary(model)$coefficients, ncol=4)[2,]
    ci <- matrix(suppressMessages(confint(model)[2,]), ncol=2)
    ci_joined <- data.frame(CI = paste(round(exp(ci[,1]), 3), round(exp(ci[,2]), 3), sep=", "))
    OR_ci <- data.frame(predictor, n, OR = round(exp(coef[1]), 3), ci_joined, P = as.numeric(format(coef[4], digits=2)))
    colnames(OR_ci) <- c("Predictors", "N", "OR", "95% CI", "P")
    OR_ci$P <- as.character(OR_ci$P)
    tab <- rbind(tab, OR_ci)
  }
  tab <- tab[-1,]
  rownames(tab) <- NULL
  if(missing(covariates)){
    colnames(tab)[3] <- "Unadj. OR"
  } else {
    colnames(tab)[3] <- "Adj. OR"
  }
  return(tab)
} 
} else {
  
# If only one variable is named, a single multivariate model is presented, with the variable and all covariates.
  
  ## Continuous.
  if(model_type=="linear"){
    tab <- data.frame(matrix(ncol = 5))
    colnames(tab) <- c("Predictors", "N", "beta", "95% CI", "P")
    model_text <- paste("lm","(",outcome,"~",variables,covars,",data=data",")", sep="")
    model <- eval(parse(text = model_text))
    n <- length(model$residuals)
    predictors <- row.names(summary(model)$coefficients)[-1]
    coef <- matrix(summary(model)$coefficients, ncol=4)
    ci <- matrix(suppressMessages(confint(model)[-1,]), ncol=2)
    ci_joined <- data.frame(CI = paste(round(ci[,1], 3), round(ci[,2], 3), sep=", "))
    lm_ci <- data.frame(predictors, n, beta = round(coef[-1,1], 3), ci_joined, P = as.numeric(format(coef[-1,4], digits=2))) 
    colnames(lm_ci) <- c("Predictors", "N", "beta", "95% CI", "P")
    lm_ci$P <- as.character(lm_ci$P)
    rownames(lm_ci) <- NULL
    tab <- lm_ci
    if(missing(covariates)){
      colnames(tab)[3] <- "Unadj. beta"
    } else {
      colnames(tab)[3] <- "Adj. beta"
    }
    return(tab)}
    
  ## Binary.
  if(model_type=="logistic"){
    tab <- data.frame(matrix(ncol = 5))
    colnames(tab) <- c("Predictors", "N", "OR", "95% CI", "P")
    model_text <- paste("glm","(",outcome,"~",variables,covars,",data=data, family=binomial",")", sep="")
    model <- eval(parse(text = model_text))
    n <- length(model$residuals)
    predictors <- row.names(summary(model)$coefficients)[-1]
    coef <- matrix(summary(model)$coefficients, ncol=4)
    ci <- matrix(exp(suppressMessages(confint(model)[-1,])), ncol=2)
    ci_joined <- data.frame(CI = paste(round(ci[,1], 3), round(ci[,2], 3), sep=", "))
    OR_ci <- data.frame(predictors, n, OR = round(exp(coef[-1,1]), 3), ci_joined, P = as.numeric(format(coef[-1,4], digits=2))) 
    colnames(OR_ci) <- c("Predictors", "N", "OR", "95% CI", "P")
    OR_ci$P <- as.character(OR_ci$P)
    rownames(OR_ci) <- NULL
    tab <- OR_ci
    if(missing(covariates)){
      colnames(tab)[3] <- "Unadj. OR"
    } else {
      colnames(tab)[3] <- "Adj. OR"
    }
    return(OR_ci)
  }

} # end of else
} # end of function
  