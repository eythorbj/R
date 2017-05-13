quantile_analysis <- function(variable, n_quantiles, method="cut2", outcome, model_type, covariates, data){

# Generating quantiles
  # method="ntile" uses dplyr::ntile which gives priority to equal numbers of observations in each quantile, ignoring the actual value, so the same value may exist in adjacent quantiles.
  if(method == "ntile"){
    suppressMessages(require(dplyr))
  names(data)[names(data) == variable] <- "temp"
  data %>% mutate(quantile = ntile(temp, n = n_quantiles)) -> data
  names(data)[names(data) == "temp"] <- variable
}
  # method="cut2" uses Hmisc::cut2 which assigns into quantiles strictly based on cut-off points, where each quantile contains a unique range of values. This may lead to differing number of observations in the quantiles, depending on the distribution of the data.
if(method == "cut2"){
  suppressMessages(require(Hmisc))
  data %>% mutate(quantile = as.numeric(cut2(x = data[[variable]], g = n_quantiles))) -> data
}
  
# Generating table comparing quantiles
  
  # Empty table
  tab <- data.frame(matrix(ncol=7, nrow=n_quantiles))
  tab[,1] <- 1:n_quantiles
  tab[1,4:7] <- c("ref","","","")
  
  # Number per quantile
  for(i in 1:n_quantiles){
    tab[i,2] <- nrow(subset(data, quantile==i))
  }
  
  # Preparing covariates for the model
  covars <- ""
  if(!missing(covariates)){
    for(i in 1:length(covariates)){
      covars <- paste(covars, covariates[i], sep="+")
    }}
  
  # If outcome is continuous, model_type = "linear".
if(model_type=="linear"){
    colnames(tab) <- c("Quantile", "N", "Mean", "Adj. difference", "CI_lw", "CI_hi", "P")
    if(missing(covariates)){
      colnames(tab)[4] <- "Unadj. difference"
    }
      # Mean for each quantile
    av <- c()
    for(i in 1:n_quantiles){
      av[i] <- mean(subset(data, quantile==i)[[outcome]], na.rm=TRUE)
    }
    tab[,3] <- round(av, 3)
    
    # Difference between quantiles, referenced to Q1
    for(i in 2:n_quantiles){
    model_text <- paste("lm","(",outcome,"~","(quantile==",i,")",covars,",data=data,","subset=quantile%in%c(1,", i,"))", sep="")
    model <- eval(parse(text = model_text))
    coef <- matrix(summary(model)$coefficients, ncol=4)[2,]
    ci <- matrix(suppressMessages(confint(model)[2,]), ncol=2)
    lm_ci <- data.frame(beta = round(coef[1], 3), ci_lw = round(ci[,1], 3), ci_hi = round(ci[,2], 3), P = as.numeric(format(coef[4], digits=2)))
    lm_ci$P <- as.character(lm_ci$P)
    tab[i,4:7] <- lm_ci[1,]
    } # End for loop
} # End if
    
  
if(model_type=="logistic"){
    colnames(tab) <- c("Quantile", "N", "Prop.", "Adj. OR", "CI_lw", "CI_hi", "P")
    if(missing(covariates)){
      colnames(tab)[4] <- "Unadj. OR"
    }
    # Proportion for each quantile
    prop <- c()
    for(i in 1:n_quantiles){
      prop[i] <- mean(subset(data, quantile==i)[[outcome]], na.rm=TRUE)*100
    }
    tab[,3] <- round(prop, 1)
    
    # Difference between quantiles, referenced to Q1
    for(i in 2:n_quantiles){
      model_text <- paste("glm","(",outcome,"==1 ~","(quantile==",i,")",covars,",data=data, family=binomial,","subset=quantile%in%c(1,", i,"))", sep="")
      model <- eval(parse(text = model_text))
      coef <- matrix(summary(model)$coefficients, ncol=4)[2,]
      ci <- matrix(suppressMessages(confint(model)[2,]), ncol=2)
      OR_ci <- data.frame(OR = round(exp(coef[1]), 3), ci_lw = round(exp(ci[,1]), 3), ci_hi = round(exp(ci[,2]), 3), P = as.numeric(format(coef[4], digits=2)))
      OR_ci$P <- as.character(OR_ci$P)
      tab[i,4:7] <- OR_ci[1,]
    } # End for loop
  } # End if
  return(tab)

} # end function
