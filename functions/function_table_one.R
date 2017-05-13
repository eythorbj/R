
table_one <- function(data, var_cont, var_bin, comparing){

statistic_cont <- c("Mean (SD)")
statistic_bin <- c("Percentage")

# Levels of the comparing variable
levels_comparing <- levels(as.factor(data[[comparing]]))

# Number of observations for each level
n <- c()
for(i in 1:length(levels_comparing)){
n[i] <- nrow(subset(data, data[[comparing]]==levels_comparing[i]))
}

# If there are both continuous and binary variables
if(!missing(var_cont) && !missing(var_bin)){

# Generating an empty table
no_comparing <- length(levels(as.factor(data[[comparing]])))
tab <- data.frame(matrix(ncol=2+no_comparing, nrow=1+length(var_cont)+length(var_bin)))
colnames(tab) <- c("Variable", "Statistic", levels_comparing)
tab$Variable <- c("", var_cont, var_bin)
tab$Statistic <- c("n", rep(statistic_cont, length(var_cont)), rep(statistic_bin, length(var_bin)))

# Filling in with data
  # N
  tab[1,3:ncol(tab)] <- n

  # Mean and SD for continuous variables
  for(i in 1:length(var_cont)){
  for(x in 1:length(levels_comparing)){
    average <- mean(subset(data, data[[comparing]]==levels_comparing[x])[[var_cont[i]]], na.rm=TRUE)
    stdev <- sd(subset(data, data[[comparing]]==levels_comparing[x])[[var_cont[i]]], na.rm=TRUE)
    tab[i+1,2+x] <- paste(round(average, 2), " (", round(stdev, 2), ")", sep="")
  }
}
  # Percentage for binary variables
  for(i in 1:length(var_bin)){
  for(x in 1:length(levels_comparing)){
    percent <- mean(subset(data, data[[comparing]]==levels_comparing[x])[[var_bin[i]]], na.rm=TRUE)*100
    tab[length(var_cont)+i+1,2+x] <- round(percent, 2)
  }
}
return(tab)
}

# If no continuous variables
if(missing(var_cont)){
  
  # Generating an empty table
  no_comparing <- length(levels(as.factor(data[[comparing]])))
  tab <- data.frame(matrix(ncol=2+no_comparing, nrow=1+length(var_bin)))
  colnames(tab) <- c("Variable", "Statistic", levels_comparing)
  tab$Variable <- c("", var_bin)
  tab$Statistic <- c("n", rep(statistic_bin, length(var_bin)))
  
  # Filling in the table
    # N
  tab[1,3:ncol(tab)] <- n
  # Percentage for binary variables (of 1)
  for(i in 1:length(var_bin)){
    for(x in 1:length(levels_comparing)){
      percent <- mean(subset(data, data[[comparing]]==levels_comparing[x])[[var_bin[i]]], na.rm=TRUE)*100
      tab[i+1,2+x] <- round(percent, 2)
    }
  }
  return(tab)
}

# If no binary variables
if(missing(var_bin)){
  
  # Generating an empty table
  no_comparing <- length(levels(as.factor(data[[comparing]])))
  tab <- data.frame(matrix(ncol=2+no_comparing, nrow=1+length(var_cont)))
  colnames(tab) <- c("Variable", "Statistic", levels_comparing)
  tab$Variable <- c("", var_cont)
  tab$Statistic <- c("n", rep(statistic_cont, length(var_cont)))
  
  # Filling in the table
  # N
  tab[1,3:ncol(tab)] <- n
  # Mean and SD for continuous variables
  for(i in 1:length(var_cont)){
    for(x in 1:length(levels_comparing)){
      average <- mean(subset(data, data[[comparing]]==levels_comparing[x])[[var_cont[i]]], na.rm=TRUE)
      stdev <- sd(subset(data, data[[comparing]]==levels_comparing[x])[[var_cont[i]]], na.rm=TRUE)
      tab[i+1,2+x] <- paste(round(average, 2), " (", round(stdev, 2), ")", sep="")
    }
  }
  return(tab)
  }
  }