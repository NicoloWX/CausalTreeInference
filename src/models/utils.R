library(Rlab)
library(dplyr)       # Data manipulation (0.8.0.1)
library(fBasics)     # Summary statistics (3042.89)
library(corrplot)    # Correlations (0.84)
library(psych)       # Correlation p-values (1.8.12)
library(grf)         # Generalized random forests (0.10.2)
library(rpart)       # Classification and regression trees, or CART (4.1-13)
library(rpart.plot)  # Plotting trees (3.0.6)
library(treeClust)   # Predicting leaf position for causal trees (1.1-7)
library(car)         # linear hypothesis testing for causal tree (3.0-2)
library(remotes)    # Install packages from github (2.0.1)
library(readr)       # Reading csv files (1.3.1)
library(tidyr)       # Database operations (0.8.3)
library(tibble)      # Modern alternative to data frames (2.1.1)
library(knitr)       # RMarkdown (1.21)
# library(kableExtra)  # Prettier RMarkdown (1.0.1)
library(ggplot2)     # general plotting tool (3.1.0)
library(haven)       # read stata files (2.0.0)
library(aod)         # hypothesis testing (1.3.1)
library(evtree)      # evolutionary learning of globally optimal trees (1.0-7)
library(estimatr)    # simple interface for OLS estimation w/ robust std errors ()
library(Rlab)

library(causalTree)
# remotes::install_github('grf-labs/sufrep') # Uncomment this to install the sufrep package
library(sufrep)

eta_1 <- function(x) {
  return(0.5*x[,1] + x[,2])
}

kappa_1 <- function(x) {
  return(0.5*x[,1])
}

eta_2 <- function(x) {
  return(0.5*rowSums(x[,1:2]) + rowSums(x[,3:6]))
}

kappa_2 <- function(x) {
  res = rep(0, length(x[,1]))
  for (i in 1:2) {
    indicator = x[,i] > 0
    res = res + indicator*x[,i]
  }
  return(res)
}

eta_3 <- function(x) {
  return(0.5*rowSums(x[,1:4]) + rowSums(x[,5:8]))
}

kappa_3 <- function(x) {
  res = rep(0, length(x[,1]))
  for (i in 1:4) {
    indicator = x[,i] > 0
    res = res + indicator*x[,i]
  }
  return(res)
}

DGP <- function(design, large_pop = FALSE) {
  
  # trial1 set-up
  tp = 0.5              # marginal probability
  if (large_pop) {
    n_htr = n_hest = 1000
  } else {
    n_htr = n_hest = 500
  }
  n_atr = 1000          # for adaptive
  n_te = 8000           # test size
  
  if (design == 1) {
    eta = eta_1
    kappa = kappa_1
    K = 2
  } else if (design == 2) {
    eta = eta_2
    kappa = kappa_2
    K = 10
  } else if (design == 3) {
    eta = eta_3
    kappa = kappa_3
    K = 20
  } else {
    stop("design is not available")
  }
  
  # generative model
  err = rnorm(n = n_atr, mean = 0, sd = 0.01)
  w = rbern(n_atr, tp)
  X = matrix(rnorm(n = K * n_atr, mean = 0, sd = 1), nrow = n_atr, ncol = K)
  Y_0 = eta(X) + 0.5*(2*rep(0, n_atr)-1)*kappa(X) + rnorm(n = n_atr, mean = 0, sd = 0.01)
  Y_1 = eta(X) + 0.5*(2*rep(1, n_atr)-1)*kappa(X) + rnorm(n = n_atr, mean = 0, sd = 0.01)
  Y_obs = w * Y_1 + (1 - w) * Y_0
  tau_true = (1-2*w)*(Y_1 - Y_0)
  
  # test data
  err_te = rnorm(n = n_te, mean = 0, sd = 0.01)
  w_te = rbern(n_te, tp)
  X_te = matrix(rnorm(n = K * n_te, mean = 0, sd = 1), nrow = n_te, ncol = K)
  Y_0_te = eta_1(X_te) + 0.5*(2*rep(0, n_te)-1)*kappa_1(X_te) + rnorm(n = n_te, mean = 0, sd = 0.01)
  Y_1_te = eta_1(X_te) + 0.5*(2*rep(1, n_te)-1)*kappa_1(X_te) + rnorm(n = n_te, mean = 0, sd = 0.01)
  Y_te = w_te * Y_1_te + (1 - w_te) * Y_0_te
  df_test = as.data.frame(cbind(X_te, w_te, Y_te))
  
  # split
  df_train = as.data.frame(cbind(X, w, Y_obs))
  split_size <- floor(nrow(df_train) * 0.5)
  split_idx <- sample(nrow(df_train), replace=FALSE, size=split_size)
  
  df_split <- df_train[split_idx,]
  df_est <- df_train[-split_idx,]
  
  columns = colnames(df_split)
  facs = columns[1:(length(columns)-2)]
  fmla_ct <- paste("Y_obs ~", paste(facs, collapse = " + "))
  
  outlist = list(
    dataTrain = df_split,
    dataEst = df_est,
    dataTest = df_test,
    formula = fmla_ct
  )
  return(outlist)
}

# demo = DGP(design = 1)
# library(MASS)
# write.matrix(demo$dataTrain,file="trainDemo.csv")
# write.matrix(demo$dataEst,file="estDemo.csv")
# write.matrix(demo$dataTest,file="testDemo.csv")