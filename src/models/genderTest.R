# set-up
# source("util_funcs.R")

males <- read.csv("C:/Users/liuhn/Desktop/Data180/males30.csv")
females <- read.csv("C:/Users/liuhn/Desktop/Data180/females30.csv")

generate_v_strings <- function(n) {
  return(paste("V", 1:n, sep=""))
}

generate_v_string_sum <- function(n) {
  v_strings <- paste("V", 1:n, sep="")
  return(paste(v_strings, collapse=" + "))
}

d = females  # change to 30, 40, 50

colnames(d) = c(generate_v_strings(dim(d)[2]-2), 'w', 'Y_obs')
# get first 5k for now
dataTrain = d[1:5000,]
dataEst = d[5001:10000,]
dataTest = d[10000:12000,]
formula = paste('Y_obs ~', generate_v_string_sum(dim(d)[2]-2))

# honest tree!
ct_unpruned <- honest.causalTree(
  formula=formula, data=dataTrain,
  est_data=dataEst, treatment=dataTrain$w,
  est_treatment=dataEst$w, 
  split.Rule="CT",
  cv.option="CT", 
  cp=0,
  split.Honest=TRUE, 
  cv.Honest=TRUE,
  minsize=25, HonestSampleSize=nrow(dataEst),
  #split.alpha = 1
)

# prune the tree
ct_cptable <- as.data.frame(ct_unpruned$cptable)
selected_cp <- which.min(ct_cptable$xerror)
optim_cp_ct <- ct_cptable[selected_cp, "CP"]
ct_pruned <- prune(tree=ct_unpruned, cp=optim_cp_ct)
tauhat_ct_est <- predict(ct_pruned, newdata=dataEst)

# num leaves part
num_leaves <- length(unique(tauhat_ct_est))
# sim_res_honest[j] = num_leaves

rpart.plot(
  x=ct_pruned,        # Pruned tree
  type=3,             # Draw separate split labels for the left and right directions
  fallen=TRUE,        # Position the leaf nodes at the bottom of the graph
  leaf.round=1,       # Rounding of the corners of the leaf node boxes
  extra=100,          # Display the percentage of observations in the node
  branch=.1,          # Shape of the branch lines
  box.palette="RdBu") # Palette for coloring the node

# get CATE
dataEst$leaf <- factor(tauhat_ct_est, labels = seq(num_leaves))

# Run the regression
ols_ct <- lm_robust(Y_obs ~ 0 + leaf + w:leaf, data=dataEst)
ols_ct_summary <- summary(ols_ct)
te_summary <- coef(ols_ct_summary)[(num_leaves+1):(2*num_leaves), c("Estimate", "Std. Error")]

# 3-D viz
set.seed(417)
library(plotly)
plot_ly(x=data_90[1:5000,]$V3, y=data_90[1:5000,]$w, z=data_90[1:5000,]$Y_obs, type="scatter3d", mode="markers", color=temp, size = 40)
