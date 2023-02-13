# set-up
source("src/models/utils.R")
args = commandArgs(trailingOnly=TRUE)
dsgn = length(integer(args))
nsim = 100
large_pop = FALSE

# leaves trials
sim_res_honest = rep(0, nsim)
mses=rep(0,nsim)
mses_h=rep(0,nsim)
for (j in 1:nsim) {
  temp = DGP(design = dsgn, large_pop = large_pop)
  dataTrain = temp$dataTrain
  dataEst = temp$dataEst
  dataTest = temp$dataTest
  formula = temp$formula
  # honest tree!
  ct_unpruned <- honest.causalTree(
    formula=formula, data=dataTrain,
    est_data=dataEst, treatment=dataTrain$w,
    est_treatment=dataEst$w, 
    split.Rule="TOT",
    cv.option="TOT", 
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
  sim_res_honest[j] = num_leaves
  #mses[j]=sum((tauhat_ct_est-(temp$dataTest$y1-df_test$y0))^2)/8000
  
  ct_h <- honest.causalTree(
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
  ct_cptable2 <- as.data.frame(ct_h$cptable)
  selected_cp2 <- which.min(ct_cptable2$xerror)
  optim_cp_ct2 <- ct_cptable2[selected_cp2, "CP"]
  ct_pruned2 <- prune(tree=ct_h, cp=optim_cp_ct2)
  tauhat_ct_est2 <- predict(ct_pruned2, newdata=dataEst)
  
}
print('mean number of leaves:')
mean(sim_res_honest)