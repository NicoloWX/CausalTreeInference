# set-up
source("util_funcs.R")
dsgn = 3
nsim = 100
large_pop = FALSE

# leaves trials
sim_res_honest = rep(0, nsim)
sim_res_adaptive = rep(0, nsim)
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
  sim_res_honest[j] = num_leaves
}
print('mean number of leaves:')
mean(sim_res_honest)
print('ratio of MSE to CT-H:')