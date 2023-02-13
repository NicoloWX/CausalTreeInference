# setup & data
dsgn = 1
nsim = 100
large_pop = TRUE

res = rep(0, nsim)
for (i in 1:nsim) {
  temp = DGP(design = dsgn, large_pop = large_pop)
  dataTrain = temp$dataTrain
  dataEst = temp$dataEst
  dataTest = temp$dataTest
  formula = temp$formula
  
  # fit the model
  # honest tree!
  ct_unpruned <- honest.causalTree(
    formula=formula, data=dataTrain,
    est_data=dataEst, treatment=dataTrain$w,
    est_treatment=dataEst$w, split.Rule="CT",
    cv.option="CT", cp=0,
    split.Honest=TRUE, cv.Honest=TRUE,
    minsize=25, HonestSampleSize=nrow(dataEst),
    split.alpha = .5)
  
  # prune the tree
  ct_cptable <- as.data.frame(ct_unpruned$cptable)
  selected_cp <- which.min(ct_cptable$xerror)
  optim_cp_ct <- ct_cptable[selected_cp, "CP"]
  ct_pruned <- prune(tree=ct_unpruned, cp=optim_cp_ct)
  tauhat_ct_est <- predict(ct_pruned, newdata=dataEst)
  
  # save the trees
  honest_ct_tree_unpruned = ct_unpruned
  honest_ct_tree_pruned = ct_pruned
  
  honest_ct_pred = predict(honest_ct_tree_pruned, newdata=dataTest)
  # Infeasible MSE
  MSE_CTH = mean((dataTest$Y_te - honest_ct_pred)^2)
  
  
  # dishonest tree
  ct_unpruned <- honest.causalTree(
    formula=formula, data=dataTrain,
    est_data=dataEst, treatment=dataTrain$w,
    est_treatment=dataEst$w, split.Rule="TOT",
    cv.option="TOT", cp=0,
    split.Honest=TRUE, cv.Honest=TRUE,
    minsize=25, HonestSampleSize=nrow(dataEst),
    split.alpha = .5)
  
  # prune the tree
  ct_cptable <- as.data.frame(ct_unpruned$cptable)
  selected_cp <- which.min(ct_cptable$xerror)
  optim_cp_ct <- ct_cptable[selected_cp, "CP"]
  ct_pruned <- prune(tree=ct_unpruned, cp=optim_cp_ct)
  tauhat_ct_est <- predict(ct_pruned, newdata=dataEst)
  
  # num leaves part
  num_leaves <- length(unique(tauhat_ct_est))
  sim_res_adaptive[j] = num_leaves
  
  # save the trees
  other_tree_unpruned = ct_unpruned
  other_tree_pruned = ct_pruned
  
  # rpart.plot(ct_pruned)
  
  other_pred = predict(other_tree_pruned, newdata=dataTest)
  # Infeasible MSE
  MSE_other = mean((dataTest$Y_te - other_pred)^2)
  
  res[i] = MSE_other / MSE_CTH
}

mean(res)