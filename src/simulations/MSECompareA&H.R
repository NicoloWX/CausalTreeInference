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
  honest_tree_unpruned = ct_unpruned
  honest_tree_pruned = ct_pruned
  
  honest_pred = predict(honest_tree_pruned, newdata=dataTest)
  # Infeasible MSE
  MSE_h = mean((dataTest$Y_te - honest_pred)^2)
  
  
  # dishonest tree
  ct_unpruned <- causalTree(
    formula=formula, data=dataTrain, 
    treatment=dataTrain$w, split.Rule="CT",
    cv.option="CT", cp=0,
    split.Honest=FALSE, cv.Honest=FALSE,
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
  adaptive_tree_unpruned = ct_unpruned
  adaptive_tree_pruned = ct_pruned
  
  rpart.plot(ct_pruned)
  
  ada_pred = predict(adaptive_tree_pruned, newdata=dataTest)
  # Infeasible MSE
  MSE_a = mean((dataTest$Y_te - ada_pred)^2)
  
  res[i] = MSE_a / MSE_h
}

mean(res)