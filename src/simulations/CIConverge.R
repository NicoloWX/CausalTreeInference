# set-up & data
dsgn = 1
nsim = 100
large_pop = FALSE

temp = DGP(design = dsgn, large_pop = large_pop)
dataTrain = temp$dataTrain
dataEst = temp$dataEst
dataTest = temp$dataTest
formula = temp$formula

# honest tree!
ct_unpruned <- honest.causalTree(
  formula=formula, data=dataTrain,
  est_data=dataEst, treatment=dataTrain$w,
  est_treatment=dataEst$w, split.Rule="policy",
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

# num leaves part
num_leaves <- length(unique(tauhat_ct_est))
sim_res_honest[j] = num_leaves

# save the trees
honest_tree_unpruned = ct_unpruned
honest_tree_pruned = ct_pruned

# adaptive tree!
ct_unpruned <- causalTree(
  formula=formula, data=dataTrain, 
  treatment=dataTrain$w, split.Rule="policy",
  cv.option="CT", cp=0,
  split.Honest=FALSE, cv.Honest=TRUE,
  minsize=25, HonestSampleSize=nrow(dataEst), 
  split.alpha = .5)

# prune the tree
ct_cptable <- as.data.frame(ct_unpruned$cptable)
selected_cp <- which.min(ct_cptable$xerror)
optim_cp_ct <- ct_cptable[selected_cp, "CP"]
ct_pruned <- prune(tree=ct_unpruned, cp=optim_cp_ct)
tauhat_ct_est <- predict(ct_pruned, newdata=dataEst)

# save the trees
adaptive_tree_unpruned = ct_unpruned
adaptive_tree_pruned = ct_pruned

# get coefs and se
dataTrain$leaves <- predict(adaptive_tree_pruned, newdata=dataTrain, type = 'vector')
dataEst$leaves <- predict(adaptive_tree_pruned, newdata=dataEst, type = 'vector')
dataTest$leaves <- predict(adaptive_tree_pruned, newdata=dataTest, type = 'vector')

dataTrain$leavesf <- factor(round(dataTrain$leaves,4))
dataEst$leavesf <- factor(round(dataEst$leaves,4))
dataTest$leavesf <- factor(round(dataTest$leaves,4))

# run regressions with indicators for the leaves interacted with the treatment indicator
if (length(levels(dataTrain$leavesf)) == 1){
  
  modelTrain <- lm(Y_obs~w, data=dataTrain)
  modelEst <- lm(Y_obs~w, data=dataEst)
  modelTest <- lm(Y_te~w_te, data=dataTest)
  
  summary(modelTrain)
  summary(modelEst)
  summary(modelTest)
  
} else{
  
  modelTrain <- lm(Y_obs~-1+leavesf+leavesf*w-w, data=dataTrain)
  modelEst <- lm(Y_obs~-1+leavesf+leavesf*w-w, data=dataEst)
  modelTest <- lm(Y_te~-1+leavesf+leavesf*w_te-w_te, data=dataTest)
  
  print("Leaf names match estimated treatment effects on training set")
  print(summary(modelTrain))
  print("Estimated treatment effects on estimation set typically more moderate than training set")
  print(summary(modelEst))
  print("Estimated treatment effects on test set typically more moderate than training set")
  print(summary(modelTest))
  
  
  # extract the coefficient vectors which are the leaf treatment effects
  coefnumh <- length(coef(modelEst))
  coefnuml <- length(coef(modelEst))/2 + 1
  
  Train.coeftr <- coef(modelTrain)[coefnuml:coefnumh]
  Est.coeftr <- coef(modelEst)[coefnuml:coefnumh]
  Test.coeftr <- coef(modelTest)[coefnuml:coefnumh]
  
  Train.se <- summary(modelTrain)$coefficients[,2][coefnuml:coefnumh]
  Est.se <- summary(modelEst)$coefficients[,2][coefnuml:coefnumh]
  Test.se <- summary(modelTest)$coefficients[,2][coefnuml:coefnumh]
  
  # calculate leaf probabilities
  
  leafprobEst <- tapply(dataEst$Y_obs,list(dataEst$leavesf),length)
  leafprobTrain <- tapply(dataTrain$Y_obs,list(dataTrain$leavesf),length)
  leafprobTest <- tapply(dataTest$Y_te,list(dataTest$leavesf),length)
  leafprob <- (leafprobEst + leafprobTrain + leafprobTest)/(nrow(dataEst) + nrow(dataTrain) + 
                                                              nrow(dataTest))
  
  #calculate variance of estimated treatment effects--typically this is higher in the training set, since there is overfitting there
  Train.coefvar <- sum(leafprob * Train.coeftr^2)-(sum(leafprob*Train.coeftr)^2)
  Est.coefvar <- sum(leafprob * Est.coeftr^2)-(sum(leafprob*Est.coeftr)^2)
  Test.coefvar <- sum(leafprob * Test.coeftr^2)-(sum(leafprob*Test.coeftr)^2)
  
  Train.mean = sum(leafprob*Train.coeftr)
  Est.mean = sum(leafprob*Est.coeftr)
  Test.mean = sum(leafprob*Test.coeftr)
  
  print("Variance of estimated treatment effects: Train, Estimation, Test Sets")
  print("Typically train has higher var--more extreme estimates--due to overfitting")
  print(c(Train.coefvar,Est.coefvar,Test.coefvar))
}

zs = qnorm(0.90)  # student-t
Train.ci.low = Train.coeftr - zs * Train.se
Train.ci.high = Train.coeftr + zs * Train.se

# multiply with x to get **prediction interval** (take average)
# Check: for different x see if the y falls into the CI on the test data
# fix one x, -> one test value -> repeat nsim times

# Est.ci.low = Est.coeftr - zs * Est.se
# Est.ci.high = Est.coeftr + zs * Est.se

# Test.ci.low = Test.coeftr - zs * Test.se
# Test.ci.high = Test.coeftr + zs * Test.se

# dishonest case!
CI_count = 0
res = rep(0, nsim)
for (i in 1:nsim) {
  # calculate the coverage prob based on the CI
  testD = DGP(design = dsgn, large_pop = large_pop)
  nTest = testD$dataTrain
  formula = testD$formula
  nTest$leaves <- predict(adaptive_tree_pruned, newdata=nTest, type = 'vector')
  nTest$leavesf <- factor(round(nTest$leaves,4))
  
  if (length(levels(dataTrain$leavesf)) == 1){
    nModel <- lm(Y_te~w_te, data=nTest)
  } else {
    nModel <- lm(Y_obs~-1+leavesf+leavesf*w-w, data=nTest)
    
    # extract the coefficient vectors which are the leaf treatment effects
    test_coef <- coef(nModel)[coefnuml:coefnumh]
  }
  
  falls = (test_coef < Train.ci.high)&(test_coef > Train.ci.low)
  if (all(falls)) {
    CI_count = CI_count + 1
  }
  res[i] = sum(falls)
}
CI_count / nsim
hist(res)


# honest case
# get coefs and se
dataTrain$leaves <- predict(honest_tree_pruned, newdata=dataTrain, type = 'vector')
dataEst$leaves <- predict(honest_tree_pruned, newdata=dataEst, type = 'vector')
dataTest$leaves <- predict(honest_tree_pruned, newdata=dataTest, type = 'vector')

dataTrain$leavesf <- factor(round(dataTrain$leaves,4))
dataEst$leavesf <- factor(round(dataEst$leaves,4))
dataTest$leavesf <- factor(round(dataTest$leaves,4))

# run regressions with indicators for the leaves interacted with the treatment indicator
if (length(levels(dataTrain$leavesf)) == 1){
  
  modelTrain <- lm(Y_obs~w, data=dataTrain)
  modelEst <- lm(Y_obs~w, data=dataEst)
  modelTest <- lm(Y_te~w_te, data=dataTest)
  
  summary(modelTrain)
  summary(modelEst)
  summary(modelTest)
  
} else{
  
  modelTrain <- lm(Y_obs~-1+leavesf+leavesf*w-w, data=dataTrain)
  modelEst <- lm(Y_obs~-1+leavesf+leavesf*w-w, data=dataEst)
  modelTest <- lm(Y_te~-1+leavesf+leavesf*w_te-w_te, data=dataTest)
  
  print("Leaf names match estimated treatment effects on training set")
  print(summary(modelTrain))
  print("Estimated treatment effects on estimation set typically more moderate than training set")
  print(summary(modelEst))
  print("Estimated treatment effects on test set typically more moderate than training set")
  print(summary(modelTest))
  
  
  # extract the coefficient vectors which are the leaf treatment effects
  coefnumh <- length(coef(modelEst))
  coefnuml <- length(coef(modelEst))/2 + 1
  
  Train.coeftr <- coef(modelTrain)[coefnuml:coefnumh]
  Est.coeftr <- coef(modelEst)[coefnuml:coefnumh]
  Test.coeftr <- coef(modelTest)[coefnuml:coefnumh]
  
  Train.se <- summary(modelTrain)$coefficients[,2][coefnuml:coefnumh]
  Est.se <- summary(modelEst)$coefficients[,2][coefnuml:coefnumh]
  Test.se <- summary(modelTest)$coefficients[,2][coefnuml:coefnumh]
  
  # calculate leaf probabilities
  
  leafprobEst <- tapply(dataEst$Y_obs,list(dataEst$leavesf),length)
  leafprobTrain <- tapply(dataTrain$Y_obs,list(dataTrain$leavesf),length)
  leafprobTest <- tapply(dataTest$Y_te,list(dataTest$leavesf),length)
  leafprob <- (leafprobEst + leafprobTrain + leafprobTest)/(nrow(dataEst) + nrow(dataTrain) + 
                                                              nrow(dataTest))
  
  #calculate variance of estimated treatment effects--typically this is higher in the training set, since there is overfitting there
  Train.coefvar <- sum(leafprob * Train.coeftr^2)-(sum(leafprob*Train.coeftr)^2)
  Est.coefvar <- sum(leafprob * Est.coeftr^2)-(sum(leafprob*Est.coeftr)^2)
  Test.coefvar <- sum(leafprob * Test.coeftr^2)-(sum(leafprob*Test.coeftr)^2)
  
  Train.mean = sum(leafprob*Train.coeftr)
  Est.mean = sum(leafprob*Est.coeftr)
  Test.mean = sum(leafprob*Test.coeftr)
  
  print("Variance of estimated treatment effects: Train, Estimation, Test Sets")
  print("Typically train has higher var--more extreme estimates--due to overfitting")
  print(c(Train.coefvar,Est.coefvar,Test.coefvar))
}

zs = qnorm(0.90)
Train.ci.low = Train.coeftr - zs * Train.se
Train.ci.high = Train.coeftr + zs * Train.se

Est.ci.low = Est.coeftr - zs * Est.se
Est.ci.high = Est.coeftr + zs * Est.se

Test.ci.low = Test.coeftr - zs * Test.se
Test.ci.high = Test.coeftr + zs * Test.se

# dishonest case!
CI_count = 0
for (i in 1:nsim) {
  # calculate the coverage prob based on the CI
  testD = DGP(design = dsgn, large_pop = large_pop)
  nTest = testD$dataTrain
  formula = testD$formula
  nTest$leaves <- predict(honest_tree_pruned, newdata=nTest, type = 'vector')
  nTest$leavesf <- factor(round(nTest$leaves,4))
  
  if (length(levels(dataTrain$leavesf)) == 1){
    nModel <- lm(Y_te~w_te, data=nTest)
  } else {
    nModel <- lm(Y_obs~-1+leavesf+leavesf*w-w, data=nTest)
    
    # extract the coefficient vectors which are the leaf treatment effects
    test_coef <- coef(nModel)[coefnuml:coefnumh]
  }
  
  falls = (test_coef < Train.ci.high)&(test_coef > Train.ci.low)
  if (all(falls)) {
    CI_count = CI_count + 1
  }
}

CI_count / nsim