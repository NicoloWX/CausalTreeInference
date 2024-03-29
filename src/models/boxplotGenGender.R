males <- read.csv("C:/Users/liuhn/Desktop/Data180/males30.csv")
females <- read.csv("C:/Users/liuhn/Desktop/Data180/females30.csv")

num_trees = 200

generate_v_strings <- function(n) {
  return(paste("V", 1:n, sep=""))
}

generate_v_string_sum <- function(n) {
  v_strings <- paste("V", 1:n, sep="")
  return(paste(v_strings, collapse=" + "))
}

d = males  # change to 30, 40, 50

colnames(d) = c(generate_v_strings(dim(d)[2]-2), 'w', 'Y_obs')
# sample w/o replacement
train_size = 50000
test_size = 5000
dataTrain = sample_n(d, train_size, replace = FALSE)  # 
clean1 <- anti_join(d, dataTrain)
dataEst = sample_n(clean1, train_size, replace = FALSE)
clean2 <- anti_join(clean1, dataEst)
dataTest = sample_n(clean2, test_size, replace = FALSE)
formula = paste('Y_obs ~', generate_v_string_sum(dim(d)[2]-2))


cf <- causal_forest(
  X = select(dataTrain, -c('w', 'Y_obs')),
  Y = dataTrain$Y_obs,
  W = dataTrain$w,
  num.trees=num_trees)

test_pred <- predict(cf, newdata=select(dataTest, -c('w', 'Y_obs')), estimate.variance=TRUE)
tauhat_cf_test1 <- test_pred$predictions

tg = subset(dataTest, w == 1)
ug = subset(dataTest, w == 0)

tmdl = lm(formula = formula, tg)
utmdl = lm(formula = formula, ug)
wt = predict(tmdl, dataTest)
ut = predict(utmdl, dataTest)
Y_star1 = wt - ut

d = females  # change to 30, 40, 50

colnames(d) = c(generate_v_strings(dim(d)[2]-2), 'w', 'Y_obs')
# sample w/o replacement
train_size = 50000
test_size = 5000
dataTrain = sample_n(d, train_size, replace = FALSE)  # 
clean1 <- anti_join(d, dataTrain)
dataEst = sample_n(clean1, train_size, replace = FALSE)
clean2 <- anti_join(clean1, dataEst)
dataTest = sample_n(clean2, test_size, replace = FALSE)
formula = paste('Y_obs ~', generate_v_string_sum(dim(d)[2]-2))

cf <- causal_forest(
  X = select(dataTrain, -c('w', 'Y_obs')),
  Y = dataTrain$Y_obs,
  W = dataTrain$w,
  num.trees=num_trees)

test_pred <- predict(cf, newdata=select(dataTest, -c('w', 'Y_obs')), estimate.variance=TRUE)
tauhat_cf_test2 <- test_pred$predictions

tg = subset(dataTest, w == 1)
ug = subset(dataTest, w == 0)

tmdl = lm(formula = formula, tg)
utmdl = lm(formula = formula, ug)
wt = predict(tmdl, dataTest)
ut = predict(utmdl, dataTest)
Y_star2 = wt - ut

mse <- data.frame(
  male_CATE = tauhat_cf_test1,
  female_CATE = tauhat_cf_test2
)

boxplot(mse, ylab = "CATE", main='CATE by Gender (forest w/ 100 trees)')
summary(mse)
apply(mse, 2, sd)

