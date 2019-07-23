install.packages("here")
library(here)
install.packages("shapper")
library("shapper")
install_shap()
TEDS2016_issueset <- read.csv("https://raw.githubusercontent.com/kho7/issuevoting/master/TEDS2016_issueset.csv")
teds2016=TEDS2016_issueset
teds2016$votetsai_nm=factor(teds2016$votetsai_nm)
teds2016$female=factor(teds2016$female)
teds2016 <- na.omit(teds2016)
# teds2016sm=c("votetsai_nm","age", "unify", "indep","income_nm", "taiwanese","female","south","whitecollar","dadminnan","dadmainland","econworse", "inequality","pubwelf")
teds2016sm=c("votetsai_nm","age","kmt","dpp","npp", "statusquo","income_nm", "taiwanese","female","south","whitecollar","dadminnan","dadmainland","econworse", "inequality","pubwelf")
summary(teds2016sm)
vote2016 <- teds2016[teds2016sm]
teds2016
install.packages("randomForest")
library(randomForest)

set.seed(123)
model_rfdpp <- randomForest(votetsai_nm ~ .  , data = vote2016)
model_rfdpp
new_voter <- data.frame(
  age = 30,
  female=1,
  unify=0,
  indep=1,
  kmt=0,
  dpp=1,
  npp=0,
  statusquo=0,
  income_nm=7,
  taiwanese=1,
  south = 1,
  whitecollar=1,
  dadminnan=1,
  dadmainland=0,
  econworse = 1,
  inequality=1,
  pubwelf=0)
#  female = factor("C", levels = c("1","0")))
predict(model_rfdpp, new_voter, type = "prob")
install.packages("DALEX")
library(DALEX)
install_dependencies()
devtools::install_github("rstudio/reticulate")
exp_rf <- explain(model_rf, data = vote2016[,-1])
exp_rf
library("shapper")
ive_rf <- shap(exp_rf, new_observation = new_voter)
ive_rf
plot(ive_rf, show_predicted = FALSE)
plot(ive_svm)
library("e1071")
model_svm <- svm(votetsai_nm~. , data = vote2016, probability = TRUE)
model_svm
attr(predict(model_svm, newdata = new_voter, probability = TRUE), "probabilities")
exp_svm <- explain(model_svm, data = vote2016[,-1])
ive_svm <- shap(exp_svm, new_voter)
plot(ive_rf, ive_svm)
plot(ive_rf, show_predicted = FALSE)
