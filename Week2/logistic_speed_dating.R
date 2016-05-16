####################################################################################################

library("dplyr")
library("ggplot2")
library("glmnet")
library("corrplot")
library("zoo")
install.packages("pROC")

library("pROC")

#####################################################################################################


######################## predict gender using activities ###############################


df = read.csv("./Week2/speeddating-aggregated.csv")

head(df)

response = data.matrix(dplyr::select(df, gender))

predictors =  data.matrix(dplyr::select(df, sports:yoga))



response_and_predictors_df = data.frame(predictors, response)

mdl = glm(gender ~ . , family = "binomial", response_and_predictors_df)

predictions = predict(mdl, response_and_predictors_df)

roc_obj = pROC::roc(response[,1], predictions)

coef(mdl)

plot(roc_obj)   #plots ROC curve

curr_auc = auc(roc_obj)





