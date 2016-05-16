############# utility functions that allow resampling and validation of a dataset ##############



#returns a boolean vector which indicates which examples are in the training set
train_bool = function(df, training_size)   
{

  train_rownums = sample( c(1:nrow(df)), training_size, replace = FALSE)
  training_mask = rep(FALSE, nrow(df))
  training_mask[train_rownums] = TRUE
  return(training_mask)
  
}  



#selects a fraction of the data, which defualts to 0.1
reduce_data = function(df, frac = 0.1)
{
  reduced_df_rows = sample( c(1:nrow(df)), as.integer(nrow(df)*frac), replace = FALSE)
  mask = rep(FALSE, nrow(df))
  mask[reduced_df_rows] = TRUE
  return(df[mask,])
}


#trains a neural net on a training subset, tests it on the rest of the data, returns R^2. Parametric with respect to formula and hidden
train_test_nn = function(df, in_formula, in_hidden, train_fraction = 0.8, in_threshold = 0.01)
{
  tr_mask = train_bool(df,nrow(df)*train_fraction)
  
  tr_data = df[tr_mask,]
  test_data = df[!tr_mask,]
  
  trained_net = neuralnet(formula = in_formula, data = tr_data, hidden = in_hidden, lifesign = "full", lifesign.step = 100, linear.output = FALSE, threshold = in_threshold)

  predicted_test_values = compute(trained_net,dplyr::select(test_data, -price))$net.result
  predicted_training_values = compute(trained_net,dplyr::select(tr_data, -price))$net.result
  

  actual_test_values = dplyr::select(test_data,price)
  actual_training_values = dplyr::select(tr_data,price)
  

  test_cor = cor(predicted_test_values, actual_test_values)
  
  training_cor = cor(predicted_training_values, actual_training_values)

  return(list("training_R" = training_cor,   "test_R" = test_cor, "trained_net" = trained_net, "tr_mask" = tr_mask, "predicted_test_values" = predicted_test_values, "actual_test_values" = actual_test_values  ))

}



#trains a h2o deep neural net on a training subset, tests it on the rest of the data, returns R^2. Parametric with respect to formula and hidden
train_test_h2o = function(df, hidden, input_dropout_ratio, hidden_dropout_ratios,  l1 = 0, l2 = 0, train_fraction = 0.8, epochs = 10)
{
  tr_mask = train_bool(df,nrow(df)*train_fraction)
  
  tr_data = df[tr_mask,]
  test_data = df[!tr_mask,]
  
  h2o_tr_data = as.h2o(tr_data, destination_frame = "h2o_data")
  h2o_test_input_data = as.h2o(dplyr::select(test_data, -price), destination_frame = "h2o_test_input_data")
  h2o_training_input_data = as.h2o(dplyr::select(tr_data, -price), destination_frame = "h2o_training_input_data")
  
  h2o_model <-   h2o.deeplearning(x = 1:26,  # column numbers for predictors
                                  y = 27,   # column number for label
                                  l1 = l1,
                                  l2 = l2,
                                  training_frame = h2o_tr_data, # data in H2O format
                                  activation = "TanhWithDropout", # 
                                  input_dropout_ratio = 0.1, # % of inputs dropout
                                  hidden_dropout_ratios = hidden_dropout_ratios, # % for nodes dropout
                                  hidden = hidden, # layer structure
                                  epochs = epochs) # max. no. of epochs
  

  h2o_predicted_test = h2o.predict(h2o_model, h2o_test_input_data )
  h2o_predicted_training = h2o.predict(h2o_model, h2o_training_input_data )
  
  predicted_test_values = as.data.frame(h2o_predicted_test)
  predicted_training_values = as.data.frame(h2o_predicted_training)
  
  actual_test_values = dplyr::select(test_data,price)
  actual_training_values = dplyr::select(tr_data,price)
  
  test_cor = cor(predicted_test_values, actual_test_values)
  
  training_cor = cor(predicted_training_values, actual_training_values)
  
  return(list("training_R" = training_cor,   "test_R" = test_cor, "h2o_model" = h2o_model, "tr_mask" = tr_mask, "predicted_test_values" = predicted_test_values, "actual_test_values" = actual_test_values  ))
  
}



train_test_linear = function(df, in_formula, train_fraction = 0.8)
{  
  tr_mask = train_bool(df,nrow(df)*train_fraction)
  
  tr_data = df[tr_mask,]
  test_data = df[!tr_mask,]
  
  
  trained_lm = lm(formula = in_formula, data = tr_data)

  predicted_test_values = predict(trained_lm, dplyr::select(test_data, -price))
  predicted_training_values = predict(trained_lm, dplyr::select(tr_data, -price))
  
  actual_test_values = dplyr::select(test_data,price)
  actual_training_values = dplyr::select(tr_data,price)
  
  test_cor = cor(predicted_test_values, actual_test_values)
  
  training_cor = cor(predicted_training_values, actual_training_values)
  
  return(list("training_R" = training_cor,   "test_R" = test_cor, "trained_lm" = trained_lm, "tr_mask" = tr_mask, "predicted_test_values" = predicted_test_values, "actual_test_values" = actual_test_values  ))
  
}


train_test_forest = function(df,mtry,  train_fraction = 0.8)
{
  tr_mask = train_bool(df,nrow(df)*train_fraction)
  
  tr_data = df[tr_mask,]
  test_data = df[!tr_mask,]

  rf_model = randomForest(price ~ . , data = tr_data, mtry = mtry)
  
  predicted_test_values = predict(rf_model, dplyr::select(test_data, -price) )
  predicted_training_values = predict(rf_model, dplyr::select(tr_data, -price) )  
  
  actual_test_values = dplyr::select(test_data,price)
  actual_training_values = dplyr::select(tr_data,price)
  
  test_cor = cor(predicted_test_values, actual_test_values)
  
  training_cor = cor(predicted_training_values, actual_training_values)
  
  return(list("training_R" = training_cor,   "test_R" = test_cor, "rf_model" = rf_model, "tr_mask" = tr_mask, "predicted_test_values" = predicted_test_values, "actual_test_values" = actual_test_values  ))
  
}











