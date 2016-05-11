############# utility functions that allow resampling and validation of a dataset ##############



#returns a boolean vector which indicates which examples are in the training set
train_bool = function(df, training_size)   
{

  train_rownums = sample( c(1:nrow(df)), training_size, replace = FALSE)
  training_mask = rep(FALSE, nrow(df))
  training_mask[train_rownums] = TRUE
  return(training_mask)
  
}  


#trains a neural net on a training subset, tests it on the rest of the data, returns R^2. Parametric with respect to formula and hidden
train_test_nn = function(df, in_formula, in_hidden, train_fraction = 0.8, in_threshold = 0.01)
{
  tr_mask = train_bool(df,nrow(df)*train_fraction)
  
  tr_data = df[tr_mask,]
  test_data = df[!tr_mask,]
  
  trained_net = neuralnet(formula = in_formula, data = tr_data, hidden = in_hidden, lifesign = "full", lifesign.step = 100, linear.output = FALSE, threshold = in_threshold)
  
  predicted_values = compute(trained_net,dplyr::select(test_data, -price))$net.result
  
  actual_values = dplyr::select(test_data,price)

  this_cor = cor(predicted_values, actual_values)

  return(list("cor" = this_cor, "trained_net" = trained_net, "tr_mask" = tr_mask, "predicted_values" = predicted_values, "actual_values" = actual_values  ))

}


#selects a fraction of the data, which defualts to 0.1
reduce_data = function(df, frac = 0.1)
{
  reduced_df_rows = sample( c(1:nrow(df)), as.integer(nrow(df)*frac), replace = FALSE)
  mask = rep(FALSE, nrow(df))
  mask[reduced_df_rows] = TRUE
  return(df[mask,])
}








