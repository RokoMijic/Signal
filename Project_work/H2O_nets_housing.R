################ requires Load_clean_housing_data.R for loading the data
source('./Project_work/Load_clean_housing_data.R')
source('./Week2/resampling_utils.R')

library(dplyr)

###### load up location, price 

loc_price = load_just_location_price()
loc_price_nonum = dplyr::select(loc_price, -num_longitude, -num_latitude)    #get rid of numeric lat long and bedrooms
red_loc_price = dplyr::sample_frac(loc_price_nonum, size = 1,  replace = FALSE)

dim(loc_price_nonum)


test_obj = 
  

h2o_test_obj[["training_R"]]
h2o_test_obj[["test_R"]]

predicted = h2o_test_obj[["predicted_test_values"]]
actual_values = h2o_test_obj[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 





h2o_test_obj1 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                              #hidden = c(40,20, 10,2), 
                              #hidden = c(20, 10,10, 10,10,2), 
                              hidden = c(125, 30, 25, 3),
                              input_dropout_ratio = 0.3,
                              hidden_dropout_ratios = c(0,0,0,0),
                              l1 = 0.0000,
                              l2 = 0.000080,
                              train_fraction = 0.75, 
                              epochs = 5000)


h2o_test_obj1[["training_R"]]
h2o_test_obj1[["test_R"]]

predicted = h2o_test_obj1[["predicted_test_values"]]
actual_values = h2o_test_obj1[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 


h2o_test_obj11 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                               #hidden = c(40,20, 10,2), 
                               #hidden = c(20, 10,10, 10,10,2), 
                               hidden = c(125, 30, 25, 3),
                               input_dropout_ratio = 0.3,
                               hidden_dropout_ratios = c(0,0,0,0),
                               l1 = 0.0000,
                               l2 = 0.000080,
                               train_fraction = 0.75, 
                               epochs = 5000)


h2o_test_obj11[["training_R"]]
h2o_test_obj11[["test_R"]]

predicted = h2o_test_obj11[["predicted_test_values"]]
actual_values = h2o_test_obj11[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 





