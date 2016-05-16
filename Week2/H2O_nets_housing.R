################ requires Load_clean_housing_data.R for loading the data
source('./Project_work/Load_clean_housing_data.R')
source('./Week2/resampling_utils.R')

library(h2o)
library(dplyr)

localH2O = h2o.init()




###### load up location, price and date, and possibly type and rooms

rm_loc_price = load_just_location_price()

reduced_loc_price_data = reduce_data(rm_loc_price, frac = 1)                                          #throw away most of the data (speed)

reduced_loc_price_data_nonum = dplyr::select(reduced_loc_price_data, -num_longitude, -num_latitude)    #get rid of numeric lat long and bedrooms

head(reduced_loc_price_data_nonum)





h2o_test_obj = train_test_h2o(df = reduced_loc_price_data_nonum, 
                              #hidden = c(40,20, 10,2), 
                              #hidden = c(20, 10,10, 10,10,2), 
                              hidden = c(125, 30, 25, 3),
                              input_dropout_ratio = 0.3,
                              hidden_dropout_ratios = c(0,0,0,0),
                              l1 = 0.0000,
                              l2 = 0.000080,
                              train_fraction = 0.75, 
                              epochs = 1000)
  

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









h2o_test_obj2 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                               #hidden = c(40,20, 10,2), 
                               #hidden = c(20, 10,10, 10,10,2), 
                               hidden = c(125, 30, 25, 3),
                               input_dropout_ratio = 0.3,
                               hidden_dropout_ratios = c(0,0,0,0),
                               l1 = 0.0000,
                               l2 = 0.000160,
                               train_fraction = 0.75, 
                               epochs = 5000)


h2o_test_obj2[["training_R"]]
h2o_test_obj2[["test_R"]]

predicted = h2o_test_obj2[["predicted_test_values"]]
actual_values = h2o_test_obj2[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 


h2o_test_obj21 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                               #hidden = c(40,20, 10,2), 
                               #hidden = c(20, 10,10, 10,10,2), 
                               hidden = c(125, 30, 25, 3),
                               input_dropout_ratio = 0.3,
                               hidden_dropout_ratios = c(0,0,0,0),
                               l1 = 0.0000,
                               l2 = 0.000160,
                               train_fraction = 0.75, 
                               epochs = 5000)


h2o_test_obj21[["training_R"]]
h2o_test_obj21[["test_R"]]

predicted = h2o_test_obj21[["predicted_test_values"]]
actual_values = h2o_test_obj21[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 



h2o_test_obj22 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                                #hidden = c(40,20, 10,2), 
                                #hidden = c(20, 10,10, 10,10,2), 
                                hidden = c(125, 30, 25, 3),
                                input_dropout_ratio = 0.3,
                                hidden_dropout_ratios = c(0,0,0,0),
                                l1 = 0.0000,
                                l2 = 0.000160,
                                train_fraction = 0.75, 
                                epochs = 5000)


h2o_test_obj22[["training_R"]]
h2o_test_obj22[["test_R"]]

predicted = h2o_test_obj21[["predicted_test_values"]]
actual_values = h2o_test_obj21[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 








h2o_test_obj3 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                               #hidden = c(40,20, 10,2), 
                               #hidden = c(20, 10,10, 10,10,2), 
                               hidden = c(150, 50, 30, 3),
                               input_dropout_ratio = 0.3,
                               hidden_dropout_ratios = c(0,0,0,0),
                               l1 = 0.0000,
                               l2 = 0.000080,
                               train_fraction = 0.75, 
                               epochs = 5000)


h2o_test_obj3[["training_R"]]
h2o_test_obj3[["test_R"]]

predicted = h2o_test_obj3[["predicted_test_values"]]
actual_values = h2o_test_obj3[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 




h2o_test_obj31 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                               #hidden = c(40,20, 10,2), 
                               #hidden = c(20, 10,10, 10,10,2), 
                               hidden = c(150, 50, 30, 3),
                               input_dropout_ratio = 0.3,
                               hidden_dropout_ratios = c(0,0,0,0),
                               l1 = 0.0000,
                               l2 = 0.000080,
                               train_fraction = 0.75, 
                               epochs = 5000)


h2o_test_obj31[["training_R"]]
h2o_test_obj31[["test_R"]]

predicted = h2o_test_obj31[["predicted_test_values"]]
actual_values = h2o_test_obj31[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 





h2o_test_obj32 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                               #hidden = c(40,20, 10,2), 
                               #hidden = c(20, 10,10, 10,10,2), 
                               hidden = c(150, 50, 30, 3),
                               input_dropout_ratio = 0.3,
                               hidden_dropout_ratios = c(0,0,0,0),
                               l1 = 0.0000,
                               l2 = 0.000080,
                               train_fraction = 0.75, 
                               epochs = 5000)


h2o_test_obj32[["training_R"]]
h2o_test_obj32[["test_R"]]

predicted = h2o_test_obj32[["predicted_test_values"]]
actual_values = h2o_test_obj32[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 






h2o_test_obj4 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                                #hidden = c(40,20, 10,2), 
                                #hidden = c(20, 10,10, 10,10,2), 
                                hidden = c(180, 60, 30, 3),
                                input_dropout_ratio = 0.4,
                                hidden_dropout_ratios = c(0,0,0,0),
                                l1 = 0.0000,
                                l2 = 0.000240,
                                train_fraction = 0.75, 
                                epochs = 5000)


h2o_test_obj4[["training_R"]]
h2o_test_obj4[["test_R"]]

predicted = h2o_test_obj4[["predicted_test_values"]]
actual_values = h2o_test_obj4[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 



h2o_test_obj41 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                               #hidden = c(40,20, 10,2), 
                               #hidden = c(20, 10,10, 10,10,2), 
                               hidden = c(180, 60, 30, 3),
                               input_dropout_ratio = 0.4,
                               hidden_dropout_ratios = c(0,0,0,0),
                               l1 = 0.0000,
                               l2 = 0.000240,
                               train_fraction = 0.75, 
                               epochs = 5000)


h2o_test_obj41[["training_R"]]
h2o_test_obj41[["test_R"]]

predicted = h2o_test_obj41[["predicted_test_values"]]
actual_values = h2o_test_obj41[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 



h2o_test_obj42 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                               #hidden = c(40,20, 10,2), 
                               #hidden = c(20, 10,10, 10,10,2), 
                               hidden = c(180, 60, 30, 3),
                               input_dropout_ratio = 0.4,
                               hidden_dropout_ratios = c(0,0,0,0),
                               l1 = 0.0000,
                               l2 = 0.000240,
                               train_fraction = 0.75, 
                               epochs = 5000)


h2o_test_obj42[["training_R"]]
h2o_test_obj42[["test_R"]]

predicted = h2o_test_obj42[["predicted_test_values"]]
actual_values = h2o_test_obj42[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 



h2o_test_obj5 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                               #hidden = c(40,20, 10,2), 
                               #hidden = c(20, 10,10, 10,10,2), 
                               hidden = c(130, 30, 10, 4),
                               input_dropout_ratio = 0.4,
                               hidden_dropout_ratios = c(0,0,0,0),
                               l1 = 0.0000,
                               l2 = 0.000120,
                               train_fraction = 0.75, 
                               epochs = 5000)


h2o_test_obj5[["training_R"]]
h2o_test_obj5[["test_R"]]

predicted = h2o_test_obj5[["predicted_test_values"]]
actual_values = h2o_test_obj5[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 



h2o_test_obj51 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                               #hidden = c(40,20, 10,2), 
                               #hidden = c(20, 10,10, 10,10,2), 
                               hidden = c(130, 30, 10, 4),
                               input_dropout_ratio = 0.4,
                               hidden_dropout_ratios = c(0,0,0,0),
                               l1 = 0.0000,
                               l2 = 0.000120,
                               train_fraction = 0.75, 
                               epochs = 5000)


h2o_test_obj51[["training_R"]]
h2o_test_obj51[["test_R"]]

predicted = h2o_test_obj51[["predicted_test_values"]]
actual_values = h2o_test_obj51[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 



h2o_test_obj52 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                               #hidden = c(40,20, 10,2), 
                               #hidden = c(20, 10,10, 10,10,2), 
                               hidden = c(130, 30, 10, 4),
                               input_dropout_ratio = 0.4,
                               hidden_dropout_ratios = c(0,0,0,0),
                               l1 = 0.0000,
                               l2 = 0.000120,
                               train_fraction = 0.75, 
                               epochs = 5000)


h2o_test_obj52[["training_R"]]
h2o_test_obj52[["test_R"]]

predicted = h2o_test_obj52[["predicted_test_values"]]
actual_values = h2o_test_obj52[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 





h2o_test_obj6 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                               #hidden = c(40,20, 10,2), 
                               #hidden = c(20, 10,10, 10,10,2), 
                               hidden = c(130, 30, 2),
                               input_dropout_ratio = 0.2,
                               hidden_dropout_ratios = c(0,0,0),
                               l1 = 0.0000,
                               l2 = 0.000060,
                               train_fraction = 0.75, 
                               epochs = 5000)


h2o_test_obj6[["training_R"]]
h2o_test_obj6[["test_R"]]

predicted = h2o_test_obj6[["predicted_test_values"]]
actual_values = h2o_test_obj6[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 



h2o_test_obj61 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                               #hidden = c(40,20, 10,2), 
                               #hidden = c(20, 10,10, 10,10,2), 
                               hidden = c(130, 30, 2),
                               input_dropout_ratio = 0.2,
                               hidden_dropout_ratios = c(0,0,0),
                               l1 = 0.0000,
                               l2 = 0.000060,
                               train_fraction = 0.75, 
                               epochs = 5000)


h2o_test_obj61[["training_R"]]
h2o_test_obj61[["test_R"]]

predicted = h2o_test_obj61[["predicted_test_values"]]
actual_values = h2o_test_obj61[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 



h2o_test_obj62 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                                #hidden = c(40,20, 10,2), 
                                #hidden = c(20, 10,10, 10,10,2), 
                                hidden = c(130, 30, 2),
                                input_dropout_ratio = 0.2,
                                hidden_dropout_ratios = c(0,0,0),
                                l1 = 0.0000,
                                l2 = 0.000060,
                                train_fraction = 0.75, 
                                epochs = 5000)


h2o_test_obj62[["training_R"]]
h2o_test_obj62[["test_R"]]

predicted = h2o_test_obj62[["predicted_test_values"]]
actual_values = h2o_test_obj62[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 





h2o_test_obj7 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                                #hidden = c(40,20, 10,2), 
                                #hidden = c(20, 10,10, 10,10,2), 
                                hidden = c(130, 30, 2),
                                input_dropout_ratio = 0.2,
                                hidden_dropout_ratios = c(0,0,0),
                                l1 = 0.0000,
                                l2 = 0.000010,
                                train_fraction = 0.75, 
                                epochs = 5000)


h2o_test_obj7[["training_R"]]
h2o_test_obj7[["test_R"]]

predicted = h2o_test_obj7[["predicted_test_values"]]
actual_values = h2o_test_obj7[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 


h2o_test_obj71 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                               #hidden = c(40,20, 10,2), 
                               #hidden = c(20, 10,10, 10,10,2), 
                               hidden = c(130, 30, 2),
                               input_dropout_ratio = 0.2,
                               hidden_dropout_ratios = c(0,0,0),
                               l1 = 0.0000,
                               l2 = 0.000010,
                               train_fraction = 0.75, 
                               epochs = 5000)


h2o_test_obj71[["training_R"]]
h2o_test_obj71[["test_R"]]

predicted = h2o_test_obj71[["predicted_test_values"]]
actual_values = h2o_test_obj71[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 



h2o_test_obj72 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                                #hidden = c(40,20, 10,2), 
                                #hidden = c(20, 10,10, 10,10,2), 
                                hidden = c(130, 30, 2),
                                input_dropout_ratio = 0.2,
                                hidden_dropout_ratios = c(0,0,0),
                                l1 = 0.0000,
                                l2 = 0.000010,
                                train_fraction = 0.75, 
                                epochs = 5000)


h2o_test_obj72[["training_R"]]
h2o_test_obj72[["test_R"]]

predicted = h2o_test_obj72[["predicted_test_values"]]
actual_values = h2o_test_obj72[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 




h2o_test_obj8 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                                #hidden = c(40,20, 10,2), 
                                #hidden = c(20, 10,10, 10,10,2), 
                                hidden = c(180, 50, 8),
                                input_dropout_ratio = 0.2,
                                hidden_dropout_ratios = c(0,0,0),
                                l1 = 0.000005,
                                l2 = 0.000010,
                                train_fraction = 0.75, 
                                epochs = 5000)


h2o_test_obj8[["training_R"]]
h2o_test_obj8[["test_R"]]

predicted = h2o_test_obj8[["predicted_test_values"]]
actual_values = h2o_test_obj8[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 


h2o_test_obj81 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                               #hidden = c(40,20, 10,2), 
                               #hidden = c(20, 10,10, 10,10,2), 
                               hidden = c(180, 50, 8),
                               input_dropout_ratio = 0.2,
                               hidden_dropout_ratios = c(0,0,0),
                               l1 = 0.000005,
                               l2 = 0.000010,
                               train_fraction = 0.75, 
                               epochs = 5000)


h2o_test_obj81[["training_R"]]
h2o_test_obj81[["test_R"]]

predicted = h2o_test_obj81[["predicted_test_values"]]
actual_values = h2o_test_obj81[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 


h2o_test_obj82 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                                #hidden = c(40,20, 10,2), 
                                #hidden = c(20, 10,10, 10,10,2), 
                                hidden = c(180, 50, 8),
                                input_dropout_ratio = 0.2,
                                hidden_dropout_ratios = c(0,0,0),
                                l1 = 0.000005,
                                l2 = 0.000010,
                                train_fraction = 0.75, 
                                epochs = 5000)


h2o_test_obj82[["training_R"]]
h2o_test_obj82[["test_R"]]

predicted = h2o_test_obj82[["predicted_test_values"]]
actual_values = h2o_test_obj82[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 



h2o_test_obj9 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                                #hidden = c(40,20, 10,2), 
                                #hidden = c(20, 10,10, 10,10,2), 
                                hidden = c(240, 60, 8),
                                input_dropout_ratio = 0.2,
                                hidden_dropout_ratios = c(0,0,0),
                                l1 = 0.0000025,
                                l2 = 0.00000,
                                train_fraction = 0.75, 
                                epochs = 5000)


h2o_test_obj9[["training_R"]]
h2o_test_obj9[["test_R"]]

predicted = h2o_test_obj9[["predicted_test_values"]]
actual_values = h2o_test_obj9[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 


h2o_test_obj91 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                               #hidden = c(40,20, 10,2), 
                               #hidden = c(20, 10,10, 10,10,2), 
                               hidden = c(240, 60, 8),
                               input_dropout_ratio = 0.2,
                               hidden_dropout_ratios = c(0,0,0),
                               l1 = 0.0000025,
                               l2 = 0.00000,
                               train_fraction = 0.75, 
                               epochs = 5000)


h2o_test_obj91[["training_R"]]
h2o_test_obj91[["test_R"]]

predicted = h2o_test_obj91[["predicted_test_values"]]
actual_values = h2o_test_obj91[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 




h2o_test_obj92 = train_test_h2o(df = reduced_loc_price_data_nonum, 
                               #hidden = c(40,20, 10,2), 
                               #hidden = c(20, 10,10, 10,10,2), 
                               hidden = c(240, 60, 8),
                               input_dropout_ratio = 0.2,
                               hidden_dropout_ratios = c(0,0,0),
                               l1 = 0.0000025,
                               l2 = 0.00000,
                               train_fraction = 0.75, 
                               epochs = 5000)


h2o_test_obj92[["training_R"]]
h2o_test_obj92[["test_R"]]

predicted = h2o_test_obj92[["predicted_test_values"]]
actual_values = h2o_test_obj92[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) 









#############################
best_nn_so_far

best_predicted = best_nn_so_far[["predicted_test_values"]]
best_actual_values = best_nn_so_far[["actual_test_values"]]

ggplot(data = data.frame(best_predicted, best_actual_values), aes(x =best_predicted, y = best_actual_values ) ) + geom_point(alpha = 0.04) 


