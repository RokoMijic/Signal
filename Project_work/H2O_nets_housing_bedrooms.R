################ requires Load_clean_housing_data.R for loading the data
source('./Project_work/Load_clean_housing_data.R')
source('./Week2/resampling_utils.R')

library(h2o)
library(dplyr)

localH2O = h2o.init()


print_to_file = function(v)
{
  write.table(t(v), file = "./Project_work/parameter-search.csv", sep = ",",  col.names = FALSE, row.names = FALSE, append=TRUE)
}


###### load up location, price, rooms throw away numerical lat, long 

rm_loc_price = load_location_price_date_type_rooms ()

reduced_loc_price_data_nonum = dplyr::select(rm_loc_price, num_bedrooms, price)   #get rid of numeric lat long and bedrooms, reorder

head(reduced_loc_price_data_nonum)
  
choosen_hidden = c(8, 3, 3)
chosen_input_dropout = 0
chosen_hidden_dropout = c(0,0,0)
chosen_l1 = 0
chosen_l2 = 10^(-4)
chosen_epochs = 300


print(choosen_hidden) 
print(chosen_input_dropout) 
print(chosen_hidden_dropout) 
print(chosen_l1) 
print(chosen_l2) 
print(chosen_epochs) 



h2o_test_obj = train_test_h2o(df = reduced_loc_price_data_nonum, 
                              hidden = choosen_hidden,
                              input_dropout_ratio = chosen_input_dropout ,
                              hidden_dropout_ratios = chosen_hidden_dropout ,
                              l1 = chosen_l1 ,
                              l2 = chosen_l2,
                              train_fraction = 0.75, 
                              epochs = chosen_epochs
                              )


test_R = h2o_test_obj[["test_R"]]
test_R
predicted = h2o_test_obj[["predicted_test_values"]]
actual_values = h2o_test_obj[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) + stat_function(fun = function(x) {x}, color = "red")





#### use a polynomial model



test_obj = train_test_linear(df = reduced_loc_price_data_nonum, 
                             in_formula = "price ~ num_bedrooms + num_bedrooms^2 + num_bedrooms^3   "
                            )


test_R = test_obj[["test_R"]]
test_R
predicted = test_obj[["predicted_test_values"]]
actual_values = test_obj[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03)







