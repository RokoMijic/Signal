source('./Project_work/Load_clean_housing_data.R')
source('./Week2/resampling_utils.R')
library(randomForest)


rm_loc_price = load_just_location_price()

reduced_loc_price_data = reduce_data(rm_loc_price, frac = 1)                                          #throw away most of the data (speed)

reduced_loc_price_data_nonum = dplyr::select(reduced_loc_price_data, -num_longitude, -num_latitude)    #get rid of numeric lat long and bedrooms

head(reduced_loc_price_data_nonum)

forest_obj = train_test_forest(df = reduced_loc_price_data_nonum, mtry = 5)

forest_obj[["training_R"]]

forest_obj[["test_R"]]


predicted_test_values = forest_obj[["predicted_test_values"]]
actual_test_values = forest_obj[["actual_test_values"]]
ggplot(data = data.frame(predicted_test_values, actual_test_values), aes(x =predicted_test_values, y = actual_test_values ) ) + geom_point(alpha = 0.10) 







