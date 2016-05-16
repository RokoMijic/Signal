rm_loc_price = load_just_location_price()                                    #load up location, price and date, and possibly type and rooms

head(rm_loc_price)


reduced_loc_price_data_nonum = dplyr::select(reduced_loc_price_data, -num_longitude, -num_latitude)    #get rid of numeric lat long and bedrooms

head(reduced_loc_price_data_nonum)

my_formula = "price ~ lat1 + lat2 + lat3 + lat4 + lat5 + lat6 + lat7 + lat8 + lat9 + lat10 + long1 + long2 + long3 + long4 + long5 + long6 + long7 + long8 + long9 + long10 "   

train_test_obj_lin = train_test_linear(df = reduced_loc_price_data_nonum,  in_formula = my_formula )

train_test_obj_lin[["training_R2"]]

train_test_obj_lin[["test_R2"]]



predicted_test_values = train_test_obj_lin[["predicted_test_values"]]
actual_test_values = train_test_obj_lin[["actual_test_values"]]
ggplot(data = data.frame(predicted_test_values, actual_test_values), aes(x =predicted_test_values, y = actual_test_values ) ) + geom_point(alpha = 0.10) 

