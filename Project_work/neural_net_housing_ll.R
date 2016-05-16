################ requires Load_clean_housing_data.R for loading the data
source('./Project_work/Load_clean_housing_data.R')
source('./Week2/resampling_utils.R')




library(NeuralNetTools)
library(neuralnet)
library(dplyr)



###### load data
                                 #load up location, price and date, and possibly type and rooms
rm_loc_price = load_just_location_price()

reduced_loc_price_data = reduce_data(rm_loc_price, frac = 1)                                          #throw away most of the data (speed)

reduced_loc_price_data_nonum = dplyr::select(reduced_loc_price_data, -num_longitude, -num_latitude)    #get rid of numeric lat long and bedrooms

head(reduced_loc_price_data_nonum)

#my_formula = "price ~ lat1 + lat2 + lat3 + lat4 + lat5 + lat6 + lat7 + lat8 + lat9 + lat10 + long1 + long2 + long3 + long4 + long5 + long6 + long7 + long8 + long9 + long10 + num_bedrooms +  date_created_bucket  + subtype_detached + subtype_semidetached + subtype_terraced + subtype_endofterrace + subtype_cottage "   
#my_formula = "price ~ lat1 + lat2 + lat3 + lat4 + lat5 + lat6 + lat7 + lat8 + lat9 + lat10 + long1 + long2 + long3 + long4 + long5 + long6 + long7 + long8 + long9 + long10 + date_created_bucket "   
my_formula = "price ~ lat1 + lat2 + lat3 + lat4 + lat5 + lat6 + lat7 + lat8 + lat9 + lat10 + long1 + long2 + long3 + long4 + long5 + long6 + long7 + long8 + long9 + long10 "   
#my_formula = "price ~ num_longitude + num_latitude"


hidden_layers = c(15,2)
hidden_layers = c(16,4)
hidden_layers = c(14,3)
hidden_layers = c(13,3)
hidden_layers = c(10,3)
hidden_layers = c(15,4)
hidden_layers = c(20,3)
hidden_layers = c(12,3)
hidden_layers = c(20,4)
hidden_layers = c(3,2)
hidden_layers = c(8,3)
hidden_layers = 20
hidden_layers = 2

test_train_obj = train_test_nn(df = reduced_loc_price_data_nonum, in_formula = my_formula, in_hidden = hidden_layers, in_threshold = 0.1)

test_train_obj[["training_R2"]]

test_train_obj[["test_R2"]]



good_net = test_train_obj[["trained_net"]]

plot(good_net)

par(mar = numeric(4), family = 'serif')
plotnet(good_net, alpha = 0.6)


predicted_test_values = test_train_obj["predicted_test_values"]
actual_test_values = unlist(test_train_obj["actual_test_values"])
ggplot(data = data.frame(predicted_test_values, actual_test_values), aes(x =predicted_test_values, y = actual_test_values ) ) + geom_point(alpha = 0.10) 











############### work through a grid of values ###########################



layer_structures = list(1, 2, 4, 8, c(2,2), c(3,2), c(3,3), c(4,2), c(8,2), c(8,3), c(8,4), c(12,3) )
layer_structures_text = as.character(layer_structures)
nstructs = length(layer_structures)

thresholds = list(0.15, 0.25, 0.4, 0.55)
thresholds_text = as.character(thresholds)
nthresh = length(thresholds)

R2_matrix = matrix(data = rep(0,nstructs*nthresh) , nrow = nstructs, ncol = nthresh )   
row.names(R2_matrix) =  layer_structures_text
col.names(R2_matrix) = thresholds_text













































