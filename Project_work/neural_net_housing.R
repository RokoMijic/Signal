################ requires Load_clean_housing_data.R for loading the data
source('./Project_work/Load_clean_housing_data.R')
source('./Week2/resampling_utils.R')




library(neuralnet)
library(dplyr)

rm_loc_price = load_just_location_price()

################# data is loaded ################

rm_not_numloc = dplyr::select(rm_loc_price, -num_longitude, -num_latitude )

nrow(rm_not_numloc)





########### train some nets #################

pricenet4 <- neuralnet(formula = price ~ lat1 + lat2 + lat3 + lat4 + lat5 + lat6 + lat7 + lat8 + lat9 + lat10 + long1 + long2 + long3 + long4 + long5 + long6 + long7 + long8 + long9 + long10, 
                      data = rm_not_numloc, hidden = c(4,4), lifesign = "full", linear.output = FALSE, threshold = 0.1)

pricenetnh2 <- neuralnet(formula = price ~ lat1 + lat2 + lat3 + lat4 + lat5 + lat6 + lat7 + lat8 + lat9 + lat10 + long1 + long2 + long3 + long4 + long5 + long6 + long7 + long8 + long9 + long10, 
                         data = rm_not_numloc, hidden = 2, lifesign = "full", linear.output = FALSE, threshold = 0.1)

pricenetnh4 <- neuralnet(formula = price ~ lat1 + lat2 + lat3 + lat4 + lat5 + lat6 + lat7 + lat8 + lat9 + lat10 + long1 + long2 + long3 + long4 + long5 + long6 + long7 + long8 + long9 + long10, 
                       data = rm_not_numloc, hidden = 4, lifesign = "full", linear.output = FALSE, threshold = 0.1)

pricenet6 <- neuralnet(formula = price ~ lat1 + lat2 + lat3 + lat4 + lat5 + lat6 + lat7 + lat8 + lat9 + lat10 + long1 + long2 + long3 + long4 + long5 + long6 + long7 + long8 + long9 + long10, 
                      data = rm_not_numloc, hidden = c(6,4), lifesign = "full", linear.output = FALSE, threshold = 0.1)

pricenet12 <- neuralnet(formula = price ~ lat1 + lat2 + lat3 + lat4 + lat5 + lat6 + lat7 + lat8 + lat9 + lat10 + long1 + long2 + long3 + long4 + long5 + long6 + long7 + long8 + long9 + long10, 
                       data = rm_not_numloc, hidden = c(12,4), lifesign = "full", linear.output = FALSE, threshold = 0.1)

pricenet14 <- neuralnet(formula = price ~ lat1 + lat2 + lat3 + lat4 + lat5 + lat6 + lat7 + lat8 + lat9 + lat10 + long1 + long2 + long3 + long4 + long5 + long6 + long7 + long8 + long9 + long10, 
                        data = rm_not_numloc, hidden = c(14,4), lifesign = "full", linear.output = FALSE, threshold = 0.1)





###### get some correlations

rm_loc_price = load_location_price_date_type_rooms()                                    #load up location, price and date, and possibly type and rooms

reduced_loc_price_data = reduce_data(rm_loc_price, frac = 1)                                          #throw away most of the data (speed)

reduced_loc_price_data_nonum = dplyr::select(reduced_loc_price_data, -num_longitude, -num_latitude)    #get rid of numeric lat long

my_formula = "price ~ lat1 + lat2 + lat3 + lat4 + lat5 + lat6 + lat7 + lat8 + lat9 + lat10 + long1 + long2 + long3 + long4 + long5 + long6 + long7 + long8 + long9 + long10 + num_bedrooms +  date_created_bucket  + subtype_detached + subtype_semidetached + subtype_terraced + subtype_endofterrace + subtype_cottage "   

#hidden_layers = c(8,3)
#hidden_layers = 4
#hidden_layers = c(12,3)
#hidden_layers = c(12,4)
#hidden_layers = c(18,3)
#hidden_layers = c(24,3)
#hidden_layers = c(18,2)
#hidden_layers = c(10,6,4)

test_train_obj = train_test_nn(df = reduced_loc_price_data_nonum, in_formula = my_formula, in_hidden = hidden_layers, in_threshold = 0.25)

my_cor = test_train_obj["cor"]
my_cor
predicted_values = test_train_obj["predicted_values"]
actual_values = unlist(test_train_obj["actual_values"])
ggplot(data = data.frame(predicted_values, actual_values), aes(x =predicted_values, y = actual_values ) ) + geom_point(alpha = 0.15) 


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













































