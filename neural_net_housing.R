library(neuralnet)

rm_loc_price = load_just_location_price()

################# data is loaded ################

rm_not_numloc = dplyr::select(rm_loc_price, -num_longitude, -num_latitude )

head(rm_not_numloc)



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



rm_not_price = dplyr::select(rm_not_numloc,-price)

actual_price = dplyr::select(rm_not_numloc,price)

predicted_price = compute(pricenetnh2,rm_not_price)$net.result

head(predicted_price)
head(actual_price)

predicted_price

cor(predicted_price,actual_price)



