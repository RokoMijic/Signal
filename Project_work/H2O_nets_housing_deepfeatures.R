################ requires Load_clean_housing_data.R for loading the data
source('./Project_work/Load_clean_housing_data.R')
source('./Week2/resampling_utils.R')


library(h2o)
library(dplyr)

localH2O = h2o.init()

###### load up location, price, rooms throw away numerical lat, long 

df = load_just_location_price()

head(df)

df_no_numeric = dplyr::select(df, -num_longitude, -num_latitude)   #get rid of numeric lat long and bedrooms, reorder
  
choosen_hidden = c(300, 80, 30, 4)
chosen_input_dropout = 0
chosen_hidden_dropout = c(0,0,0,0)
chosen_l1 = 0
chosen_l2 = 10^(-4)
chosen_epochs = 10



h2o_test_obj = train_test_h2o(df = df_no_numeric, 
                              hidden = choosen_hidden,
                              input_dropout_ratio = chosen_input_dropout ,
                              hidden_dropout_ratios = chosen_hidden_dropout ,
                              l1 = chosen_l1 ,
                              l2 = chosen_l2,
                              train_fraction = 0.85, 
                              epochs = chosen_epochs
                              )


test_R = h2o_test_obj[["test_R"]]
test_R
predicted = h2o_test_obj[["predicted_test_values"]]
actual_values = h2o_test_obj[["actual_test_values"]]

ggplot(data = data.frame(predicted, actual_values), aes(x =predicted, y = actual_values ) ) + geom_point(alpha = 0.03) + stat_function(fun = function(x) {x}, color = "red")

deep = cbind(dplyr::select(df,-price, -starts_with("lat"), -starts_with("long")), h2o_test_obj[["deep"]])

head(deep)

cor(dplyr::select(deep, contains("DF"), price )  )
head(dplyr::select(deep, starts_with("num")))


head(dplyr::select(deep, -price ))

write.csv(dplyr::select(deep, starts_with("num") ), file = "./Project_work/deepfeatures1.csv", row.names = FALSE )


( ggplot() + geom_point(data = deep, aes(x=num_longitude, y=num_latitude), color = "red", alpha =  (deep$DF.L3.C1 + 1)*0.03 ) +
     geom_point(data = deep, aes(x=num_longitude, y=num_latitude), color = "blue", alpha =  (deep$DF.L3.C2 + 1)*0.03 ) +
     geom_point(data = deep, aes(x=num_longitude, y=num_latitude), color = "green", alpha =  (deep$DF.L3.C3 + 1)*0.03 ) +
     geom_point(data = deep, aes(x=num_longitude, y=num_latitude), color = "blue", alpha =  (deep$DF.L3.C4 + 1)*0.03 ) +
     geom_point(data = deep, aes(x=num_longitude, y=num_latitude), color = "blue", alpha =  (deep$DF.L3.C5 + 1)*0.03 ) 
)



rgb(red = 0.5, blue = 0.5, green = 0, alpha = 0.5)







