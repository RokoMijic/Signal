library(caret)
library(neuralnet)
library(dplyr)

source('./Project_work/Load_clean_housing_data.R')
source('./Week2/resampling_utils.R')

df_base = reduce_data(load_location_price_date(), 0.2)
head(df_base)
df_num = dplyr::select(df_base ,-starts_with("lat"), -starts_with("long")  )
df_indi = dplyr::select(df_base ,-num_latitude, -num_longitude  )

#select just the numerical lat/long
#df = df_num
df = df_indi

in_train = createDataPartition(y = df$price,  p = .75,  list = FALSE)

training = df[in_train,]
head(training)

testing = df[-in_train,]

#nngrid <- expand.grid(.layer1 = c(2, 3, 4, 6, 10), .layer2 = c(2,3,4), .layer3 = 1)

nngrid <- expand.grid(.layer1 = c(5,3,2), .layer2 = c(2,3), .layer3 = 0)

nngrid

#indi_formula = "price ~ lat1 + lat2 + lat3 + lat4 + lat5 + lat6 + lat7 + lat8 + lat9 + lat10 + long1 + long2 + long3 + long4 + long5 + long6 + long7 + long8 + long9 + long10"   
#num_formula = "price ~ num_latitude + num_longitude"  

my_trControl = trainControl(verboseIter = TRUE, method = "cv", number = 3)

neural_fit = train(form = price ~ num_latitude + num_longitude + date_created_bucket, data = training, method = "neuralnet", tuneGrid = nngrid, trControl = my_trControl ) 

neural_fit

indi_fit = train(form = price ~ lat1 + lat2 + lat3 + lat4 + lat5 + lat6 + lat7 + lat8 + lat9 + lat10 + long1 + long2 + long3 + long4 + long5 + long6 + long7 + long8 + long9 + long10 + date_created_bucket, data = training, method = "neuralnet", tuneGrid = nngrid, trControl = my_trControl ) 

indi_fit











