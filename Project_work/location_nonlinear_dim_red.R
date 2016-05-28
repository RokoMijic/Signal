################ requires Load_clean_housing_data.R for loading the data
source('./Project_work/Load_clean_housing_data.R')
source('./Week2/resampling_utils.R')

install.packages("tsne")
library(tsne)
library(dplyr)

###### load up location, price 

loc_price = load_just_location_price()
loc_price_nonum = dplyr::select(loc_price, -num_longitude, -num_latitude)    #get rid of numeric lat long and bedrooms
red_loc_price = dplyr::sample_frac(loc_price_nonum, size = 0.1,  replace = FALSE)

dim(red_loc_price)

tsne(

test_obj = 
  



