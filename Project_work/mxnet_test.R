################ requires Load_clean_housing_data.R for loading the data
setwd("/media/r/16A40437A4041BBD/Users/rmiji/OneDrive/R_working_dir/Signal")
source('./Project_work/Load_clean_housing_data.R')
source('./Week2/resampling_utils.R')

library(mxnet)

library(dplyr)

print_to_file = function(v)
{
  write.table(t(v), file = "./Project_work/parameter-search-GPU.csv", sep = ",",  col.names = FALSE, row.names = FALSE, append=TRUE)
}


rm_loc_price = load_just_location_price()

reduced_loc_price_data_nonum = dplyr::select(rm_loc_price, -num_longitude, -num_latitude)

head(reduced_loc_price_data_nonum)




