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
#rm_loc_price = load_just_location_price()
head(rm_loc_price)
#reduced_loc_price_data_nonum = dplyr::select(rm_loc_price, -num_longitude, -num_latitude)

reduced_loc_price_data_nonum = dplyr::select(rm_loc_price, -num_longitude, -num_latitude, -date_created_bucket)   #get rid of numeric lat long and bedrooms, reorder

head(reduced_loc_price_data_nonum)

reduced_loc_price_data_nonum = reduced_loc_price_data_nonum[,c(c(1:20),c(22:27),21)] 

head(reduced_loc_price_data_nonum)

head(reduced_loc_price_data_nonum[,27])



  
choosen_hidden = c(300, 30, 3)
chosen_input_dropout = 0
chosen_hidden_dropout = 0
chosen_l1 = 0
chosen_l2 = 10^(-4)
chosen_epochs = 500


print(choosen_hidden) 
print(chosen_input_dropout) 
print(chosen_hidden_dropout) 
print(chosen_l1) 
print(chosen_l2) 
print(chosen_epochs) 

test_R = c(0,0)
training_R = c(0,0)


h2o_test_obj = train_test_h2o(df = reduced_loc_price_data_nonum, 
                              hidden = choosen_hidden,
                              input_dropout_ratio = chosen_input_dropout ,
                              hidden_dropout_ratios = chosen_hidden_dropout ,
                              l1 = chosen_l1 ,
                              l2 = chosen_l2,
                              train_fraction = 0.75, 
                              epochs = chosen_epochs
                              )

training_R[[j]] =   h2o_test_obj[["training_R"]]
test_R[[j]] = h2o_test_obj[["test_R"]]
  

h2o_test_obj[["predicted_test_values"]]











