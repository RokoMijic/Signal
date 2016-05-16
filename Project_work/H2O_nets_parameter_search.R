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


###### load up location, price, throw away numerical lat, long 

rm_loc_price = load_location_price_date_type_rooms ()
#rm_loc_price = load_just_location_price()
head(rm_loc_price)
#reduced_loc_price_data_nonum = dplyr::select(rm_loc_price, -num_longitude, -num_latitude)

reduced_loc_price_data_nonum = dplyr::select(rm_loc_price, -num_longitude, -num_latitude, -date_created_bucket)   #get rid of numeric lat long and bedrooms, reorder

head(reduced_loc_price_data_nonum)

reduced_loc_price_data_nonum = reduced_loc_price_data_nonum[,c(c(1:20),c(22:27),21)] 

head(reduced_loc_price_data_nonum)

head(reduced_loc_price_data_nonum[,27])




hidden_list = list(       c(225, 60, 50, 20, 2),
                          c(450, 25, 10, 3), 
                          c(130, 30, 3), 
                          c(228, 6, 2),
                          c(450, 30, 3),
                          c(260, 25, 2),
                          c(250, 8, 4),
                          c(220, 20, 3),
                          c(150, 3), 
                          c(220, 4)
                          )

input_dropout_ratio_list = c(10^seq(-5,-0.3, length.out = 12))
hidden_dropout_ratios_list = c(0)
l1_list = c(0)
l2_list = c(0,0,10^seq(-9,-0.5, length.out = 14))

k=2   #number of runs of each point in parameter space



for(i in c(1:500))
{

  
  choosen_hidden = sample(hidden_list, 1)[[1]]
  chosen_input_dropout = sample(input_dropout_ratio_list,1)
  chosen_hidden_dropout = rep(sample(hidden_dropout_ratios_list,1),length(choosen_hidden))
  chosen_l1 = sample(l1_list,1)
  chosen_l2 = sample(l2_list,1)
  chosen_epochs = 500

  
  print(choosen_hidden) 
  print(chosen_input_dropout) 
  print(chosen_hidden_dropout) 
  print(chosen_l1) 
  print(chosen_l2) 
  print(chosen_epochs) 
  
  test_R = c(0,0)
  training_R = c(0,0)
  
  
  for(j in c(1:k))
  {
  
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
    
  }
  
  chosen_hidden_txt = "|"
  
  for(x in choosen_hidden)
  {
    chosen_hidden_txt = paste(chosen_hidden_txt,x,"|" )
  }
  
  info = c(i, mean(test_R), test_R[[1]], test_R[[2]], mean(training_R), chosen_hidden_txt, chosen_input_dropout, chosen_l2, chosen_epochs)
  
  print_to_file(info)


}


h2o_test_obj[["predicted_test_values"]]











