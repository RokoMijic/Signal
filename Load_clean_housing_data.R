library(dplyr)

load_clean_housing_data = function()
{
  
  rm_raw = read.csv('rightmove1_0.csv')
  rm_proc = rm_raw
  
  rm_proc = rm_proc[rm_proc$excluded == 0,]                 #lose excluded houses
  rm_proc = rm_proc[rm_proc$num_bedrooms <= 50,]            #lose unreasonable numbers of bedrooms
  rm_proc = rm_proc[rm_proc$num_photos <= 50,]              #lose unreasonable numbers of photos
  rm_proc = rm_proc[rm_proc$date_created_bucket >= 19,]     #need to lose properties from before we started rating
  
  row.names(rm_proc) = rm_proc$scraped_rightmove_id         #set row names
  
  return(rm_proc)

}

load_normalized_housing_data = function()
{
   
  rm_norm  = load_clean_housing_data()
  
  ###### normalize stuff ########
  rm_norm$price = rm_norm$price/600000  
  rm_norm$date_created_bucket = rm_norm$date_created_bucket/max(rm_norm$date_created_bucket)
  rm_norm$num_photos = rm_norm$num_photos/max(rm_norm$num_photos)
  rm_norm$num_bedrooms = rm_norm$num_bedrooms/max(rm_norm$num_bedrooms)
  
  return(rm_norm)
  
}

load_just_location_price = function()
{
  rm_ml = load_normalized_housing_data()
  
  return(select(rm_ml,starts_with("lat"), starts_with("long"),starts_with("price")  ))
}

  

