library(dplyr)

load_clean_housing_data = function()
{
  
  rm_raw = read.csv('./Project_work/rightmove1_1.csv')
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

  rm_selected = dplyr::select( load_normalized_housing_data(), starts_with("lat"), starts_with("long"), num_latitude, num_longitude, starts_with("price")  ) 
  
  rm_cleaned = rm_selected[rm_selected$num_latitude > -3 & rm_selected$num_latitude < 3 & rm_selected$num_longitude > -3 & rm_selected$num_longitude < 3 ,]
  
  return(  data.frame(rm_cleaned)   )
}


load_location_price_date = function()
{
  
  rm_selected = dplyr::select( load_normalized_housing_data(), starts_with("lat"), starts_with("long"), num_latitude, num_longitude,  price, date_created_bucket  ) 
  
  rm_cleaned = rm_selected[rm_selected$num_latitude > -3 & rm_selected$num_latitude < 3 & rm_selected$num_longitude > -3 & rm_selected$num_longitude < 3 ,]
  
  return(  data.frame(rm_cleaned)   )
}



load_location_price_date_type_rooms = function()
{
  
  rm_selected = dplyr::select( load_normalized_housing_data(), starts_with("lat"), starts_with("long"), num_latitude, num_longitude,  price, date_created_bucket, num_bedrooms,subtype_detached,subtype_semidetached,subtype_terraced,subtype_endofterrace,subtype_cottage  ) 
  
  rm_cleaned = rm_selected[rm_selected$num_latitude > -3 & rm_selected$num_latitude < 3 & rm_selected$num_longitude > -3 & rm_selected$num_longitude < 3 ,]
  
  return(  data.frame(rm_cleaned)   )
}



  

