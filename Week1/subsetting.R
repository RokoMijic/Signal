

x = 1:10
x[c(FALSE, TRUE)] = 100

mtcars[1:20] 

fruits = c(a="apple", b="banana", x=NA)

x = c("a", "b", "a", "a", "b", "x", "b", "a")

order_data_frame = function(df)
{
  names_vec = names(df)
  ordered_names = order(names_vec)
  return(df[ordered_names])
  
}  

order_data_frame(mtcars)


randompermute_data_frame = function(df, rowpermute=FALSE)
{
  col_names_vec = names(df)
  randomized_col_names = sample(col_names_vec)
  
  if(rowpermute)
  {
    row_names = rownames(df)
    rand_row_names = sample(row_names)
    
    return(df[rand_row_names, randomized_col_names])    
  }
  
  return(df[randomized_col_names])
  
}  

randompermute_data_frame(mtcars, TRUE)


sample_k_with_replacement = function(df, k)
{
  
  
  columns_to_select = sample(names(df), size = k,replace = TRUE)
  
  return(df[columns_to_select])
  
}

m_contiguous_rows = function(df,m)
{
  
  rowstart = sample(c(1:nrow(df) - m) , size = 1)
  
  print(rowstart)
  
  print(m)
  
  print(c(rowstart:(rowstart+m) ))
  
  return(df[c(rowstart:(rowstart+m)),]) 
  
}

delete_named_column = function(df, name)
{

  cols_for_deletion = rep(FALSE,ncol(df)) 
  
  for(i in 1:ncol(df))
  {
    
    if(colnames(df)[i] == name)
    {
      cols_for_deletion[i] = TRUE
      
    }
    
  }
  
  return(df[!cols_for_deletion])

}  




