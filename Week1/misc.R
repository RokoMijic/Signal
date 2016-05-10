
de_nester = function(input_object, depth)
{
  if(typeof(input_object) == "list")
  {

    unwrapped_object = unlist(input_object, recursive = FALSE)
    
    unwrapped_length = length(unwrapped_object)
    
    depths_array = rep(0,unwrapped_length)
    
    for( x in 1:unwrapped_length )
    {
      depths_array[x] = de_nester(unwrapped_object[x], depth + 1)
    }
    
    return(max(depths_array))

  }
  
  else
  {
    return(depth)
  }  
}  




listdominos = function(n)
{  
  Dominos = list()
  
  for( i in c(1:n))
  {
    for( j in c(1:n))
    {
      if(i>= j)
      {
        Domino = list(i, j)
        Dominos[[length(Dominos)+1]] = Domino  #[[]] isused here since we are setting the *element*
      }

    }
    
  }
  
  return(Dominos)
  
}


checkcircle = function(L)
{
  
  num_doms = length(L)

  
  for(i in c(1:(num_doms-1)))
  {

    ith_dom = L[[i]] 
    i_plus_oneth_dom = L[[i+1]] 
    
    if(ith_dom[[2]] == i_plus_oneth_dom[[1]] )
    {
    }
    else
    {
      return(FALSE)
    }
    
  }
  
  last_dom = L[[length(L)]]
  first_dom = L[[1]]
  
  if(last_dom[[2]] == first_dom[[1]])
  {
  }
  else
  {
    return(FALSE)    
  }
  
  return(TRUE)
}

make_circle = function()
  
{
  
  
  
}


count_letters = function(df)
{
  concatnames = ""
  
  answer = rep(0,26)
  
  for(x in colnames(df))
  {
    concatnames = paste(concatnames,x)
  }
  
  split_chars = unlist(strsplit(concatnames,split = c(), fixed= TRUE ))
  
  alphabet = unlist(strsplit("abcdefghijklmnopqrstuvwxyz",split = c(), fixed= TRUE ))
  
  for(i in c(1:26))
  {
    answer[[i]] = length(grep(alphabet[[i]], split_chars, ignore.case = TRUE, value = FALSE))
    names(answer)[[i]] = alphabet[[i]]
    
  }
  
  return(answer)
  
}


change_names = function(df)
{
  l = length(colnames(df))
  
  for(i in c(1:l))
  {
    currname = colnames(df)[i]
    newname = paste(gsub(" ", ".", currname), "_mod", sep="")
    
    colnames(df)[i] = newname
    
  }
  
  return(df)
  
}
  



remove_last4 = function(df)
{
  numcols = length(colnames(df))
  
  for(i in c(1:numcols))
  {
    
    currname = colnames(df)[i]
    currlen = length(currname)
    
    if(currlen <= 4)
    {
      newname = ""
    }
    else
    {
      newname = substr(currname,1,currlen-4)
    }

    colnames(df)[i] = newname
    
  }
  
  return(df)  
  
}


join_row_names = function(df)
{
  numrows = length(rownames(df))
  
  answer = ""
  
  for(i in c(1:numrows))
  {
    
    currname = rownames(df)[i]
    
    if(i==1)
    {
      answer = paste(answer, currname, sep = "")
    }
    else
    {
      answer = paste(answer, "_", currname, sep = "")
    }
    
  }
  
  return(answer)
}


spiral = function(df)
{
  
  
  
}  

spiral_x = function(t,n)
{
  if(t<=n)
  {
    return(t)
  }
  elseif(t<=2*n-1)
  {
    return(n)
  }
  elseif(t<=3*n-2)
  {
    return(3*n-t-1)
  }
  elseif(t<=4*n-3)
  {
    return(spiral_x(t-4*n+3,n-1))+1
  }
  
}
  
spiral_y = function(t,n)
{
  if(t<=n)
  {
    return(1)
  }
  elseif(t<=2*n-1)
  {
    return(t-n+1)
  }
  elseif(t<=3*n-2)
  {
    return(4*n-t+2)
  }
  elseif(t<=4*n-3)
  {
    return(spiral_y(t-4*n+3,n-1))+1
  }
  
}



  
