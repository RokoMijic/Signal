#functional_programming

lapply(1:10, function(x) 2*x)

col

unlist(lapply(mtcars, function(x) class(x) ))

data.frame(lapply(mtcars, function(x) (x - mean(x))/(sd(x)) ))




class(unlist(mtcars[1])) == "numeric"

df = data.frame(matrix(1:100, nrow=10))
df[1:5] = lapply(df[1:5], as.character) 

lapply(df, function(x) { if(is.numeric(unlist(x)) ){return(   (x - mean(x))/sd(x)   ) } else {return(x)}  } ) 





my_lapply = function(L,f)
{
  answer = list(rep(NULL,length(L)))
  
  unlisted = unlist(L)
  
  for(i in c(1:length(L)))
  {
    answer[[i]] = f(unlisted[[i]])
  }

  return(answer)
  
}


my_lapply(list(1,2,3), function(x) {x^2})


each_minus_prev = function(df)
{
  answer = df
  
  answer[,1] = df[,1]
  
  for(i in c(2:ncol(df)) )
  {
    answer[,i] = df[,i] - df[,i-1]
  }
  
  return(answer)
}

my_df = data.frame(matrix(1:100, nrow=10))
my_df

each_minus_prev(my_df)

mean_ig_na = function(v)
{
  return(mean(v, na.rm = TRUE))
}

w = c(1,NA,5)
  
mean_ig_na(w) 
  
L = lapply(1:5, function(x) sample(c(1:4, NA)))

L

  
lapply(L, mean_ig_na)






