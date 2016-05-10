
mylist = list(adam = 1, bob = 3, claire = 55)

names(mylist)

attr(mylist,"names")

double_col_names = function(inlist)
{
  existing_names = attr(inlist,"names")
  newnames = c()
  for( x in existing_names)
  {
    x = paste0(x,x)
    newnames = c(newnames,x)
  }
  attr(inlist,'names') = newnames
  return(inlist)
}

double_col_names(mylist)

## factor time 
arb = c(1,2,3,2,3,4,9)
factor(arb)
arb2 = c('apple', 'banana', 'carrot','apple')
x = factor(arb2)
w = c(136.1, 223.4, ".", 51)

w
typeof(w)

q = factor(w)  #data import does this
q

char_q = as.character(q)
char_q
correct_q = as.double(char_q) # correctly coerce to double
correct_q

q
typeof(q)

mistake_q = as.double(q)   #lose all your data and be sad
mistake_q


f1 = factor(letters)
f2 = rev(factor(letters))
f3 = factor(letters, levels = rev(letters))
f1
f2
f3

fruits_no_na = c("bob", "sweden", "-", "alice")
fruits = c("bob", NA , "-", "alice")


add_na_level = function(vec)
{
  #answer = factor(vec)
  
  if(NA %in% vec)
  {
    return(factor(vec,exclude = c()))
  }
  
  return(factor(vec))
  
}

levfruits = add_na_level(fruits)

levfruits

factor(fruits)

#function for df to 

df = mtcars

floorcols = function(df)
{
  
  for(i in 1:floor(ncol(df)))
  {
    df[i] = factor(df[i])
  }
  return(df)
}






five_unique_to_factor = function(df)
{
  
  for(i in 1:ncol(df))
  {
    
    num_unique = length(unique(df[[i]]))

    print(num_unique)
    
    if(num_unique <= 5)
    {
      df[[i]] = factor(df[[i]])  
    }
    else
    {
    }
    
  }
  return(df)
}

df2 = five_unique_to_factor(df)
str(df2)

natester = data.frame(first = c(1,2,2,2,3,NA,2,NA,3,NA),
                      second = c('a','a','a',NA_character_ ,'b','b','b','b','b','b'))


replace_nas = function(df)
{
  for (i in 1:ncol(df))
  {
    toreplace = names(sort(table(df[[i]]),decreasing = TRUE)[1])

    df[is.na(df[[i]]),i] = toreplace
    
  }
  return(df)
}

toprint = replace_nas(natester)
toprint




replace_randomly_nas = function(df)
{
  set.seed(50)
  for (i in 1:ncol(df))
  {
    non_nans = df[!is.na(df[[i]]),i]
    
    toreplace = sample(non_nans,1)
    
    df[is.na(df[[i]]),i] = toreplace
    
  }
  return(df)
}

toprint = replace_randomly_nas(natester)
toprint

df = mtcars[1:10,]
for (n in c('cyl','am','carb')){
  df[[n]] = factor(df[[n]])
}




minifactor = function(df)
{
  toreturn = df
  
  for(i in names(df))
  {
    currcol = df[,i]

    if(class(currcol) == 'factor')
    {
      toreturn[,i] = NULL
      
      levels_currcol = levels(currcol)
      
      currnumlevels = length(levels_currcol)
      
      for(j in c(1:currnumlevels) )
      {
        if(j!=1){
          currlevel = levels_currcol[j]
          curr_indicator_col = (currcol == currlevel)
          #print(curr_indicator_col)
          newname = paste(i,currlevel,sep = '_')
          toreturn[,newname] = as.integer(curr_indicator_col)
        }
      }
      
    }
  }

  return(toreturn)
}


show = minifactor(df)
show

##loading time.dat

tdat = data.frame(load("time.dat"))



x = list(1,2,3)
y = unlist(x) 

z = c(y,y)

typeof(z)

strptime("01:00PM", "%I:%M %p" )
