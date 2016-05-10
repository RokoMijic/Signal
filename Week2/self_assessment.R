#part 1

draw_xy = function(k)
{
  x = runif(k, min = 0, max = 1)
  y = runif(k, min = 0, max = x)
  
  answer = data.frame(x,y)
  return(answer)
}


df = draw_xy(10000)





bin_results = function(df,w=0.01)
{

  N_bins = as.integer(1/w)
  rows = nrow(df)
  
  bins = rep(0,N_bins)
  bincounts = rep(0,N_bins)
  
  
  for( j in c(1:rows))
  {
    for( i in c(1:N_bins))
    {
      
      if(((i-1)*w  <=  df[[j,'y']]) & (df[[j,'y']] < i*w )  )
      {
        bins[[i]] = bins[[i]] + df[[j,'x']]
        bincounts[[i]] = bincounts[[i]] + 1
      }
      
    }
  }
  
  average_Xs = bins/bincounts

  binmaxes = seq(w,1, w)  
  
  return(data.frame(binmaxes, average_Xs))

}


binned = bin_results(df)

ggplot( binned, aes(x = binmaxes, y = average_Xs ) ) + geom_point() + stat_function( fun = (function(x) {(x-1)/(log(x))}), colour = "red"   )

#############################################################################################################


#part 2

install.packages('zoo')
install.packages('psych')
library(psych)
library(dplyr)
library(zoo)

df = msq

numrows = nrow(df)

na_df = is.na(df)
num_nas = colSums(na_df)
frac_nas = num_nas/numrows

sorted_fracs = sort(frac_nas, decreasing = true() )

sorted_fracs



selected_data = select(df, active:scornful, Extraversion, Neuroticism )

head(selected_data)

na_agg_sel = na.aggregate(selected_data)


qplot(na_agg_sel$Extraversion, geom="histogram") 

qplot(na_agg_sel$Neuroticism, geom="histogram") 

ggplot(data = na_agg_sel, aes(Extraversion, Neuroticism) ) + geom_point() + geom_smooth()

head(na_agg_sel)

extra_model = lm(Extraversion ~  . -Neuroticism, data  = na_agg_sel)


neuro_model = lm( Neuroticism ~  . - Extraversion, data  = na_agg_sel)


extra_coeffs = sort(coef(extra_model), decreasing = TRUE  )

neoro_coefs = sort(coef(neuro_model), decreasing = TRUE  ) 

extra_coeffs
neoro_coefs



