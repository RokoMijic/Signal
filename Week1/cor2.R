cor2 <- function(df)
{
  cor_matrix = cor(df, use="complete-pairs")
  rounded_percent_matrix = round(100*cor_matrix)
  return(rounded_percent_matrix)
}