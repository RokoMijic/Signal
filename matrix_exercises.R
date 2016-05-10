TestMat = matrix(1:20, nrow=10)

rearrange_rows_cols = function(M)
{
  answer = matrix(M, nrow = dim(W)[[2]] )
}

df = data.frame(matrix(1:100,nrow=10))
df[5, 5] = NA
df[6, 6] = NA

df

df[is.na(df)]

entries_divisible_by_k = function(df,k)
{
  divisibility_mask = df %% k == 0
  
  divisible = df[divisibility_mask] 
  
  return(divisible)
  
}

min_matrix = function(m,n)
{

  ones_m = rep(1,m) 
  countup_m = c(1:m)

  ones_n = rep(1,n) 
  countup_n = c(1:n)
  
  counting_rownum = countup_n %*% t(ones_m)

  counting_colnum = ones_n %*% t(countup_m) 
  
  answer = pmin(counting_rownum, counting_colnum)
  return(answer)

}

is_symmetric = function(M)
{
  entries_thesame = M == t(M)
  
  all_thesame = as.logical(min(entries_thesame))
  
  return(all_thesame) 
  
}

trace = function(M)
{
  return(sum(diag(M)))
}

mystery = function(x)
{
  matrix(c(cos(x), -sin(x), sin(x), cos(x)), nrow=2)
}

mystery(2*pi)

my_list = list(1,2,3,4)

list_to_matrix = function(L)
{
  vec = unlist(L)
  ans_matrix = matrix(vec,nrow = 2)
  return(ans_matrix)
}

A = matrix(c(1:6),nrow = 3 )











