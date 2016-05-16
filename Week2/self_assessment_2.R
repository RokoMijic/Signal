####################################################################################################
############################### Signal Self Assessment 2 ROKO MIJIC ################################
####################################################################################################


####################################################################################################
########################################### Part 1 #################################################
####################################################################################################


library("dplyr")
library("ggplot2")
library("glmnet")
library("psych")
library("corrplot")
library("zoo")

set.seed(1)

############################ global parameters #################################

alphas = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)

lambdas = 10^seq(from = 1, to = -3, length.out = 50 )

n = 10   #number of folds 


############################ extract data #################################


df = dplyr::select(msq, active:scornful, Extraversion, Neuroticism)

df = na.aggregate(df)   #replace NAs

nrw = nrow(df)   #number of rows in the data

features = data.matrix(dplyr::select(df, -Extraversion, -Neuroticism))

extra = data.matrix(dplyr::select(df, Extraversion) )

neuro = data.matrix(dplyr::select(df, Neuroticism))



############################ generate fold assignments #################################


fold_ass = function(df_in, n_in)
{
  rownums = c(1:nrow(df_in))
  return((rownums %% n_in)+1)
}

############################ precompute subsets #################################



train_features_list = list()
test_features_list = list()
train_extra_list = list()
train_neuro_list = list()



fan = fold_ass(features, n)

for(i in c(1:n))
{

  train_features_list[[i]] = data.matrix(features[(fan!=i),])
  test_features_list[[i]] = data.matrix(features[(fan==i),])
  train_extra_list[[i]] = extra[(fan!=i),]
  train_neuro_list[[i]] = neuro[(fan!=i),]

  
  
}



############################ create results dataframe #################################


results = data.frame()


############################ rmse function #################################


rmse = function(x,y)
{
  n = length(x)
  rms = (1/n*t(x-y)%*%(x-y))^0.5
  return(rms)
}

############################ iterate over alpha, lambda #################################

for( a in alphas)
{
  fits_extra = list(rep(0,n))
  fits_neuro = list(rep(0,n))
  
  for(i in c(1:n))
  {

    fits_extra[[i]] = glmnet(train_features_list[[i]], train_extra_list[[i]], family = "gaussian", alpha = a, lambda = lambdas )
    fits_neuro[[i]] = glmnet(train_features_list[[i]], train_neuro_list[[i]], family = "gaussian", alpha = a, lambda = lambdas )
    
  }
  
  for(l in lambdas)
  {
    print(l)
    preds_extra = rep(0,nrw)
    preds_neuro = rep(0,nrw)

    
    for(j in c(1:n))
    {
      preds_extra[fan == j] = predict(fits_extra[[j]],test_features_list[[j]], s = l)
      preds_neuro[fan == j] = predict(fits_neuro[[j]],test_features_list[[j]], s = l)
    }
    
    rmse_extra =  rmse(preds_extra,extra)
    rmse_neuro =  rmse(preds_neuro,neuro)
    
    rw = c(a, l , rmse_extra, rmse_neuro )
    
    results = rbind(results,rw)
    
  }
  
}


############################ argmin function #################################

argmin = function(v)
{
  index = 1
  smallest_found = v[[1]]
  
  for(i in c(1:length(v)) )
  {
    curr = v[[i]]
    if(curr < smallest_found)
    {
      smallest_found = curr
      index = i
    }
  }
  return(index)
}

############################ extract vals #################################

colnames(results) = c("alpha", "lambda", "RMSE_Extra", "RMSE_Neuro")

RMSE_extra_col = data.matrix(results["RMSE_Extra"])
RMSE_extra_col


idx_extra = argmin(RMSE_extra_col) 

extra_alpha = results[idx_extra,"alpha"]
extra_lambda = results[idx_extra,"lambda"]

extra_alpha
extra_lambda

RMSE_neuro_col = data.matrix(results["RMSE_Neuro"])

idx_neuro = argmin(RMSE_neuro_col) 

neuro_alpha = results[idx_neuro,"alpha"]
neuro_lambda = results[idx_neuro,"lambda"]

neuro_alpha
neuro_lambda



############################ train on whole dataset #################################


whole_fit_extra = glmnet(features, extra, family = "gaussian", alpha = extra_alpha, lambda = extra_lambda )

whole_fit_neuro = glmnet(features, neuro, family = "gaussian", alpha = neuro_alpha, lambda = neuro_lambda )

extra_coefs = as.numeric(coef(whole_fit_extra, s = extra_lambda ))

neuro_coefs = as.numeric(coef(whole_fit_neuro, s = neuro_lambda ))

length(neuro_coefs)

############################ make coefficient matrix #################################

coef_matrix = data.frame(extra_coefs, neuro_coefs)
coef_matrix = coef_matrix[-1,] 
colnames(coef_matrix) = c("Extraversion", "Neuroticism")
row.names(coef_matrix) = colnames(features)

ncoeffs = nrow(coef_matrix)

quantile(abs(coef_matrix[,"Extraversion"]))

q3_extra = as.numeric(quantile(abs(coef_matrix[,"Extraversion"]))[4])
q3_extra

quantile(abs(coef_matrix[,"Neuroticism"]))

q3_neuro = as.numeric(quantile(abs(coef_matrix[,"Neuroticism"]))[4])
q3_neuro


#filtered_coeff_matrix = dplyr::filter(coef_matrix, abs(Extraversion) >= q3_extra & abs(Neuroticism) >= q3_neuro )
filtered_coeff_matrix = coef_matrix[abs(coef_matrix$Extraversion) >= q3_extra & abs(coef_matrix$Neuroticism) >= q3_neuro,]

############ don't use dplyr if you want to preserve row names! ################

filtered_coeff_matrix

corrplot(data.matrix(filtered_coeff_matrix), is.corr=FALSE)


############################ interpretation #################################

#extraversion and neuroticism anti-correlate, so a positive predictor of one must be a negative predictor of the other 



####################################################################################################
########################################### Part 2 #################################################
####################################################################################################


############################ hash collisions #################################

#Your hash function assigns each object to a number between 1:10,
#each with equal probability. With 10 objects, what is the probability
#of a hash collision?

#algebra solution: number of non-colliding assignments = 10!
#                  number of possible assignments = 10^10
#                  probability of no collisions = 10!/(10^10)
#                  probability of some collisions = 1- 10!/(10^10)
#                  probability = 0.9996371

1-factorial(10)/(10^10)

#returns 0.9996371

#experimental method

numtrials = 1000000

numcollisions = 0

for(i in c(1:numtrials))
{
  
  hashvals = sample(c(1:10), 10, replace = TRUE)

  if(length(unique(hashvals))  != 10 )
  {
    numcollisions = numcollisions + 1
  }
  
}

numcollisions/numtrials

#returns 0.999623


########### What is the expected number of hash collisions? #############

#algebra method: 
# let X be the number of unused hashes. 
# Then we can make a distribution for X using theory

PX = rep(0,11)
EX =  rep(0,11)

for(x in c(0:10))
{
  PX[x] = (choose(10,x)^2*factorial(10-x)*(10-x)^x)/(10^10)
  EX[x] = x*PX[x]
}


sum(PX)
sum(EX)



#experimental method

numtrials = 300000

sumcollisions = 0

for(i in c(1:numtrials))
{
  
  hashvals = sample(c(1:10), 10, replace = TRUE)
  
  num_collisions = 10-length(unique(hashvals))  
  
  sumcollisions = sumcollisions + num_collisions

}

sumcollisions/numtrials

#returns 3.48653


########### What is the expected number of unused hashes? #############
#algebra solution: TODO

numtrials = 300000

sumunused = 0

for(i in c(1:numtrials))
{
  
  hashvals = sample(c(1:10), 10, replace = TRUE)
  
  num_unused = length(setdiff(c(1:10), unique(hashvals) ) )
  
  sumunused = sumunused + num_unused
  
}

sumunused/numtrials

#returns 3.485293



############################ rolling the dice #################################

#Given a fair, 6-sided dice, what's the expected number of rolls you have
#to make before each number (1, 2, . . . , 6) shows up at least once?



numtrials = 100000

sumrolls = 0

for(i in c(1:numtrials))
{
  
  number_list = rep(0,6)
  num_rolls = 0
  
  while(prod(number_list)==0)
  {
    num_rolls = num_rolls + 1
    newroll = sample(c(1:6),1)
    number_list[[newroll]] = 1
  }
  

  sumrolls = sumrolls + num_rolls
  
}

sumrolls/numtrials


#returns approximately 14.7



############################ Bobo the Amoeba #################################


#Bobo the amoeba can divide into 0, 1, 2, or 3 amoebas with equal
#probability. (Dividing into 0 means that Bobo dies.) Each of Bobo's
#descendants have the same probabilities. What's the probability that
#Bobo's lineage eventually dies out?



#next-gen function

next_gen = function(n, N=500)
{
  if(n > N)
  {
    return(n+1)
  }
  else
  {
    return(sum(sample(c(0,1,2,3),n,replace = TRUE)))    
  }
}


#initialise values
num_lineages = 10000 
n_gens = 30

sim_matrix = matrix(rep(0,num_lineages*n_gens), ncol = num_lineages )

sim_matrix[1,] = 1

for(t in c(1:n_gens))
{
  
  for(l in c(1:num_lineages))
  {
    sim_matrix[t+1,l] = next_gen(sim_matrix[t,l])
  }

}


exist_matrix = 1*(sim_matrix != 0)

head(exist_matrix)

existence_prob = rowSums(exist_matrix)/num_lineages

existence_prob

qplot(c(1:n_gens), existence_prob)

#wolfram alpha indicates a probability of extinction of 0.41421

1-0.41421




