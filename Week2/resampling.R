library(dplyr)
set.seed(1)

runif(5)

df = read.csv("speedDatingSimple.csv")

head(df)

gender0 = filter(df, gender == 0)

head(gender0)

gender1 =  filter(df, gender == 1)

head(gender1)

lm(attr_o ~ .-sinc_o -intel_o -fun_o -amb_o -X -gender , gender0)

g0_model =  lm(attr_o ~ .-sinc_o -intel_o -fun_o -amb_o -X -gender , gender0)
sort(coef(g0_model))
g1_model =  lm(attr_o ~ .-sinc_o -intel_o -fun_o -amb_o -X -gender , gender1)
sort(coef(g1_model))

num_rows = nrow(gender0)

sample_size = as.integer(num_rows/2)

gender0

set.seed(37)
training_set = sample_n(gender0, sample_size, replace = FALSE)

sampOnce = function(df, sample_size)
{

  train_rownums = sample( c(1:nrow(df)), sample_size, replace = FALSE)
  test_rownums = c(1:nrow(df))[-train_rownums]
  
  train_data = df[train_rownums,]
  test_data = df[test_rownums,]
  
  trained_model = lm(attr_o ~ .-sinc_o -intel_o -fun_o -amb_o -X -gender,train_data)
  
  summary(trained_model)
  
  predictions = predict(trained_model, test_data )
  
  R = cor(predictions, test_data$attr_o)
  
  R
  
  ggplot(data.frame(predictions, test_data$attr_o), aes(x =predictions, y = test_data$attr_o ) ) + geom_point() + geom_smooth(method = 'lm')
  
  return(R^2)
}


sampOnce(gender0, 160)


coeflist = sapply(1:100, function(x) sampOnce(df))


train = sample_n(gender0, sample_size, replace = FALSE)
model = lm(attr_o ~ .-sinc_o -intel_o -fun_o -amb_o -X -gender,train)
summary(model)
cor(model)
cor(gender0)






