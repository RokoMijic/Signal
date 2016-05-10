library(ggplot2)

x=seq(-5,5,0.1)
plot(x,dnorm(x,0,1.5),type="l",col="green")

x
pnorm(310,527,105)
pnorm(-1,0,1)

#Tuscon
pnorm(7,5.89,2.23) - pnorm(4,5.89,2.23)

qnorm(0.9,400,83)

pnorm(0.666,0,1)

pnorm(0.666)

qnorm(0.25)

100*pnorm(600,500,100)

getSamples = function(a,n)
{
  x = rnorm(n)
  y = a*x + rnorm(n,0,(1-(a^2))^0.5)
  toreturn = data.frame(x,y)
  return(toreturn)
}

toplot = getSamples(0.999,1000)
ggplot(toplot,aes(x = toplot$i,y=toplot$y)) + geom_point() + geom_smooth(method = 'lm')


avalues = seq(0.1,0.9,0.1)

nvalues = c(100,500,2500,10000)


estimateSlopes = function(a,n,numtrials = 500)
{
  toreturn = c()
  for(trial in c(1:numtrials))
  {
    samplesdf = getSamples(a,n)
    
    a_est = coef(lm(x ~ y, samplesdf))[[2]]
    toreturn = c(toreturn,a_est)
  }
  return(data.frame(value = toreturn))
}

toprint = estimateSlopes(0.5,10,numtrials = 1000)
summary(toprint)

# Histogram
toprint = data.frame(estimateSlopes(0.5,4,numtrials = 5000)) 
binwidth = 0.01

ggplot(toprint, aes(x=value)) + geom_histogram(binwidth = 0.1 , aes(y=..density..) )

toprint

#roko's way

for 

numrows = length(nvalues)

numcols = length(avalues)

dmatrix = outer(rep(0,numrows), rep(0,numcols))

dmatrix

dfSD = data.frame(avalues, row.names = nvalues )



#dave's way


toprint = estimateSlopes(0.5,5,numtrials = 10000)
toprint
head(toprint)


