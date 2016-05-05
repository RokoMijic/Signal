### UN INFANT MORTALITY DATA ###

# Write code here to load packages and data
library(car)
library(ggplot2)
library(GGally)

df = UN

# Write code here to calculate correlations

incorrect_cor = cor(df)

# Write code here to make a new dataframe with incomplete rows omitted

my_cor = cor(df, use="complete.obs")

# Write code here to examine the distribution of the data

df2 = na.omit(UN)
ggpairs(df2)

# Write code here to take the log transformation of the data

ldf = log(df2)

# Write code here to examine the distribution of the log-transformed data

ggpairs(ldf)

# Calculate linear fit of infant mortality vs. GDP
linear_fit = lm(infant.mortality ~ gdp, df2)

# Calculate linear fit of log(infant mortality) vs. log(GDP)
loglog_fit = lm(infant.mortality ~ gdp, ldf)

# Plot the linear fit of infant mortality vs. GDP
ggplot(ldf, aes(gdp, infant.mortality)) + geom_point() + geom_smooth(method = "lm")
ggplot(df2, aes(gdp, infant.mortality)) + geom_point() + geom_smooth(method = "lm")

# Plot of linear fit residuals
qplot(df2$gdp, linear_fit$residuals)

# Plot of linear fit residuals after log transformation of GDP and infant mortality
qplot(df2$gdp, df2$infant.mortality - exp(fitted(loglog_fit)))

# Calculate linear fit of log(infant mortality) vs. GDP
loglinear_fit = lm(infant.mortality ~ gdp, ldf)

loglindf$gdp = df2$gdp
loglindf$infant.mortality = log(df2$infant.mortality)

loglin_fit = lm(infant.mortality ~ gdp, loglindf)

ggplot(loglindf, aes(gdp, infant.mortality)) + geom_point() + geom_smooth(method = "lm")

qplot(loglindf$gdp, loglindf$infant.mortality - fitted(loglin_fit))
