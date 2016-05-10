# use this to install multiplot:
#install.packages('Rmisc')


library(HistData)
library(plyr)
library(dplyr)
library(Rmisc)
library(ggplot2)

df = GaltonFamilies


# anatomically accurate assignment of 0 and 1 to female and male
df$gender = as.numeric(GaltonFamilies$gender=='male')

#rename a column

names(df)[names(df)=="midparentHeight"] <- "mid_parent_height"
names(df)[names(df)=="childHeight"] <- "child_height"

#aggregate family results

df_agg = dplyr::summarise(group_by(df, family), father = mean(father), mother = mean(mother), mid_parent_height = mean(midparentHeight), num_children = max(children), num_children_again = max(childNum), num_male = sum(gender), mean_child_height = mean(childHeight))

df_agg$num_children_again = NULL



#plot the results

plot_father = ggplot(df_agg, aes(x=father, y = mean_child_height) )  + geom_point()

plot_mother = ggplot(df_agg, aes(x=mother, y = mean_child_height) )  + geom_point()  

plot_mid_parent = ggplot(df_agg, aes(x=mid_parent_height, y = mean_child_height) )  + geom_point()  

multiplot(plot_father, plot_mother, cols = 2)

multiplot(plot_father, plot_mother, plot_mid_parent, cols = 3)

# create linear models for these things

linear_agg_father_child = lm(father ~ mean_child_height, df_agg)

linear_agg_mother_child = lm(mother ~ mean_child_height, df_agg)

linear_agg_mid_parent_child = lm(mid_parent_height ~ mean_child_height, df_agg)

# plot these fits

plot_father_fit = ggplot(df_agg, aes(father, mean_child_height)) + geom_point() + geom_smooth(method = "lm")
plot_mother_fit = ggplot(df_agg, aes(mother, mean_child_height)) + geom_point() + geom_smooth(method = "lm")
plot_mid_parent_fit = ggplot(df_agg, aes(mid_parent_height, mean_child_height)) + geom_point() + geom_smooth(method = "lm")

multiplot(plot_father_fit, plot_mother_fit, plot_mid_parent_fit, cols = 3)

#now make a histogram

ggplot(df_agg, aes(x=num_children)) + geom_histogram(binwidth = 1)

#now work with aggregated data

linear_father_child = lm(father ~ child_height, df)

linear_mother_child = lm(mother ~ child_height, df)

linear_agg_mid_parent_child = lm(mid_parent_height ~ child_height, df)


#restrict genders

df_boys = filter(df,gender==1)
df_girls = filter(df,gender==0)


linear_father_son = lm(father ~ child_height, df_boys)
linear_father_daughter = lm(father ~ child_height, df_girls)

linear_mother_daughter = lm(mother ~ child_height, df_girls)
linear_mother_son = lm(mother ~ child_height, df_boys)

cor(df_boys$father, df_boys$child_height)
cor(df_boys$mother, df_boys$child_height)
cor(df_girls$father, df_girls$child_height)
cor(df_girls$mother, df_girls$child_height)




