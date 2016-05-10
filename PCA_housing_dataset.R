
rm_ml = load_normalized_housing_data()   #load the data


#######  get the names of the good, bad, great houses ######

goodnames = row.names(rm_ml[rm_ml$rated_good==1,])
greatnames = row.names(rm_ml[rm_ml$rated_great==1,])
badnames = row.names(rm_ml[rm_ml$rated_bad==1,])



###### drop unwanted columns ######

rm_ml$scraped_rightmove_id = NULL     # drop the rmid column from rm_ml
rm_ml$excluded = NULL                 # drop whether it is excluded
rm_ml$rated_good = NULL               #
rm_ml$rated_bad = NULL                # drop the ratings
rm_ml$rated_great = NULL              #


head(rm_ml)

#rm_ml is now between 0 and 1 inclusive, with no ratings and no ids, no excluded

pc_obj = prcomp(rm_ml)

pc_rm = data.frame(pc_obj$x)

pc_stdev = pc_obj$sdev

plot(pc_stdev)

head(pc_rm)

(ggplot() + geom_point(data = pc_rm, aes(x=PC1, y=PC2), alpha=0.015) 
+ geom_point(data = pc_rm[goodnames,], aes(x=PC1, y=PC2), alpha=0.7, color = "darkgreen") 
+ geom_point(data = pc_rm[greatnames,], aes(x=PC1, y=PC2), alpha=0.7, color = "green")
+ geom_point(data = pc_rm[badnames,], aes(x=PC1, y=PC2), alpha=0.1, color = "darkred")
)

(ggplot() + geom_point(data = pc_rm, aes(x=PC2, y=PC3), alpha=0.015) 
+ geom_point(data = pc_rm[goodnames,], aes(x=PC2, y=PC3), alpha=0.7, color = "darkgreen") 
+ geom_point(data = pc_rm[greatnames,], aes(x=PC2, y=PC3), alpha=0.7, color = "green")
+ geom_point(data = pc_rm[badnames,], aes(x=PC2, y=PC3), alpha=0.1, color = "darkred")
)

(ggplot() + geom_point(data = pc_rm, aes(x=PC3, y=PC4), alpha=0.015) 
+ geom_point(data = pc_rm[goodnames,], aes(x=PC3, y=PC4), alpha=0.7, color = "darkgreen") 
+ geom_point(data = pc_rm[greatnames,], aes(x=PC3, y=PC4), alpha=0.7, color = "green")
+ geom_point(data = pc_rm[badnames,], aes(x=PC3, y=PC4), alpha=0.1, color = "darkred")
)

(ggplot() + geom_point(data = pc_rm, aes(x=PC4, y=PC5), alpha=0.015) 
+ geom_point(data = pc_rm[goodnames,], aes(x=PC4, y=PC5), alpha=0.7, color = "darkgreen") 
+ geom_point(data = pc_rm[greatnames,], aes(x=PC4, y=PC5), alpha=0.7, color = "green")
+ geom_point(data = pc_rm[badnames,], aes(x=PC4, y=PC5), alpha=0.1, color = "darkred")
)

(ggplot() + geom_point(data = pc_rm, aes(x=PC1, y=PC3), alpha=0.015) 
+ geom_point(data = pc_rm[goodnames,], aes(x=PC1, y=PC3), alpha=0.7, color = "darkgreen") 
+ geom_point(data = pc_rm[greatnames,], aes(x=PC1, y=PC3), alpha=0.7, color = "green")
+ geom_point(data = pc_rm[badnames,], aes(x=PC1, y=PC3), alpha=0.1, color = "darkred")
)

(ggplot() + geom_point(data = pc_rm, aes(x=PC1, y=PC4), alpha=0.015) 
+ geom_point(data = pc_rm[goodnames,], aes(x=PC1, y=PC4), alpha=0.7, color = "darkgreen") 
+ geom_point(data = pc_rm[greatnames,], aes(x=PC1, y=PC4), alpha=0.7, color = "green")
+ geom_point(data = pc_rm[badnames,], aes(x=PC1, y=PC4), alpha=0.1, color = "darkred")
)














