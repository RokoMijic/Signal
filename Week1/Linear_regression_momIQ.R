library("foreign")
kid_df = data.frame(read.dta("C:/Users/rmiji/OneDrive/R_working_dir/Signal/child.iq/kidiq.dta")) 
head(kid_df)

fit_3 = lm(kid_score ~ mom_hs + mom_iq, kid_df)
fit_3

ggplot(kid_df, aes(x = mom_iq , y = kid_score )) + geom_point() + geom_smooth(method = "lm")


#Ex 3.4

childiq_df = data.frame(read.dta("C:/Users/rmiji/OneDrive/R_working_dir/Signal/child.iq/child.iq.dta")) 

head(childiq_df)

childtest_on_momage_reg = lm(ppvt ~ momage, childiq_df)
childtest_on_momage_reg

summary(childtest_on_momage_reg)


childtest_on_momage_ed_reg = lm(ppvt ~ momage + educ_cat, childiq_df)
childtest_on_momage_ed_reg

summary(child_test_on_momage_reg)

completed_HS = as.integer((childiq_df$educ_cat >=2))
completed_HS

childiq_df = data.frame(childiq_df,completed_HS)
head(childiq_df)

childtest_on_momage_momhs_reg = lm(ppvt ~ momage + completed_HS + completed_HS*momage, childiq_df)

coef(childtest_on_momage_momhs_reg)


(ggplot() + 
  geom_point(data = childiq_df[childiq_df$completed_HS==0,], aes(x = momage , y = ppvt), color = "green", alpha = 0.7) + 
  geom_point(data = childiq_df[childiq_df$completed_HS==1,], aes(x = momage , y = ppvt), color = "purple", alpha = 0.3) 
  ) 


(ggplot() + 
  geom_jitter(data = childiq_df[childiq_df$completed_HS==0,], aes(x = momage , y = ppvt), color = "green", alpha = 0.7, width = 0.5, height = 0) + 
  geom_jitter(data = childiq_df[childiq_df$completed_HS==1,], aes(x = momage , y = ppvt), color = "purple", alpha = 0.3, width = 0.5, height = 0) + 
  geom_smooth(data = childiq_df[childiq_df$completed_HS==0,], method = "lm", aes(x = momage, y=ppvt), color = "green", alpha = 0.2) +
  geom_smooth(data = childiq_df[childiq_df$completed_HS==1,], method = "lm", aes(x = momage, y=ppvt), color = "purple", alpha = 0.2) 

) 


#do a training and validation set

childiq_df = data.frame(read.dta("C:/Users/rmiji/OneDrive/R_working_dir/Signal/child.iq/child.iq.dta")) 

childiq_0_200_df = childiq_df[1:200,]
childiq_201_400_df = childiq_df[201:400,]

childtest_on_momage_momhs_reg_0_200 = lm(ppvt ~ momage + educ_cat + educ_cat*momage, childiq_0_200_df)

coef(childtest_on_momage_momhs_reg_0_200)

predicted_201_400 = predict(childtest_on_momage_momhs_reg_0_200, childiq_201_400_df )

comparison_df = data.frame(childiq_201_400_df, predicted_201_400)

head(comparison_df)

(ggplot() + 
  geom_jitter(data = comparison_df, aes(x = momage , y = ppvt), color = "gold", alpha = 0.7, width = 0.5, height = 0) + 
  geom_jitter(data = comparison_df, aes(x = momage , y = predicted_201_400), color = "DarkRed", alpha = 0.3, width = 0.5, height = 0) + 
  geom_smooth(data = comparison_df, method = "lm", aes(x = momage, y=ppvt), color = "gold", alpha = 0.2) +
  geom_smooth(data = comparison_df, method = "lm", aes(x = momage, y=predicted_201_400), color = "DarkRed", alpha = 0.2) 

) 

summary(childtest_on_momage_momhs_reg_0_200)

