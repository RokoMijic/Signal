library("foreign")
beauty_df = read.csv("C:/Users/rmiji/OneDrive/R_working_dir/Signal/beauty/ProfEvaltnsBeautyPublic.csv")
head(beauty_df)

beauty_courseval_model = lm(courseevaluation  ~ btystdave + tenured + minority + age + female + nonenglish + lower + tenuretrack + formal, beauty_df)



(ggplot() + 
  geom_jitter(data = beauty_df, aes(x = btystdave , y = courseevaluation), color = "red", alpha = 0.7, width = 0, height = 0) + 
  geom_smooth(data = beauty_df, method = "lm", aes(x = btystdave, y=courseevaluation), color = "DarkRed", alpha = 0.2) 
)

beauty_courseval_model = lm(courseevaluation  ~ (btystdave + tenured + minority + age + female + nonenglish + lower + tenuretrack + formal)^2, beauty_df)

min_beauty_courseval_model = lm(courseevaluation ~ 1, beauty_df)

formula_max_beauty_courseval_model = formula(beauty_courseval_model)

summary(beauty_courseval_model)

k=2

step_model = step(min_beauty_courseval_model, direction='forward', scope=formula_max_beauty_courseval_model, k=k)

one_var_model = lm(courseevaluation  ~ btystdave, beauty_df)

beauty_predictions_df = data.frame(beauty_df,  one_var_pred_eval = predict(one_var_model, beauty_df), control_pred_eval = predict(step_model,beauty_df) )

head(beauty_predictions_df)

(ggplot(beauty_predictions_df) + 
  geom_jitter(data = beauty_predictions_df, aes(x = btystdave , y = courseevaluation), color = "red", alpha = 0.7, width = 0, height = 0) + 
  geom_jitter(data = beauty_predictions_df, aes(x = btystdave , y = one_var_pred_eval), color = "orange", alpha = 0.7, width = 0, height = 0) +
  geom_jitter(data = beauty_predictions_df, aes(x = btystdave , y = control_pred_eval), color = "green", alpha = 0.7, width = 0, height = 0) 
  #geom_smooth(data = beauty_predictions_df, method = "lm", aes(x = btystdave, y=one_var_pred_eval), color = "DarkRed", alpha = 0.2) +
  #geom_smooth(data = beauty_predictions_df, method = "lm", aes(x = btystdave, y=control_pred_eval), color = "DarkGreen", alpha = 0.2) 

)
k
formula(step_model)


