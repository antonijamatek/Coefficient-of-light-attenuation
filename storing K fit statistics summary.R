#STORING STATISTICS SUMMARY TO THE .XLSX TABLE

#SOLAR NOON  
#initialize empty vectors to store the values
K_estimate <- numeric()
std_error <- numeric()
t_value <- numeric()
p_value <- numeric()
residual_std_errors <- numeric()
#iterate over fit_summary and extract the values
for (i in 1:length(fit_summary.)) {
  summary <- fit_summary.[[i]]
  K_estimate <- c(K_estimate, summary$parameters["K.", "Estimate"])
  std_error <- c(std_error, summary$parameters["K.", "Std. Error"])
  t_value <- c(t_value, summary$parameters["K.", "t value"])
  p_value <- c(p_value, summary$parameters["K.", "Pr(>|t|)"])
  residual_std_errors[i] <- fit_summary.[[i]]$sigma
}
#create a data frame
fit_summary_df <- data.frame(
  Day = n.,
  K_Estimate = K_estimate,
  Std_Error = std_error,
  t_value = t_value,
  p_value = p_value,
  Residual_Std_Error = residual_std_errors)
#save the data frame in table 
write.csv(fit_summary_df, "fit_summary-solar noon.csv", row.names = FALSE) #csv format
writexl::write_xlsx(fit_summary_df, "fit_summary-solar noon.xlsx") #xlsx format

#MORNING
#initialize empty vectors to store the values
K_estimate <- numeric()
std_error <- numeric()
t_value <- numeric()
p_value <- numeric()
residual_std_errors <- numeric()
#iterate over fit_summary and extract the values
for (i in 1:length(fit_summary_am)) {
  summary <- fit_summary_am[[i]]
  K_estimate <- c(K_estimate, summary$parameters["K_am", "Estimate"])
  std_error <- c(std_error, summary$parameters["K_am", "Std. Error"])
  t_value <- c(t_value, summary$parameters["K_am", "t value"])
  p_value <- c(p_value, summary$parameters["K_am", "Pr(>|t|)"])
  residual_std_errors[i] <- fit_summary_am[[i]]$sigma
}
#create a data frame
fit_summary_df_am <- data.frame(
  Day = n_am,
  K_Estimate = K_estimate,
  Std_Error = std_error,
  t_value = t_value,
  p_value = p_value,
  Residual_Std_Error = residual_std_errors)
#save the data frame in table 
writexl::write_xlsx(fit_summary_df_am, "fit_summary-morning.xlsx") #xlsx format

#AFTERNOON
#initialize empty vectors to store the values
K_estimate <- numeric()
std_error <- numeric()
t_value <- numeric()
p_value <- numeric()
residual_std_errors <- numeric()
#iterate over fit_summary and extract the values
for (i in 1:length(fit_summary_pm)) {
  summary <- fit_summary_pm[[i]]
  K_estimate <- c(K_estimate, summary$parameters["K_pm", "Estimate"])
  std_error <- c(std_error, summary$parameters["K_pm", "Std. Error"])
  t_value <- c(t_value, summary$parameters["K_pm", "t value"])
  p_value <- c(p_value, summary$parameters["K_pm", "Pr(>|t|)"])
  residual_std_errors[i] <- fit_summary_pm[[i]]$sigma
}
#create a data frame
fit_summary_df_pm <- data.frame(
  Day = n_pm,
  K_Estimate = K_estimate,
  Std_Error = std_error,
  t_value = t_value,
  p_value = p_value,
  Residual_Std_Error = residual_std_errors)
#save the data frame in table 
writexl::write_xlsx(fit_summary_df_pm, "fit_summary-afternoon.xlsx")
