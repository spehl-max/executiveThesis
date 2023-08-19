remove_errant_reads_exponential <- function(input_df, na.rm = TRUE, ...){

#remove errant reads:
models <- input_df %>% 
  nest_by(sample_name) %>% 
  mutate(mod = list(lm(cpm ~ exp(-1.56939*days_from_filtering) + exp(-.259525*days_from_filtering), data = data))) %>%
  mutate(student_resid = list(rstudent(mod)))

student_resid_unlist <- data.frame(unlist(models$student_resid))

colnames(student_resid_unlist) <- "student_residuals"

input_df <- input_df %>% bind_cols(student_resid_unlist) %>% 
  filter(abs(student_residuals) < sigma)

return(input_df)
}