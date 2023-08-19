single_param_model_results <- function(input_df, na.rm = TRUE, ...){

exp_model <- input_df %>% nest_by(sample_name) %>%  
  mutate(mod = list(lm(cpm ~ decay_function, data = data))) %>% summarise(tidy(mod), .groups = "rowwise")


#make row name more sensible
exp_model <- exp_model %>%
  mutate(term = replace(term, term == "(Intercept)", "intercept")) %>%
  mutate(term = replace(term, term == "decay_function", "a"))

#isolate out a
exp_model_y <- exp_model %>% filter(term == "a") %>% select(-term)

#take relevant vars from base_sheet
input_df_abridged <- input_df %>% group_by(sample_name) %>% slice(1) %>% select(sample_name, shelf, address_in_shelf, date_time_filtering, date_time_sampling, water_volume_L) %>% transform(detector = paste(shelf, toupper(address_in_shelf), sep = ""))

#join the output_df
exp_model <- input_df_abridged %>% select(-shelf, -address_in_shelf) %>% left_join(exp_model_y, by = "sample_name") %>% arrange(date_time_sampling) %>% select(sample_name, estimate, std.error, statistic, p.value, date_time_filtering, date_time_sampling, water_volume_L, detector) 

return(exp_model)

}