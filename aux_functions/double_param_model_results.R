double_param_model_results <- function(input_df, na.rm = TRUE, ...){

  exp_model <- input_df %>% nest_by(sample_name) %>%  
    mutate(mod = list(lm(cpm ~ exp(-1.563653*days_from_filtering) + exp(-0.25993019*days_from_filtering), data = data))) %>%
    summarise(tidy(mod), .groups = "rowwise")
  
  
  #make row name more sensible
  exp_model <- exp_model %>%
    mutate(term = replace(term, term == "(Intercept)", "intercept")) %>%
    mutate(term = replace(term, term == "exp(-0.25993019 * days_from_filtering)", "a")) %>%
    mutate(term = replace(term, term == "exp(-1.563653 * days_from_filtering)", "b"))
  
  #isolate out a
  exp_model_y <- exp_model %>% filter(term == "a") %>% select(-term)
  
  exp_model_pb <- exp_model %>% filter(term == "b") %>% select(sample_name, estimate, std.error, statistic, p.value) %>%
    rename(estimate.pb = estimate, std.error.pb = std.error, statistic.pb = statistic, p.value.pb = p.value)
  
  #take relevant vars from base_sheet
input_df_abridged <- input_df %>% group_by(sample_name) %>% slice(1) %>% 
    select(sample_name, shelf, address_in_shelf, date_time_filtering, date_time_sampling, water_volume_L) %>% 
    transform(detector = paste(shelf, toupper(address_in_shelf), sep = ""))
  
  #join the output_df
  exp_model <- input_df_abridged %>% select(-shelf, -address_in_shelf) %>% 
  left_join(exp_model_y, by = "sample_name") %>% 
  left_join(exp_model_pb, by = "sample_name") %>% arrange(date_time_sampling) %>% 
  select(sample_name, estimate, std.error, statistic, p.value, date_time_filtering, date_time_sampling, water_volume_L, detector, 
         estimate.pb, std.error.pb, statistic.pb, p.value.pb) 
  
  return(exp_model)
  
}