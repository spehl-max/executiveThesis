data_wrangler <- function(input_beta_counts, input_meta_data, na.rm = TRUE, ...){

#every fifth row of data
every_fifth_row <- input_beta_counts %>% filter(v_5 == 9000) 

#Change Var Names
every_fifth_row <- every_fifth_row %>% rename(shelf = v_1, 
                                              date_counted = v_3, 
                                              time_counted = v_4, 
                                              count_duration_seconds = v_5)

every_fifth_row <- every_fifth_row %>% mutate(count_duration_minutes = count_duration_seconds/60)

#Select relevant vars
every_fifth_row <- every_fifth_row %>% select(-c(3,7))

#get a date time and drop the seperate date time vars
every_fifth_row <- every_fifth_row %>% mutate(date_counted=as.Date(date_counted, "%d-%b-%y")) %>% 
  mutate(date_time_counted = as.POSIXct(paste(date_counted, time_counted))) %>%
  select(-c(time_counted, date_counted))

#every_not_fifth_row
base_sheet <- input_beta_counts %>% filter(v_5 != 9000)

#CHECKPOINT
##checker <- dim(base_sheet)[1] / dim(every_fifth_row)[1]
#if(checker == 4){
#  print("Good to go")
#}else{
#  print("something's wrong")}


#rename_vars
base_sheet <- base_sheet %>% rename(address_in_shelf = v_1, sample_name_abridged = v_2, alpha = v_3, beta = v_4) %>%
  select(-c(6,7))

#Change the data type of beta column
base_sheet <- base_sheet %>% mutate(beta = as.numeric(beta))

#check all rows have a corresponding key
#base_sheet %>% anti_join(every_fifth_row) %>% dim(.) %>% .[1]

#combine the sheets back together
base_sheet <- base_sheet %>% left_join(every_fifth_row, by = "key")

#delete the key column
base_sheet <- base_sheet %>% select(-key)

#tidy up sample names
base_sheet <- base_sheet %>% filter(!str_detect(sample_name_abridged, "tap38th|tap40th")) %>% mutate(sample_name_abridged = trimws(str_sub(sample_name_abridged, start = 1L, end = 5L)))

#filter out non-tap samples
base_sheet <- base_sheet %>% filter(str_detect(sample_name_abridged, 'tap'))


#import input_meta_data 1 for the 10 jug experiment
#NB in excel the datetime columns must be text format, and must conform
#to the following convention,
#9/4/20 13:05
#to represent september fourth 2020 at 1:05 pm
#Need to select all columns with date and change format to,
#"m/d/yyyy hh:mm"

#check sample names
base_sheet %>% anti_join(input_meta_data, by = "sample_name_abridged") %>% select(sample_name_abridged) %>% distinct() 

#join the beta counts to the input_meta_data
base_sheet <- base_sheet %>% inner_join(input_meta_data, by = "sample_name_abridged")

#convert POSIXct to lubridate
base_sheet <- base_sheet %>% mutate(date_time_counted_l = ymd_hms(date_time_counted), 
                                    date_time_filtering_l = ymd_hms(date_time_filtering), 
                                    date_time_sampling_l = ymd_hms(date_time_sampling))
                                    

#Add duration columns
base_sheet <- base_sheet %>% mutate(elapsed_time_l = date_time_filtering_l %--% date_time_counted_l) %>%
    mutate(mins_from_filtering = as.numeric(as.duration(elapsed_time_l/dminutes(1)))) %>% 
    mutate(days_from_filtering = as.numeric(as.duration(elapsed_time_l/ddays(1)))) %>%
    mutate(age_sample_at_time_of_counting = date_time_sampling_l %--%  date_time_filtering_l) %>%
    mutate(age_sample_at_time_of_counting_days = as.numeric(as.duration(age_sample_at_time_of_counting/ddays(1))))
  


#compute_counts_per_minute
base_sheet <- base_sheet %>% mutate(cpm = beta/count_duration_minutes)

#compute e to the neg lambda t
base_sheet <- base_sheet %>% mutate(decay_function = exp(-0.00018*mins_from_filtering))

return(base_sheet)
}
