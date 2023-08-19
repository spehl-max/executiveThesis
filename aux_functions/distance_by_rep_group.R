distance_by_rep_group <- function(input_matrix, input_MD){
  #I'll compute the BCD on the abundance table
  BCD_Matrix <- as.matrix(vegdist(decostand(input_matrix, "norm"), method = "bray", diag = TRUE, upper = TRUE))
  
  #Then I'll convert the matrix to a data-frame
  
  distance_tibble_tidy <- as_tibble(BCD_Matrix) %>%
    
    #I will make the row names into a column, named "distance from"
    mutate(distance_from = rownames(BCD_Matrix)) %>% select(distance_from, everything()) %>%
    #I will pivot the data wider, and create a column called "distance to" and a column called "BCD"
    pivot_longer(cols = starts_with("tap"), names_to = "distance_to", values_to = "BCD") %>% 
    
    #I will remove all of the zero distances
    filter(BCD!=0) %>%
    #I will isolate isolate out the replicate samples by filtering so that both "distance from" and "distance to" are in the metadata isRep == True
    filter((distance_from %in% input_MD[input_MD$isPCRrep == TRUE,]$sample_name_long & distance_to %in% input_MD[input_MD$isPCRrep == TRUE,]$sample_name_long) | (distance_from %in% input_MD[input_MD$isFilterRep == TRUE,]$sample_name_long & distance_to %in% input_MD[input_MD$isFilterRep == TRUE,]$sample_name_long) | (distance_to == "tap29_m" & distance_from %in% input_MD[input_MD$isSizeRep == TRUE,]$sample_name_long) | (str_sub(distance_from, start = 1L, end = 5L) != str_sub(distance_to, start = 1L, end = 5L))) %>%
    #add var for which kind of replicate
    left_join(input_MD %>% select(sample_name_long, isPCRrep, isFilterRep, isSizeRep), by = c("distance_to" = "sample_name_long")) %>%
    #Then I will make 2 dummy vars, one with the subset "distance from" and one with the subset "distance to"
    mutate(distance_from_sub = str_sub(distance_from, start = 1L, end = 5L), distance_to_sub = str_sub(distance_to, start = 1L, end = 5L)) %>%
    #then I will create a var with the kind of replicate being examined
    mutate(whichRep = ifelse(distance_from_sub == distance_to_sub, ifelse(isPCRrep == TRUE, "PCR", ifelse(isFilterRep == TRUE, "Filter", ifelse(distance_to == "tap29_m", "Size", "Error"))), "Unique")) %>%
    #cut out the unnecessary vars
    select(BCD, whichRep) %>%
    #take only the distinct values
    distinct() %>%
    mutate(whichRep = factor(whichRep, levels = c("Unique", "PCR", "Filter", "Size")))
  
  
  return(distance_tibble_tidy)
}