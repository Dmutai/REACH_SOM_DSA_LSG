
select_multiple_output_formatting <- function(sm,sm_binary) {
  
  colnames(sm_binary) <-  gsub(".*\\.","",colnames(sm_binary))
  w <- which(sm_binary==1,arr.ind=TRUE)
  sm_binary[w] <- names(sm_binary)[w[,"col"]]
  sm_binary[sm_binary == 0] <- ''
  
  sm_binary <-  data.frame(str_squish(str_trim(do.call(paste, c(sm_binary, sep = " ")))) ) 
  colnames(sm_binary)[1] <- sm
  sm_binary
}

select_multiple_apply_constraints <- function(aggregation_output_select_multiple) {
  #### cccm_management #####
  #### not(selected(., "dnk") and count-selected(.) > 1) and not(selected(.,"no_management") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("cccm_management")&!ends_with(".dnk")&!ends_with(".no_management")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"cccm_management.dnk"] <- 0
  aggregation_output_select_multiple[x,"cccm_management.no_management"] <- 0
  
  #### cccm_committees ####
  #### not(selected(., "no_committees") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("cccm_committees")&!ends_with(".dnk")&!ends_with(".no_committees")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"cccm_committees.dnk"] <- 0
  aggregation_output_select_multiple[x,"cccm_committees.no_committees"] <- 0
  
  #### nfi_items_available	####
  #### not(selected(., "none") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("nfi_items_available")&!ends_with(".none")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"nfi_items_available.none"] <- 0
  
  #### nfi_access_impediments ####
  ####	not(selected(., "no_impediments") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("nfi_access_impediments")&!ends_with(".dnk")&!ends_with(".no_impediments")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"nfi_access_impediments.dnk"] <- 0
  aggregation_output_select_multiple[x,"nfi_access_impediments.no_impediments"] <- 0
  
  
  #### water_treatment_methods	####
  #### not(selected(., "no_treatment") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("water_treatment_methods")&!ends_with(".dnk")&!ends_with(".no_treatment")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"water_treatment_methods.dnk"] <- 0
  aggregation_output_select_multiple[x,"water_treatment_methods.no_treatment"] <- 0
  
  #### water_treatment_methods	####
  #### not(selected(., "no_treatment") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("water_treatment_methods")&!ends_with(".dnk")&!ends_with(".no_treatment")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"water_treatment_methods.dnk"] <- 0
  aggregation_output_select_multiple[x,"water_treatment_methods.no_treatment"] <- 0
  
  #### water_access_barriers	####
  #### not(selected(., "no_problem") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("water_access_barriers")&!ends_with(".dnk")&!ends_with(".no_problem")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"water_access_barriers.dnk"] <- 0
  aggregation_output_select_multiple[x,"water_access_barriers.no_problem"] <- 0
  
  
  #### health_services	####
  #### not(selected(., "none") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("health_services")&!ends_with(".dnk")&!ends_with(".none")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"health_services.dnk"] <- 0
  aggregation_output_select_multiple[x,"health_services.none"] <- 0
  
  #### nutrition_distributions	####
  #### not(selected(., "none") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("nutrition_distributions")&!ends_with(".dnk")&!ends_with(".none")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"nutrition_distributions.dnk"] <- 0
  aggregation_output_select_multiple[x,"nutrition_distributions.none"] <- 0
  
  #### support	####
  #### not(selected(., "none") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("support")&!ends_with(".dnk")&!ends_with(".none")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"support.dnk"] <- 0
  aggregation_output_select_multiple[x,"support.none"] <- 0
  
  #### hygiene_access_impediments	####
  #### not(selected(., "no_problem") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("hygiene_access_impediments")&!ends_with(".dnk")&!ends_with(".no_problem")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"hygiene_access_impediments.dnk"] <- 0
  aggregation_output_select_multiple[x,"hygiene_access_impediments.no_problem"] <- 0
  
  #### health_barriers	####
  #### not(selected(., "no_problem") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("health_barriers")&!ends_with(".dnk")&!ends_with(".no_problem")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"health_barriers.dnk"] <- 0
  aggregation_output_select_multiple[x,"health_barriers.no_problem"] <- 0
  
  #### nutrition_services	####
  #### not(selected(., "no_problem") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("nutrition_services")&!ends_with(".dnk")&!ends_with(".no_problem")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"nutrition_services.dnk"] <- 0
  aggregation_output_select_multiple[x,"nutrition_services.no_problem"] <- 0
  
  #### aap_informationsources####
  #### not(selected(., "no_info") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("aap_informationsources")&!ends_with(".dnk")&!ends_with(".no_info")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"aap_informationsources.dnk"] <- 0
  aggregation_output_select_multiple[x,"aap_informationsources.no_info"] <- 0
  
  #### aap_informationsources_pwd	####
  #### not(selected(., "no_info") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("aap_informationsources_pwd")&!ends_with(".dnk")&!ends_with(".no_info")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"aap_informationsources_pwd.dnk"] <- 0
  aggregation_output_select_multiple[x,"aap_informationsources_pwd.no_info"] <- 0
  
  #### protection_incidents	####
  #### not(selected(., "no_incidents") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) and not(selected(., "pnta") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("protection_incidents")&!ends_with(".dnk")&!ends_with(".pnta")&!ends_with(".no_incidents")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"protection_incidents.dnk"] <- 0
  aggregation_output_select_multiple[x,"protection_incidents.no_incidents"] <- 0
  aggregation_output_select_multiple[x,"protection_incidents.pnta"] <- 0
  
  #### sanitation_access_impediments	####
  #### not(selected(., "no_impediments") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("sanitation_access_impediments")&!ends_with(".dnk")&!ends_with(".no_impediments")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"sanitation_access_impediments.dnk"] <- 0
  aggregation_output_select_multiple[x,"sanitation_access_impediments.no_impediments"] <- 0
  
  #### support_access_impediments	####
  #### not(selected(., "no_impediments") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("support_access_impediments")&!ends_with(".dnk")&!ends_with(".no_impediments")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"support_access_impediments.dnk"] <- 0
  aggregation_output_select_multiple[x,"support_access_impediments.no_impediments"] <- 0
  
  #### health_facilities	####
  #### not(selected(., "no_health_facility") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("health_facilities")&!ends_with(".dnk")&!ends_with(".no_health_facility")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"health_facilities.dnk"] <- 0
  aggregation_output_select_multiple[x,"health_facilities.no_health_facility"] <- 0
  
  #### health_problems	####
  #### not(selected(., "no_health_issues") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("health_problems")&!ends_with(".dnk")&!ends_with(".no_health_issues")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"health_problems.dnk"] <- 0
  aggregation_output_select_multiple[x,"health_problems.no_health_issues"] <- 0
  
  
  #### education_facilities	####
  #### not(selected(., "no_available") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("education_facilities")&!ends_with(".dnk")&!ends_with(".no_available")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"education_facilities.dnk"] <- 0
  aggregation_output_select_multiple[x,"education_facilities.no_available"] <- 0
  
  
  #### action_to_prevent	####
  #### not(selected(., "no_action") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("action_to_prevent")&!ends_with(".dnk")&!ends_with(".no_action")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"action_to_prevent.dnk"] <- 0
  aggregation_output_select_multiple[x,"action_to_prevent.no_action"] <- 0
  
  #### action_to_prevent	####
  #### not(selected(., "no_action") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("action_to_prevent")&!ends_with(".dnk")&!ends_with(".no_action")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"action_to_prevent.dnk"] <- 0
  aggregation_output_select_multiple[x,"action_to_prevent.no_action"] <- 0
  
  #### other_reason_coping	####
  #### not(selected(., "dnk") and count-selected(.) > 1) and not(selected(., "pnta") and count-selected(.) > 1) and not(selected(., "no_reason") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("other_reason_coping")&!ends_with(".dnk")&!ends_with(".pnta")&!ends_with(".no_reason")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"other_reason_coping.dnk"] <- 0
  aggregation_output_select_multiple[x,"other_reason_coping.no_reason"] <- 0
  aggregation_output_select_multiple[x,"other_reason_coping.pnta"] <- 0
  
  #### insecure_areas ####
  #### not(selected(., "dnk") and count-selected(.) > 1) and not(selected(., "pnta") and count-selected(.) > 1) and not(selected(., "no_problems") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("insecure_areas")&!ends_with(".dnk")&!ends_with(".pnta")&!ends_with(".no_problems")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"insecure_areas.dnk"] <- 0
  aggregation_output_select_multiple[x,"insecure_areas.no_problems"] <- 0
  aggregation_output_select_multiple[x,"insecure_areas.pnta"] <- 0
  
  #### foodsecurity_coping_food	####
  #### not(selected(., "dnk") and count-selected(.) > 1) and not(selected(., "pnta") and count-selected(.) > 1) and not(selected(., "no_action") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("foodsecurity_coping_food")&!ends_with(".dnk")&!ends_with(".pnta")&!ends_with(".no_action")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"foodsecurity_coping_food.dnk"] <- 0
  aggregation_output_select_multiple[x,"foodsecurity_coping_food.no_action"] <- 0
  aggregation_output_select_multiple[x,"foodsecurity_coping_food.pnta"] <- 0
  
  #### foodsecurity_access_barriers	####
  #### not(selected(., "dnk") and count-selected(.) > 1) and not(selected(., "pnta") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("foodsecurity_access_barriers")&!ends_with(".dnk")&!ends_with(".no_pnta")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"foodsecurity_access_barriers.dnk"] <- 0
  aggregation_output_select_multiple[x,"foodsecurity_access_barriers.pnta"] <- 0
  
  #### protection_incidents_place	####
  #### not(selected(., "dnk") and count-selected(.) > 1) and not(selected(., "pnta") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("protection_incidents_place")&!ends_with(".dnk")&!ends_with(".no_pnta")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"protection_incidents_place.dnk"] <- 0
  aggregation_output_select_multiple[x,"protection_incidents_place.pnta"] <- 0
  
  #### aap_languages	####
  #### (not(selected(., "pnta") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1)) and count-selected(.)<=3
  x <- which(aggregation_output_select_multiple %>% select(starts_with("aap_languages")&!ends_with(".dnk")&!ends_with(".no_pnta")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"aap_languages.dnk"] <- 0
  aggregation_output_select_multiple[x,"aap_languages.pnta"] <- 0
  
  #### aap_humanitarianassistanceproblems	####
  #### not(selected(., "no_problems") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) and not(selected(., "pnta") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("aap_humanitarianassistanceproblems")&!ends_with(".dnk")&!ends_with(".pnta")&!ends_with(".no_action")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"aap_humanitarianassistanceproblems.dnk"] <- 0
  aggregation_output_select_multiple[x,"aap_humanitarianassistanceproblems.no_problems"] <- 0
  aggregation_output_select_multiple[x,"aap_humanitarianassistanceproblems.pnta"] <- 0
  
  
  #### aap_access_barriers	####
  #### not(selected(., "no_problem") and count-selected(.) > 1) and not(selected(., "dnk") and count-selected(.) > 1) and not(selected(., "pnta") and count-selected(.) > 1)
  x <- which(aggregation_output_select_multiple %>% select(starts_with("aap_access_barriers")&!ends_with(".dnk")&!ends_with(".pnta")&!ends_with(".no_action")) %>% 
               rowSums() >=1 )
  
  aggregation_output_select_multiple[x,"aap_access_barriers.dnk"] <- 0
  aggregation_output_select_multiple[x,"aap_access_barriers.no_problem"] <- 0
  aggregation_output_select_multiple[x,"aap_access_barriers.pnta"] <- 0
  
  
  return(aggregation_output_select_multiple)
}

fn_select_one_mode <- function(x) {
  if(all(is.na(x))){return(NA)}

  uniqx <- unique(na.omit(x))
  
  
  if (length(which(tabulate(match(x, uniqx)) == max(tabulate(match(x, uniqx))))) > 1) {
    return("NC")
  }
  
  uniqx[which.max(tabulate(match(x, uniqx)))]
  
}

fn_select_one_yes_prevalence <- function(x) {
  if(all(is.na(x))){return(NA)}
  
  ifelse(any(x=="yes"), 
         "yes",
         fn_select_one_mode(x))

}

one_sd_mean <- function(x) {
  if(all(is.na(x))){return(NA)}
  x <- na.omit(x)
  if(length(unique(x))==1){
    return(x[1])
  }
  ceiling(mean(x[abs(x - mean(x)) < sd(x)]))
}

is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))

fn_select_one_mode_subset <- function(x,subset_var=NULL,role=NULL) {
  if(all(is.na(x))){return(NA)}
 
  
  if (hasArg(subset_var) & hasArg(role)) {
    
    if(length(which(subset_var %in% role))!=0) {
      x <- x[which(subset_var %in% role)]
    }
    
  }
  
  
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
  
}

one_sd_mean_subset <- function(x,subset_var=NULL,role=NULL) {
  if(all(is.na(x))){return(NA)}
  x <- na.omit(x)
  
  if (hasArg(subset_var) & hasArg(role)) {
    
    if(length(which(subset_var %in% role))!=0) {
      x <- x[which(subset_var %in% role)]
    }
    
  }
  
  if(length(unique(x))==1){
    return(x[1])
  }
  ceiling(mean(x[abs(x - mean(x)) < sd(x)]))
}


