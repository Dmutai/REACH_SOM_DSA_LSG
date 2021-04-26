rm(list = ls())

if (!require("pacman")) install.packages("pacman")
p_load(rio,
       tidyverse,
       koboquest,
       crayon,
       hypegrammaR,
       composr)


data <- read.csv("output/Aggregation/aggregation_output.csv",stringsAsFactors = F)


# data[data=="dnk"] <- NA
# data[data=="NC"] <- NA

data_indicators <- data %>% mutate (
  ##### SNFI - Non Critical##### 
  
  #### SNFI index 1 
  ###YS should be the otherway round, 1 is "no" and 0 is "yes".
  ###ASSIL Done
  snfi_nc_index1 = case_when(
    nfi_access == "no" ~ 1,
    nfi_access == "yes" ~ 0,
  ),
  
  #### SNFI index 2
  ###YS the list was short listed to 7 items only for SNFI; would it be easier to the sum of the 7 dummies variables?
  ###ASSIL corrected

  snfi_nc_index2 = case_when(
    
    rowSums(across(c(nfi_items_available.sleep_mats,
                     nfi_items_available.plastic_sheets,
                     nfi_items_available.blankets,
                     nfi_items_available.jerry_cans_buckets,
                     nfi_items_available.cooking_utensils,
                     nfi_items_available.mosquito_nets,
                     nfi_items_available.solar_lamp)),na.rm = T) >=  5  ~ 1 ,
    
    rowSums(across(c(nfi_items_available.sleep_mats,
                     nfi_items_available.plastic_sheets,
                     nfi_items_available.blankets,
                     nfi_items_available.jerry_cans_buckets,
                     nfi_items_available.cooking_utensils,
                     nfi_items_available.mosquito_nets,
                     nfi_items_available.solar_lamp)),na.rm = T) >= 1 ~ 0 
    
    
  ),
  #### SNFI index 3
  ###YS ok
  snfi_nc_index3 = case_when(
    
    sm_selected(nfi_access_distance_min,exactly = "more_60") ~ 1,
    
    sm_selected(nfi_access_distance_min,any = c("less_15","15_30","30_60")) ~ 0,
  ),
  
  #### SNFI index 4
  ###YS ok
  snfi_nc_index4 = case_when(
    
    sm_selected(support,none =  c("shelter_kit", "nfi_kit", "constr_rehab_water", "constr_rehab_latrine", "hygiene_kit")) ~ 1,
    
    sm_selected(support,any = c("shelter_kit", "nfi_kit", "constr_rehab_water", "constr_rehab_latrine", "hygiene_kit") ) ~ 0,
  ),
  
  #### SNFI index 5
  ###YS ok 
  snfi_nc_index5 = case_when(
    evictions_tenureagreement=="yes_written" ~ 0,
    evictions_tenureagreement %in% c("yes_oral","no_agreement") ~ 1
  ),
  
  #### SNFI index 6
  ###YS ok 
  snfi_nc_index6 = case_when(
    shelter_publiclighting == "no" ~ 1,
    shelter_publiclighting == "yes" ~ 0,
  ),
  
  #### SNFI index 7
  ###YS the indicators speaks of the number of % of type of houses; the indicator used is the select multiple
  #should it be the buul, solid_apartment, unfinished, make_shit, none which are the % of the households?
  
  ###ASSIL I think the % in here means the number of a set of options selected par rapport all of the options 
  #  snfi_nc_index7 = case_when(
  #   rowSums(across(c(shelter_types.buul,shelter_types.tent,shelter_types.shelter_kit)))/
  #     rowSums(across(starts_with("shelter_types."))) > 0.5 ~ 1,
  #   
  #   rowSums(across(c(shelter_types.timber_plastic_cgi,shelter_types.cgi_wall_roof,
  #                    shelter_types.mud_stick_cgi,shelter_types.plywood_cgi,
  #                    shelter_types.stone_brick_cgi1,shelter_types.stone_brick_cgi2)))/
  #     rowSums(across(starts_with("shelter_types."))) > 0.5 ~ 0,
  # 
  # ),
  
  #### SNFI index 8
  ###YS why is 0 coded as NA; have the prop equals to zero should mean 0; hence 0 and not NA? 
  ###ASSIl 0 is not coded as NA, actually none of the shelter_flood_destroyed or shelter_fire_destroyed
  # is equal to 0 so the sum is always >0. But when both of the variables are NA and then I divide by
  # cccm_populationestimates_shelters the result is 0 (run this sum(c(NA,NA),na.rm = T)/100) 
  # when it should be NA
  snfi_nc_index8 = case_when(
  
    # rowSums(across(c(shelter_flood_destroyed,shelter_fire_destroyed)),na.rm = T)/
    #   cccm_populationestimates_shelters == 0 ~ as.numeric(NA),
    
    rowSums(across(c(shelter_flood_destroyed,shelter_fire_destroyed)),na.rm = T)/
      cccm_populationestimates_shelters <=0.25 ~ 0,
    
    rowSums(across(c(shelter_flood_destroyed,shelter_fire_destroyed)),na.rm = T)/
      cccm_populationestimates_shelters > 0.25 ~ 1,
   
  ),

  #) %>% select(shelter_flood_destroyed,shelter_fire_destroyed,snfi_nc_index8) %>% View()
  
  
  #### SNFI index 9
  ###YS ok
  snfi_nc_index9 = case_when(
    evictions_notice %in% c("no","dnk") ~ 0,
    evictions_notice == "yes" ~ 1
  ),
  ##### WASH - Critical ##### 
  #### WASH index 1
  ###YS river and other are missing
  #could share the list of improved and un-improved water sources.
  ###ASSIL I shared the list on the excel file and updated the vectors here 
  
  wash_index1 = case_when(
    sm_selected(water_treatment_methods,none=c("boiling", "cloth_filter", "other_filter", "aquatabs") ) &
      water_sources_primary %in%  c("river") ~ 4,
    
    sm_selected(water_treatment_methods,none=c("boiling", "cloth_filter", "other_filter", "aquatabs") ) &
      water_sources_primary %in%  c("unprot_well", "berkad") ~ 3,
    
    sm_selected(water_treatment_methods,any=c("boiling", "cloth_filter", "other_filter", "aquatabs") ) &
      water_sources_primary %in% c("unprot_well", "berkad", "river")  ~ 2,
    
    water_sources_primary %in% c("water_kiosk", "vendors_shop", "piped", "prot_well_no_handpump", "prot_well_handpump", "water_tank_tap", "water_trucking_distrib", "borehole_pump")  ~ 1
    
  ),
  
  #### WASH index 2
  ###YS ok
  wash_index2 = case_when(
    sm_selected(water_access_barriers,any= c("fetching_activity")) ~ 4,
    sm_selected(water_access_barriers,any= c("insufficient_points", "not_functioning","groups_no_access","waterpoints_disabilities")) ~ 3,
    sm_selected(water_access_barriers, any= c("waterpoints_far", "no_water_market", "water_expensive", "no_containers", "poor_quality")) ~ 2,
    sm_selected(water_access_barriers,any = c("no_problem")) ~ 1,
    
  ),
  
  #### WASH index 3
  ###YS ok
  wash_index3 = case_when(
    sm_selected(hygiene_access_impediments,any= c("dangerous")) ~ 4,
    sm_selected(hygiene_access_impediments,any= c("lack_quant", 
                                                  "no_funct_full",
                                                  "too_far",
                                                  "difficult_reach",
                                                  "groups_no_access")) ~ 3,
    sm_selected(hygiene_access_impediments, any= c("unhygienic", 
                                                   "not_privat",
                                                   "no_gender_segr")) ~ 2,
    sm_selected(hygiene_access_impediments,any = c("no_problem")) ~ 1,
    
  ),
  
  ##### WASH - Non Critical ##### 
  
  #### WASH Non critical index 1
  wash_nc_index1 = case_when(
    hygiene_handwashingfacilities == "zerototwentyfive"~ 0,
    hygiene_handwashingfacilities %in% c("twentyfivetofifty",
                                         "fiftytoseventyfive",
                                         "seventyfivetoonehundred") ~ 1,

    
  ),
  
  #### WASH Non critical index 2
  ###YS ok 
  wash_nc_index2 = case_when(
    sanitation_toiletlighting %in% c("zerototwentyfive","twentyfivetofifty") ~ 0,
    sanitation_toiletlighting %in% c("fiftytoseventyfive",
                                         "seventyfivetoonehundred") ~ 1,
    
    
  ),
  
  #### WASH Non critical index 3
  ###YS ok
  wash_nc_index3 = case_when(
    sanitation_access_distance_min %in% c("less_15","15_30","30_60") ~ 0,
    sanitation_access_distance_min %in% c("more_60") ~ 1,

    
  ),

  #### WASH Non critical index 4
  ###YS ok
  wash_nc_index4 = case_when(
    sanitation_solidwastedisposal %in% c("burial_close_houses","burial_close_ws",
                                         "in_open","burning") ~ 1,
    sanitation_solidwastedisposal %in% c("covered_pit","burial_disgnated_areas") ~ 0,
    
    
  ),
  
  #### WASH Non critical index 5
  ###YS ok
  wash_nc_index5 = case_when(
    
    sm_selected(sanitation_access_distance_min,exactly = "more_60") ~ 1,
    
    sm_selected(sanitation_access_distance_min,any = c("less_15","15_30","30_60")) ~ 0,
  ),

  
  
  #### WASH Non critical index 6
  ###YS as above, could you share the listof improved/unimproved source of water.
  ###ASSIL I shared the list on the excel file and updated the vectors here 
  wash_nc_index6 = case_when(
    sm_selected(water_access_distance_min,exactly = "more_60") |
      water_sources_primary %in% c("unprot_well", "berkad", "river")  ~ 1,
    
    sm_selected(water_access_distance_min,any = c("less_15","15_30","30_60") ) &
      water_sources_primary %in% c("water_kiosk", "vendors_shop", "piped", "prot_well_no_handpump", "prot_well_handpump", "water_tank_tap", "water_trucking_distrib", "borehole_pump")  ~ 0,
    

  ),
  
  
  ##### HLT - Critical ##### 
  #### HLT index 1 
  ###YS ok
  health_index1 = case_when(
    health_access_distance_min %in% c("more_60") ~ 4,
    health_access_distance_min %in% c("less_15","15_30","30_60") ~ 1,
  ),
  
  #### HLT index 2
  health_index2 = case_when(
    rowSums(across(c(health_problems.malaria,
                     health_problems.fever,
                     health_problems.awd_cholera,
                     health_problems.resp_problems,
                     health_problems.malnutrition,
                     health_problems.gastrointernal,
                     health_problems.injuries,
                     health_problems.measles)))>=4 ~ 4,
    
    rowSums(across(c(health_problems.malaria,
                     health_problems.fever,
                     health_problems.awd_cholera,
                     health_problems.resp_problems,
                     health_problems.malnutrition,
                     health_problems.gastrointernal,
                     health_problems.injuries,
                     health_problems.measles)))==3 ~ 3,
    
    rowSums(across(c(health_problems.malaria,
                     health_problems.fever,
                     health_problems.awd_cholera,
                     health_problems.resp_problems,
                     health_problems.malnutrition,
                     health_problems.gastrointernal,
                     health_problems.injuries,
                     health_problems.measles)))==2 ~ 2,
    ###YS should be <= 1
    ###YS You are right 
    rowSums(across(c(health_problems.malaria,
                     health_problems.fever,
                     health_problems.awd_cholera,
                     health_problems.resp_problems,
                     health_problems.malnutrition,
                     health_problems.gastrointernal,
                     health_problems.injuries,
                     health_problems.measles)))<=1 ~ 1,
  ),

  #### HLT index 3
  health_index3 = case_when(
    health_women_unskilledhealthpersonnel %in% c("all") ~ 4,
    health_women_unskilledhealthpersonnel %in% c("many","some") ~ 3,
    health_women_unskilledhealthpersonnel %in% c("few") ~ 2,
    health_women_unskilledhealthpersonnel %in% c("none") ~ 1,
  ),
  
  
  ##### HLT - Non Critical ##### 
  #### HLT Non Critical index 1
  ###YS ok
  health_nc_index1 = case_when(
    sm_selected(health_facilities,any = "no_health_facility") ~ 1,
    sm_selected(health_facilities,any = c("first_aid_post", 
                             "pharmacy", 
                             "district_hospital", 
                             "mobile_clinic", 
                             "private_clinic",
                             "ngo_clinic", 
                             "govt_clinic")) ~ 0,
  ),
  
  #### HLT Non Critical index 2
  ###YS ok
  health_nc_index2 = case_when(
    sm_selected(health_services,any = "none") ~ 1,
    sm_selected(health_services,any = c("prim_hc", 
                             "vaccinations",
                             "child_hc", 
                             "maternal_hc", 
                             "nutrition_services",
                             "hiv_counsell_testing", 
                             "mental_health_services")) ~ 0,
  ),
  
  #### HLT Non Critical index 3
  health_nc_index3 = case_when(
    sickness %in% c("all","many","some") ~ 1,
    sickness %in% c("few","none") ~ 0,
  ),
  
  #### HLT Non Critical index 4
  health_nc_index4 = case_when(
    wounds %in% c("all","many","some") ~ 1,
    wounds %in% c("few","none") ~ 0,
  ),
  
  #### HLT Non Critical index 5
  health_nc_index5 = case_when(
    disabilities %in% c("all","many","some") ~ 1,
    disabilities %in% c("few","none") ~ 0,
  ),
  
  #### HLT Non Critical index 6
  health_nc_index6 = case_when(
    mental_health %in% c("all","many","some") ~ 1,
    mental_health %in% c("few","none") ~ 0,
  ),
  
  #### HLT Non Critical index 7
  health_nc_index7 = case_when(
    adeqaute_health_men %in% c("all","many","some") ~ 1,
    adeqaute_health_men %in% c("few","none") ~ 0,
  ),
  
  #### HLT Non Critical index 8
  health_nc_index8 = case_when(
    adeqaute_health_women %in% c("all","many","some") ~ 1,
    adeqaute_health_women %in% c("few","none") ~ 0,
  ),
  
  ###YS this is a critical indicator
  #### HLT Non Critical index 9
  ### Deleted
  # health_nc_index9 = case_when(
  #   health_women_unskilledhealthpersonnel %in% c("all","many","some") ~ 1,
  #   health_women_unskilledhealthpersonnel %in% c("few","none") ~ 0,
  # ),
  
  #### HLT Non Critical index 9
  ###YS ok
  health_nc_index9 = case_when(
    sm_selected(health_barriers, any= c("cost", 
                                        "no_qualified", 
                                        "documents",
                                        "no_referral",
                                        "not_open",
                                        "far_away", 
                                        "refuse_treatment", 
                                        "no_medicine",
                                        "no_treatment_avail", 
                                        "pwd_excluded")) ~ 1,
    sm_selected(health_barriers,any = c("no_problem")) ~ 0,
  ),
  
  ##### Nutrition - Critical ##### 
  #### Nutrition Critical index 1
  ### YS it should be from 1 to 4
  ###ASSIl Corrected 
  nutrition_index1 = case_when(
    nutrition_prop_malnurish %in% c("all") ~ 5,
    nutrition_prop_malnurish %in% c("many") ~ 4,
    nutrition_prop_malnurish %in% c("few") ~ 3,
    nutrition_prop_malnurish %in% c("none") ~ 0,
  ),
  
  ##### Nutrition - Non Critical ##### 
  #### Nutrition Non Critical index 1
  ###YS OK
  nutrition_nc_index1 = case_when(
    nutrition_access_distance_min %in% c("more_60","none_avail") ~ 1,
    nutrition_access_distance_min %in% c("less_15","15_30","30_60") ~ 0,
  ),
  
  #### Nutrition Non Critical index 2
  ###YS ok
  nutrition_nc_index2 = case_when(
    rowSums(across(c(nutrition_services.cost,nutrition_services.no_qualified,nutrition_services.documents,nutrition_services.no_referral,nutrition_services.not_open,nutrition_services.far_away,nutrition_services.refuse_treatment,nutrition_services.no_medicine,nutrition_services.no_treatment_avail,nutrition_services.pwd_excluded))) >=3 ~ 1,
    rowSums(across(c(nutrition_services.cost,nutrition_services.no_qualified,nutrition_services.documents,nutrition_services.no_referral,nutrition_services.not_open,nutrition_services.far_away,nutrition_services.refuse_treatment,nutrition_services.no_medicine,nutrition_services.no_treatment_avail,nutrition_services.pwd_excluded))) <=2 ~ 0,
  ),
  
  ##### FS - Critical ##### 
  #### FS Critical index 1
  ###YS: OK 
  fs_index1 = case_when(
    foodsecurity_livelihood %in% c("all","many","some") ~ 4,
    foodsecurity_livelihood %in% c("few") ~ 3,
    foodsecurity_livelihood %in% c("none") ~ 1,
  ),
  
  #### FSL Critical index 2
  ###YS: OK  but hunger_worst has a BIG trend
  ###Assil I'll check that
  
  # fs_index2 = case_when(
  #   hunger_level %in% c("hunger_worst") ~ 5,
  #   hunger_level %in% c("hunger_severe") ~ 4,
  #   hunger_level %in% c("hunger_small") ~ 2,
  #   hunger_level %in% c("no_hunger") ~ 1,
  # ),
  # 
  ##### FSL - Non Critical ##### 
  #### FSL Non Critical index 1
  ###YS ok
  fs_nc_index1 = case_when(
    foodsecurity_access == "no" ~ 1,
    foodsecurity_access == "yes" ~ 0,
    
  ),
  
  #### FSL Non Critical index 2
  ###YS ok
  fs_nc_index2 = case_when(
    foodsecurity_primary %in% c("food_assist_ngo", 
                                "food_assist_govt", 
                                "gifts_friends_family", 
                                "borrow_debt") ~ 1,
    
    foodsecurity_primary %in% c("market_purchases",
                                "hh_production", 
                                "own_livestock", 
                                "fish_forage_hunt", 
                                "trade_labour") ~ 0,
    
    
  ),
  
  #### FSL Non Critical index 3
  ###YS ok
  fs_nc_index3 = case_when(
    foodsecurity_land_livestock == "no" &
      foodsecurity_land_agriculture == "no" &
      fishing %in% c("none") ~ 1,
    
    foodsecurity_land_livestock == "yes" |
      foodsecurity_land_agriculture == "yes" |
        fishing %in% c("few","some","many","all") ~ 0,
    
    
    
  ),
  
  ##### Education -  Critical ##### 
  #### Education Critical index 1
  ###YS education should not have a score of 4. 
  ###ASSIL Corrected 
  education_index1 = case_when(
    sm_selected(education_barriers_boys,any = c("child_recruited_ag", "displacement_conflict")) ~ 3,
    sm_selected(education_barriers_boys,any = c("security_concerns", "child_lack_documentation", "no_gender_separ", "help_at_home", "costs", "child_pycho_distress", "work_outside_home", "flood", "marriage_pregnant")) ~ 2,
    sm_selected(education_barriers_boys,any = c("no_problem", "no_aware_education_opportunities", "parents_no_value_edu", "parents_no_approve_curric", "cultural_beliefs", "language",
                                                "far_away", "closed", "poor_infrastructure", "lack_quali_staff", "no_wash_at_school", "overcrowded")) ~ 1,
  ),
  
  #### Education Critical index 2
  ###YS education should not have a score of 4. 
  ###ASSIL Corrected 
  education_index2 = case_when(
    sm_selected(education_barriers_girls,any = c("child_recruited_ag", "displacement_conflict")) ~ 3,
    sm_selected(education_barriers_girls,any = c("security_concerns", "child_lack_documentation", "no_gender_separ", "help_at_home", "costs", "child_pycho_distress", "work_outside_home", "flood", "marriage_pregnant")) ~ 2,
    sm_selected(education_barriers_girls,any = c("no_problem", "no_aware_education_opportunities", "parents_no_value_edu", "parents_no_approve_curric", "cultural_beliefs", "language",
                                                 "far_away", "closed", "poor_infrastructure", "lack_quali_staff", "no_wash_at_school", "overcrowded")) ~ 1,
  ),
  
  


  
  ##### Education - Non Critical ##### 
  #### Education Non Critical index 1
  ###YS ok
  education_nc_index1 = case_when(
    sm_selected(education_facilities,any = c("no_available")) ~ 1,
    sm_selected(education_facilities,any = c("primary", "secondary", "quoranic", "basic_edu")) ~ 0,
  ),
  
  #### Education Non Critical index 2
  ###YS ok
  education_nc_index2 = case_when(
    not_functional == 0 ~ 1,
    not_functional >= 1 ~ 0,
  ),
  
  #### Education Non Critical index 3
  ###YS ok 
  education_nc_index3 = case_when(
    education_facilities_watersources == "zerototwentyfive"~ 1,
    education_facilities_watersources %in% c("twentyfivetofifty",
                                         "fiftytoseventyfive",
                                         "seventyfivetoonehundred") ~ 0,
  ),
  
  #### Education Non Critical index 4
  ###YS ok 
  education_nc_index4 = case_when(
    education_facilities_fence == "zerototwentyfive"~ 1,
    education_facilities_fence %in% c("twentyfivetofifty",
                                             "fiftytoseventyfive",
                                             "seventyfivetoonehundred") ~ 0,
  ),
  
  #### Education Non Critical index 5
  ###YS ok 
  education_nc_index5 = case_when(
    education_access_distance_min %in% c("more_60") ~ 1,
    education_access_distance_min %in% c("less_15","15_30","30_60") ~ 0,
  ),
  
  
  #### Education Non Critical index 6
  ###YS ok 
  education_nc_index6 = case_when(
    boys_5_12 %in% c("few","none") ~ 1,
    boys_5_12 %in% c("all","many","some") ~ 0,
  ),
  
  #### Education Non Critical index 7
  ###YS ok 
  education_nc_index7 = case_when(
    girls_5_12 %in% c("few","none") ~ 1,
    girls_5_12 %in% c("all","many","some") ~ 0,
  ),
  
  #### Education Non Critical index 8
  ###YS ok 
  education_nc_index8 = case_when(
    boys_13_17 %in% c("few","none") ~ 1,
    boys_13_17 %in% c("all","many","some") ~ 0,
  ),
  
  #### Education Non Critical index 9
  ###YS ok 
  education_nc_index9 = case_when(
    girls_13_17 %in% c("few","none") ~ 1,
    girls_13_17 %in% c("all","many","some") ~ 0,
  ),
  ##### Protecion - Critical ##### 
  #### Protecion Critical index 1
  ###YS ok
  protection_index1 = case_when(
    sm_selected(protection_incidents,any = c("disappear", "forced_recruit", "armed_violence", "uxo")) ~ 4,
    sm_selected(protection_incidents,any = c("gbv", "arrest_detention", "abduction", "displacement", "violence_aid_distrib", "destruction_property", "unaccomp_child")) ~ 3,
    sm_selected(protection_incidents,any = c("illegal_tax", "inter_communal", "exploit_abuse_access_aid", "land_grabbing", "denied_access_justice")) ~ 2,
    sm_selected(protection_incidents,any = c("no_incidents")) ~ 1,
  ),   

  #### Protecion Critical index 2
  ###YS ok
  protection_index2 = case_when(
    sm_selected(insecure_areas,any = c("in_shelter", "water_point", "latrines", "bathing_areas", "schools", "way_to_school")) ~ 4,
    sm_selected(insecure_areas,any = c("outside_settlement", "way_to_market", "health_centres", "humanitarian_aid", "nutrition_centres", "markets")) ~ 3,
    sm_selected(insecure_areas,any = c("no_problems")) ~ 1,
  ),   
  
  
  ##### Protecion - Non Critical ##### 
  #### Protecion Non Critical index 1
  ###YS ok
  protection_nc_index1 = case_when(
    protection_womenspace == "no" ~ 1,
    protection_womenspace == "yes" ~ 0,
    
  ),
  
  #### Protecion Non Critical index 2
  ###YS ok
  protection_nc_index2 = case_when(
    protection_childfriendlyspace == "no" ~ 1,
    protection_childfriendlyspace == "yes" ~ 0,
    
  ),
  
  #### Protecion Non Critical index 3
  ###YS ok
  protection_nc_index3 = case_when(
    protection_restrictions_day == "yes" ~ 1,
    protection_restrictions_day == "no" ~ 0,
    
  ),
  
  #### Protecion Non Critical index 4
  protection_nc_index4 = case_when(
    protection_restrictions_night == "yes" ~ 1,
    protection_restrictions_night == "no" ~ 0,
    
  )
  )


##### Computing LSGs ##### 
### Following the MSNA guidance for sectoral analysis 

snfi_non_critical_indicators <- c("snfi_nc_index1", "snfi_nc_index2", "snfi_nc_index3", "snfi_nc_index4", "snfi_nc_index5", "snfi_nc_index6", "snfi_nc_index8", "snfi_nc_index9")


wash_critical_indicators <- c("wash_index1","wash_index2","wash_index3")
wash_non_critical_indicators <- c("wash_nc_index1", "wash_nc_index2", "wash_nc_index3", "wash_nc_index4","wash_nc_index5","wash_nc_index6")


health_critical_indicators <- c("health_index1","health_index2","health_index3")
health_non_critical_indicators <- c("health_nc_index1",
                                    "health_nc_index2", "health_nc_index3", "health_nc_index4", "health_nc_index5",
                                    "health_nc_index6", "health_nc_index7", "health_nc_index8", "health_nc_index9")

nutrition_critical_indicators <- c("nutrition_index1")
nutrition_non_critical_indicators <- c("nutrition_nc_index1", "nutrition_nc_index2")


fs_critical_indicators <- c("fs_index1")
fs_non_critical_indicators <- c("fs_nc_index1", "fs_nc_index2", "fs_nc_index3")


education_critical_indicators <- c("education_index1","education_index2")
education_non_critical_indicators <- c("education_nc_index1","education_nc_index2","education_nc_index3", "education_nc_index4","education_nc_index5",
                                       "education_nc_index6","education_nc_index7","education_nc_index8","education_nc_index9")


protection_critical_indicators <- c("protection_index1","protection_index2")
protection_non_critical_indicators <- c("protection_nc_index1", "protection_nc_index2", "protection_nc_index3", "protection_nc_index4")

score_mean <- function(x) {
  case_when(x >2/3 ~ 3,
            x > 1/3 ~ 2,
            x <= 1/3 ~ 1)
}

data_indicators <-  data_indicators %>% 
  mutate(
    mean_nc_snfi = score_mean(rowMeans(select(.,!!!syms(snfi_non_critical_indicators)), na.rm = T)),
    mean_nc_wash = score_mean(rowMeans(select(.,!!!syms(wash_non_critical_indicators)), na.rm = T)),
    mean_nc_hlt = score_mean(rowMeans(select(.,!!!syms(health_non_critical_indicators)), na.rm = T)),
    mean_nc_nutrition = score_mean(rowMeans(select(.,!!!syms(nutrition_non_critical_indicators)), na.rm = T)),
    mean_nc_fs = score_mean(rowMeans(select(.,!!!syms(fs_non_critical_indicators)), na.rm = T)),
    mean_nc_edu = score_mean(rowMeans(select(.,!!!syms(education_non_critical_indicators)), na.rm = T)),
    mean_nc_protection = score_mean(rowMeans(select(.,!!!syms(protection_non_critical_indicators)), na.rm = T))
    
  ) 


data_indicators <-  data_indicators %>% 
  
  mutate(
    
    snfi_score = mean_nc_snfi,
    
    wash_score = pmax(!!!syms(wash_critical_indicators),mean_nc_wash,na.rm = T),
  
    protection_score = pmax(!!!syms(protection_critical_indicators),mean_nc_protection,na.rm = T),
                          
    fs_score = pmax(!!!syms(fs_critical_indicators),mean_nc_fs,na.rm = T),
                   
    health_score = pmax(!!!syms(health_critical_indicators),mean_nc_hlt,na.rm = T),
                     
    nutrition_score = pmax(!!!syms(nutrition_critical_indicators),mean_nc_nutrition,na.rm = T),

    education_score = pmax(!!!syms(education_critical_indicators),mean_nc_edu,na.rm = T)
                       
  ) 

data_indicators <- data_indicators %>% 
  
  mutate(
    
    protection_need = case_when(
      protection_score <= 2 ~ 0,
      protection_score >= 3 ~ 1),
    
    education_need = case_when(
      education_score <= 2 ~ 0,
      education_score >= 3 ~ 1),
    
    nutrition_need = case_when(
      nutrition_score <= 2 ~ 0,
      nutrition_score >= 3 ~ 1),
    
    fs_need = case_when(
      fs_score <= 2 ~ 0,
      fs_score >= 3 ~ 1),
    
    snfi_need = case_when(
      snfi_score <= 2 ~ 0,
      snfi_score >= 3 ~ 1),
    
    wash_need = case_when(
      wash_score <= 2 ~ 0,
      wash_score >= 3 ~ 1),
    
    health_need = case_when(
      health_score <= 2 ~ 0,
      health_score >= 3 ~ 1),
  
  )


data_indicators <- data_indicators %>% 
  
  mutate(
      number_of_needs = rowSums(select(., protection_need, wash_need, health_need, education_need, fs_need, nutrition_need, snfi_need), na.rm=T)
  )


output <- data_indicators %>% select(
                          "idp_code",
                          matches("index[[:digit:]]"),
                          ends_with("_score"),
                          ends_with("_need"),
                          number_of_needs) 


write.csv(data_indicators,'output/Indicators/aggregation_output_plus_lsg.csv',row.names = F)




