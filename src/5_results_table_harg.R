rm(list = ls())

if (!require("pacman")) install.packages("pacman")
p_load(rio,
       tidyverse,
       koboquest,
       crayon,
       hypegrammaR,
       composr)

source("src/functions/results_table_functions.R")


### Importing data and kobo tool sheets 


koboToolPath = "input/kobo tool/kobo_tool_modified.xlsx"


data <- read.csv("output/Indicators/aggregation_output_plus_lsg.csv",stringsAsFactors = F)


questions = import(koboToolPath,sheet="survey") %>% select(-1) %>% filter(!is.na(name))

choices = import(koboToolPath,sheet="choices")

### Creating the questionnaire object
questionnaire <- load_questionnaire(data = data,
                                    questions = questions,
                                    choices = choices,
                                    choices.label.column.to.use = "label::English (en)")



#### Converting scores and indices to characters

data <- data %>% mutate_at(c("snfi_nc_index1", "snfi_nc_index2", "snfi_nc_index3", "snfi_nc_index4", "snfi_nc_index5", "snfi_nc_index6", "snfi_nc_index8", "snfi_nc_index9", "wash_index1", "wash_index2", "wash_index3", "wash_nc_index1", "wash_nc_index2", "wash_nc_index3", "wash_nc_index4", "wash_nc_index5", "wash_nc_index6", "health_index1", "health_index2", "health_index3", "health_nc_index1", "health_nc_index2", "health_nc_index3", "health_nc_index4", "health_nc_index5", "health_nc_index6", "health_nc_index7", "health_nc_index8", "health_nc_index9", "nutrition_index1", "nutrition_nc_index1", "nutrition_nc_index2", "fs_index1", "fs_nc_index1", "fs_nc_index2", "fs_nc_index3", "education_index1", "education_index2", "education_nc_index1", "education_nc_index2", "education_nc_index3", "education_nc_index4", "education_nc_index5", "education_nc_index6", "education_nc_index7", "education_nc_index8", "education_nc_index9", "protection_index1", "protection_index2", "protection_nc_index1", "protection_nc_index2", "protection_nc_index3", "protection_nc_index4", "snfi_score", "wash_score", "protection_score", "fs_score", "health_score", "nutrition_score", "education_score", "protection_need", "education_need", "nutrition_need", "fs_need", "snfi_need", "wash_need", "health_need","number_of_needs")
                           ,as.character)


#### Remove non needed variables 

dataForAnalysis <- data %>% select(-idp_code,-localisation_settlement_name,
                                   -localisation_region,
                                   -cccm_district_origin_first,-cccm_district_origin_second,
                                   -cccm_idps_origin_first,-cccm_idps_origin_second)

target_settlements <- c("DSA4-SO12-015", "DSA4-SO12-015", 
                        "DSA4-SO12-016", "DSA4-SO12-017", "DSA4-SO12-018", "DSA4-SO12-019", "DSA4-SO12-020", "DSA4-SO12-022",
                       " DSA4-SO12-023",
                        "DSA4-SO12-025",
                        "DSA4-SO12-028",
                        "DSA4-SO12-033",
                        "DSA4_SO12_038")


### Running the analysis

dataForAnalysis <- dataForAnalysis %>% 
                          filter()


{
  start_time <- Sys.time()
  results_table <- table_maker(data = dataForAnalysis,
                               questionnaire_object = questionnaire,
                               questionnaire = questions,
                               choices = choices,
                               weighting_function = NULL,
                               labels = T,
                               language = "English",
                               main_col_name = "Somalia",
                               "localisation_district"

  )
  end_time <- Sys.time() ; execution = end_time - start_time   
}



write.csv(results_table,"output/Results table/results_table_output.csv", row.names = F)


