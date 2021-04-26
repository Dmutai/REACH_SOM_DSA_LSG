
rm(list = ls())

#####################################

source("src/1_aggregation.R") ## output : output/Aggregation/aggregation_output.csv

source("src/2_indicators.R") ## output : output/Indicators/aggregation_output_plus_lsg.csv

source("src/3_results_table.R") ## output : output/Results table/results_table_output.csv

source("src/4_data_merge.R") ## output : output/Data merge/data_merge_output.csv

#####################################