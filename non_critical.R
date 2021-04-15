severity <- function(x) {
        x <- mean(x, na.rm = T)
        ifelse(x<0.99,
               ifelse(floor(((round(x,digits = 2)-0.001)/0.3333)+1)>0,
                      floor(((round(x,digits = 2)-0.001)/0.3333)+1),1)
               ,
               3)
}


severity(c(1,1,1))
severity(c(1,1,0)) #gives 3 instead of 2.
severity(c(1,0,0))
severity(c(1,1,1,0))
severity(c(1,1,0,0))
severity(c(1,0,0,0))
severity(c(NA,NA,NA))

non_crit <- function(x) {
        x <- mean(x, na.rm = T) 
        dplyr::case_when(x >2/3 ~ 3,
                  x > 1/3 ~ 2,
                  x <= 1/3 ~ 1)
}

non_crit(c(1,1,1))
non_crit(c(1,1,0))
non_crit(c(1,0,0))
non_crit(c(1,1,1,0))
non_crit(c(1,1,0,0))
non_crit(c(1,0,0,0))
non_crit(c(NA,NA,NA))

non_crit_score <- function(dataframe, cols) {
        dataframe %>% 
                mutate(mean_col = rowMeans(select(.,!!!syms(cols)), na.rm = T), 
                       non_crit_score =         dplyr::case_when(mean_col >2/3 ~ 3,
                                                                 mean_col > 1/3 ~ 2,
                                                                 mean_col <= 1/3 ~ 1)) %>%
                pull(non_crit_score)


}
data_indicators$snfi_non_crit <- non_crit_score(data_indicators, snfi_non_critical_indicators)

data_indicators %>% 
        mutate(snfi_score = pmax(snfi_non_crit,na.rm = T)) %>% 
        select(snfi_score)
