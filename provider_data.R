#require(memisc)
require(knitr)
require(reshape2)
require(dplyr)
require(plyr)
require(rmarkdown)
require(scales)
source('service_limits_coverage.R')

oamc_letters <- letters[1: which(letters == "u")]
ss_letters    <- letters[1: which(letters == "k")]
#wrma <- as.data.set(spss.system.file("//Dolores/Active/Projects/WRMA_Funding_Streams/Phase_1/SurveyData/RW Funding Streams 5.sav"))
load("wrma_df.Rda")

#Add variables for Tracker survey questions
wrma_df['record_dollars'] <- apply(wrma_df %>% select(contains("Q25_.+_No")),1,  min, na.rm= TRUE)
wrma_df['ch_ins'] = regexpr("^We maintain records of changes", wrma_df$Q20) > 0
wrma_df['proc_or_serv'] <- !is.na(wrma_df$Q26b3) | !is.na(wrma_df$Q26b4)

wrma_df[wrma_df == Inf] <- NA
wrma_df  <- wrma_df %>% mutate(record_dollars = ifelse(record_dollars == 0 , 1, ifelse(record_dollars == 1, 0, NA)))

services <-  wrma_df %>% group_by(providerid) %>%
                     dplyr::select(contains("\\bQ1[5-6].[2-4]._r\\b")) %>%
                     mutate_each(funs(. == "Yes")) %>%                     
                     group_by()

#Argh! For loops. Well, they work for now.
for (i in oamc_letters) {
  services <- limits_or_nevcov(services, 15, i, 'limits')
  services <- limits_or_nevcov(services, 15, i, 'nevercov')
}

for (i in ss_letters) {
  services <- limits_or_nevcov(services, 16, i, 'limits')
  services <- limits_or_nevcov(services, 16, i, 'nevercov')
}

services <- services %>% select(providerid, ends_with("limits"), ends_with("nevercov"))
services[services == -Inf] <- NA

prov_data <- wrma_df %>% filter(providerid == 353)
prov_services  <- services %>% filter(providerid == 353)

prov_services[prov_services == 1] <- "Yes"
prov_services[prov_services == 0] <- "No"

dollars  <- wrma_df %>% 
            select(providerid, TOTALREVENUE, RWTOTALREVENUE, 
                   contains("percentrevenue"), 
                   contains("q29|q30"), 
                   contains("percentpart")) %>%
            mutate(rw_share = RWTOTALREVENUE / TOTALREVENUE)

#Characteristics variables for All Grantees
char <- wrma_df %>%
        mutate(pcmh = q7 == "Yes", 
               hh   = regexpr("^Yes", q8) > 0, 
               `MCO Participation` = "", 
               mcaid = !is.na(q11_1) | !is.na(q11_2) | !is.na(q11_3) | !is.na(q11_4), 
               priv  = !is.na(q10_1) | !is.na(q10_2) | !is.na(q10_3) | !is.na(q10_4),
               exch = regexpr("^Yes", q13) > 0
               ) %>%
               dplyr::select(providerid, pcmh, hh, `MCO Participation`, mcaid, priv, exch) 

# Characteristics Table
char_table <- prov_data %>%
             mutate(`Patient centered medical home` = q7, 
                    `Health Home` = q8, 
                    `MCO Participation` = "", 
                     Medicaid = ifelse(!is.na(q11_1) | !is.na(q11_2) | !is.na(q11_3) | !is.na(q11_4), "Yes", "No"), 
                     Private  = ifelse(!is.na(q10_1) | !is.na(q10_2) | !is.na(q10_3) | !is.na(q10_4), "Yes", "No"), 
                    `Insurance Exchange` = q13) %>%
             melt(id.var = "providerid") %>%
             dplyr::select(-providerid) %>%
             inner_join(
               (char %>% 
                 summarize(`Patient centered medical home` = mean(pcmh), 
                           `Health Home` = mean(hh), 
                            Medicaid     = mean(mcaid), 
                            Private      = mean(priv),
                           `Insurance Exchange` = mean(exch)) %>%
                 mutate(`MCO Participation`  = "") %>%
                 melt(measure.vars = c("Patient centered medical home", "Health Home", 
                                       "MCO Participation", "Medicaid", "Private", "Insurance Exchange"))), 
               by = "variable") 

char_table <- char_table %>%   
             mutate(value.y = ifelse(value.y != "", paste(as.character(signif(as.numeric(value.y), 2) * 100), "%", sep = ""), value.y))

names(char_table) <- c("Agency Characteristics", "Your Response", "All Sample Grantees")

# Service Tables for This Provider
oamc_prov  <- servtable(prov_services, "Q15")
ss_prov    <- servtable(prov_services, "Q16")
# services Tables for All Providers
oamc_all    <- allservtable(services, "Q15")
ss_all      <- allservtable(services, "Q16")

# Join Provider Table and All Providers, Name Columns for Output #

oamc_total  <- oamc_prov %>%
             inner_join(oamc_all, by = "variable") %>%
             select(variable, limits.x, limits.y, nevercov.x, nevercov.y) 

names(oamc_total)  <- c("", "Utilization Limits - Your Response", 
                         "Utilization Limits - National Avg.", 
                         "Never Covered - Your Response", 
                         "Never Covered - National Avg.")

ss_total <- ss_prov %>%
            inner_join(ss_all, by = "variable") %>%
            select(variable, limits.x, limits.y, nevercov.x, nevercov.y)

names(ss_total)  <- c("", "Utilization Limits - Your Response", 
                        "Utilization Limits - National Avg.", 
                        "Never Covered - Your Response", 
                        "Never Covered - National Avg.")

funds <- fundstable(dollars, 353)
              