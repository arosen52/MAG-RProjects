#require(memisc)
require(knitr)
require(reshape2)
require(dplyr)
require(plyr)
require(rmarkdown)
require(scales)
source('service_limits_coverage.R')

# Create objects and load data sets you will need for the reports
oamc_letters  <- letters[1: which(letters == "u")]
ss_letters    <- letters[1: which(letters == "k")]

providernames <- read.csv("//DOLORES/Active/Projects/WRMA_Funding_Streams/Phase_1/SurveyData/providernames&ids.csv", 
                          stringsAsFactors = FALSE) 
providernames <- providernames %>% select(providerid, Grantee.Name)

oamc_services <- read.csv("//DOLORES/Active/Projects/WRMA_Funding_Streams/Phase_1/SurveyData/CoreServDefinitions.csv", 
                          stringsAsFactors = FALSE) 

sup_services  <- read.csv("//DOLORES/Active/Projects/WRMA_Funding_Streams/Phase_1/SurveyData/SupServDefinitions.csv", 
                          stringsAsFactors = FALSE) 

load("wrma_df.Rda")

#Add variables for Tracker survey questions
wrma_df['record_dollars'] <- apply(wrma_df %>% select(contains("Q25_.+_No")),1,  min, na.rm= TRUE)
wrma_df['ch_ins'] = regexpr("^We maintain records of changes", wrma_df$Q20) > 0
wrma_df['proc_or_serv'] <- !is.na(wrma_df$Q26b3) | !is.na(wrma_df$Q26b4)
wrma_df  <- wrma_df %>% mutate(record_dollars = ifelse(record_dollars == 0 , 1, ifelse(record_dollars == 1, 0, NA)))
wrma_df[wrma_df == Inf] <- NA

# Select the Services variables
services <-  wrma_df %>% group_by(providerid) %>%
                     dplyr::select(contains("\\bQ1[5-6].[2-4]._r\\b")) %>%
                     mutate_each(funs(. == "Yes")) %>%                     
                     group_by()

#Argh! For loops. Well, they work for now. Looks across insurance types
# for each service and identifies where there were utilization limits/
# clinical requirements or never covered.
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


dollars  <- wrma_df %>% 
            select(providerid, TOTALREVENUE, RWTOTALREVENUE, 
                   contains("percentrevenue"), 
                   contains("q29|q30"), 
                   contains("percentpart")) %>%
            mutate(rw_share = RWTOTALREVENUE / TOTALREVENUE)

ids <- wrma_df %>% select(providerid) 

make_report(210)
#lapply(ids[c(1,3, 5, 7, 9),], make_report)
