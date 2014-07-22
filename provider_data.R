require(memisc)
require(knitr)
require(reshape2)
require(dplyr)
require(rmarkdown)
require(plyr)

wrma <- as.data.set(spss.system.file("//Dolores/Active/Projects/WRMA_Funding_Streams/Phase_1/SurveyData/RW Funding Streams 5.sav"))
load("wrma_df.Rda")

services <-  wrma_df %>% group_by(providerid) %>%
                     select(contains("\\bQ1[5-6].[2-4].+_r\\b")) %>% 
                     mutate_each(funs(ifelse(. == "Yes", 1, 0))) %>%
                     group_by()

services$oamc_util_clin <- apply(services %>% select(contains("\\bQ15.[2-4][a-b]_r\\b")), 1, max, na.rm = TRUE)
services$oamc_nev_cov   <- apply(services %>% select(contains("\\bQ15.[2-4]c_r\\b")), 1, max, na.rm = TRUE)

services$ss_util_clin  <- apply(services %>% select(contains("\\bQ16.[2-4][a-b]_r\\b")), 1, max, na.rm = TRUE)
services$ss_nev_cov    <- apply(services %>% select(contains("\\bQ16.[2-4]c_r\\b")), 1, max, na.rm = TRUE)

prov_data <- wrma_df %>% filter(providerid == 353)
prov_services  <- services %>% (providerid == 353)

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

oamc_table  <- services %>%
               select(oamc_clin_util, oamc_nev_cov) %>%
               summarize()