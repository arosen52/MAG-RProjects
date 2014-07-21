require(memisc)
require(knitr)
require(reshape2)
require(dplyr)
require(rmarkdown)

wrma <- as.data.set(spss.system.file("//Dolores/Active/Projects/WRMA_Funding_Streams/Phase_1/SurveyData/RW Funding Streams 5.sav"), 
                    stringsAsFactors = FALSE)

wrma_df <- data.frame(wrma, stringsAsFactors = FALSE)

char <- wrma_df %>%
        mutate(pcmh = q7 == "Yes", 
               hh   = regexpr("^Yes", q8) > 0, 
               `MCO Participation` = "", 
               mcaid = !is.na(q11_1) | !is.na(q11_2) | !is.na(q11_3) | !is.na(q11_4), 
               priv  = ) %>%
        dplyr::select(providerid, pcmh, hh, `MCO Participation`, mcaid) # MASS conflicts with dplyr select

prov_char <- wrma_df %>%
             filter(providerid == 353) %>%
             mutate(`Patient centered medical home` = q7, 
                    `Health Home` = q8, 
                    `MCO Participation` = "", 
                    `  Medicaid` = ifelse(!is.na(q11_1) | !is.na(q11_2) | !is.na(q11_3) | !is.na(q11_4), "Yes", "No")) %>%
             melt(id.var = "providerid") %>%
             dplyr::select(-providerid) %>%
             inner_join(
               (char %>% 
                 summarize(`Patient centered medical home` = mean(pcmh), 
                           `Health Home` = mean(hh), 
                           `  Medicaid`  = mean(mcaid)) %>%
                 mutate(`MCO Participation`  = "") %>%
                 melt(measure.vars = c("Patient centered medical home", "Health Home", "MCO Participation", "  Medicaid"))), 
               by = "variable") 

prov_char <- prov_char %>% 
             mutate(value.y = ifelse(value.y != "", paste(as.character(signif(as.numeric(value.y), 2) * 100), "%", sep = ""), value.y))

names(prov_char) <- c("Agency Characteristics", "Your Response", "All Sample Grantees")
provider <- "Giannis"
render('ReportTemplate.Rmd', pdf_document(highlight = "default", toc= FALSE), output_file = paste(provider, ".pdf", sep=""))
render('ReportTemplate.Rmd', word_document(), output_file = paste(provider, ".docx", sep=""))
