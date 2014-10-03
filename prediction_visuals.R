library(ggplot2)
library(scales)
library(dplyr)
library(rmarkdown)

setwd("//Dolores/Active/Projects/DDS/NONMORTALITY/RISK MODEL/CharacteristicsUpdate")
preds2 <- read.csv("predictions_Jul2013_Jun2014_nose.csv", stringsAsFactors = FALSE)
preds <- read.csv("predictions_Jul2013_Jun2014.csv", stringsAsFactors = FALSE)
preds2 <- preds2 %>% select(variable, incident, p_wald_cons)

incs <- list(unique(preds$incident))

incs['incnodeath'] <- "Any Non-Mortality"
incs['death']      <- "Mortality"
incs['neglect']    <- "Suspected Neglect"
incs['meder']      <- "Medication Error"
incs['abuse']      <- "Suspected Abuse"
incs['mhosps']     <- "Unplanned \nPsychiatric Hospitalization"
incs['hospitnops'] <- "Unplanned \nMedical Hospitalization"
incs['crime']      <- "Victim of Crime"
incs['missing']    <- "Missing Person"
incs['injurynome'] <- "Injury"

incs2 <- data.frame(incident = unlist(incs)[1:10], 
                   Incident = unlist(incs)[11:20], stringsAsFactors = FALSE)


preds <- preds %>% group_by(incident) %>%
         left_join(preds2, by = c("incident", "variable")) %>%
         mutate(base = b[which(variable == "_cons")], 
                #relative_diff = (b / b[which(variable == "_cons")]) -1 ) %>%
                relative_diff = (b / b[which(variable == "_cons")]) -1) %>%
         inner_join(incs2, by = "incident")
        

preds$varlab[preds$variable == "1.dual_diag"] <- "Dual Diagnoses"
save(preds, file = "predictiondata.RData")

render("report_output.Rmd", output_file = "Characterizing Update.pdf")
