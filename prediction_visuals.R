library(ggplot2)
library(scales)
library(dplyr)

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
                relative_diff = ifelse(p_wald_cons <= .05, (b / b[which(variable == "_cons")]) -1, NA)) %>%
         inner_join(incs2, by = "incident")
        

g <- ggplot(preds %>% filter(group == "Chronic Conditions"), aes(x = varlab, y = Incident))
g + geom_tile(aes(fill = relative_diff) , colour= "black") + theme_grey(base_size = 9) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0,0)) + 
  scale_fill_gradient2(name = "Relative \nDifference", low = "blue", mid = "white", high = "red", na.value = "grey", labels = percent) + 
  geom_text(aes(label = ifelse(pvalue <= .05, percent(round(relative_diff, 2)), ""))) +
  theme(axis.ticks = element_blank(), line = element_blank(), 
        axis.text.y = element_text(size = rel(1.5), colour = "black"), 
        axis.text.x = element_text(size = rel(1.5), colour = "black"), 
        legend.text = element_text(size = rel(1)), 
        legend.title = element_text(size = rel(1.5))) + ylab("") + xlab("")
