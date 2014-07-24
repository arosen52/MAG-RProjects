limits_or_nevcov <- function (df=services, main, sub, type) {
  if (type == 'limits') {
    
    varname <- paste('Q', main, sub, type, sep = "")
    rex <- paste('Q', main, sub, '[2-4][a-b]_r', sep = "")
    varmax <- apply(df %>% select(contains(rex)), 1, max, na.rm = TRUE)
    df[varname] <- varmax
    return(df)
    
  } else if (type == 'nevercov') {
    
    varname  <- paste('Q', main, sub, type, sep = "")
    rex <- paste('Q', main, sub, '[2-4]c_r', sep = "")
    varmax <- apply(df %>% select(contains(rex)), 1, max, na.rm = TRUE)
    df[varname] <- varmax
    return(df)
    
  }
}

servtable   <- function (df, id, rex) {
                        x <- df
                        x[x == 1] <- "Yes"
                        x[x == 0] <- "No"
                        x <- x %>%
                        filter(providerid == id) %>%
                        select(providerid, starts_with(rex)) %>%
                        melt(id.var = "providerid") %>%
                        mutate(type = substr(variable, 5, nchar(as.character(variable))), 
                        variable = substr(variable, 1, 4)) %>%    
                        group_by(variable, type) %>%
                        dcast(variable ~ type) 
                        
                        y <-  df %>%
                        select(providerid, starts_with(rex)) %>%
                        melt(id.var = c("providerid")) %>%
                        mutate(type = substr(variable, 5, nchar(as.character(variable))), 
                        variable = substr(variable, 1, 4)) %>%    
                        group_by(variable, type) %>%
                        dplyr::summarise(avg = mean(value, na.rm = TRUE)) %>%
                        mutate(Average = percent(round(as.numeric(avg), 2))) %>% 
                        dcast(variable ~ type)
                        
                        x_total  <- x %>%
                          inner_join(y, by = "variable") %>%
                          select(variable, limits.x, limits.y, nevercov.x, nevercov.y) %>%
                          filter(limits.x != "")
                        
                        names(x_total)  <- c("name", "Utilization Limits - Your Response", 
                                                                   "Utilization Limits - National Avg.", 
                                                                   "Never Covered - Your Response", 
                                                                   "Never Covered - National Avg.")
                                           
                        return(x_total)
}

fundstable <- function(df, id) {x <- df %>%
                            filter(providerid == id) %>%
                            select( 
                                   contains("Q29"),                                   
                                   RWTOTALREVENUE, 
                                   contains("Q30"), 
                                   TOTALREVENUE)
                          
                            table_rownames <- c("Part A", "Part B", "Part C", "Part D", "Part F", 
                                                "Total RWHAP", "Medicaid", "Medicare", "Private Insurance", "Total")
                            names(x)  <- table_rownames
                            x <- melt(x)

                            y <- df %>% 
                            filter(providerid == id) %>%  
                            select(PercentPartA, PercentPartB, PercentPartC, 
                                   PercentPartD, PercentPartF, rw_share, 
                                   PercentRevenueMCAID, PercentRevenueMCARE, 
                                   PercentRevenuePI) %>%
                            mutate(Total = 1)

                            names(y) <- table_rownames
                            y <- melt(y)

                            z <- df %>% 
                              select(PercentPartA, PercentPartB, PercentPartC, 
                                     PercentPartD, PercentPartF, rw_share, 
                                     PercentRevenueMCAID, PercentRevenueMCARE, 
                                     PercentRevenuePI) %>%
                              mutate(Total = 1) 
                            
                            names(z)  <- table_rownames
                            head(z)
                            z <- melt(z) 
                            
                            z <- z %>%  group_by(variable) %>% 
                                 dplyr::summarize(avg = mean(value, na.rm = TRUE))
                            
                          
                            x <- x %>% inner_join(y, by = "variable") %>% inner_join(z, by = "variable")
                            
                            names(x) <- c("Funding Source", "Your Response - $", "Your Response - %", "Average of All Sample Grantees")
                            
                            return(x)

} 

tracker_table <- function(df, id) { 
                                 t_rownames <- c("Maintain changes in client health coverage over time", 
                                                 "Capture all services for PLWHA in a single system", 
                                                 "Record per-service per-client dollar amounts", 
                                                 "Capture services at the subservice or procedure level")
                                 id_df  <- df %>%
                                 filter(providerid == id) %>%
                                 select(providerid, ch_ins, Q26, record_dollars, proc_or_serv) %>%
                                 mutate(ch_ins = ifelse(ch_ins == "TRUE", "Yes", "No"), 
                                        proc_or_serv = ifelse(proc_or_serv == 1, "Yes", "No"),
                                        record_dollars = ifelse(record_dollars == 1, "Yes", "No")) %>%
                                 melt(id.var = "providerid") %>%
                                   inner_join(
                                      df %>%
                                       mutate(Q26 = Q26 == "Yes") %>%
                                       select(providerid, ch_ins, Q26, record_dollars, proc_or_serv) %>%
                                       melt(id.var = "providerid") %>%
                                       group_by(variable) %>%
                                       dplyr::summarize(avg = mean(value, na.rm = TRUE)), 
                                   by = "variable") %>%
                                mutate(variable = t_rownames, 
                                       avg = percent(round(avg, 2))) %>%
                                select(-providerid) 
                                
                                names(id_df) <- c("Whether able to...", "Your Response", "All Sample Grantees")
                                return(id_df)
                                                   }

#Characteristics variables for All Grantees
chartable <- function(df, id)  { ch <- df %>%
                                   mutate(pcmh = q7 == "Yes", 
                                          hh   = regexpr("^Yes", q8) > 0, 
                                         `MCO Participation` = "", 
                                          mcaid = !is.na(q11_1) | !is.na(q11_2) | !is.na(q11_3) | !is.na(q11_4), 
                                          priv  = !is.na(q10_1) | !is.na(q10_2) | !is.na(q10_3) | !is.na(q10_4),
                                          exch = regexpr("^Yes", q13) > 0
                                          ) %>%
                                   dplyr::select(providerid, pcmh, hh, `MCO Participation`, mcaid, priv, exch) 


                            ch_id_table <- df %>%
                              filter(providerid == id) %>%
                              mutate(`Patient centered medical home` = q7, 
                                     `Health Home` = q8, 
                                     `MCO Participation` = "", 
                                     Medicaid = ifelse(!is.na(q11_1) | !is.na(q11_2) | !is.na(q11_3) | !is.na(q11_4), "Yes", "No"), 
                                     Private  = ifelse(!is.na(q10_1) | !is.na(q10_2) | !is.na(q10_3) | !is.na(q10_4), "Yes", "No"), 
                                     `Insurance Exchange` = q13) %>%
                              melt(id.var = "providerid") %>%
                              dplyr::select(-providerid) %>%
                              inner_join(
                                (ch %>% 
                                   summarize(`Patient centered medical home` = mean(pcmh), 
                                             `Health Home` = mean(hh), 
                                             Medicaid     = mean(mcaid), 
                                             Private      = mean(priv),
                                             `Insurance Exchange` = mean(exch)) %>%
                                   mutate(`MCO Participation`  = "") %>%
                                   melt(measure.vars = c("Patient centered medical home", "Health Home", 
                                                         "MCO Participation", "Medicaid", "Private", "Insurance Exchange"))), 
                                by = "variable") 

ch_id_table <- ch_id_table %>%   
  mutate(value.y = ifelse(value.y != "", paste(as.character(signif(as.numeric(value.y), 2) * 100), "%", sep = ""), value.y))

names(ch_id_table) <- c("Agency Characteristics", "Your Response", "All Sample Grantees") 
return(ch_id_table)
}

make_report <- function (x) {
  
  id <- x
  providername <- providernames['Grantee.Name'][which(providernames['providerid']==x),]
  filename <- gsub("'|/", "_", providername)
  oamc_headings <- c('Diagnostic Testing', 'Preventative Care Screenings', 
                     'Immunization', 'Medication Prescription and Support', 
                     'Risk Counseling and Risk Management', 
                     'Specialist Care and Chronic Condition Management')
  
  # Characteristics Table
  
  p_chars <- chartable(wrma_df, id)
  
  # services Tables
  oamc_all    <- oamc_services %>% 
    left_join(servtable(services, id, "Q15"), by = "name") %>%
    select(-name) %>%
    filter(`Utilization Limits - Your Response` != "" & 
             `Never Covered - Your Response`      != "" | 
             OAMCSubservices %in% oamc_headings)
  
  names(oamc_all)[1] <- "OAMC Subservices"
  oamc_all[is.na(oamc_all)] <- ""
  
  ss_all      <- sup_services %>%
    left_join(servtable(services, id, "Q16"), by= "name") %>%
    select(-name)
  names(ss_all)[1] <- "Additional Medical and Support Services"
  
  # Funding Source Table 
  funds <- fundstable(dollars, id)
  
  # Tracker table
  
  tracker <- tracker_table(wrma_df, id)
  
  render('ReportTemplate.Rmd', pdf_document(highlight = "default", toc= TRUE), output_file = paste(filename, ".pdf", sep=""))
}