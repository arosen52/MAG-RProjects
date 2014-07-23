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

servtable   <- function (df, rex) {df %>%
                        select(providerid, starts_with(rex)) %>%
                        melt(id.var = "providerid") %>%
                        mutate(type = substr(variable, 5, nchar(as.character(variable))), 
                        variable = substr(variable, 1, 4)) %>%    
                        group_by(variable, type) %>%
                        dcast(variable ~ type) 
                        
}

allservtable <- function(df, rex) {df %>%
                        select(providerid, starts_with(rex)) %>%
                        melt(id.var = c("providerid")) %>%
                        mutate(type = substr(variable, 5, nchar(as.character(variable))), 
                        variable = substr(variable, 1, 4)) %>%    
                        group_by(variable, type) %>%
                        dplyr::summarise(avg = mean(value, na.rm = TRUE)) %>%
                        mutate(Average = percent(round(avg, 2))) %>% 
                        dcast(variable ~ type)
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

tracker_table <- function(id) { 
                                 t_rownames <- c("Maintain changes in client health coverage over time", 
                                                 "Capture all services for PLWHA in a single system", 
                                                 "Record per-service per-client dollar amounts", 
                                                 "Capture services at the subservice or procedure level")
                                 id_df  <- wrma_df %>%
                                 filter(providerid == id) %>%
                                 select(providerid, ch_ins, Q26, record_dollars, proc_or_serv) %>%
                                 melt(id.var = "providerid") %>%
                                   inner_join(
                                     wrma_df %>%
                                       mutate(Q26 = Q26 == "Yes") %>%
                                       select(providerid, ch_ins, Q26, record_dollars, proc_or_serv) %>%
                                       melt(id.var = "providerid") %>%
                                       group_by(variable) %>%
                                       dplyr::summarize(avg = mean(value, na.rm = TRUE)), 
                                   by = "variable") %>%
                                mutate(variable = t_rownames) %>%
                                select(-providerid)
                                
                                id_df$avg <- percent(round(id_df$avg,2 ))
                                return(id_df)
                                                   }