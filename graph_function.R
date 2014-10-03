matgraph <- function(data = preds, gr, len, width, height) {
  
  library(ggplot2)
  library(dplyr)
  library(scales)
  
  df <- data %>% 
    filter(group == gr) %>% 
    mutate(varlab = gsub("?\\(.+\\)", "", varlab)) %>%
    mutate(varlab = unlist(lapply(strwrap(varlab, width = len, simplify = F), 
                                             paste, collapse = "\n"))) 
  df$relative_diff[df$p_wald_cons > .05] <- NA
  g <- ggplot(df, aes(x = varlab, y = Incident))
  g <- g + geom_tile() + geom_tile(aes(fill = relative_diff) , colour= "black") + theme_grey(base_size = 9) +
           scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0,0)) + 
                           scale_fill_gradient2(name = "Relative \nChange In Risk", low = "blue", 
                                                mid = "white", 
                                                high = "red", 
                                                na.value = "grey", labels = percent, 
                                                guide = "colorbar") + 
                           geom_text(aes(label = ifelse(p_wald_cons <= .05, percent(round(relative_diff, 2)), "")),  size = 4.5) +
                           theme(axis.ticks = element_blank(), line = element_blank(), 
                                 axis.text.y = element_text(size = rel(1.25), colour = "black"), 
                                 axis.text.x = element_text(size = rel(1.25), colour = "black"), 
                                 legend.text = element_text(size = rel(1)), 
                                 legend.title = element_text(size = rel(1))) + ylab("") + xlab("") 
  ggsave(paste(gr, ".png"), width = width, height = height)
print(g)}
