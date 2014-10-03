library(knitr)
library(dplyr)
library(ggplot2)

providerids <- 353

render('ReportTemplate.Rmd', pdf_document())

provider <- "Giannis"
render('ReportTemplate.Rmd', pdf_document(highlight = "default", toc= FALSE), output_file = paste(provider, ".pdf", sep=""))
render('ReportTemplate.Rmd', word_document(), output_file = paste(provider, ".docx", sep=""))

# #lapply(providers, function(x) {
#   provider <- "Giannis"
#   knit2html('ReportTemplate.Rmd', 
#             output = paste(provider, ".html", sep = ""), 
#             stylesheet = 'custom.css')
# #})