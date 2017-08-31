# Reference 
# http://www.r-bloggers.com/how-to-source-an-r-script-automatically-on-a-mac-using-automator-and-ical/
# http://www.engadget.com/2013/03/18/triggering-applescripts-from-calendar-alerts-in-mountain-lion/

library(knitr)
library(markdown)
library(rmarkdown)

rm(mtcars)

mtcars <- mtcars
## knitr loop

names(mtcars)
cte_data <- read.csv("D:/cte_one_pager/demo_s8.csv", stringsAsFactors = FALSE) %>%
                select(-X)

cte_data1 <- cte_data[1:2,]

rownames(cte_data1) <- cte_data1[,1]

cte_data2 <- cte_data1[,-1]


for (dbn in unique(rownames(cte_data2))){
  rmarkdown::render('D:/cte_rmarkdown_script.Rmd', 
                    output_file =  paste("report_", dbn, '_', Sys.Date(), ".docx", sep=''), 
                    output_dir = 'D:/rpt')
  # for pdf reports  
  #   rmarkdown::render(input = "/Users/majerus/Desktop/R/auto_reporting/test/r_script_pdf.Rmd", 
  #           output_format = "pdf_document",
  #           output_file = paste("test_report_", car, Sys.Date(), ".pdf", sep=''),
  #           output_dir = "/Users/majerus/Desktop/R/auto_reporting/test/reports")
  
}

