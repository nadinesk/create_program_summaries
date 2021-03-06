# Reference 
# http://www.r-bloggers.com/how-to-source-an-r-script-automatically-on-a-mac-using-automator-and-ical/
# http://www.engadget.com/2013/03/18/triggering-applescripts-from-calendar-alerts-in-mountain-lion/

library(dplyr)
library(knitr)
library(markdown)
library(rmarkdown)

## knitr loop


cte_data <- read.csv("D:/cte_one_pager/demo_s8.csv", stringsAsFactors = FALSE) %>%
                select(-X)

cte_data1 <- cte_data

rownames(cte_data1) <- cte_data1[,1]

cte_data2 <- cte_data1[,-1]


for (dbn in unique(rownames(cte_data2))){
  rmarkdown::render('D:/rpt/cte_markdown_script.Rmd', 
                    output_file =  paste("report_", dbn, '_', Sys.Date(), ".docx", sep=''), 
                    output_dir = 'D:/cte_one_pager/rpts')
 
}

