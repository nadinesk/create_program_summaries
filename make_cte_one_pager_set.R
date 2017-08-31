# School Name - LCGMS  X
# Location (include  map?) - LCGMS X
# DBN - LCGMS X
# Contact - ? 
# BPQ (?) - ? 
# Principal - LCGMS X
# Principal contact - LCGMS X
# Enrollment - D:/ptech/DemographicSnapshot201213to201617Public_FINAL1.xlsx", sheet="School X
# Percent of population CTE enrolled - Level types - use 2015-16 and 2016-17 x
# CTE credit accumulation - use 2016-17 data X
# Graduation Rate - 2015-16 data - D:/ptech/2015_2016_HS_SQR_Results_2017_01_05.xlsx, sheet="Student Achievement" X
# College Readiness Index -> same as grad rate, with 'Metric Value - Four-Year College Readiness Index' X
# Funding (VTEA/SAM/SIF/FSF and costs) -website -> http://schools.nyc.gov/AboutUs/funding/schoolbudgets/FY14FairStudentFundingBudget.htm?schoolcode=M392  X
# CTE Programs - and status - CTE master list - SQL pull 
  # use programs unique file, run SQL pull to get program names
# Other OPSR Programs - some master list from retreat
# Industry Partners ? 
# 
# 

library(openxlsx)
library(dtplyr)
library(dplyr)
library(data.table)
library(splitstackshape)
library(plyr)
library(lubridate)
library(lazyeval)
library(RODBC)
library(RODBCext)
library(knitr)
library(reshape2)
library(rvest)
library(RODBC)
library(RODBCext)

setwd("K:/OPSR/MP Planning/Operations, Finance, & Research/Research/OPSR Data Team Files/Nadine/central_repo")

# OPEN DATE AND GRADES #####################################################################################
dbns <- read.csv("D:/cte_one_pager/lcgms.csv") %>%
            filter(Location.Type.Description == 'Career Technical')

dbns_open_grades_contact <- dbns %>%
  select(ATS.System.Code, Location.Name, Open.Date, Grades.Final, Geographical.District.Code, 
         Primary.Address, City, NTA_Name, Principal.Name, Principal.Title, Principal.Phone.Number, 
         BFSC.Director.Name, BFSC.Director.Title, BFSC.Director.Phone) %>%
        rename(c('ATS.System.Code' = 'DBN'))

# TOTAL ENROLLMENT  #####################################################################################

demo_s <- read.xlsx("D:/cte_one_pager/DemographicSnapshot201213to201617Public_FINAL1.xlsx", sheet="School")

demo_s2 <- demo_s %>%
  select(DBN, Year, Total.Enrollment ) 

tbl_df(demo_s2)

demo_s2_melt <- dcast(demo_s2, DBN ~ Year, value.var = "Total.Enrollment")

demo_s2_melt

demo_s3 <- dbns_open_grades_contact %>%
  left_join(demo_s2_melt, by="DBN")


# EXPENDITURE INFORMATION #####################################################################################

exp_stuff <- function(link, df_name, df_name_table, dbn, df_name_table1) {
  df_name <- read_html(link)
  
  df_name_table <- df_name %>%
    html_nodes("table") %>%
    .[[3]] %>%
    html_table()
  
  colnames(df_name_table) = df_name_table[1,]
  df_name_table = df_name_table[-1,]
  df_name_table = df_name_table[1:7,]
  
  df_name_table$DBN <- dbn
  
  names(df_name_table)[1] <- "This.School"
  names(df_name_table)[2] <- "Values"
  
  fn <- paste("D:/ptech/exp_html/", dbn, ".csv", sep="")
  
  write.csv(df_name_table, fn)
  
  df_name_table1 <- dcast(df_name_table, DBN ~ This.School, value.var = "Values")
  
  df_name_table1
  
}

exp_02M135_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/02/M135/AboutUs/Statistics/expenditures.htm', exp_02M135, exp_02M135_table, '02M135',exp_02M135_table1)
exp_02M135_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/02/M135/AboutUs/Statistics/expenditures.htm', exp_02M135, exp_02M135_table, '02M135',exp_02M135_table1)
exp_02M139_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/02/M139/AboutUs/Statistics/expenditures.htm', exp_02M139, exp_02M139_table, '02M139',exp_02M139_table1)
exp_02M280_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/02/M280/AboutUs/Statistics/expenditures.htm', exp_02M280, exp_02M280_table, '02M280',exp_02M280_table1)
exp_02M282_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/02/M282/AboutUs/Statistics/expenditures.htm', exp_02M282, exp_02M282_table, '02M282',exp_02M282_table1)
exp_02M288_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/02/M288/AboutUs/Statistics/expenditures.htm', exp_02M288, exp_02M288_table, '02M288',exp_02M288_table1)
exp_02M393_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/02/M393/AboutUs/Statistics/expenditures.htm', exp_02M393, exp_02M393_table, '02M393',exp_02M393_table1)
exp_02M507_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/02/M507/AboutUs/Statistics/expenditures.htm', exp_02M507, exp_02M507_table, '02M507',exp_02M507_table1)
exp_02M533_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/02/M533/AboutUs/Statistics/expenditures.htm', exp_02M533, exp_02M533_table, '02M533',exp_02M533_table1)
exp_02M546_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/02/M546/AboutUs/Statistics/expenditures.htm', exp_02M546, exp_02M546_table, '02M546',exp_02M546_table1)
exp_02M551_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/02/M551/AboutUs/Statistics/expenditures.htm', exp_02M551, exp_02M551_table, '02M551',exp_02M551_table1)
exp_02M600_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/02/M600/AboutUs/Statistics/expenditures.htm', exp_02M600, exp_02M600_table, '02M600',exp_02M600_table1)
exp_02M615_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/02/M615/AboutUs/Statistics/expenditures.htm', exp_02M615, exp_02M615_table, '02M615',exp_02M615_table1)
exp_02M630_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/02/M630/AboutUs/Statistics/expenditures.htm', exp_02M630, exp_02M630_table, '02M630',exp_02M630_table1)
exp_03M402_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/03/M402/AboutUs/Statistics/expenditures.htm', exp_03M402, exp_03M402_table, '03M402',exp_03M402_table1)
exp_05M157_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/05/M157/AboutUs/Statistics/expenditures.htm', exp_05M157, exp_05M157_table, '05M157',exp_05M157_table1)
exp_06M211_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/06/M211/AboutUs/Statistics/expenditures.htm', exp_06M211, exp_06M211_table, '06M211',exp_06M211_table1)
exp_07X259_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/07/X259/AboutUs/Statistics/expenditures.htm', exp_07X259, exp_07X259_table, '07X259',exp_07X259_table1)
exp_07X522_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/07/X522/AboutUs/Statistics/expenditures.htm', exp_07X522, exp_07X522_table, '07X522',exp_07X522_table1)
exp_07X600_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/07/X600/AboutUs/Statistics/expenditures.htm', exp_07X600, exp_07X600_table, '07X600',exp_07X600_table1)
exp_08X559_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/08/X559/AboutUs/Statistics/expenditures.htm', exp_08X559, exp_08X559_table, '08X559',exp_08X559_table1)
exp_10X264_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/10/X264/AboutUs/Statistics/expenditures.htm', exp_10X264, exp_10X264_table, '10X264',exp_10X264_table1)
exp_10X524_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/10/X524/AboutUs/Statistics/expenditures.htm', exp_10X524, exp_10X524_table, '10X524',exp_10X524_table1)
exp_10X565_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/10/X565/AboutUs/Statistics/expenditures.htm', exp_10X565, exp_10X565_table, '10X565',exp_10X565_table1)
exp_11X275_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/11/X275/AboutUs/Statistics/expenditures.htm', exp_11X275, exp_11X275_table, '11X275',exp_11X275_table1)
exp_13K605_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/13/K605/AboutUs/Statistics/expenditures.htm', exp_13K605, exp_13K605_table, '13K605',exp_13K605_table1)
exp_13K674_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/13/K674/AboutUs/Statistics/expenditures.htm', exp_13K674, exp_13K674_table, '13K674',exp_13K674_table1)
exp_14K558_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/14/K558/AboutUs/Statistics/expenditures.htm', exp_14K558, exp_14K558_table, '14K558',exp_14K558_table1)
exp_14K610_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/14/K610/AboutUs/Statistics/expenditures.htm', exp_14K610, exp_14K610_table, '14K610',exp_14K610_table1)
exp_17K122_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/17/K122/AboutUs/Statistics/expenditures.htm', exp_17K122, exp_17K122_table, '17K122',exp_17K122_table1)
exp_17K600_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/17/K600/AboutUs/Statistics/expenditures.htm', exp_17K600, exp_17K600_table, '17K600',exp_17K600_table1)
exp_17K751_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/17/K751/AboutUs/Statistics/expenditures.htm', exp_17K751, exp_17K751_table, '17K751',exp_17K751_table1)
exp_18K617_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/18/K617/AboutUs/Statistics/expenditures.htm', exp_18K617, exp_18K617_table, '18K617',exp_18K617_table1)
exp_19K615_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/19/K615/AboutUs/Statistics/expenditures.htm', exp_19K615, exp_19K615_table, '19K615',exp_19K615_table1)
exp_19K618_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/19/K618/AboutUs/Statistics/expenditures.htm', exp_19K618, exp_19K618_table, '19K618',exp_19K618_table1)
exp_19K660_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/19/K660/AboutUs/Statistics/expenditures.htm', exp_19K660, exp_19K660_table, '19K660',exp_19K660_table1)
exp_19K764_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/19/K764/AboutUs/Statistics/expenditures.htm', exp_19K764, exp_19K764_table, '19K764',exp_19K764_table1)
exp_21K620_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/21/K620/AboutUs/Statistics/expenditures.htm', exp_21K620, exp_21K620_table, '21K620',exp_21K620_table1)
exp_24Q600_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/24/Q600/AboutUs/Statistics/expenditures.htm', exp_24Q600, exp_24Q600_table, '24Q600',exp_24Q600_table1)
exp_24Q610_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/24/Q610/AboutUs/Statistics/expenditures.htm', exp_24Q610, exp_24Q610_table, '24Q610',exp_24Q610_table1)
exp_26Q315_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/26/Q315/AboutUs/Statistics/expenditures.htm', exp_26Q315, exp_26Q315_table, '26Q315',exp_26Q315_table1)
exp_27Q650_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/27/Q650/AboutUs/Statistics/expenditures.htm', exp_27Q650, exp_27Q650_table, '27Q650',exp_27Q650_table1)
exp_28Q620_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/28/Q620/AboutUs/Statistics/expenditures.htm', exp_28Q620, exp_28Q620_table, '28Q620',exp_28Q620_table1)
exp_29Q243_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/29/Q243/AboutUs/Statistics/expenditures.htm', exp_29Q243, exp_29Q243_table, '29Q243',exp_29Q243_table1)
exp_29Q313_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/29/Q313/AboutUs/Statistics/expenditures.htm', exp_29Q313, exp_29Q313_table, '29Q313',exp_29Q313_table1)
exp_30Q258_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/30/Q258/AboutUs/Statistics/expenditures.htm', exp_30Q258, exp_30Q258_table, '30Q258',exp_30Q258_table1)
exp_30Q301_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/30/Q301/AboutUs/Statistics/expenditures.htm', exp_30Q301, exp_30Q301_table, '30Q301',exp_30Q301_table1)
exp_31R600_table2 <- exp_stuff('http://schools.nyc.gov/SchoolPortals/31/R600/AboutUs/Statistics/expenditures.htm', exp_31R600, exp_31R600_table, '31R600',exp_31R600_table1)


exp_comb <- (rbind(exp_02M135_table2,
                   exp_02M139_table2,
                   exp_02M280_table2,
                   exp_02M282_table2,
                   exp_02M288_table2,
                   exp_02M393_table2,
                   exp_02M507_table2,
                   exp_02M533_table2,
                   exp_02M546_table2,
                   exp_02M551_table2,
                   exp_02M600_table2,
                   exp_02M615_table2,
                   exp_02M630_table2,
                   exp_03M402_table2,
                   exp_05M157_table2,
                   exp_06M211_table2,
                   exp_07X259_table2,
                   exp_07X522_table2,
                   exp_07X600_table2,
                   exp_08X559_table2,
                   exp_10X264_table2,
                   exp_10X524_table2,
                   exp_10X565_table2,
                   exp_11X275_table2,
                   exp_13K605_table2,
                   exp_13K674_table2,
                   exp_14K558_table2,
                   exp_14K610_table2,
                   exp_17K122_table2,
                   exp_17K600_table2,
                   exp_17K751_table2,
                   exp_18K617_table2,
                   exp_19K615_table2,
                   exp_19K618_table2,
                   exp_19K660_table2,
                   exp_19K764_table2,
                   exp_21K620_table2,
                   exp_24Q600_table2,
                   exp_24Q610_table2,
                   exp_26Q315_table2,
                   exp_27Q650_table2,
                   exp_28Q620_table2,
                   exp_29Q243_table2,
                   exp_29Q313_table2,
                   exp_30Q258_table2,
                   exp_30Q301_table2,
                   exp_31R600_table2
                   
))

write.csv(exp_comb, "D:/cte_one_pager/cte_exp/exp_combined.csv")

demo_s4 <- demo_s3 %>%
  left_join(exp_comb, by="DBN")


write.csv(demo_s4, "D:/cte_one_pager/demo_s4cte.csv")

# FSF INFORMATION #####################################################################################


fsf_stuff_combine <-  function(yr1,yr2,yr3,yr4,yr5,yr6,dbn, dbn_substring) {
  
  link_pt1 <- "http://schools.nyc.gov/AboutUs/funding/schoolbudgets/"
  link_pt2 <- "FairStudentFundingBudget.htm?schoolcode="
  
  new_link_yr2 <- paste(link_pt1, yr2, link_pt2, dbn_substring, sep="")
  
  html_output_yr2 <- read_html(new_link_yr2)
  
  col_3_name_yr2 <- paste(yr1, "Actual.Registers", sep=".")
  col_5_name_yr2 <- paste(yr2, "Projected.Register", sep=".")
  col_7_name_yr2 <- paste(yr2, "Projected.Formula", sep=".")
  col_9_name_yr2 <- paste(yr1, "to", yr2, "Register.Change...Register", sep=".")
  col_11_name_yr2 <- paste(yr1, "to", yr2, "Register.Change...Formula", sep=".")
  
  fsf_amt_table_yr2 <- html_output_yr2 %>%
    html_nodes("table") %>%
    .[[14]] %>%
    html_table() %>%
    select(c(2,3,5,7,9,11)) %>%
    rename(c('X2' = 'Title')) %>%
    rename(c('X3' = col_3_name_yr2)) %>%
    rename(c('X5' = col_5_name_yr2)) %>%
    rename(c('X7' = col_7_name_yr2)) %>%
    rename(c('X9' = col_9_name_yr2)) %>%
    rename(c('X11' = col_11_name_yr2)) %>%
    select(-c(1))
  
  #######
  
  new_link_yr3 <- paste(link_pt1, yr3, link_pt2, dbn_substring, sep="")
  
  html_output_yr3 <- read_html(new_link_yr3)
  
  col_3_name_yr3 <- paste(yr2, "Actual.Registers", sep=".")
  col_5_name_yr3 <- paste(yr3, "Projected.Register", sep=".")
  col_7_name_yr3 <- paste(yr3, "Projected.Formula", sep=".")
  col_9_name_yr3 <- paste(yr2, "to", yr3, "Register.Change...Register", sep=".")
  col_11_name_yr3 <- paste(yr2, "to", yr3, "Register.Change...Formula", sep=".")
  
  fsf_amt_table_yr3 <- html_output_yr3 %>%
    html_nodes("table") %>%
    .[[14]] %>%
    html_table() %>%
    select(c(2,3,5,7,9,11)) %>%
    rename(c('X2' = 'Title')) %>%
    rename(c('X3' = col_3_name_yr3)) %>%
    rename(c('X5' = col_5_name_yr3)) %>%
    rename(c('X7' = col_7_name_yr3)) %>%
    rename(c('X9' = col_9_name_yr3)) %>%
    rename(c('X11' = col_11_name_yr3)) %>%
    select(-c(1))
  
  #######
  
  new_link_yr4 <- paste(link_pt1, yr4, link_pt2, dbn_substring, sep="")
  
  html_output_yr4 <- read_html(new_link_yr4)
  
  col_3_name_yr4 <- paste(yr3, "Actual Registers", sep=" ")
  col_5_name_yr4 <- paste(yr4, "Projected-Register", sep=" ")
  col_7_name_yr4 <- paste(yr4, "Projected-Formula", sep=" ")
  col_9_name_yr4 <- paste(yr3, "to", yr4, "Register Change - Register", sep=" ")
  col_11_name_yr4 <- paste(yr3, "to", yr4, "Register Change - Formula", sep=" ")
  
  fsf_amt_table_yr4 <- html_output_yr4 %>%
    html_nodes("table") %>%
    .[[14]] %>%
    html_table() %>%
    select(c(2,3,5,7,9,11)) %>%
    rename(c('X2' = 'Title')) %>%
    rename(c('X3' = col_3_name_yr4)) %>%
    rename(c('X5' = col_5_name_yr4)) %>%
    rename(c('X7' = col_7_name_yr4)) %>%
    rename(c('X9' = col_9_name_yr4)) %>%
    rename(c('X11' = col_11_name_yr4)) %>%
    select(-c(1))
  
  
  #######
  
  new_link_yr5 <- paste(link_pt1, yr5, link_pt2, dbn_substring, sep="")
  
  html_output_yr5 <- read_html(new_link_yr5)
  
  col_3_name_yr5 <- paste(yr4, "Actual Registers", sep=" ")
  col_5_name_yr5 <- paste(yr5, "Projected-Register", sep=" ")
  col_7_name_yr5 <- paste(yr5, "Projected-Formula", sep=" ")
  col_9_name_yr5 <- paste(yr4, "to", yr5, "Register Change - Register", sep=" ")
  col_11_name_yr5 <- paste(yr4, "to", yr5, "Register Change - Formula", sep=" ")
  
  fsf_amt_table_yr5 <- html_output_yr5 %>%
    html_nodes("table") %>%
    .[[14]] %>%
    html_table() %>%
    select(c(2,3,5,7,9,11)) %>%
    rename(c('X2' = 'Title')) %>%
    rename(c('X3' = col_3_name_yr5)) %>%
    rename(c('X5' = col_5_name_yr5)) %>%
    rename(c('X7' = col_7_name_yr5)) %>%
    rename(c('X9' = col_9_name_yr5)) %>%
    rename(c('X11' = col_11_name_yr5)) %>%
    select(-c(1))
  
  #######
  
  new_link_yr6 <- paste(link_pt1, yr6, link_pt2, dbn_substring, sep="")
  
  html_output_yr6 <- read_html(new_link_yr6)
  
  col_3_name_yr6 <- paste(yr5, "Actual Registers", sep=" ")
  col_5_name_yr6 <- paste(yr6, "Projected-Register", sep=" ")
  col_7_name_yr6 <- paste(yr6, "Projected-Formula", sep=" ")
  col_9_name_yr6 <- paste(yr5, "to", yr6, "Register Change - Register", sep=" ")
  col_11_name_yr6 <- paste(yr5, "to", yr6, "Register Change - Formula", sep=" ")
  
  fsf_amt_table_yr6 <- html_output_yr6 %>%
    html_nodes("table") %>%
    .[[14]] %>%
    html_table() %>%
    select(c(2,3,5,7,9,11)) %>%
    rename(c('X2' = 'Title')) %>%
    rename(c('X3' = col_3_name_yr6)) %>%
    rename(c('X5' = col_5_name_yr6)) %>%
    rename(c('X7' = col_7_name_yr6)) %>%
    rename(c('X9' = col_9_name_yr6)) %>%
    rename(c('X11' = col_11_name_yr6)) %>%
    select(-c(1))
  
    
    fsf_c <- cbind(fsf_amt_table_yr2,fsf_amt_table_yr3,fsf_amt_table_yr4,fsf_amt_table_yr5,fsf_amt_table_yr6)
    
    fsf_c$DBN <- dbn
    
    fsf_c
    
    
    
}

fsf_stuff_combine_yr3_5 <-  function(yr3,yr4,yr5,yr6,dbn, dbn_substring) {
  
  link_pt1 <- "http://schools.nyc.gov/AboutUs/funding/schoolbudgets/"
  link_pt2 <- "FairStudentFundingBudget.htm?schoolcode="
  
  fsf_amt_table_yr2 <- data.frame(`FY13 Actual Registers` ='NA', 
                                  `FY14 Projected-Register` ='NA', 
                                  `FY14 Projected-Formula`  ='NA', 
                                  `FY13 to FY14 Register Change - Register`  ='NA',
                                  `FY13 to FY14 Register Change - Formula`  ='NA')
                                  
  fsf_amt_table_yr3 <- data.frame(`FY14 Actual Registers` ='NA', 
                                  `FY15 Projected-Register` ='NA', 
                                  `FY15 Projected-Formula`  ='NA', 
                                  `FY14 to FY15 Register Change - Register`  ='NA',
                                  `FY14 to FY15 Register Change - Formula`  ='NA')
  
  
  #######
  
  new_link_yr4 <- paste(link_pt1, yr4, link_pt2, dbn_substring, sep="")
  
  html_output_yr4 <- read_html(new_link_yr4)
  
  col_3_name_yr4 <- paste(yr3, "Actual Registers", sep=" ")
  col_5_name_yr4 <- paste(yr4, "Projected-Register", sep=" ")
  col_7_name_yr4 <- paste(yr4, "Projected-Formula", sep=" ")
  col_9_name_yr4 <- paste(yr3, "to", yr4, "Register Change - Register", sep=" ")
  col_11_name_yr4 <- paste(yr3, "to", yr4, "Register Change - Formula", sep=" ")
  
  fsf_amt_table_yr4 <- html_output_yr4 %>%
    html_nodes("table") %>%
    .[[14]] %>%
    html_table() %>%
    select(c(2,3,5,7,9,11)) %>%
    rename(c('X2' = 'Title')) %>%
    rename(c('X3' = col_3_name_yr4)) %>%
    rename(c('X5' = col_5_name_yr4)) %>%
    rename(c('X7' = col_7_name_yr4)) %>%
    rename(c('X9' = col_9_name_yr4)) %>%
    rename(c('X11' = col_11_name_yr4)) %>%
    select(-c(1))
  
  
  #######
  
  new_link_yr5 <- paste(link_pt1, yr5, link_pt2, dbn_substring, sep="")
  
  html_output_yr5 <- read_html(new_link_yr5)
  
  col_3_name_yr5 <- paste(yr4, "Actual Registers", sep=" ")
  col_5_name_yr5 <- paste(yr5, "Projected-Register", sep=" ")
  col_7_name_yr5 <- paste(yr5, "Projected-Formula", sep=" ")
  col_9_name_yr5 <- paste(yr4, "to", yr5, "Register Change - Register", sep=" ")
  col_11_name_yr5 <- paste(yr4, "to", yr5, "Register Change - Formula", sep=" ")
  
  fsf_amt_table_yr5 <- html_output_yr5 %>%
    html_nodes("table") %>%
    .[[14]] %>%
    html_table() %>%
    select(c(2,3,5,7,9,11)) %>%
    rename(c('X2' = 'Title')) %>%
    rename(c('X3' = col_3_name_yr5)) %>%
    rename(c('X5' = col_5_name_yr5)) %>%
    rename(c('X7' = col_7_name_yr5)) %>%
    rename(c('X9' = col_9_name_yr5)) %>%
    rename(c('X11' = col_11_name_yr5)) %>%
    select(-c(1))
  
  #######
  
  new_link_yr6 <- paste(link_pt1, yr6, link_pt2, dbn_substring, sep="")
  
  html_output_yr6 <- read_html(new_link_yr6)
  
  col_3_name_yr6 <- paste(yr5, "Actual Registers", sep=" ")
  col_5_name_yr6 <- paste(yr6, "Projected-Register", sep=" ")
  col_7_name_yr6 <- paste(yr6, "Projected-Formula", sep=" ")
  col_9_name_yr6 <- paste(yr5, "to", yr6, "Register Change - Register", sep=" ")
  col_11_name_yr6 <- paste(yr5, "to", yr6, "Register Change - Formula", sep=" ")
  
  fsf_amt_table_yr6 <- html_output_yr6 %>%
    html_nodes("table") %>%
    .[[14]] %>%
    html_table() %>%
    select(c(2,3,5,7,9,11)) %>%
    rename(c('X2' = 'Title')) %>%
    rename(c('X3' = col_3_name_yr6)) %>%
    rename(c('X5' = col_5_name_yr6)) %>%
    rename(c('X7' = col_7_name_yr6)) %>%
    rename(c('X9' = col_9_name_yr6)) %>%
    rename(c('X11' = col_11_name_yr6)) %>%
    select(-c(1))
  
  
  fsf_c <- cbind(fsf_amt_table_yr2,fsf_amt_table_yr3,fsf_amt_table_yr4,fsf_amt_table_yr5,fsf_amt_table_yr6)
  
  fsf_c$DBN <- dbn
  
  fsf_c
  
  
  
}

fsf_c_02M135 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '02M135','M135')
fsf_c_02M139 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '02M139','M139')
fsf_c_02M280 <- fsf_stuff_combine_yr3_5('FY15', 'FY16', 'FY17', 'FY18', '02M280','M280')
fsf_c_02M282 <- fsf_stuff_combine_yr3_5('FY15', 'FY16', 'FY17', 'FY18', '02M282','M282')
fsf_c_02M288 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '02M288','M288')
fsf_c_02M393 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '02M393','M393')
fsf_c_02M507 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '02M507','M507')
fsf_c_02M533 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '02M533','M533')
fsf_c_02M546 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '02M546','M546')
fsf_c_02M551 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '02M551','M551')
fsf_c_02M600 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '02M600','M600')
fsf_c_02M615 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '02M615','M615')
fsf_c_02M630 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '02M630','M630')
fsf_c_03M402 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '03M402','M402')
fsf_c_05M157 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '05M157','M157')
fsf_c_06M211 <- fsf_stuff_combine_yr3_5('FY15', 'FY16', 'FY17', 'FY18', '06M211','M211')
fsf_c_07X259 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '07X259','X259')
fsf_c_07X522 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '07X522','X522')
fsf_c_07X600 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '07X600','X600')
fsf_c_08X559 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '08X559','X559')
fsf_c_10X264 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '10X264','X264')
fsf_c_10X524 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '10X524','X524')
fsf_c_10X565 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '10X565','X565')
fsf_c_11X275 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '11X275','X275')
fsf_c_13K605 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '13K605','K605')
fsf_c_13K674 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '13K674','K674')
fsf_c_14K558 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '14K558','K558')
fsf_c_14K610 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '14K610','K610')
fsf_c_17K122 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '17K122','K122')
fsf_c_17K600 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '17K600','K600')
fsf_c_17K751 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '17K751','K751')
fsf_c_18K617 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '18K617','K617')
fsf_c_19K615 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '19K615','K615')
fsf_c_19K618 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '19K618','K618')
fsf_c_19K660 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '19K660','K660')
fsf_c_19K764 <- fsf_stuff_combine_yr3_5('FY15', 'FY16', 'FY17', 'FY18', '19K764','K764')
fsf_c_21K620 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '21K620','K620')
fsf_c_24Q600 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '24Q600','Q600')
fsf_c_24Q610 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '24Q610','Q610')
fsf_c_26Q315 <- fsf_stuff_combine_yr3_5('FY15', 'FY16', 'FY17', 'FY18', '26Q315','Q315')
fsf_c_27Q650 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '27Q650','Q650')
fsf_c_28Q620 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '28Q620','Q620')
fsf_c_29Q243 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '29Q243','Q243')
fsf_c_29Q313 <- fsf_stuff_combine_yr3_5('FY15', 'FY16', 'FY17', 'FY18', '29Q313','Q313')
fsf_c_30Q258 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '30Q258','Q258')
fsf_c_30Q301 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '30Q301','Q301')
fsf_c_31R600 <- fsf_stuff_combine('FY13', 'FY14', 'FY15', 'FY16', 'FY17', 'FY18', '31R600','R600')


fsc_c_all_cte <- rbind(
  fsf_c_02M135,
  fsf_c_02M139,
  fsf_c_02M280,
  fsf_c_02M282,
  fsf_c_02M288,
  fsf_c_02M393,
  fsf_c_02M507,
  fsf_c_02M533,
  fsf_c_02M546,
  fsf_c_02M551,
  fsf_c_02M600,
  fsf_c_02M615,
  fsf_c_02M630,
  fsf_c_03M402,
  fsf_c_05M157,
  fsf_c_06M211,
  fsf_c_07X259,
  fsf_c_07X522,
  fsf_c_07X600,
  fsf_c_08X559,
  fsf_c_10X264,
  fsf_c_10X524,
  fsf_c_10X565,
  fsf_c_11X275,
  fsf_c_13K605,
  fsf_c_13K674,
  fsf_c_14K558,
  fsf_c_14K610,
  fsf_c_17K122,
  fsf_c_17K600,
  fsf_c_17K751,
  fsf_c_18K617,
  fsf_c_19K615,
  fsf_c_19K618,
  fsf_c_19K660,
  fsf_c_19K764,
  fsf_c_21K620,
  fsf_c_24Q600,
  fsf_c_24Q610,
  fsf_c_26Q315,
  fsf_c_27Q650,
  fsf_c_28Q620,
  fsf_c_29Q243,
  fsf_c_29Q313,
  fsf_c_30Q258,
  fsf_c_30Q301,
  fsf_c_31R600
)

# names(fsc_c_all_cte)
# names(fsc_c_all_cte)[26] <- "DBN"
demo_s4
demo_s5 <- demo_s4 %>%
              left_join(fsc_c_all_cte, by= "DBN")

str(demo_s5)

write.csv(demo_s5, "D:/cte_one_pager/demo_s5.csv")


###### CTE FSF PORTION ########################################


portfolio_stuff_combine <-  function(dbn_substring,dbn,yr2) {
  
  link_pt1 <- "http://schools.nyc.gov/AboutUs/funding/schoolbudgets/"
  link_pt2 <- "FairStudentFundingBudget.htm?schoolcode="
  
  new_link <- paste(link_pt1, yr2, link_pt2, dbn_substring, sep="")
  
  html_output <- read_html(new_link)
  
  col_names <- html_output %>%
    html_nodes("table") %>%
    .[[2]] %>%
    html_table(fill=TRUE)
  
  col_names
  
  portfolio <- html_output %>%
    html_nodes("table") %>%
    .[[12]] %>%
    html_table() %>%
    select(-c(1))
  
  
  portfolio_cte <- html_output %>%
    html_nodes("table") %>%
    .[[13]] %>%
    html_table() %>%
    select(-c(1))
  
  
  names(portfolio) <- sapply(col_names, paste, collapse="/")
  names(portfolio_cte) <- sapply(col_names, paste, collapse="/")
  
  t1 <- rbind(portfolio, portfolio_cte)
  
  t1$DBN <- dbn
  
  t1
  
}



portfolio_02M135 <- portfolio_stuff_combine('M135','02M135','FY18')
portfolio_02M139 <- portfolio_stuff_combine('M139','02M139','FY18')
portfolio_02M280 <- portfolio_stuff_combine('M280','02M280','FY18')
portfolio_02M282 <- portfolio_stuff_combine('M282','02M282','FY18')
portfolio_02M288 <- portfolio_stuff_combine('M288','02M288','FY18')
portfolio_02M393 <- portfolio_stuff_combine('M393','02M393','FY18')
portfolio_02M507 <- portfolio_stuff_combine('M507','02M507','FY18')
portfolio_02M533 <- portfolio_stuff_combine('M533','02M533','FY18')
portfolio_02M546 <- portfolio_stuff_combine('M546','02M546','FY18')
portfolio_02M551 <- portfolio_stuff_combine('M551','02M551','FY18')
portfolio_02M600 <- portfolio_stuff_combine('M600','02M600','FY18')
portfolio_02M615 <- portfolio_stuff_combine('M615','02M615','FY18')
portfolio_02M630 <- portfolio_stuff_combine('M630','02M630','FY18')
portfolio_03M402 <- portfolio_stuff_combine('M402','03M402','FY18')
portfolio_05M157 <- portfolio_stuff_combine('M157','05M157','FY18')
portfolio_06M211 <- portfolio_stuff_combine('M211','06M211','FY18')
portfolio_07X259 <- portfolio_stuff_combine('X259','07X259','FY18')
portfolio_07X522 <- portfolio_stuff_combine('X522','07X522','FY18')
portfolio_07X600 <- portfolio_stuff_combine('X600','07X600','FY18')
portfolio_08X559 <- portfolio_stuff_combine('X559','08X559','FY18')
portfolio_10X264 <- portfolio_stuff_combine('X264','10X264','FY18')
portfolio_10X524 <- portfolio_stuff_combine('X524','10X524','FY18')
portfolio_10X565 <- portfolio_stuff_combine('X565','10X565','FY18')
portfolio_11X275 <- portfolio_stuff_combine('X275','11X275','FY18')
portfolio_13K605 <- portfolio_stuff_combine('K605','13K605','FY18')
portfolio_13K674 <- portfolio_stuff_combine('K674','13K674','FY18')
portfolio_14K558 <- portfolio_stuff_combine('K558','14K558','FY18')
portfolio_14K610 <- portfolio_stuff_combine('K610','14K610','FY18')
portfolio_17K122 <- portfolio_stuff_combine('K122','17K122','FY18')
portfolio_17K600 <- portfolio_stuff_combine('K600','17K600','FY18')
portfolio_17K751 <- portfolio_stuff_combine('K751','17K751','FY18')
portfolio_18K617 <- portfolio_stuff_combine('K617','18K617','FY18')
portfolio_19K615 <- portfolio_stuff_combine('K615','19K615','FY18')
portfolio_19K618 <- portfolio_stuff_combine('K618','19K618','FY18')
portfolio_19K660 <- portfolio_stuff_combine('K660','19K660','FY18')
portfolio_19K764 <- portfolio_stuff_combine('K764','19K764','FY18')
portfolio_21K620 <- portfolio_stuff_combine('K620','21K620','FY18')
portfolio_24Q600 <- portfolio_stuff_combine('Q600','24Q600','FY18')
portfolio_24Q610 <- portfolio_stuff_combine('Q610','24Q610','FY18')
portfolio_26Q315 <- portfolio_stuff_combine('Q315','26Q315','FY18')
portfolio_27Q650 <- portfolio_stuff_combine('Q650','27Q650','FY18')
portfolio_28Q620 <- portfolio_stuff_combine('Q620','28Q620','FY18')
portfolio_29Q243 <- portfolio_stuff_combine('Q243','29Q243','FY18')
portfolio_29Q313 <- portfolio_stuff_combine('Q313','29Q313','FY18')
portfolio_30Q258 <- portfolio_stuff_combine('Q258','30Q258','FY18')
portfolio_30Q301 <- portfolio_stuff_combine('Q301','30Q301','FY18')
portfolio_31R600 <- portfolio_stuff_combine('R600','31R600','FY18')



portfolio_cte_combined <- rbind(portfolio_02M135,
                                portfolio_02M139,
                                portfolio_02M280,
                                portfolio_02M282,
                                portfolio_02M288,
                                portfolio_02M393,
                                portfolio_02M507,
                                portfolio_02M533,
                                portfolio_02M546,
                                portfolio_02M551,
                                portfolio_02M600,
                                portfolio_02M615,
                                portfolio_02M630,
                                portfolio_03M402,
                                portfolio_05M157,
                                portfolio_06M211,
                                portfolio_07X259,
                                portfolio_07X522,
                                portfolio_07X600,
                                portfolio_08X559,
                                portfolio_10X264,
                                portfolio_10X524,
                                portfolio_10X565,
                                portfolio_11X275,
                                portfolio_13K605,
                                portfolio_13K674,
                                portfolio_14K558,
                                portfolio_14K610,
                                portfolio_17K122,
                                portfolio_17K600,
                                portfolio_17K751,
                                portfolio_18K617,
                                portfolio_19K615,
                                portfolio_19K618,
                                portfolio_19K660,
                                portfolio_19K764,
                                portfolio_21K620,
                                portfolio_24Q600,
                                portfolio_24Q610,
                                portfolio_26Q315,
                                portfolio_27Q650,
                                portfolio_28Q620,
                                portfolio_29Q243,
                                portfolio_29Q313,
                                portfolio_30Q258,
                                portfolio_30Q301,
                                portfolio_31R600
)

names(portfolio_cte_combined)[9] <- "FY18Projected-Formula"

portfolio_cte_combined$`FY18Projected-Formula` <- ifelse(portfolio_cte_combined$`FY18Projected-Formula` == "", "$0",portfolio_cte_combined$`FY18Projected-Formula`)
portfolio_cte_combined$FY18Projected_Formula_2 <-gsub('\\$','',portfolio_cte_combined$`FY18Projected-Formula`)
portfolio_cte_combined$FY18Projected_Formula_3 <-as.numeric(gsub(',','',portfolio_cte_combined$FY18Projected_Formula_2))


write.csv(portfolio_cte_combined, "D:/cte_one_pager/portfolio_cte_combined.csv")






############ CTE ENROLLED, CTE CREDITS #################################

cte_any1617 <- read.xlsx("Round 2 Register Count - 8-10-17.xlsx", sheet="8-10-17 Year End Data") %>%
                            filter(Student.type != 'Unenrolled - no CTE credits attempted')

table(cte_any1617$Student.type)

cte_any1516 <- read.csv("K:/OPSR/MP Planning/Operations, Finance, & Research/Research/OPSR Data Team Files/Nadine/central_repo/2015-16.csv")

table(cte_any1516$leveltype)
table(cte_any1617$Student.type)
names(cte_any1617)

cte_filter1617 <- cte_any1617 %>%
                    filter(schooldbn == '02M135' |
                             schooldbn == '02M139' |
                             schooldbn == '02M280' |
                             schooldbn == '02M282' |
                             schooldbn == '02M288' |
                             schooldbn == '02M393' |
                             schooldbn == '02M507' |
                             schooldbn == '02M533' |
                             schooldbn == '02M546' |
                             schooldbn == '02M551' |
                             schooldbn == '02M600' |
                             schooldbn == '02M615' |
                             schooldbn == '02M630' |
                             schooldbn == '03M402' |
                             schooldbn == '05M157' |
                             schooldbn == '06M211' |
                             schooldbn == '07X259' |
                             schooldbn == '07X522' |
                             schooldbn == '07X600' |
                             schooldbn == '08X559' |
                             schooldbn == '10X264' |
                             schooldbn == '10X524' |
                             schooldbn == '10X565' |
                             schooldbn == '11X275' |
                             schooldbn == '13K605' |
                             schooldbn == '13K674' |
                             schooldbn == '14K558' |
                             schooldbn == '14K610' |
                             schooldbn == '17K122' |
                             schooldbn == '17K600' |
                             schooldbn == '17K751' |
                             schooldbn == '18K617' |
                             schooldbn == '19K615' |
                             schooldbn == '19K618' |
                             schooldbn == '19K660' |
                             schooldbn == '19K764' |
                             schooldbn == '21K620' |
                             schooldbn == '24Q600' |
                             schooldbn == '24Q610' |
                             schooldbn == '26Q315' |
                             schooldbn == '27Q650' |
                             schooldbn == '28Q620' |
                             schooldbn == '29Q243' |
                             schooldbn == '29Q313' |
                             schooldbn == '30Q258' |
                             schooldbn == '30Q301' |
                             schooldbn == '31R600' 
                    )

cte_filter1516 <- cte_any1516 %>%
                            filter(dbn == '02M135' |
                                     dbn == '02M139' |
                                     dbn == '02M280' |
                                     dbn == '02M282' |
                                     dbn == '02M288' |
                                     dbn == '02M393' |
                                     dbn == '02M507' |
                                     dbn == '02M533' |
                                     dbn == '02M546' |
                                     dbn == '02M551' |
                                     dbn == '02M600' |
                                     dbn == '02M615' |
                                     dbn == '02M630' |
                                     dbn == '03M402' |
                                     dbn == '05M157' |
                                     dbn == '06M211' |
                                     dbn == '07X259' |
                                     dbn == '07X522' |
                                     dbn == '07X600' |
                                     dbn == '08X559' |
                                     dbn == '10X264' |
                                     dbn == '10X524' |
                                     dbn == '10X565' |
                                     dbn == '11X275' |
                                     dbn == '13K605' |
                                     dbn == '13K674' |
                                     dbn == '14K558' |
                                     dbn == '14K610' |
                                     dbn == '17K122' |
                                     dbn == '17K600' |
                                     dbn == '17K751' |
                                     dbn == '18K617' |
                                     dbn == '19K615' |
                                     dbn == '19K618' |
                                     dbn == '19K660' |
                                     dbn == '19K764' |
                                     dbn == '21K620' |
                                     dbn == '24Q600' |
                                     dbn == '24Q610' |
                                     dbn == '26Q315' |
                                     dbn == '27Q650' |
                                     dbn == '28Q620' |
                                     dbn == '29Q243' |
                                     dbn == '29Q313' |
                                     dbn == '30Q258' |
                                     dbn == '30Q301' |
                                     dbn == '31R600' 
                            )

table(cte_filter1516$leveltype)


dbn_lt_1617 <- as.data.frame(table(cte_filter1617$schooldbn,cte_filter1617$Student.type )) %>%
                            filter(Freq > 0)

dbn_lt_1516 <- as.data.frame(table(cte_filter1516$dbn,cte_filter1516$leveltype )) %>%
                            filter(Freq > 0)

dbn_lt_1516$year <- '2015-16'
dbn_lt_1617$year <- '2016-17'

dbn_lt_1516

dbn_lt_comb <- rbind(dbn_lt_1617, dbn_lt_1516)

dbn_lt_comb$category <- ifelse(dbn_lt_comb$Var2 != "Unenrolled - Attempted CTE credits", "E", "U") 

#table(dbn_lt_comb$year, dbn_lt_comb$category)

dbn_lt_comb_1 <- dbn_lt_comb %>%
            select(-Var2) %>%
            group_by(Var1, category, year) %>%
            summarise_each(funs(sum))

dbn_lt_comb_1

dbn_lt_comb_1_melt <- dcast(dbn_lt_comb_1, Var1 ~ year + category, value.var="Freq") %>%
  rename(c("Var1" = "DBN")) %>%
  rename(c("2015-16_E" = "2015_16_CTE_Enr")) %>%
  rename(c("2015-16_U" = "2015_16_CTE_UnEnr")) %>%
  rename(c("2016-17_E" = "2016_17_CTE_Enr")) %>%
  rename(c("2016-17_U" = "2016_17_CTE_UnEnr")) 
  
  
dbn_lt_comb_1_melt

dbn_lt_comb_1_melt[is.na(dbn_lt_comb_1_melt)] <- 0

sum(dbn_lt_comb_1_melt$`2015_16_CTE_Enr`) + sum(dbn_lt_comb_1_melt$`2015_16_CTE_UnEnr`)
sum(dbn_lt_comb_1_melt$`2016_17_CTE_Enr`) + sum(dbn_lt_comb_1_melt$`2016_17_CTE_UnEnr`)


demo_s6 <- demo_s5 %>%
  left_join(dbn_lt_comb_1_melt, by= "DBN")

str(demo_s6)

write.csv(demo_s6, "D:/cte_one_pager/demo_s6.csv")

##### CTE credits ########
names(cte_filter1617)
cte_cred_1617 <- cte_filter1617 %>%
                    select(schooldbn, Total.CTE.credits.attempted) %>%
                    group_by(schooldbn) %>%
                    summarise_each(funs(sum)) %>%
                    rename(c("schooldbn"  = "DBN")) %>%
                    rename(c("Total.CTE.credits.attempted"  = "sum_CTE_credits")) %>%
                    mutate(year = '2016-17')

cte_cred_1516 <- cte_filter1516 %>%
                    select(dbn, CTE_Credits_Attempted) %>%
                    group_by(dbn) %>%
                    summarise_each(funs(sum)) %>%
                    rename(c("dbn"  = "DBN")) %>%
                    rename(c("CTE_Credits_Attempted"  = "sum_CTE_credits")) %>%
                    mutate(year = '2015-16')


cte_cred_comb <- rbind(cte_cred_1516, cte_cred_1617) 

tbl_df(cte_cred_comb)

cte_cred_comb_melt <- dcast(cte_cred_comb, DBN ~ year, value.var = "sum_CTE_credits") %>%
                          rename(c("2015-16" = "CTE_credits_201516")) %>%
                          rename(c("2016-17" = "CTE_credits_201617"))


cte_cred_comb_melt[is.na(cte_cred_comb_melt)] <- 0

cte_cred_comb_melt


demo_s7 <- demo_s6 %>%
  left_join(cte_cred_comb_melt, by= "DBN")

str(demo_s7)

write.csv(demo_s7, "D:/cte_one_pager/demo_s7.csv")


################# GRADUATION RATE, COLLEGE READINESS INDEX ##########################################

sqr <- read.xlsx("2015_2016_HS_SQR_Results_2017_01_05.xlsx", sheet="Student Achievement")
names(sqr)
sqr1 <- sqr %>%
  select(DBN, `Metric.Value.-.Graduation.Rate,.4.year`, `Metric.Value.-.Graduation.Rate,.6-Year`,`Metric.Value.-.Four-Year.College.Readiness.Index`,
         `Metric.Value.-.College.Readiness.Index,.6-Year` )

sqr_filter <- sqr1 %>%
                filter(DBN == '02M135' |
                                      DBN == '02M139' |
                                      DBN == '02M280' |
                                      DBN == '02M282' |
                                      DBN == '02M288' |
                                      DBN == '02M393' |
                                      DBN == '02M507' |
                                      DBN == '02M533' |
                                      DBN == '02M546' |
                                      DBN == '02M551' |
                                      DBN == '02M600' |
                                      DBN == '02M615' |
                                      DBN == '02M630' |
                                      DBN == '03M402' |
                                      DBN == '05M157' |
                                      DBN == '06M211' |
                                      DBN == '07X259' |
                                      DBN == '07X522' |
                                      DBN == '07X600' |
                                      DBN == '08X559' |
                                      DBN == '10X264' |
                                      DBN == '10X524' |
                                      DBN == '10X565' |
                                      DBN == '11X275' |
                                      DBN == '13K605' |
                                      DBN == '13K674' |
                                      DBN == '14K558' |
                                      DBN == '14K610' |
                                      DBN == '17K122' |
                                      DBN == '17K600' |
                                      DBN == '17K751' |
                                      DBN == '18K617' |
                                      DBN == '19K615' |
                                      DBN == '19K618' |
                                      DBN == '19K660' |
                                      DBN == '19K764' |
                                      DBN == '21K620' |
                                      DBN == '24Q600' |
                                      DBN == '24Q610' |
                                      DBN == '26Q315' |
                                      DBN == '27Q650' |
                                      DBN == '28Q620' |
                                      DBN == '29Q243' |
                                      DBN == '29Q313' |
                                      DBN == '30Q258' |
                                      DBN == '30Q301' |
                                      DBN == '31R600' 
                )




demo_s8 <- demo_s7 %>%
  left_join(sqr_filter, by= "DBN")

str(demo_s8)

write.csv(demo_s8, "D:/cte_one_pager/demo_s8.csv")


##### CTE PROGRAMS #######

pgms_funded <- read.xlsx("Round 2 Register Count - 8-10-17.xlsx", sheet="FY18 Elig Prgms Final") 
tbl_df(pgms_funded)

write.csv(pgms_funded, "D:/cte_one_pager/pgms_funded.csv")


#### OPSR Programs Master List ############

opsr_ml <- read.xlsx("OPSR Master Program Inventory for Sharepoint List.xlsx", sheet="May 2017 Master")

write.csv(opsr_ml, "D:/cte_one_pager/opsr_ml.csv")




### Other ###

names(cte_filter1516)

dbn_pgm_credits <- cte_filter1516 %>%
                      select(dbn, CTE_Credits_Attempted, anycip) %>%
                      group_by(dbn, anycip) %>%
                      summarise_each(funs(sum))

dbn_pgm_credits_melt <- dcast(dbn_pgm_credits, dbn ~ anycip, value.var = "CTE_Credits_Attempted")

dbn_pgm_credits_melt



avg_sch <- exp_stuff("http://schools.nyc.gov/SchoolPortals/01/M696/AboutUs/Statistics/expenditures.htm", exp_avg, exp_avg_table, 'avg',exp_avg_table1)

write.csv(avg_sch, "D:/cte_one_pager/cost_entire_system.csv")


