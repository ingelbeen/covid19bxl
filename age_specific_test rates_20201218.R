####################################################
# BXL CONTACT TRACING ANALYSES                     #
# AGE-SPECIFIC TEST RATES                          #
####################################################
# last update 081220 by Brecht Ingelbeen

rm(list = ls())

# SET OUTPUT DIRECTORY
OutputDirectory <- "./report_outputs/"
OutputDirectoryPaper <- "./report_outputs/figures_paper/"
OutputDirectoryData <- "../paper/data/clean/regression/"

PACKAGES = c(
  "readxl", "writexl", "lubridate", "zoo", "ggplot2", "scales","reshape","boot","lubridate",
  "epicontacts", "tidyverse", "formattable", "igraph", "viridis", "ggthemes"
)

# install packages if needed
for (pack_name in PACKAGES) {
  if (!pack_name %in% rownames(installed.packages()))
    install.packages(pack_name)
}

# load package
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, scales,epitrix,lubridate,
               epicontacts, tidyverse, formattable, igraph, Hmisc, viridis,ggthemes)
# pacman::p_load(here,readxl,lubridate,haven,dplyr,ggplot2,scales,zoo,reshape2,tidyr,stringr,wesanderson,tidyr,epitools,knitr,
#                forcats, rio,patchwork,sitrep,linelist,matchmaker,incidence,aweek,epitrix,sf,ggspatial, backports,rgdal,
#                RColorBrewer,surveillance, broom, haven, skimr, tidyverse, visdat, pander)

# LOAD IN FUNCTIONS
source("./functions/multiplot.R")

# Import test dataset
testsbxl <- read.csv("./data/clean/tests_bxl.csv",sep=";")
#testsbxl <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/COVID19_Bel/COVID19_Bxl/Data_Bxl_041220/testsbxl.csv")
#testsbxl <- read.csv("/Users/laurene/Desktop/EPIET 2018/ITM COVID-19/Brussel data/Pseudonymising data/Paper/tests_bxl.csv", sep=";")

# import population data
Bxl_population_2020 <- read.csv("./data/clean/Bxl_population_2020.csv")
#Bxl_population_2020 <- read.csv("/Users/laurene/Desktop/EPIET 2018/ITM COVID-19/Brussel data/Pseudonymising data/Paper/Bxl_population_2020.csv")

# summarise counts
# testsbxl$date <- as.Date(testsbxl$Date_Stat)
testsbxl$date <- date(dmy(testsbxl$Date_Stat))

testsbxl$AGEGROUP <- as.character(testsbxl$AGR)
testsbxl$AGEGROUP[testsbxl$AGEGROUP=="0-10"] <- "0-9"
testsbxl$AGEGROUP[testsbxl$AGEGROUP=="okt/19"] <- "10-19"
testsbxl$AGEGROUP[testsbxl$AGEGROUP=="70-79"] <- "70+"
testsbxl$AGEGROUP[testsbxl$AGEGROUP=="80-89"] <- "70+"
testsbxl$AGEGROUP[testsbxl$AGEGROUP=="90+"] <- "70+"

tests_by_date <- testsbxl %>%
  filter(AGEGROUP!="UNK") %>%
  group_by(date, AGEGROUP) %>%
  summarise(pos = sum(POS), total = sum(TOTAL))
head(tests_by_date)

# all ages combined
testsall <- tests_by_date %>%
  group_by(date) %>%
  summarise(totalallages=sum(total))
# column with all dates
dates <- subset(testsall, select = date)

# 0-9 years
tests09 <- subset(tests_by_date, AGEGROUP == "0-9" & date > "2020-07-27")
tests09 <- merge(tests09, dates, all.y = TRUE)
tests09$total[is.na(tests09$total)] <- 0
tests09ts <- zoo(tests09$total, order.by = tests09$date)
tests09MA7days <- rollapply(tests09ts, width=7, FUN=mean, align='center')
tests09ts <- merge(tests09ts, tests09MA7days)
tests09ts_df <- data.frame(tests09ts)
tests09ts_df <- tibble::rownames_to_column(tests09ts_df, "date")
tests09ts_df$date <- as.Date(tests09ts_df$date)

cases09 <- subset(tests_by_date, AGEGROUP == "0-9" & date > "2020-07-27")
cases09 <- merge(cases09, dates, all.y = TRUE)
cases09$pos[is.na(cases09$pos)] <- 0
cases09ts <- zoo(cases09$pos, order.by = cases09$date)
cases09MA7days <- rollapply(cases09ts, width=7, FUN=mean, align='center')
cases09ts <- merge(cases09ts, cases09MA7days)
cases09ts_df <- data.frame(cases09ts)
cases09ts_df <- tibble::rownames_to_column(cases09ts_df, "date")
cases09ts_df$date <- as.Date(cases09ts_df$date)

positivity09 <- merge(tests09ts_df, cases09ts_df, by = "date", all = TRUE)
positivity09$pct09 <- positivity09$cases09MA7days*100/positivity09$tests09MA7days
head(positivity09)  

# 10-19y
tests1019 <- subset(tests_by_date, AGEGROUP == "10-19" & date > "2020-07-27")
tests1019 <- merge(tests1019, dates, all.y = TRUE)
tests1019$total[is.na(tests1019$total)] <- 0
tests1019ts <- zoo(tests1019$total, order.by = tests1019$date)
tests1019MA7days <- rollapply(tests1019ts, width=7, FUN=mean, align='center')
tests1019ts <- merge(tests1019ts, tests1019MA7days)
tests1019ts_df <- data.frame(tests1019ts)
tests1019ts_df <- tibble::rownames_to_column(tests1019ts_df, "date")
tests1019ts_df$date <- as.Date(tests1019ts_df$date)

cases1019 <- subset(tests_by_date, AGEGROUP == "10-19" & date > "2020-07-27")
cases1019 <- merge(cases1019, dates, all.y = TRUE)
cases1019$pos[is.na(cases1019$pos)] <- 0
cases1019ts <- zoo(cases1019$pos, order.by = cases1019$date)
cases1019MA7days <- rollapply(cases1019ts, width=7, FUN=mean, align='center')
cases1019ts <- merge(cases1019ts, cases1019MA7days)
cases1019ts_df <- data.frame(cases1019ts)
cases1019ts_df <- tibble::rownames_to_column(cases1019ts_df, "date")
cases1019ts_df$date <- as.Date(cases1019ts_df$date)

positivity1019 <- merge(tests1019ts_df, cases1019ts_df, by = "date", all = TRUE)
positivity1019$pct1019 <- positivity1019$cases1019MA7days*100/positivity1019$tests1019MA7days
head(positivity1019)  

# 20-29y 
tests2029 <- subset(tests_by_date, AGEGROUP == "20-29" & date > "2020-07-27")
tests2029 <- merge(tests2029, dates, all.y = TRUE)
tests2029$total[is.na(tests2029$total)] <- 0
tests2029ts <- zoo(tests2029$total, order.by = tests2029$date)
tests2029MA7days <- rollapply(tests2029ts, width=7, FUN=mean, align='center')
tests2029ts <- merge(tests2029ts, tests2029MA7days)
tests2029ts_df <- data.frame(tests2029ts)
tests2029ts_df <- tibble::rownames_to_column(tests2029ts_df, "date")
tests2029ts_df$date <- as.Date(tests2029ts_df$date)

cases2029 <- subset(tests_by_date, AGEGROUP == "20-29" & date > "2020-07-27")
cases2029 <- merge(cases2029, dates, all.y = TRUE)
cases2029$pos[is.na(cases2029$pos)] <- 0
cases2029ts <- zoo(cases2029$pos, order.by = cases2029$date)
cases2029MA7days <- rollapply(cases2029ts, width=7, FUN=mean, align='center')
cases2029ts <- merge(cases2029ts, cases2029MA7days)
cases2029ts_df <- data.frame(cases2029ts)
cases2029ts_df <- tibble::rownames_to_column(cases2029ts_df, "date")
cases2029ts_df$date <- as.Date(cases2029ts_df$date)

positivity2029 <- merge(tests2029ts_df, cases2029ts_df, by = "date", all = TRUE)
positivity2029$pct2029 <- positivity2029$cases2029MA7days*100/positivity2029$tests2029MA7days
head(positivity2029)  

# 30-39y
tests3039 <- subset(tests_by_date, AGEGROUP == "30-39" & date > "2020-07-27")
tests3039 <- merge(tests3039, dates, all.y = TRUE)
tests3039$total[is.na(tests3039$total)] <- 0
tests3039ts <- zoo(tests3039$total, order.by = tests3039$date)
tests3039MA7days <- rollapply(tests3039ts, width=7, FUN=mean, align='center')
tests3039ts <- merge(tests3039ts, tests3039MA7days)
tests3039ts_df <- data.frame(tests3039ts)
tests3039ts_df <- tibble::rownames_to_column(tests3039ts_df, "date")
tests3039ts_df$date <- as.Date(tests3039ts_df$date)

cases3039 <- subset(tests_by_date, AGEGROUP == "30-39" & date > "2020-07-27")
cases3039 <- merge(cases3039, dates, all.y = TRUE)
cases3039$pos[is.na(cases3039$pos)] <- 0
cases3039ts <- zoo(cases3039$pos, order.by = cases3039$date)
cases3039MA7days <- rollapply(cases3039ts, width=7, FUN=mean, align='center')
cases3039ts <- merge(cases3039ts, cases3039MA7days)
cases3039ts_df <- data.frame(cases3039ts)
cases3039ts_df <- tibble::rownames_to_column(cases3039ts_df, "date")
cases3039ts_df$date <- as.Date(cases3039ts_df$date)

positivity3039 <- merge(tests3039ts_df, cases3039ts_df, by = "date", all = TRUE)
positivity3039$pct3039 <- positivity3039$cases3039MA7days*100/positivity3039$tests3039MA7days
head(positivity3039) 

# 40-49y
tests4049 <- subset(tests_by_date, AGEGROUP == "40-49" & date > "2020-07-27")
tests4049 <- merge(tests4049, dates, all.y = TRUE)
tests4049$total[is.na(tests4049$total)] <- 0
tests4049ts <- zoo(tests4049$total, order.by = tests4049$date)
tests4049MA7days <- rollapply(tests4049ts, width=7, FUN=mean, align='center')
tests4049ts <- merge(tests4049ts, tests4049MA7days)
tests4049ts_df <- data.frame(tests4049ts)
tests4049ts_df <- tibble::rownames_to_column(tests4049ts_df, "date")
tests4049ts_df$date <- as.Date(tests4049ts_df$date)

cases4049 <- subset(tests_by_date, AGEGROUP == "40-49" & date > "2020-07-27")
cases4049 <- merge(cases4049, dates, all.y = TRUE)
cases4049$pos[is.na(cases4049$pos)] <- 0
cases4049ts <- zoo(cases4049$pos, order.by = cases4049$date)
cases4049MA7days <- rollapply(cases4049ts, width=7, FUN=mean, align='center')
cases4049ts <- merge(cases4049ts, cases4049MA7days)
cases4049ts_df <- data.frame(cases4049ts)
cases4049ts_df <- tibble::rownames_to_column(cases4049ts_df, "date")
cases4049ts_df$date <- as.Date(cases4049ts_df$date)

positivity4049 <- merge(tests4049ts_df, cases4049ts_df, by = "date", all = TRUE)
positivity4049$pct4049 <- positivity4049$cases4049MA7days*100/positivity4049$tests4049MA7days
head(positivity4049) 

# 50-59y
tests5059 <- subset(tests_by_date, AGEGROUP == "50-59" & date > "2020-07-27")
tests5059 <- merge(tests5059, dates, all.y = TRUE)
tests5059$total[is.na(tests5059$total)] <- 0
tests5059ts <- zoo(tests5059$total, order.by = tests5059$date)
tests5059MA7days <- rollapply(tests5059ts, width=7, FUN=mean, align='center')
tests5059ts <- merge(tests5059ts, tests5059MA7days)
tests5059ts_df <- data.frame(tests5059ts)
tests5059ts_df <- tibble::rownames_to_column(tests5059ts_df, "date")
tests5059ts_df$date <- as.Date(tests5059ts_df$date)

cases5059 <- subset(tests_by_date, AGEGROUP == "50-59" & date > "2020-07-27")
cases5059 <- merge(cases5059, dates, all.y = TRUE)
cases5059$pos[is.na(cases5059$pos)] <- 0
cases5059ts <- zoo(cases5059$pos, order.by = cases5059$date)
cases5059MA7days <- rollapply(cases5059ts, width=7, FUN=mean, align='center')
cases5059ts <- merge(cases5059ts, cases5059MA7days)
cases5059ts_df <- data.frame(cases5059ts)
cases5059ts_df <- tibble::rownames_to_column(cases5059ts_df, "date")
cases5059ts_df$date <- as.Date(cases5059ts_df$date)

positivity5059 <- merge(tests5059ts_df, cases5059ts_df, by = "date", all = TRUE)
positivity5059$pct5059 <- positivity5059$cases5059MA7days*100/positivity5059$tests5059MA7days
head(positivity5059) 

# 60-69y
tests6069 <- subset(tests_by_date, AGEGROUP == "60-69" & date > "2020-07-27")
tests6069 <- merge(tests6069, dates, all.y = TRUE)
tests6069$total[is.na(tests6069$total)] <- 0
tests6069ts <- zoo(tests6069$total, order.by = tests6069$date)
tests6069MA7days <- rollapply(tests6069ts, width=7, FUN=mean, align='center')
tests6069ts <- merge(tests6069ts, tests6069MA7days)
tests6069ts_df <- data.frame(tests6069ts)
tests6069ts_df <- tibble::rownames_to_column(tests6069ts_df, "date")
tests6069ts_df$date <- as.Date(tests6069ts_df$date)

cases6069 <- subset(tests_by_date, AGEGROUP == "60-69" & date > "2020-07-27")
cases6069 <- merge(cases6069, dates, all.y = TRUE)
cases6069$pos[is.na(cases6069$pos)] <- 0
cases6069ts <- zoo(cases6069$pos, order.by = cases6069$date)
cases6069MA7days <- rollapply(cases6069ts, width=7, FUN=mean, align='center')
cases6069ts <- merge(cases6069ts, cases6069MA7days)
cases6069ts_df <- data.frame(cases6069ts)
cases6069ts_df <- tibble::rownames_to_column(cases6069ts_df, "date")
cases6069ts_df$date <- as.Date(cases6069ts_df$date)

positivity6069 <- merge(tests6069ts_df, cases6069ts_df, by = "date", all = TRUE)
positivity6069$pct6069 <- positivity6069$cases6069MA7days*100/positivity6069$tests6069MA7days
head(positivity6069) 

# 70+ years
tests70plus <- subset(tests_by_date, AGEGROUP == "70+" & date > "2020-07-27")
tests70plus <- merge(tests70plus, dates, all.y = TRUE)
tests70plus$total[is.na(tests70plus$total)] <- 0
tests70plusts <- zoo(tests70plus$total, order.by = tests70plus$date)
tests70plusMA7days <- rollapply(tests70plusts, width=7, FUN=mean, align='center')
tests70plusts <- merge(tests70plusts, tests70plusMA7days)
tests70plusts_df <- data.frame(tests70plusts)
tests70plusts_df <- tibble::rownames_to_column(tests70plusts_df, "date")
tests70plusts_df$date <- as.Date(tests70plusts_df$date)

cases70plus <- subset(tests_by_date, AGEGROUP == "70+" & date > "2020-07-27")
cases70plus <- merge(cases70plus, dates, all.y = TRUE)
cases70plus$pos[is.na(cases70plus$pos)] <- 0
cases70plusts <- zoo(cases70plus$pos, order.by = cases70plus$date)
cases70plusMA7days <- rollapply(cases70plusts, width=7, FUN=mean, align='center')
cases70plusts <- merge(cases70plusts, cases70plusMA7days)
cases70plusts_df <- data.frame(cases70plusts)
cases70plusts_df <- tibble::rownames_to_column(cases70plusts_df, "date")
cases70plusts_df$date <- as.Date(cases70plusts_df$date)

positivity70plus <- merge(tests70plusts_df, cases70plusts_df, by = "date", all = TRUE)
positivity70plus$pct70plus <- positivity70plus$cases70plusMA7days*100/positivity70plus$tests70plusMA7days
head(positivity70plus) 

# merge all
positivity_by_age <- merge(positivity09, positivity1019, by = "date", all = TRUE)
positivity_by_age <- merge(positivity_by_age, positivity2029, by = "date", all = TRUE)
positivity_by_age <- merge(positivity_by_age, positivity3039, by = "date", all = TRUE)
positivity_by_age <- merge(positivity_by_age, positivity4049, by = "date", all = TRUE)
positivity_by_age <- merge(positivity_by_age, positivity5059, by = "date", all = TRUE)
positivity_by_age <- merge(positivity_by_age, positivity6069, by = "date", all = TRUE)
positivity_by_age <- merge(positivity_by_age, positivity70plus, by = "date", all = TRUE)

# select only study period
positivity_by_age <- subset(positivity_by_age, positivity_by_age$date>"2020-07-31"&positivity_by_age$date<"2020-11-13")

# remove age group missings
tests_by_date_agerecorded <- subset(tests_by_date, tests_by_date$AGEGROUP!="UNK")




###########################################
# Testing rates per age group
###########################################


total_pop_Bxl_2020 <- Bxl_population_2020 %>%
  summarise(sum(Aantal))
total_pop_Bxl_2020

# pop by age
age_pop_Bxl_2020 <- Bxl_population_2020 
age_pop_Bxl_2020$Age.Group_str <- as.character(age_pop_Bxl_2020$Age.group)
age_pop_Bxl_2020$agegr <- "70+"
age_pop_Bxl_2020$agegr[age_pop_Bxl_2020$Age.Group_str=="0-4 year"|age_pop_Bxl_2020$Age.Group_str=="5-9 year"] <- "0-9"
age_pop_Bxl_2020$agegr[age_pop_Bxl_2020$Age.Group_str=="10-14 year"|age_pop_Bxl_2020$Age.Group_str=="15-19 year"] <- "10-19"
age_pop_Bxl_2020$agegr[age_pop_Bxl_2020$Age.Group_str=="20-24 year"|age_pop_Bxl_2020$Age.Group_str=="25-29 year"] <- "20-29"
age_pop_Bxl_2020$agegr[age_pop_Bxl_2020$Age.Group_str=="30-34 year"|age_pop_Bxl_2020$Age.Group_str=="35-39 year"] <- "30-39"
age_pop_Bxl_2020$agegr[age_pop_Bxl_2020$Age.Group_str=="40-44 year"|age_pop_Bxl_2020$Age.Group_str=="45-49 year"] <- "40-49"
age_pop_Bxl_2020$agegr[age_pop_Bxl_2020$Age.Group_str=="50-54 year"|age_pop_Bxl_2020$Age.Group_str=="55-59 year"] <- "50-59"
age_pop_Bxl_2020$agegr[age_pop_Bxl_2020$Age.Group_str=="60-64 year"|age_pop_Bxl_2020$Age.Group_str=="65-69 year"] <- "60-69"
sum_age_pop_Bxl_2020 <- age_pop_Bxl_2020 %>%
  group_by(agegr) %>%
  summarise(pop=sum(Aantal))
sum_age_pop_Bxl_2020

# summarize by age group (to have the total population per age group)
test_pop <- age_pop_Bxl_2020 %>%
  group_by(agegr) %>%
summarise(n_test_agegr = sum(Aantal))

# add population data to the tests data
test_rate <- tests_by_date
test_rate$population[tests_by_date$AGEGROUP == "0-9"] <- 163741
test_rate$population[tests_by_date$AGEGROUP == "10-19"] <- 140462
test_rate$population[tests_by_date$AGEGROUP == "20-29"] <- 181940
test_rate$population[tests_by_date$AGEGROUP == "30-39"] <- 200083
test_rate$population[tests_by_date$AGEGROUP == "40-49"] <- 173777
test_rate$population[tests_by_date$AGEGROUP == "50-59"] <- 142874
test_rate$population[tests_by_date$AGEGROUP == "60-69"] <- 100741
test_rate$population[tests_by_date$AGEGROUP == "70+"] <- 114637


test_rate$rate <- (test_rate$total/test_rate$population)*100


# Create columns of test rates per age group per day
rate_0_9 <- subset(test_rate, AGEGROUP == "0-9")
rate_0_9$test_rate_0_9 <- rate_0_9$rate
rate_0_9$rate <- NULL
rate_0_9$AGEGROUP <- NULL
rate_0_9$pos <- NULL
rate_0_9$total <- NULL
rate_0_9$population <- NULL


rate_10_19 <- subset(test_rate, AGEGROUP == "10-19")
rate_10_19$test_rate_10_19 <- rate_10_19$rate
rate_10_19$rate <- NULL
rate_10_19$rate <- NULL
rate_10_19$AGEGROUP <- NULL
rate_10_19$pos <- NULL
rate_10_19$total <- NULL
rate_10_19$population <- NULL


rate_20_29 <- subset(test_rate, AGEGROUP == "20-29")
rate_20_29$test_rate_20_29 <- rate_20_29$rate
rate_20_29$rate <- NULL
rate_20_29$rate <- NULL
rate_20_29$AGEGROUP <- NULL
rate_20_29$pos <- NULL
rate_20_29$total <- NULL
rate_20_29$population <- NULL

rate_30_39 <- subset(test_rate, AGEGROUP == "30-39")
rate_30_39$test_rate_30_39 <- rate_30_39$rate
rate_30_39$rate <- NULL
rate_30_39$rate <- NULL
rate_30_39$AGEGROUP <- NULL
rate_30_39$pos <- NULL
rate_30_39$total <- NULL
rate_30_39$population <- NULL

rate_40_49 <- subset(test_rate, AGEGROUP == "40-49")
rate_40_49$test_rate_40_49 <- rate_40_49$rate
rate_40_49$rate <- NULL
rate_40_49$rate <- NULL
rate_40_49$AGEGROUP <- NULL
rate_40_49$pos <- NULL
rate_40_49$total <- NULL
rate_40_49$population <- NULL

rate_50_59 <- subset(test_rate, AGEGROUP == "50-59")
rate_50_59$test_rate_50_59 <- rate_50_59$rate
rate_50_59$rate <- NULL
rate_50_59$rate <- NULL
rate_50_59$AGEGROUP <- NULL
rate_50_59$pos <- NULL
rate_50_59$total <- NULL
rate_50_59$population <- NULL


rate_60_69 <- subset(test_rate, AGEGROUP == "60-69")
rate_60_69$test_rate_60_69 <- rate_60_69$rate
rate_60_69$rate <- NULL
rate_60_69$rate <- NULL
rate_60_69$AGEGROUP <- NULL
rate_60_69$pos <- NULL
rate_60_69$total <- NULL
rate_60_69$population <- NULL

rate_70plus <- subset(test_rate, AGEGROUP == "70+")
rate_70plus$test_rate_70plus <- rate_70plus$rate
rate_70plus$rate <- NULL
rate_70plus$rate <- NULL
rate_70plus$AGEGROUP <- NULL
rate_70plus$pos <- NULL
rate_70plus$total <- NULL
rate_70plus$population <- NULL

# merge all
rate_by_age <- merge(rate_0_9 %>% ungroup(), rate_10_19  %>% ungroup(), by = "date", all = TRUE)
rate_by_age <- merge(rate_by_age %>% ungroup(), rate_20_29 %>% ungroup(), by = "date", all = TRUE)
rate_by_age <- merge(rate_by_age %>% ungroup(), rate_30_39 %>% ungroup(), by = "date", all = TRUE)
rate_by_age <- merge(rate_by_age %>% ungroup(), rate_40_49 %>% ungroup(), by = "date", all = TRUE)
rate_by_age <- merge(rate_by_age %>% ungroup(), rate_50_59 %>% ungroup(), by = "date", all = TRUE)
rate_by_age <- merge(rate_by_age %>% ungroup(), rate_60_69 %>% ungroup(), by = "date", all = TRUE)
rate_by_age <- merge(rate_by_age %>% ungroup(), rate_70plus %>% ungroup(), by = "date", all = TRUE)

# select only study period
rate_by_age <- subset(rate_by_age, rate_by_age$date>"2020-07-31" & rate_by_age$date<"2020-11-13")



# Instead of taking raw counts/population let's take the moving average
# Creating moving average per age group and merge all

# 0-9 years
tests_rate_09 <- subset(test_rate, AGEGROUP == "0-9")
tests_rate_09 <- merge(tests_rate_09, dates, all.y = TRUE)
tests_rate_09$rate[is.na(tests_rate_09$rate)] <- 0
tests_rate_09ts <- zoo(tests_rate_09$rate, order.by = tests_rate_09$date)
tests_rate_09MA7days <- rollapply(tests_rate_09ts, width=7, FUN=mean, align='center')
tests_rate_09ts <- merge(tests_rate_09ts, tests_rate_09MA7days)
tests09ts_df <- data.frame(tests_rate_09ts)
tests09ts_df <- tibble::rownames_to_column(tests09ts_df, "date")
tests09ts_df$date <- as.Date(tests09ts_df$date)

# 10-19 years
tests_rate_1019 <- subset(test_rate, AGEGROUP == "10-19")
tests_rate_1019 <- merge(tests_rate_1019, dates, all.y = TRUE)
tests_rate_1019$rate[is.na(tests_rate_1019$rate)] <- 0
tests_rate_1019ts <- zoo(tests_rate_1019$rate, order.by = tests_rate_1019$date)
tests_rate_1019MA7days <- rollapply(tests_rate_1019ts, width=7, FUN=mean, align='center')
tests_rate_1019ts <- merge(tests_rate_1019ts, tests_rate_1019MA7days)
tests1019ts_df <- data.frame(tests_rate_1019ts)
tests1019ts_df <- tibble::rownames_to_column(tests1019ts_df, "date")
tests1019ts_df$date <- as.Date(tests1019ts_df$date)

# # Save variables (linelists) to an Rdata file
# save(tests_rate_1019, file = "tests_rate_1019.RData")

# 20-29 years
tests_rate_2029 <- subset(test_rate, AGEGROUP == "20-29")
tests_rate_2029 <- merge(tests_rate_2029, dates, all.y = TRUE)
tests_rate_2029$rate[is.na(tests_rate_2029$rate)] <- 0
tests_rate_2029ts <- zoo(tests_rate_2029$rate, order.by = tests_rate_2029$date)
tests_rate_2029MA7days <- rollapply(tests_rate_2029ts, width=7, FUN=mean, align='center')
tests_rate_2029ts <- merge(tests_rate_2029ts, tests_rate_2029MA7days)
tests2029ts_df <- data.frame(tests_rate_2029ts)
tests2029ts_df <- tibble::rownames_to_column(tests2029ts_df, "date")
tests2029ts_df$date <- as.Date(tests2029ts_df$date)

# 30-39 years
tests_rate_3039 <- subset(test_rate, AGEGROUP == "30-39")
tests_rate_3039 <- merge(tests_rate_3039, dates, all.y = TRUE)
tests_rate_3039$rate[is.na(tests_rate_3039$rate)] <- 0
tests_rate_3039ts <- zoo(tests_rate_3039$rate, order.by = tests_rate_3039$date)
tests_rate_3039MA7days <- rollapply(tests_rate_3039ts, width=7, FUN=mean, align='center')
tests_rate_3039ts <- merge(tests_rate_3039ts, tests_rate_3039MA7days)
tests3039ts_df <- data.frame(tests_rate_3039ts)
tests3039ts_df <- tibble::rownames_to_column(tests3039ts_df, "date")
tests3039ts_df$date <- as.Date(tests3039ts_df$date)

# 40-49 years
tests_rate_4049 <- subset(test_rate, AGEGROUP == "40-49")
tests_rate_4049 <- merge(tests_rate_4049, dates, all.y = TRUE)
tests_rate_4049$rate[is.na(tests_rate_4049$rate)] <- 0
tests_rate_4049ts <- zoo(tests_rate_4049$rate, order.by = tests_rate_4049$date)
tests_rate_4049MA7days <- rollapply(tests_rate_4049ts, width=7, FUN=mean, align='center')
tests_rate_4049ts <- merge(tests_rate_4049ts, tests_rate_4049MA7days)
tests4049ts_df <- data.frame(tests_rate_4049ts)
tests4049ts_df <- tibble::rownames_to_column(tests4049ts_df, "date")
tests4049ts_df$date <- as.Date(tests4049ts_df$date)

# 50-59 years
tests_rate_5059 <- subset(test_rate, AGEGROUP == "50-59")
tests_rate_5059 <- merge(tests_rate_5059, dates, all.y = TRUE)
tests_rate_5059$rate[is.na(tests_rate_5059$rate)] <- 0
tests_rate_5059ts <- zoo(tests_rate_5059$rate, order.by = tests_rate_5059$date)
tests_rate_5059MA7days <- rollapply(tests_rate_5059ts, width=7, FUN=mean, align='center')
tests_rate_5059ts <- merge(tests_rate_5059ts, tests_rate_5059MA7days)
tests5059ts_df <- data.frame(tests_rate_5059ts)
tests5059ts_df <- tibble::rownames_to_column(tests5059ts_df, "date")
tests5059ts_df$date <- as.Date(tests5059ts_df$date)

# 60-69 years
tests_rate_6069 <- subset(test_rate, AGEGROUP == "60-69")
tests_rate_6069 <- merge(tests_rate_6069, dates, all.y = TRUE)
tests_rate_6069$rate[is.na(tests_rate_6069$rate)] <- 0
tests_rate_6069ts <- zoo(tests_rate_6069$rate, order.by = tests_rate_6069$date)
tests_rate_6069MA7days <- rollapply(tests_rate_6069ts, width=7, FUN=mean, align='center')
tests_rate_6069ts <- merge(tests_rate_6069ts, tests_rate_6069MA7days)
tests6069ts_df <- data.frame(tests_rate_6069ts)
tests6069ts_df <- tibble::rownames_to_column(tests6069ts_df, "date")
tests6069ts_df$date <- as.Date(tests6069ts_df$date)


# 70+ years
tests_rate_70plus <- subset(test_rate, AGEGROUP == "70+")
tests_rate_70plus <- merge(tests_rate_70plus, dates, all.y = TRUE)
tests_rate_70plus$rate[is.na(tests_rate_70plus$rate)] <- 0
tests_rate_70plusts <- zoo(tests_rate_70plus$rate, order.by = tests_rate_70plus$date)
tests_rate_70plusMA7days <- rollapply(tests_rate_70plusts, width=7, FUN=mean, align='center')
tests_rate_70plusts <- merge(tests_rate_70plusts, tests_rate_70plusMA7days)
tests70plusts_df <- data.frame(tests_rate_70plusts)
tests70plusts_df <- tibble::rownames_to_column(tests70plusts_df, "date")
tests70plusts_df$date <- as.Date(tests70plusts_df$date)

# merge all
rate_by_age_MA <- merge(tests09ts_df %>% ungroup(), tests1019ts_df  %>% ungroup(), by = "date", all = TRUE)
rate_by_age_MA <- merge(rate_by_age_MA %>% ungroup(), tests2029ts_df %>% ungroup(), by = "date", all = TRUE)
rate_by_age_MA <- merge(rate_by_age_MA %>% ungroup(), tests3039ts_df %>% ungroup(), by = "date", all = TRUE)
rate_by_age_MA <- merge(rate_by_age_MA %>% ungroup(), tests4049ts_df %>% ungroup(), by = "date", all = TRUE)
rate_by_age_MA <- merge(rate_by_age_MA %>% ungroup(), tests5059ts_df %>% ungroup(), by = "date", all = TRUE)
rate_by_age_MA <- merge(rate_by_age_MA %>% ungroup(), tests6069ts_df %>% ungroup(), by = "date", all = TRUE)
rate_by_age_MA <- merge(rate_by_age_MA %>% ungroup(), tests70plusts_df %>% ungroup(), by = "date", all = TRUE)

# Select period of interest
rate_by_age_MA <- subset(rate_by_age_MA, rate_by_age_MA$date>"2020-07-31" & rate_by_age_MA$date<"2020-11-13" )

# PLOTS

# plot the test rates per age group over time
ggplot2::theme_set(theme_classic(base_size = 18))
rate_agegr <- ggplot(rate_by_age, aes(x = date)) + 
  geom_line(aes(y = test_rate_0_9, colour = "0-9 years")) +
  geom_line(aes(y = test_rate_10_19, colour = "10-19 years")) +
  geom_line(aes(y = test_rate_20_29, colour = "20-29 years")) + 
  geom_line(aes(y = test_rate_30_39, colour = "30-39 years")) + 
  geom_line(aes(y = test_rate_40_49, colour = "40-49 years")) + 
  geom_line(aes(y = test_rate_50_59, colour = "50-59 years")) + 
  geom_line(aes(y = test_rate_60_69, colour = "60-69 years")) + 
  geom_line(aes(y = test_rate_70plus, colour = "70+ years")) + 
  labs(title="", x = "", y="Testing rate (%)") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank()) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_date(breaks = pretty_breaks(10))
rate_agegr

#### testplot  ####
ggplot2::theme_set(theme_classic(base_size = 18))
testcurve <- ggplot(tests_by_date, aes(x=date, y=total, fill=factor(AGEGROUP))) +
  geom_col()+
  labs(title="", x = "", y="Number of tests performed") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank()) +
  scale_x_date(breaks = pretty_breaks(10)) +
  facet_wrap(~AGEGROUP) 
testcurve
#ggsave(testcurve, filename = "testcurve.png", width = 9, height = 4)


# PLOT USED FOR PAPER
####################################################################################
# plot the test rates per age group over time
lsize = 1.2
lalpha=0.4
mycol = economist_pal()(8)

ggplot2::theme_set(theme_classic(base_size = 18))
rate_agegr_MA <- ggplot(rate_by_age_MA, aes(x = date)) + 
  geom_line(aes(y = tests_rate_09MA7days, colour = "0-9 years"),size =lsize) +
  geom_line(aes(y = tests_rate_1019MA7days, colour = "10-19 years"), size =lsize) +
  geom_line(aes(y = tests_rate_2029MA7days, colour = "20-29 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = tests_rate_3039MA7days, colour = "30-39 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = tests_rate_4049MA7days, colour = "40-49 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = tests_rate_5059MA7days, colour = "50-59 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = tests_rate_6069MA7days, colour = "60-69 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = tests_rate_70plusMA7days, colour = "70+ years"),size =lsize,alpha=lalpha) + 
  labs(title="", x = "", y="Testing rate (7-day moving average, %)") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        axis.text = element_text(size=14),
        axis.title=element_text(size=15),
        legend.text= element_text(size=10),
        title= element_text(size=15)) +
 # scale_fill_brewer(palette = "Set1") +
  scale_colour_economist() +
  scale_x_date(breaks = pretty_breaks(10))
rate_agegr_MA 


#### positivity by age curves  ####
ggplot2::theme_set(theme_classic(base_size = 18))
agepositivity <- ggplot(positivity_by_age, aes(date)) + 
  geom_line(aes(y = pct09, colour = "0-9 years"),size =lsize) + 
  geom_line(aes(y = pct1019, colour = "10-19 years"),size =lsize) +
  geom_line(aes(y = pct2029, colour = "20-29 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = pct3039, colour = "30-39 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = pct4049, colour = "40-49 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = pct5059, colour = "50-59 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = pct6069, colour = "60-69 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = pct70plus, colour = "70+ years"),size =lsize,alpha=lalpha) + 
  labs(title="", x = "", y="Positivity (7-day moving average, %)") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        axis.text = element_text(size=14),
        axis.title=element_text(size=15),
        legend.text= element_text(size=10),
        title= element_text(size=15)) +
  #scale_fill_brewer(palette = "Set1") +
  scale_colour_economist() +
  scale_x_date(breaks = pretty_breaks(10))
agepositivity

# SAVE PLOTS  
pdf(file=paste0(OutputDirectoryPaper,"SuppFig3_20201218.pdf"), width=9, height=10)
multiplot(rate_agegr_MA, agepositivity, cols = 1)
dev.off()

ggsave(agepositivity, filename = paste0(OutputDirectory,"agepositivity.png"), width = 9, height = 6)
ggsave(rate_agegr_MA, filename = paste0(OutputDirectory,"agetesting_rate.png"), width = 9, height = 6)
# 
