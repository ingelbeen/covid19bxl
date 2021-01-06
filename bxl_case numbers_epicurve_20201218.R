####################################################
# BXL CONTACT TRACING ANALYSES                     #
# GENERAL EPICURVE BY AGE                          #
####################################################

rm(list = ls())

OutputDirectory <- "./report_outputs/"
OutputDirectoryPaper <- "./report_outputs/figures_paper/"
OutputDirectoryData <- "../paper/data/clean/regression/"


PACKAGES = c(
  "readxl", "writexl", "lubridate", "zoo", "ggplot2", "scales","reshape","boot",
  "epicontacts", "tidyverse", "formattable", "igraph", "viridis", "ggthemes"
)

# install packages if needed
for (pack_name in PACKAGES) {
  if (!pack_name %in% rownames(installed.packages()))
    install.packages(pack_name)
}

# load package
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, scales, reshape, boot,
               epicontacts, tidyverse, formattable, igraph, Hmisc, viridis,ggthemes)

source("./functions/multiplot.R")
# Load data
#Filename <- "./data/clean/cleaned_dataset.RData"
latest_date = "2020-11-12"


# functions
# Function to tidy Poisson regression output
glmtidy <- function(x, caption = ''){
  pander(tidy(x, exponentiate = TRUE, conf.int = TRUE),
         caption = caption)
}
# Function to tidy Poisson regression statistics
glmstats <- function(x){
  pander(glance(x))
}
# 
# # import dataset
# casesBE <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/COVID19_Bel/COVID19_Bxl/COVID19BE_CASES_AGESEX.csv")
casesBE <- read.csv("./data/clean/COVID19BE_CASES_AGESEX.csv")

# # the latest one is here : https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv
# testsBE <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/COVID19_Bel/COVID19_Bxl/COVID19BE_tests.csv")
testsBE <- read.csv("./data/clean/COVID19BE_tests.csv")
# # the latest one is here : https://epistat.sciensano.be/Data/COVID19BE_tests.csv

# population Bxl downloaded from https://statbel.fgov.be/en/themes/population/structure-population
#Bxl_population_2020 <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/COVID19_Bel/COVID19_Bxl/Bxl_population_2020.csv")
Bxl_population_2020 <- read.csv("./data/clean/Bxl_population_2020.csv")

##################################################
# CHANGE DATE VARIABLE TO LATEST DATE
casesBE$date <- as.Date(casesBE$DATE)
testsBE$date <- as.Date(testsBE$DATE)


# Population data
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

# summarize by age group
casesBXL <- casesBE %>%
  filter(!is.na(date)&date>"2020-07-26"&date<latest_date&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(date, AGEGROUP) %>%
  summarise(n=sum(CASES))
casesBXL

#### 1. total n° reported in Bxl, by agegroup ####
totalBXL <- casesBE %>%
  filter(!is.na(date)&date>"2020-07-31"&date<=latest_date&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  summarise(n=sum(CASES))
totalBXL
# attack rate
totalBXL/total_pop_Bxl_2020

# total cases and attack rate by age
agetotalBXL <- casesBE %>%
  filter(!is.na(date)&date>"2020-07-31"&date<=latest_date&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(AGEGROUP) %>%
  summarise(n=sum(CASES), pct=(n*100/totalBXL[,1]))
agetotalBXL

ARbyage <- merge(agetotalBXL, sum_age_pop_Bxl_2020, by.x = "AGEGROUP", by.y = "agegr", all = TRUE)
ARbyage$AR <- ARbyage$n/ARbyage$pop
ARbyage

totalBXL_July_Aug <- casesBE %>%
  filter(!is.na(date)&date>"2020-06-30"&date<"2020-09-01"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  summarise(n=sum(CASES))
totalBXL_July_Aug

totalBXL_July <- casesBE %>%
  filter(!is.na(date)&date>"2020-06-30"&date<"2020-08-01"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  summarise(n=sum(CASES))
totalBXL_July

totalBXL_Aug <- casesBE %>%
  filter(!is.na(date)&date>"2020-07-31"&date<"2020-09-01"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  summarise(n=sum(CASES))
totalBXL_Aug

totalBXL_Sep <- casesBE %>%
  filter(!is.na(date)&date>"2020-08-31"&date<"2020-10-01"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  summarise(n=sum(CASES))
totalBXL_Sep

totalBXL_Oct <- casesBE %>%
  filter(!is.na(date)&date>"2020-09-30"&date<"2020-11-01"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  summarise(n=sum(CASES))
totalBXL_Oct

totalBXL_Nov <- casesBE %>%
  filter(!is.na(date)&date>"2020-10-30"&date<="2020-11-30"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  summarise(n=sum(CASES))
totalBXL_Nov

totalBXL_Octuntiltestingchange <- casesBE %>%
  filter(!is.na(date)&date>"2020-09-30"&date<"2020-10-21"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  summarise(n=sum(CASES))
totalBXL_Octuntiltestingchange

totalBXL_aftertestingchange <- casesBE %>%
  filter(!is.na(date)&date>"2020-10-20"&date<="2020-11-30"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  summarise(n=sum(CASES))
totalBXL_aftertestingchange

agetotalBXL_July_Aug <- casesBE %>%
  filter(!is.na(date)&date>"2020-06-30"&date<"2020-09-01"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(AGEGROUP) %>%
  summarise(n=sum(CASES), pct=n*100/totalBXL_July_Aug[,1])
agetotalBXL_July_Aug

totalBXL_Oct_Nov <- casesBE %>%
  filter(!is.na(date)&date>"2020-09-30"&date<="2020-11-30"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  summarise(n=sum(CASES))
totalBXL_Oct_Nov

totalBXL_Oct_Nov <- casesBE %>%
  filter(!is.na(date)&date>"2020-09-30"&date<="2020-11-30"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(AGEGROUP) %>%
  summarise(n=sum(CASES), pct=n/totalBXL_Oct_Nov[,1])
totalBXL_Oct_Nov

# peak
totalcasesBXL <- casesBE %>%
  filter(!is.na(date)&date>"2020-06-26"&date<"2020-11-30"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(date) %>%
  summarise(n=sum(CASES))
casesBXL <- casesBE %>%
  filter(!is.na(date)&date>"2020-06-26"&date<"2020-11-30"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(date, AGEGROUP) %>%
  summarise(n=sum(CASES))

# proportion by age in Jul, Aug and Sep
agetotalBXL_Jul <- casesBE %>%
  filter(!is.na(date)&date>"2020-06-30"&date<"2020-08-01"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(AGEGROUP) %>%
  summarise(n=sum(CASES), pct=n*100/totalBXL_July[,1])
agetotalBXL_Jul

agetotalBXL_Aug <- casesBE %>%
  filter(!is.na(date)&date>"2020-07-31"&date<"2020-09-01"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(AGEGROUP) %>%
  summarise(n=sum(CASES), pct=n*100/totalBXL_Aug[,1])
agetotalBXL_Aug

agetotalBXL_Sep <- casesBE %>%
  filter(!is.na(date)&date>"2020-08-31"&date<"2020-10-01"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(AGEGROUP) %>%
  summarise(n=sum(CASES), pct=n*100/totalBXL_Sep[,1])
agetotalBXL_Sep

agetotalBXL_Octuntiltestingchange <- casesBE %>%
  filter(!is.na(date)&date>"2020-09-30"&date<"2020-10-21"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(AGEGROUP) %>%
  summarise(n=sum(CASES), pct=n*100/totalBXL_Octuntiltestingchange[,1])
agetotalBXL_Octuntiltestingchange

agetotalBXL_aftertestingchange <- casesBE %>%
  filter(!is.na(date)&date>"2020-10-20"&date<"2020-11-30"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(AGEGROUP) %>%
  summarise(n=sum(CASES), pct=n*100/totalBXL_aftertestingchange[,1])
agetotalBXL_aftertestingchange

#### 2. test positivity moving average ####
testsBXL <- subset(testsBE, REGION == "Brussels" & date > "2020-07-27")
# create moving 7 day avg for the number of tests done
testsBXLts <- zoo(testsBXL$TESTS_ALL, order.by = testsBXL$date)
testsMA7days <- rollapply(testsBXLts, width=7, FUN=mean, align='center')
testsBXLts <- merge(testsBXLts, testsMA7days)
testsBXLts_df <- data.frame(testsBXLts)
# create moving 7 day avg for the number of tests tested positive
positivesBXLts <- zoo(testsBXL$TESTS_ALL_POS, order.by = testsBXL$date)
posMA7days <- rollapply(positivesBXLts, width=7, FUN=mean, align='center')
posBXLts <- merge(positivesBXLts, posMA7days)
posBXLts_df <- data.frame(posBXLts)
# merge both
testsMA <- merge(testsBXLts, posBXLts, all = TRUE)
testsMA_df <- data.frame(testsMA)
testsMA_df$positivity <- NA
testsMA_df$positivity <- (testsMA_df$posMA7days/testsMA_df$testsMA7days)*100
testsMA_df <- tibble::rownames_to_column(testsMA_df, "date")
testsMA_df$date <- as.Date(testsMA_df$date)
# filter to keep only study period
testsMA_df_AugNov <- testsMA_df %>%
  filter(date>"2020-07-31" & date<"2020-11-30")

# describe number of tests
totaltestsAugNov <- testsBXL %>%
  filter(date>"2020-07-31") %>%
  summarise(sum(TESTS_ALL))
totaltestsAugNov


#### 3. age group moving avg  ####
#### 3.1. creating moving avg per age group and merge all ####
# create df with all study dates
dates <- testsMA_df %>%
  select(date)
dates$date <- as.Date(dates$date)
# all ages combined
casesall <- casesBXL %>%
  group_by(date) %>%
  summarise(n=sum(n))
casesall <- merge(casesall, dates, all.y = TRUE)
casesall$n[is.na(casesall$n)] <- 0

# create moving 7 day avg for the number of cases in this age cat 
casesallts <- zoo(casesall$n, order.by = casesall$date)
casesallMA7days <- rollapply(casesallts, width=7, FUN=mean, align='center')
casesallts <- merge(casesallts, casesallMA7days)
casesallts_df <- data.frame(casesallts)
casesallts_df <- tibble::rownames_to_column(casesallts_df, "date")
casesallts_df$date <- as.Date(casesallts_df$date)

# 0-9 years
cases09 <- subset(casesBXL, AGEGROUP == "0-9" & date > "2020-07-27")
cases09 <- merge(cases09, dates, all.y = TRUE)
cases09$n[is.na(cases09$n)] <- 0
cases09ts <- zoo(cases09$n, order.by = cases09$date)
cases09MA7days <- rollapply(cases09ts, width=7, FUN=mean, align='center')
cases09ts <- merge(cases09ts, cases09MA7days)
cases09ts_df <- data.frame(cases09ts)
cases09ts_df <- tibble::rownames_to_column(cases09ts_df, "date")
cases09ts_df$date <- as.Date(cases09ts_df$date)

# 10-19y
cases1019 <- subset(casesBXL, AGEGROUP == "10-19" & date > "2020-07-27")
cases1019 <- merge(cases1019, dates, all.y = TRUE)
cases1019$n[is.na(cases1019$n)] <- 0
cases1019ts <- zoo(cases1019$n, order.by = cases1019$date)
cases1019MA7days <- rollapply(cases1019ts, width=7, FUN=mean, align='center')
cases1019ts <- merge(cases1019ts, cases1019MA7days)
cases1019ts_df <- data.frame(cases1019ts)
cases1019ts_df <- tibble::rownames_to_column(cases1019ts_df, "date")
cases1019ts_df$date <- as.Date(cases1019ts_df$date)

# 20-29y 
cases2029 <- subset(casesBXL, AGEGROUP == "20-29" & date > "2020-07-27")
cases2029 <- merge(cases2029, dates, all.y = TRUE)
cases2029$n[is.na(cases2029$n)] <- 0
cases2029ts <- zoo(cases2029$n, order.by = cases2029$date)
cases2029MA7days <- rollapply(cases2029ts, width=7, FUN=mean, align='center')
cases2029ts <- merge(cases2029ts, cases2029MA7days)
cases2029ts_df <- data.frame(cases2029ts)
cases2029ts_df <- tibble::rownames_to_column(cases2029ts_df, "date")
cases2029ts_df$date <- as.Date(cases2029ts_df$date)

# 30-39y
cases3039 <- subset(casesBXL, AGEGROUP == "30-39" & date > "2020-07-27")
cases3039 <- merge(cases3039, dates, all.y = TRUE)
cases3039$n[is.na(cases3039$n)] <- 0
cases3039ts <- zoo(cases3039$n, order.by = cases3039$date)
cases3039MA7days <- rollapply(cases3039ts, width=7, FUN=mean, align='center')
cases3039ts <- merge(cases3039ts, cases3039MA7days)
cases3039ts_df <- data.frame(cases3039ts)
cases3039ts_df <- tibble::rownames_to_column(cases3039ts_df, "date")
cases3039ts_df$date <- as.Date(cases3039ts_df$date)

# 40-49y
cases4049 <- subset(casesBXL, AGEGROUP == "40-49" & date > "2020-07-27")
cases4049 <- merge(cases4049, dates, all.y = TRUE)
cases4049$n[is.na(cases4049$n)] <- 0
cases4049ts <- zoo(cases4049$n, order.by = cases4049$date)
cases4049MA7days <- rollapply(cases4049ts, width=7, FUN=mean, align='center')
cases4049ts <- merge(cases4049ts, cases4049MA7days)
cases4049ts_df <- data.frame(cases4049ts)
cases4049ts_df <- tibble::rownames_to_column(cases4049ts_df, "date")
cases4049ts_df$date <- as.Date(cases4049ts_df$date)

# 50-59y
cases5059 <- subset(casesBXL, AGEGROUP == "50-59" & date > "2020-07-27")
cases5059 <- merge(cases5059, dates, all.y = TRUE)
cases5059$n[is.na(cases5059$n)] <- 0
cases5059ts <- zoo(cases5059$n, order.by = cases5059$date)
cases5059MA7days <- rollapply(cases5059ts, width=7, FUN=mean, align='center')
cases5059ts <- merge(cases5059ts, cases5059MA7days)
cases5059ts_df <- data.frame(cases5059ts)
cases5059ts_df <- tibble::rownames_to_column(cases5059ts_df, "date")
cases5059ts_df$date <- as.Date(cases5059ts_df$date)

# 60-69y
cases6069 <- subset(casesBXL, AGEGROUP == "60-69" & date > "2020-07-27")
cases6069 <- merge(cases6069, dates, all.y = TRUE)
cases6069$n[is.na(cases6069$n)] <- 0
cases6069ts <- zoo(cases6069$n, order.by = cases6069$date)
cases6069MA7days <- rollapply(cases6069ts, width=7, FUN=mean, align='center')
cases6069ts <- merge(cases6069ts, cases6069MA7days)
cases6069ts_df <- data.frame(cases6069ts)
cases6069ts_df <- tibble::rownames_to_column(cases6069ts_df, "date")
cases6069ts_df$date <- as.Date(cases6069ts_df$date)

# 70+ years
cases70plus <- casesBXL %>%
  filter((AGEGROUP == "70-79"|AGEGROUP == "80-89"|AGEGROUP == "90+") & date > "2020-07-27") %>%
  group_by(date) %>%
  summarise(ntotal=sum(n))
cases70plus <- merge(cases70plus, dates, all.y = TRUE)
cases70plus$ntotal[is.na(cases70plus$ntotal)] <- 0
cases70plusts <- zoo(cases70plus$ntotal, order.by = cases70plus$date)
cases70plusMA7days <- rollapply(cases70plusts, width=7, FUN=mean, align='center')
cases70plusts <- merge(cases70plusts, cases70plusMA7days)
cases70plusts_df <- data.frame(cases70plusts)
cases70plusts_df <- tibble::rownames_to_column(cases70plusts_df, "date")
cases70plusts_df$date <- as.Date(cases70plusts_df$date)

# merge all
casesBXL_age_MA <- merge(cases09ts_df, cases1019ts_df, by = "date", all = TRUE)
casesBXL_age_MA <- merge(casesBXL_age_MA, cases2029ts_df, by = "date", all = TRUE)
casesBXL_age_MA <- merge(casesBXL_age_MA, cases3039ts_df, by = "date", all = TRUE)
casesBXL_age_MA <- merge(casesBXL_age_MA, cases4049ts_df, by = "date", all = TRUE)
casesBXL_age_MA <- merge(casesBXL_age_MA, cases5059ts_df, by = "date", all = TRUE)
casesBXL_age_MA <- merge(casesBXL_age_MA, cases6069ts_df, by = "date", all = TRUE)
casesBXL_age_MA <- merge(casesBXL_age_MA, cases70plusts_df, by = "date", all = TRUE)
casesBXL_age_MA <- merge(casesBXL_age_MA, casesallts_df, by = "date", all = TRUE)
casesBXL_age_MA_counts <- subset(casesBXL_age_MA, 
                                 select = c("date","cases09MA7days","cases1019MA7days","cases2029MA7days","cases3039MA7days","cases4049MA7days","cases5059MA7days","cases6069MA7days","cases70plusMA7days"))
casesBXL_age_MA_counts <- subset(casesBXL_age_MA_counts, date>"2020-07-31" & date<"2020-11-30")
casesBXL_age_MA_counts_long <- melt(casesBXL_age_MA_counts, id.vars = "date")
casesBXL_age_MA_counts_long$variable <- as.character(casesBXL_age_MA_counts_long$variable)
casesBXL_age_MA_counts_long$variable[casesBXL_age_MA_counts_long$variable=="cases09MA7days"] <- "0-9 years"
casesBXL_age_MA_counts_long$variable[casesBXL_age_MA_counts_long$variable=="cases1019MA7days"] <- "10-19 years"
casesBXL_age_MA_counts_long$variable[casesBXL_age_MA_counts_long$variable=="cases2029MA7days"] <- "20-29 years"
casesBXL_age_MA_counts_long$variable[casesBXL_age_MA_counts_long$variable=="cases3039MA7days"] <- "30-39 years"
casesBXL_age_MA_counts_long$variable[casesBXL_age_MA_counts_long$variable=="cases4049MA7days"] <- "40-49 years"
casesBXL_age_MA_counts_long$variable[casesBXL_age_MA_counts_long$variable=="cases5059MA7days"] <- "50-59 years"
casesBXL_age_MA_counts_long$variable[casesBXL_age_MA_counts_long$variable=="cases6069MA7days"] <- "60-69 years"
casesBXL_age_MA_counts_long$variable[casesBXL_age_MA_counts_long$variable=="cases70plusMA7days"] <- "70+ years"

#percentages of moving averages  
casesBXL_age_MA$p09 <- (casesBXL_age_MA$cases09MA7days/casesBXL_age_MA$casesallMA7days)*100
casesBXL_age_MA$p1019 <- (casesBXL_age_MA$cases1019MA7days/casesBXL_age_MA$casesallMA7days)*100
casesBXL_age_MA$p2029 <- (casesBXL_age_MA$cases2029MA7days/casesBXL_age_MA$casesallMA7days)*100
casesBXL_age_MA$p3039 <- (casesBXL_age_MA$cases3039MA7days/casesBXL_age_MA$casesallMA7days)*100
casesBXL_age_MA$p4049 <- (casesBXL_age_MA$cases4049MA7days/casesBXL_age_MA$casesallMA7days)*100
casesBXL_age_MA$p5059 <- (casesBXL_age_MA$cases5059MA7days/casesBXL_age_MA$casesallMA7days)*100
casesBXL_age_MA$p6069 <- (casesBXL_age_MA$cases6069MA7days/casesBXL_age_MA$casesallMA7days)*100
casesBXL_age_MA$p70plus <- (casesBXL_age_MA$cases70plusMA7days/casesBXL_age_MA$casesallMA7days)*100
casesBXL_age_MA_pct <- subset(casesBXL_age_MA, select = c("date","p09","p1019","p2029","p3039","p4049","p5059","p6069","p70plus"))
casesBXL_age_MA_pct <- subset(casesBXL_age_MA_pct, date>"2020-07-31"&date<"2020-11-30")

casesBXL_age_MA_pct_long = melt(casesBXL_age_MA_pct, id.vars = "date")
casesBXL_age_MA_pct_long$variable <- as.character(casesBXL_age_MA_pct_long$variable)
casesBXL_age_MA_pct_long$variable[casesBXL_age_MA_pct_long$variable=="p09"] <- "0-9 years"
casesBXL_age_MA_pct_long$variable[casesBXL_age_MA_pct_long$variable=="p1019"] <- "10-19 years"
casesBXL_age_MA_pct_long$variable[casesBXL_age_MA_pct_long$variable=="p2029"] <- "20-29 years"
casesBXL_age_MA_pct_long$variable[casesBXL_age_MA_pct_long$variable=="p3039"] <- "30-39 years"
casesBXL_age_MA_pct_long$variable[casesBXL_age_MA_pct_long$variable=="p4049"] <- "40-49 years"
casesBXL_age_MA_pct_long$variable[casesBXL_age_MA_pct_long$variable=="p5059"] <- "50-59 years"
casesBXL_age_MA_pct_long$variable[casesBXL_age_MA_pct_long$variable=="p6069"] <- "60-69 years"
casesBXL_age_MA_pct_long$variable[casesBXL_age_MA_pct_long$variable=="p70plus"] <- "70+ years"

#percentages of daily count
casesBXL_age_MA$p09c <- (casesBXL_age_MA$cases09ts/casesBXL_age_MA$casesallts)*100
casesBXL_age_MA$p1019c <- (casesBXL_age_MA$cases1019ts/casesBXL_age_MA$casesallts)*100
casesBXL_age_MA$p2029c <- (casesBXL_age_MA$cases2029ts/casesBXL_age_MA$casesallts)*100
casesBXL_age_MA$p3039c <- (casesBXL_age_MA$cases3039ts/casesBXL_age_MA$casesallts)*100
casesBXL_age_MA$p4049c <- (casesBXL_age_MA$cases4049ts/casesBXL_age_MA$casesallts)*100
casesBXL_age_MA$p5059c <- (casesBXL_age_MA$cases5059ts/casesBXL_age_MA$casesallts)*100
casesBXL_age_MA$p6069c <- (casesBXL_age_MA$cases6069ts/casesBXL_age_MA$casesallts)*100
casesBXL_age_MA$p70plusc <- (casesBXL_age_MA$cases70plusts/casesBXL_age_MA$casesallts)*100
casesBXL_age_daycount_pct <- subset(casesBXL_age_MA, select = c("date","p09c","p1019c","p2029c","p3039c","p4049c","p5059c","p6069c","p70plusc"))
casesBXL_age_daycount_pct <- subset(casesBXL_age_daycount_pct, date>"2020-07-31" & date<"2020-11-12")



# I want the count of cases per day for all and by age group 
casesBXL_count <- merge(cases09ts_df, cases1019ts_df, by = "date", all = TRUE)
casesBXL_count  <- merge(casesBXL_count , cases2029ts_df, by = "date", all = TRUE)
casesBXL_count  <- merge(casesBXL_count , cases3039ts_df, by = "date", all = TRUE)
casesBXL_count  <- merge(casesBXL_count , cases4049ts_df, by = "date", all = TRUE)
casesBXL_count  <- merge(casesBXL_count , cases5059ts_df, by = "date", all = TRUE)
casesBXL_count  <- merge(casesBXL_count , cases6069ts_df, by = "date", all = TRUE)
casesBXL_count  <- merge(casesBXL_count , cases70plusts_df, by = "date", all = TRUE)


# Remove the moving average variables
casesBXL_count$cases09MA7days  <- NULL
casesBXL_count$cases1019MA7days  <- NULL
casesBXL_count$cases2029MA7days  <- NULL
casesBXL_count$cases3039MA7days  <- NULL
casesBXL_count$cases4049MA7days  <- NULL
casesBXL_count$cases5059MA7days  <- NULL
casesBXL_count$cases6069MA7days  <- NULL
casesBXL_count$cases70plusMA7days  <- NULL
casesBXL_count$casesallMA7days  <- NULL

# Put the data in a long format
casesBXL_count <- subset(casesBXL_count, date<"2020-10-01"& date>"2020-07-31")
casesBXL_age_counts_long <- melt(casesBXL_count, id.vars = "date")
casesBXL_age_counts_long$variable <- as.character(casesBXL_age_counts_long$variable)
casesBXL_age_counts_long$variable[casesBXL_age_counts_long$variable=="cases09ts"] <- "0-9 years"
casesBXL_age_counts_long$variable[casesBXL_age_counts_long$variable=="cases1019ts"] <- "10-19 years"
casesBXL_age_counts_long$variable[casesBXL_age_counts_long$variable=="cases2029ts"] <- "20-29 years"
casesBXL_age_counts_long$variable[casesBXL_age_counts_long$variable=="cases3039ts"] <- "30-39 years"
casesBXL_age_counts_long$variable[casesBXL_age_counts_long$variable=="cases4049ts"] <- "40-49 years"
casesBXL_age_counts_long$variable[casesBXL_age_counts_long$variable=="cases5059ts"] <- "50-59 years"
casesBXL_age_counts_long$variable[casesBXL_age_counts_long$variable=="cases6069ts"] <- "60-69 years"
casesBXL_age_counts_long$variable[casesBXL_age_counts_long$variable=="cases70plusts"] <- "70+ years"


# Create variable "age_cat": 1 when age "10-19 years" and 0 for all others
casesBXL_age_counts_long$age_cat <- 0
casesBXL_age_counts_long$age_cat[casesBXL_age_counts_long$variable=="10-19 years"] <- 1
casesBXL_age_counts_long$age_cat <- factor(casesBXL_age_counts_long$age_cat)


# Create variable "school": 0 when school when closed and 1 when schools were open
casesBXL_age_counts_long$school[casesBXL_age_counts_long$date<"2020-09-05"] <- 0
casesBXL_age_counts_long$school[casesBXL_age_counts_long$date>"2020-09-04"] <- 1
casesBXL_age_counts_long$school <- factor(casesBXL_age_counts_long$school)

# Inlcude variable "total_n_cases": total number of cases reported on a specific day
casesBXL_age_counts_long <- left_join(casesBXL_age_counts_long,casesallts_df)


# Remove the non-necessary variables
casesBXL_age_counts_long$cases09ts <- NULL
casesBXL_age_counts_long$cases1019ts <- NULL
casesBXL_age_counts_long$cases2029ts <- NULL
casesBXL_age_counts_long$cases3039ts <- NULL
casesBXL_age_counts_long$cases4049ts <- NULL
casesBXL_age_counts_long$cases5059ts <- NULL
casesBXL_age_counts_long$cases6069ts <- NULL
casesBXL_age_counts_long$cases70plusts <- NULL
casesBXL_age_counts_long$casesallMA7days <- NULL

# Rename variables
casesBXL_age_counts_long$age_group <- casesBXL_age_counts_long$variable
casesBXL_age_counts_long$variable <- NULL

casesBXL_age_counts_long$n_cases <- casesBXL_age_counts_long$value
casesBXL_age_counts_long$value <- NULL

casesBXL_age_counts_long$total_n_cases <- casesBXL_age_counts_long$casesallts
casesBXL_age_counts_long$casesallts <- NULL


# I want the count of cases per day for all and by age group 
casesBXL_count <- merge(cases09ts_df, cases1019ts_df, by = "date", all = TRUE)
casesBXL_count  <- merge(casesBXL_count , cases2029ts_df, by = "date", all = TRUE)
casesBXL_count  <- merge(casesBXL_count , cases3039ts_df, by = "date", all = TRUE)
casesBXL_count  <- merge(casesBXL_count , cases4049ts_df, by = "date", all = TRUE)
casesBXL_count  <- merge(casesBXL_count , cases5059ts_df, by = "date", all = TRUE)
casesBXL_count  <- merge(casesBXL_count , cases6069ts_df, by = "date", all = TRUE)
casesBXL_count  <- merge(casesBXL_count , cases70plusts_df, by = "date", all = TRUE)


# Remove the moving average variables
casesBXL_count$cases09MA7days  <- NULL
casesBXL_count$cases1019MA7days  <- NULL
casesBXL_count$cases2029MA7days  <- NULL
casesBXL_count$cases3039MA7days  <- NULL
casesBXL_count$cases4049MA7days  <- NULL
casesBXL_count$cases5059MA7days  <- NULL
casesBXL_count$cases6069MA7days  <- NULL
casesBXL_count$cases70plusMA7days  <- NULL
casesBXL_count$casesallMA7days  <- NULL

# Put the data in a long format
casesBXL_count <- subset(casesBXL_count, date<"2020-10-01"& date>"2020-07-31")
casesBXL_age_counts_long <- melt(casesBXL_count, id.vars = "date")
casesBXL_age_counts_long$variable <- as.character(casesBXL_age_counts_long$variable)
casesBXL_age_counts_long$variable[casesBXL_age_counts_long$variable=="cases09ts"] <- "0-9 years"
casesBXL_age_counts_long$variable[casesBXL_age_counts_long$variable=="cases1019ts"] <- "10-19 years"
casesBXL_age_counts_long$variable[casesBXL_age_counts_long$variable=="cases2029ts"] <- "20-29 years"
casesBXL_age_counts_long$variable[casesBXL_age_counts_long$variable=="cases3039ts"] <- "30-39 years"
casesBXL_age_counts_long$variable[casesBXL_age_counts_long$variable=="cases4049ts"] <- "40-49 years"
casesBXL_age_counts_long$variable[casesBXL_age_counts_long$variable=="cases5059ts"] <- "50-59 years"
casesBXL_age_counts_long$variable[casesBXL_age_counts_long$variable=="cases6069ts"] <- "60-69 years"
casesBXL_age_counts_long$variable[casesBXL_age_counts_long$variable=="cases70plusts"] <- "70+ years"


# Create variable "age_cat": 1 when age "10-19 years" and 0 for all others
casesBXL_age_counts_long$age_cat <- 0
casesBXL_age_counts_long$age_cat[casesBXL_age_counts_long$variable=="10-19 years"] <- 1
casesBXL_age_counts_long$age_cat <- factor(casesBXL_age_counts_long$age_cat)


# Create variable "school": 0 when school when closed and 1 when schools were open
casesBXL_age_counts_long$school[casesBXL_age_counts_long$date<"2020-09-05"] <- 0
casesBXL_age_counts_long$school[casesBXL_age_counts_long$date>"2020-09-04"] <- 1
casesBXL_age_counts_long$school <- factor(casesBXL_age_counts_long$school)

# Inlcude variable "total_n_cases": total number of cases reported on a specific day
casesBXL_age_counts_long <- left_join(casesBXL_age_counts_long,casesallts_df)


# Remove the non-necessary variables
casesBXL_age_counts_long$cases09ts <- NULL
casesBXL_age_counts_long$cases1019ts <- NULL
casesBXL_age_counts_long$cases2029ts <- NULL
casesBXL_age_counts_long$cases3039ts <- NULL
casesBXL_age_counts_long$cases4049ts <- NULL
casesBXL_age_counts_long$cases5059ts <- NULL
casesBXL_age_counts_long$cases6069ts <- NULL
casesBXL_age_counts_long$cases70plusts <- NULL
casesBXL_age_counts_long$casesallMA7days <- NULL

# Rename variables
casesBXL_age_counts_long$age_group <- casesBXL_age_counts_long$variable
casesBXL_age_counts_long$variable <- NULL

casesBXL_age_counts_long$n_cases <- casesBXL_age_counts_long$value
casesBXL_age_counts_long$value <- NULL

casesBXL_age_counts_long$total_n_cases <- casesBXL_age_counts_long$casesallts
casesBXL_age_counts_long$casesallts <- NULL


##########################################################
# CREATE PLOTS 
##########################################################
#### 2. test positivity moving average ####
# positivity_plot 
ggplot2::theme_set(theme_classic(base_size = 18))
positivity_plot <- ggplot(testsMA_df_AugNov) + 
  geom_line(aes(x = as.Date(date), y = positivity), color = "red", size = 1) +
  labs(title="", x = "", y="Percentage of positive tests (7-day moving average, %)") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank()) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_date(breaks = pretty_breaks(10))
positivity_plot
#ggsave(positivity_plot, filename = "positivity_plot.png", width = 7, height = 4)

#testplot
test_plot <- ggplot(testsMA_df_AugNov) + 
  geom_col(aes(x = as.Date(date), y = testsBXLts)) +
  labs(title="", x = "", y="Number of tests performed") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank()) +
  scale_x_date(breaks = pretty_breaks(10))
test_plot
#ggsave(test_plot, filename = "test_plot_181120.png", width = 7, height = 4)

#### 3.2 epicurve numbers ####
ggplot2::theme_set(theme_classic(base_size = 18))
epicurve <- ggplot(casesBXL_age_MA_counts_long, aes(x=date, y=value, fill=factor(variable))) +
  geom_col()+
  labs(title="", x = "", y="Number of reported cases (7-day moving average)") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank()) +
  scale_x_date(breaks = pretty_breaks(10))
epicurve
#ggsave(epicurve, filename = "epicurve_Bxl_181120.png", width = 9, height = 4)

#### 3.3 proportion by age group ####
# area chart
ggplot2::theme_set(theme_classic(base_size = 18))
area_ages <- ggplot(casesBXL_age_MA_counts_long, aes(x=date, y=value, fill=factor(variable))) +
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels=scales::percent) +
  labs(title="", x = "", y="Number of reported cases (7-day moving average)") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank()) +
  scale_x_date(breaks = pretty_breaks(10))
area_ages
#ggsave(area_ages, filename = "area_ages_Bxl_181120.png", width = 9, height = 4)
  
# line graph
lsize = 1.2
lalpha=0.4

#ggplot2::theme_set(theme_economist())
proplines <- ggplot(casesBXL_age_MA_pct, aes(date)) + 
  geom_line(aes(y = p09, colour = "0-9 years"),size =lsize, alpha=lalpha) + 
  geom_line(aes(y = p1019, colour = "10-19 years"),size =lsize) +
  geom_line(aes(y = p2029, colour = "20-29 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = p3039, colour = "30-39 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = p4049, colour = "40-49 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = p5059, colour = "50-59 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = p6069, colour = "60-69 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = p70plus, colour = "70+ years"),size =lsize,alpha=lalpha) + 
  labs(title="", x = "", y="Pct of reported cases (7-day moving average, %)") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank()) +
  scale_x_date(breaks = pretty_breaks(10))
proplines
#ggsave(proplines, filename = "proplines_Bxl_181120.png", width = 9, height = 4)

# HEATMAP CASES OVER TIME
mycol <- c( "lightcyan", "yellow", "red",  "navy")

ggplot(casesBXL_age_MA_counts_long, aes(x=date,y=variable, fill=value))+geom_tile() +
  scale_fill_gradientn(colours=mycol)+
  # scale_fill_viridis_c()+
  labs(y="", x = "", title="Age-specific reported cases (7-day moving average)",
       fill="Cases") 


#ggsave(hm_pct, filename = "prop_heatmap_Bxl_181120.png", path = OutputDirectory,width = 12, height = 7)

###########################################################
# PLOTS TO USE FOR PAPER 

mycol <- c( "lightcyan", "yellow", "red",  "navy")

# HEATMAP PROP CASES OVER TIME
hm_pct = 
  casesBXL_age_MA_pct_long %>% filter(date<=latest_date) %>%
  ggplot(., aes(x=date,y=variable, fill=value))+geom_tile() +
  scale_fill_gradientn(colours = mycol)+
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        axis.text = element_text(size=15),
        axis.title=element_text(size=15),
        legend.text= element_text(size=13),
        title= element_text(size=15),
        legend.position = "bottom")+
  scale_x_date(breaks = pretty_breaks(10))+
  labs(y="", x = "",title = "Reported cases by age (7-day moving average, %)",
       fill="")
hm_pct

# LINE GRAPH PROPORTION OF CASES
mycol = economist_pal()(8)
#mycol = c( "lightcyan", "#00887d",  "#ee8f71","#7c260b" )

lsize = 1.2
lalpha=0.4

#ggplot2::theme_set(theme_economist())
proplines <- ggplot(casesBXL_age_MA_pct, aes(date))+
  geom_line(aes(y = p09, colour = "0-9 years"),size =lsize) + 
  geom_line(aes(y = p1019, colour = "10-19 years"),size =lsize) +
  geom_line(aes(y = p2029, colour = "20-29 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = p3039, colour = "30-39 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = p4049, colour = "40-49 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = p5059, colour = "50-59 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = p6069, colour = "60-69 years"),size =lsize,alpha=lalpha) + 
  geom_line(aes(y = p70plus, colour = "70+ years"),size =lsize,alpha=lalpha) + 
  labs(title="", x = "", y="% of reported cases") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        axis.text = element_text(size=14),
        axis.title=element_text(size=15),
        legend.text= element_text(size=10),
        title= element_text(size=15)) +
  scale_colour_economist() +
  scale_x_date(breaks = pretty_breaks(10))
proplines
#ggsave(proplines, filename = "proplines_Bxl_301120.png",path=OutputDirectory, width = 10, height = 6)

# EPICURVE CASES
epicurve <- ggplot(casesBXL_age_MA_counts_long, aes(x=date, y=value, fill=factor(variable))) +
  geom_col()+
  labs(title="Reported cases by age (7-day moving average)", x = "", y="Number of reported cases") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        axis.text = element_text(size=14),
        axis.title=element_text(size=15),
        legend.text= element_text(size=10),
        title= element_text(size=15))+#,
        #legend.position = c(0.08, 0.7)) +
  scale_x_date(breaks = pretty_breaks(10))+
  scale_fill_economist() 
epicurve
#ggsave(epicurve, filename = "epicurve_Bxl_301120.png", path = OutputDirectory,width = 10, height = 6)

# SAVE FILES
# pdf(file=paste0(OutputDirectory,"epicurve_20201218.pdf"), width=16, height=10)
# epicurve
# dev.off()
# 
# pdf(file=paste0(OutputDirectory,"prop_curve_20201218.pdf"), width=16, height=10)
# proplines
# dev.off()
# 
# pdf(file=paste0(OutputDirectory,"prop_heatmap_20201218.pdf"), width=16, height=10)
# hm_pct
# dev.off()

pdf(file=paste0(OutputDirectoryPaper,"Fig1_20201218.pdf"), width=9, height=10)
multiplot(epicurve, proplines, cols = 1)
dev.off()

pdf(file=paste0(OutputDirectoryPaper,"SuppFig1B_20201218.pdf"), width=9, height=6)
hm_pct
dev.off()

ggsave(proplines, filename = "proplines_Bxl.png",path=OutputDirectory, width = 10, height = 6)
ggsave(epicurve, filename = "epicurve_Bxl.png",path=OutputDirectory, width = 10, height = 6)

# png(file=paste0(OutputDirectory,"epicurve_prop_301120.png"), width=800, height=700)
#multiplot(epicurve, hm_pct, cols = 1)
# dev.off()


# SAVE DATA FOR POISSON REGRESSION
save(casesBXL_age_counts_long, file = paste0(OutputDirectoryData,"casesBXL_age_count.Rdata"))
save(testsBXL, file = paste0(OutputDirectoryData,"testsBXL.Rdata"))
