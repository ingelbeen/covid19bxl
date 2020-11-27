####################################################
# BXL CONTACT TRACING ANALYSES                     #
# GENERAL EPICURVE BY AGE                          #
####################################################

#  required packages
pacman::p_load(here,readxl,lubridate,haven,dplyr,ggplot2,scales,zoo,reshape2,tidyr,stringr,wesanderson,tidyr,epitools,knitr,forcats, rio,patchwork,sitrep,linelist,matchmaker,incidence,aweek,epitrix,sf,ggspatial, backports,rgdal,RColorBrewer,surveillance, broom, haven, skimr, tidyverse, visdat, pander)

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

# import dataset
casesBE <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/COVID19_Bel/COVID19_Bxl/COVID19BE_CASES_AGESEX.csv")
# the latest one is here : https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv
testsBE <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/COVID19_Bel/COVID19_Bxl/COVID19BE_tests.csv")
# the latest one is here : https://epistat.sciensano.be/Data/COVID19BE_tests.csv

casesBE$date <- as.Date(casesBE$DATE)
testsBE$date <- as.Date(testsBE$DATE)

# combine age groups
casesBE$AGEGROUP <- as.character(casesBE$AGEGROUP)
casesBE$agegr <- "70+"
casesBE$agegr[casesBE$AGEGROUP=="0-9"] <- "0-9"
casesBE$agegr[casesBE$AGEGROUP=="10-19"] <- "10-19"
casesBE$agegr[casesBE$AGEGROUP=="20-29"] <- "20-29"
casesBE$agegr[casesBE$AGEGROUP=="30-39"] <- "30-39"
casesBE$agegr[casesBE$AGEGROUP=="40-49"] <- "40-49"
casesBE$agegr[casesBE$AGEGROUP=="50-59"] <- "50-59"
casesBE$agegr[casesBE$AGEGROUP=="60-69"] <- "60-69"

# add periods with different sets of control measures
casesBE$NPI[casesBE$date<"2020-09-01"] <- "schools closed & 5 close contacts allowed"
casesBE$NPI[casesBE$date>"2020-08-30"&casesBE$date<"2020-09-30"] <- "schools open & 5 close contacts allowed"
casesBE$NPI[casesBE$date>"2020-09-29"&casesBE$date<"2020-10-06"] <- "schools open & limit close contacts suspended"
casesBE$NPI[casesBE$date>"2020-10-05"&casesBE$date<"2020-10-26"] <- "schools open, bars closed & 3 close contacts allowed"
casesBE$NPI[casesBE$date>"2020-10-25"&casesBE$date<"2020-11-02"] <- "schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed"
casesBE$NPI[casesBE$date>"2020-11-01"&casesBE$date<"2020-11-13"] <- "schools closed, mandatory teleworking, non-essential\nshops closed and all of the above"
# make it a factor
casesBE$NPIf <- factor(casesBE$NPI, levels = c("schools closed & 5 close contacts allowed","schools open & 5 close contacts allowed","schools open & limit close contacts suspended","schools open, bars closed & 3 close contacts allowed","schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed","schools closed, mandatory teleworking, non-essential\nshops closed and all of the above"))
  

# population Bxl downloaded from https://statbel.fgov.be/en/themes/population/structure-population
Bxl_population_2020 <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/COVID19_Bel/COVID19_Bxl/Bxl_population_2020.csv")
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
  filter(!is.na(date)&date>"2020-07-26"&date<"2020-11-15"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(date, agegr) %>%
  summarise(n=sum(CASES))
casesBXL

#### 1. total n° reported in Bxl, by agegroup ####
totalBXL <- casesBE %>%
  filter(!is.na(date)&date>"2020-07-31"&date<"2020-11-13"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  summarise(n=sum(CASES))
totalBXL
# attack rate
totalBXL/total_pop_Bxl_2020

# total cases and attack rate by age
agetotalBXL <- casesBE %>%
  filter(!is.na(date)&date>"2020-07-31"&date<"2020-11-13"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(agegr) %>%
  summarise(n=sum(CASES), pct=(n*100/totalBXL[,1]))
agetotalBXL

ARbyage <- merge(agetotalBXL, sum_age_pop_Bxl_2020, by.x = "agegr", by.y = "agegr", all = TRUE)
ARbyage$AR <- ARbyage$n/ARbyage$pop
ARbyage

# attack rate by age, facetted for each NPI period
agetotalBXL_byNPI <- casesBE %>%
  filter(!is.na(date)&date>"2020-07-31"&date<"2020-11-13"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(NPIf, agegr) %>%
  summarise(n=sum(CASES), pct=(n*100/totalBXL[,1]))
head(agetotalBXL_byNPI)

ARbyage_NPI <- merge(agetotalBXL_byNPI, sum_age_pop_Bxl_2020, by.x = "agegr", by.y = "agegr", all = TRUE)
ARbyage_NPI$AR <- ARbyage_NPI$n*100/ARbyage_NPI$pop
head(ARbyage_NPI)

# No legend, since the information is redundant
plot_AR_byage_NPI <- ggplot(ARbyage_NPI, aes(x=as.factor(agegr), y=AR, fill=as.factor(NPIf))) +
#                                             , fill=factor(NPI))) +
  geom_bar(stat="identity", colour="black") +
  facet_wrap(~as.factor(NPIf), scales="free") +
  theme(legend.position="none") +
  labs(x="Age (years)",y="Attack rate (%)") +
  guides(fill=FALSE) +
  theme_minimal() 
plot_AR_byage_NPI
ggsave(plot_AR_byage_NPI, filename = "plot_AR_byage_NPI.png",  width = 11, height = 6, bg = "white")


# peak
totalcasesBXL <- casesBE %>%
  filter(!is.na(date)&date>"2020-06-26"&date<"2020-11-15"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(date) %>%
  summarise(n=sum(CASES))
casesBXL <- casesBE %>%
  filter(!is.na(date)&date>"2020-06-26"&date<"2020-11-15"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(date, AGEGROUP) %>%
  summarise(n=sum(CASES))

# age proportions pre and post change test strategy 
# totals
totalBXL_2weeksuntiltestingchange <- casesBE %>%
  filter(!is.na(date)&date>"2020-10-06"&date<"2020-10-21"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  summarise(n=sum(CASES))
totalBXL_2weeksuntiltestingchange

totalBXL_2weeksaftertestingchange <- casesBE %>%
  filter(!is.na(date)&date>"2020-10-20"&date<"2020-11-04"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  summarise(n=sum(CASES))
totalBXL_2weeksaftertestingchange
# proportions by age 
agetotalBXL_2weeksbeforetestingchange <- casesBE %>%
  filter(!is.na(date)&date>"2020-10-06"&date<"2020-10-21"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(AGEGROUP) %>%
  summarise(n=sum(CASES), pct=n*100/totalBXL_2weeksuntiltestingchange[,1])
agetotalBXL_2weeksbeforetestingchange

agetotalBXL_2weeksaftertestingchange <- casesBE %>%
  filter(!is.na(date)&date>"2020-10-20"&date<"2020-11-04"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(AGEGROUP) %>%
  summarise(n=sum(CASES), pct=n*100/totalBXL_2weeksaftertestingchange[,1])
agetotalBXL_2weeksaftertestingchange

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
  filter(date>"2020-07-31" & date<"2020-11-13")

# describe number of tests
totaltestsAugNov <- testsBXL %>%
  filter(date>"2020-07-31") %>%
  summarise(sum(TESTS_ALL))
totaltestsAugNov

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
ggsave(positivity_plot, filename = "positivity_plot.png", width = 7, height = 4)

#testplot
test_plot <- ggplot(testsMA_df_AugNov) + 
  geom_col(aes(x = as.Date(date), y = testsBXLts)) +
  labs(title="", x = "", y="Number of tests performed") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank()) +
  scale_x_date(breaks = pretty_breaks(10))
test_plot
ggsave(test_plot, filename = "test_plot_181120.png", width = 7, height = 4)

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
# save this dataframe
save(casesallts_df, file = "casesMA7days.Rdata")

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
casesBXL_age_MA_counts <- subset(casesBXL_age_MA_counts, date>"2020-07-31" & date<"2020-11-13")
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
casesBXL_age_MA_pct <- subset(casesBXL_age_MA_pct, date>"2020-07-31"&date<"2020-11-13")

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
ggsave(epicurve, filename = "epicurve_Bxl_181120.png", width = 9, height = 4)

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
ggsave(area_ages, filename = "area_ages_Bxl_181120.png", width = 9, height = 4)
  
# line graph
ggplot2::theme_set(theme_classic(base_size = 18))
proplines <- ggplot(casesBXL_age_MA_pct, aes(date)) + 
  geom_line(aes(y = p09, colour = "0-9 years")) + 
  geom_line(aes(y = p1019, colour = "10-19 years")) +
  geom_line(aes(y = p2029, colour = "20-29 years")) + 
  geom_line(aes(y = p3039, colour = "30-39 years")) + 
  geom_line(aes(y = p4049, colour = "40-49 years")) + 
  geom_line(aes(y = p5059, colour = "50-59 years")) + 
  geom_line(aes(y = p6069, colour = "60-69 years")) + 
  geom_line(aes(y = p70plus, colour = "70+ years")) + 
  labs(title="", x = "", y="Pct of reported cases (7-day moving average, %)") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank()) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_date(breaks = pretty_breaks(10))
proplines
ggsave(proplines, filename = "proplines_Bxl_181120.png", width = 9, height = 4)

#### 4. linear regression by age group ####
#### 4.1. teenagers effect 1 sep ####
# on counts, for poisson or nbreg
teenagers <- casesBXL_age_MA_counts_long %>%
  filter(date<"2020-10-21"&date>"2020-07-31") 
teenagers$age <- "other age"
teenagers$age[teenagers$variable=="10-19 years"] <- "10-19 years"
teenagers$value[is.na(teenagers$value)] <- 0
teenagers <- teenagers %>%
  group_by(date, age) %>%
  summarise(n=sum(value))
teenagers$school[teenagers$date<"2020-09-05"] <- "closed"
teenagers$school[teenagers$date>"2020-09-04"] <- "open"

# poisson reg
poisson_schools <- glm(n ~ date + age + school + date*school + age*date, family = poisson(), data=teenagers) # the components, e.g. coefficients can be seen using names(linmodel) and linmodel$...
summary(poisson_schools)
glmtidy(poisson_schools)

# using proportions instead of counts
teenagersprop <- casesBXL_age_MA_pct %>%
  filter(date<"2020-10-01"&date>"2020-07-31") %>%
  select(date, p1019)
teenagersprop$schools[teenagersprop$date<"2020-09-05"] <- "closed"
teenagersprop$schools[teenagersprop$date>"2020-09-04"&teenagersprop$date<"2020-10-31"] <- "open"

linregmodel_schools_prop <- lm(p1019 ~ date + date*schools, data=teenagersprop) # the components, e.g. coefficients can be seen using names(linmodel) and linmodel$...
summary(linregmodel_schools_prop)
linregmodel_schools_prop_aug <- augment(linregmodel_schools_prop)
tidy(linregmodel_schools_prop)

# plot fitted vs. observed
prop_teenagers <- ggplot(linregmodel_schools_prop_aug) +
  geom_point(aes(x = date, y = p1019, colour = schools), alpha = 0.5) +
  geom_line(aes(x = date, y = .fitted, colour = schools)) +
  labs(title="", x = "", y="Percentage 10-19 years old (%)") +
  scale_y_continuous(limits = c(0, 50)) +
  theme_bw() 
prop_teenagers
ggsave(prop_teenagers, filename = "prop_teenagers_Bxl_181120.png", width = 5, height = 3)

# scale date to numeric
teenagersprop_numericdate <- teenagersprop
teenagersprop_numericdate$date <- as.numeric(teenagersprop_numericdate$date)-18474
linregmodel_schools_prop <- lm(p1019 ~ date + date*schools, data=teenagersprop_numericdate) # the components, e.g. coefficients can be seen using names(linmodel) and linmodel$...
summary(linregmodel_schools_prop)
linregmodel_schools_prop_aug <- augment(linregmodel_schools_prop)
tidy(linregmodel_schools_prop)

# not relying on moving averages
# using proportions instead of counts
teenagersprop <- casesBXL_age_dailycount_pct %>%
  filter(date<"2020-10-01"&date>"2020-07-31") %>%
  select(date, p1019c)
teenagersprop$schools[teenagersprop$date<"2020-09-05"] <- "closed"
teenagersprop$schools[teenagersprop$date>"2020-09-04"&teenagersprop$date<"2020-10-31"] <- "open"

linregmodel_schools_prop <- lm(p1019c ~ date + date*schools, data=teenagersprop) # the components, e.g. coefficients can be seen using names(linmodel) and linmodel$...
summary(linregmodel_schools_prop)
linregmodel_schools_prop_aug <- augment(linregmodel_schools_prop)
tidy(linregmodel_schools_prop)

# plot fitted vs. observed
prop_teenagers <- ggplot(linregmodel_schools_prop_aug) +
  geom_point(aes(x = date, y = p1019c, colour = schools), alpha = 0.5) +
  geom_line(aes(x = date, y = .fitted, colour = schools)) +
  labs(title="", x = "", y="Percentage 10-19 years old (%)") +
  theme_bw() 
prop_teenagers

#### 4.2. teenagers effect autumn break ####
# can't be assessed because of changing strategy relatively shortly before

#### 4.3. children and young adults ####
childrenyoungadultsprop <- casesBXL_age_MA_pct
childrenyoungadultsprop$young <- childrenyoungadultsprop$p09 + childrenyoungadultsprop$p1019 + childrenyoungadultsprop$p2029
childrenyoungadultsprop$testing[childrenyoungadultsprop$date<"2020-10-22"] <- "all high risk contacts"
childrenyoungadultsprop$testing[childrenyoungadultsprop$date>"2020-10-21"] <- "symptomatic high risk contacts only"

linregmodel_testing_prop <- lm(young ~ date + date*testing, data=childrenyoungadultsprop) # the components, e.g. coefficients can be seen using names(linmodel) and linmodel$...
summary(linregmodel_testing_prop)
linregmodel_testing_prop_aug <- augment(linregmodel_testing_prop)
tidy(linregmodel_testing_prop)

# plot fitted vs. observed
prop_childrenyoungadults <- ggplot(linregmodel_testing_prop_aug) +
  geom_point(aes(x = date, y = young, colour = testing), alpha = 0.5) +
  geom_line(aes(x = date, y = .fitted, colour = testing)) +
  labs(title="", x = "", y="Percentage 0-29 years old (%)") +
  theme_bw() 
prop_childrenyoungadults
ggsave(prop_childrenyoungadults, filename = "prop_childrenyoungadults_Bxl_181120.png", width = 5, height = 3)

#### 4.4. adults 20-39yo and effect of bars and restaurants ####
# not relying on moving averages & using proportions 
youngadultsprop <- casesBXL_age_daycount_pct 
youngadultsprop$ad <- youngadultsprop$p3039c + youngadultsprop$p2029c
youngadultsprop <- youngadultsprop %>%
  filter(date<"2020-11-07"&date>"2020-08-31") %>%
  select(date, ad)
youngadultsprop$measures[youngadultsprop$date<"2020-10-13"] <- "bars+restaurants open & >14 close contacts allowed"
youngadultsprop$measures[youngadultsprop$date>"2020-10-12"&youngadultsprop$date<"2020-10-30"] <- "bars closed & 3 close contacts allowed"
youngadultsprop$measures[youngadultsprop$date>"2020-10-29"] <- "bars+restaurants closed & 1 close contact allowed & curfew & indoor sports prohibited"

linregmodel_youngadultsprop <- lm(ad ~ date + date*measures, data=youngadultsprop) # the components, e.g. coefficients can be seen using names(linmodel) and linmodel$...
summary(linregmodel_youngadultsprop)
linregmodel_youngadultsprop_aug <- augment(linregmodel_youngadultsprop)
tidy(linregmodel_youngadultsprop)

# plot fitted vs. observed
measures <- ggplot(linregmodel_youngadultsprop_aug) +
  geom_point(aes(x = date, y = ad, colour = measures), alpha = 0.5) +
  geom_line(aes(x = date, y = .fitted, colour = measures)) +
  labs(title="", x = "", y="Percentage 20-39 years old (%)") +
  theme_bw() 
measures

# relying on moving averages & using proportions 
youngadultspropMA <- casesBXL_age_MA_pct 
youngadultspropMA$ad <- youngadultspropMA$p3039 + youngadultspropMA$p2029
youngadultspropMA <- youngadultspropMA %>%
  filter(date<"2020-11-06"&date>"2020-08-31") %>%
  select(date, ad)
youngadultspropMA$measures[youngadultspropMA$date<"2020-10-13"] <- "bars+restaurants open & >14 close contacts allowed"
youngadultspropMA$measures[youngadultspropMA$date>"2020-10-12"&youngadultspropMA$date<"2020-10-30"] <- "bars closed & 3 close contacts allowed"
youngadultspropMA$measures[youngadultspropMA$date>"2020-10-29"] <- "bars+restaurants closed & 1 close contact allowed & curfew & indoor sports ban"

linregmodel_youngadultspropMA <- lm(ad ~ date + date*measures, data=youngadultspropMA) # the components, e.g. coefficients can be seen using names(linmodel) and linmodel$...
summary(linregmodel_youngadultspropMA)
linregmodel_youngadultspropMA_aug <- augment(linregmodel_youngadultspropMA)
tidy(linregmodel_youngadultspropMA)

# plot fitted vs. observed
measures_MA <- ggplot(linregmodel_youngadultspropMA_aug) +
  geom_point(aes(x = date, y = ad, colour = measures), alpha = 0.5) +
  geom_line(aes(x = date, y = .fitted, colour = measures)) +
  labs(title="", x = "", y="Percentage 20-39 years old (%)") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  theme(legend.position="bottom", legend.direction="vertical", legend.title = element_blank())
measures_MA
ggsave(measures_MA, filename = "measures_MA_Bxl_181120.png", width = 5.2, height = 5)

# scale date to numeric
youngadultspropMA_numericdate <- youngadultspropMA
youngadultspropMA_numericdate$date <- as.numeric(youngadultspropMA_numericdate$date)-	18505
linregmodel_youngadultspropMA_numericdate <- lm(ad ~ date + date*measures, data=youngadultspropMA_numericdate) # the components, e.g. coefficients can be seen using names(linmodel) and linmodel$...
summary(linregmodel_youngadultspropMA_numericdate)
linregmodel_youngadultspropMA_numericdate_aug <- augment(linregmodel_youngadultspropMA_numericdate)
tidy(linregmodel_youngadultspropMA_numericdate)

# p-value for all open to bars closed
youngadultspropMAbarsclosed <- youngadultspropMA %>%
  filter(date<"2020-10-30")
linregmodel_youngadultspropMAbarsclosed <- lm(ad ~ date + measures + date*measures, data=youngadultspropMAbarsclosed) # the components, e.g. coefficients can be seen using names(linmodel) and linmodel$...
summary(linregmodel_youngadultspropMAbarsclosed)
tidy(linregmodel_youngadultspropMAbarsclosed)

# p-value for bars closed to bars and restaurants closed
youngadultspropMAoct26 <- youngadultspropMA
youngadultspropMAoct26 <- youngadultspropMA %>%
  filter(date>"2020-10-12")
linregmodel_youngadultspropMAoct26 <- lm(ad ~ date + measures + date*measures, data=youngadultspropMAoct26) # the components, e.g. coefficients can be seen using names(linmodel) and linmodel$...
summary(linregmodel_youngadultspropMAoct26)
tidy(linregmodel_youngadultspropMAoct26)
