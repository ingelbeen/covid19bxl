################################################################################
# DESCRIPTIVE OVERVIEW OF CASE AND CONTACT DATABASE
################################################################################

# Date: 1 November 2020
# E van Kleef

rm(list = ls())

# SET OUTPUT DIRECTORY
OutputDirectory <- "./report_outputs/"
OutputDirectoryPaper <- "./report_outputs/figures_paper/"
OutputDirectoryData <- "../paper/data/clean/regression/"



PACKAGES = c(
  "readxl", "writexl", "lubridate", "zoo", "ggplot2", "scales", "epitrix","gridExtra",
  "epicontacts", "tidyverse", "formattable", "igraph", "viridis", "ggthemes"
)

# install packages if needed
for (pack_name in PACKAGES) {
  if (!pack_name %in% rownames(installed.packages()))
    install.packages(pack_name)
}

# load package
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, scales,epitrix,gridExtra,
  epicontacts, tidyverse, formattable, igraph, Hmisc, viridis,ggthemes)

source("./functions/multiplot.R")
# Load data
#Filename <- "./data/clean/cleaned_dataset.RData"
latest_date = "2020-11-30" # To allow for evaluation transmission events after school closure, can chose 2 extra weeks
#latest_date = "2020-11-12"
latest_date_contacts = "2020-11-12"

start_date = "2020-08-01"

# LOAD IN DATA
Filename <- paste0("./data/clean/cleaned_datasets_linked_",latest_date,".RData")
load(Filename)

# Adjust datasets to start 1 August and other than case_case_contact to limit to 12 November
case_case_contact = case_case_contact %>% filter(date.x <=latest_date_contacts & date.x>=start_date)
case_contact = case_contact %>% filter(date.x <=latest_date_contacts & date.x>=start_date)
final_linelist = final_linelist %>% filter(date <= latest_date_contacts& date>=start_date)
no_dupli_TO2 = no_dupli_TO2 %>% filter(date <= latest_date_contacts & date>=start_date)
sciensano_linelist = sciensano_linelist %>% filter(date <= latest_date_contacts & date>=start_date)

###################################
# PREAMBLE OF DATA
###################################

# Cleaning
##################################

# FOR LINKED DATASET USE THIS
age_labels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70+")
age_labels2 = c("0-11","12-14","15-19","20-24","25-30","30-39","40-49","50-59","60-69","70+")
case_case_contact = case_case_contact %>% 
  mutate(age_cat.x2 = cut2(age.x, c(10,20,30,40,50,60,70)),
         age_cat.x2 = factor(age_cat.x2, labels=age_labels),
         age_cat.y2 = cut2(age.y, c(10,20,30,40,50,60,70),labels=age_labels),
         age_cat.y2 = factor(age_cat.y2, labels=age_labels),
         age_cat.x3 = cut2(age.x, c(12,15,20,25,30,40,50,60,70)),
         age_cat.x3 = factor(age_cat.x3, labels=age_labels2),
         age_cat.y3 = cut2(age.y, c(12,15,20,25,30,40,50,60,70)),
         age_cat.y3 = factor(age_cat.y3, labels=age_labels2)
         )



# There seem to be duplicate contacts in the case case database
dup = case_case_contact$id.y[which(duplicated(case_case_contact$id.y))]
#View(case_case_contact%>%filter(uniqueID_case.y%in%dup))

# Majority seem to have the same index case. Not sure why they appeared as duplicates in the database
case_case_contact = case_case_contact %>%
  filter(!duplicated(id.y)) %>%
  mutate(si_report = as.integer(date.y - date.x)) %>%
  filter(si_report > 0 & si_report < 22)

# Cases before and after september
###################################
knots = as.Date(c("2020-09-01","2020-09-30","2020-10-07","2020-10-26","2020-11-02"))

# Remove individuals with no age recorded
transmissions_p_age = case_case_contact %>% filter(!is.na(age.y))

# Intervention dates + 2 day lag
case_case_contact_aug = transmissions_p_age%>%filter(date.x>="2020-08-01" & date.x<="2020-09-02")
case_case_contact_sep= transmissions_p_age%>%filter(date.x>"2020-09-02" &date.x<="2020-10-08")
case_case_contact_oct= transmissions_p_age%>%filter(date.x>"2020-10-08" &date.x<="2020-11-03")
case_case_contact_nov= transmissions_p_age%>%filter(date.x>"2020-11-03" &date.x<"2020-12-01")

# Moving average
###################################
# Aggregate per day
daily_cases <- final_linelist %>% group_by(date) %>% summarise(frequency = n())
daily_casestz <- zoo(daily_cases$frequency, order.by = daily_cases$date)
MA7_days <- rollapply(daily_casestz, width=7, FUN=function(x) mean(x[-7]), align='right')

daily_cases_MA7 <- merge(daily_casestz, MA7_days)
daily_cases_MA7_df <- data.frame(daily_cases_MA7)
daily_cases_MA7_df$date = index(daily_cases_MA7)


# Number of contacts per case
###################################
contacts_per_case = no_dupli_TO2 %>% 
  group_by(date, sciensano_origin_ticket_number) %>% 
  summarise(n=(n()))

# Mean of contact per day - excluding cases that reported no contacts (assuming they were not contacted)
mean_contacts_by_day_ex_nocontacts <- contacts_per_case %>% 
  summarise(n = mean(n))

# Create mean contacts per day - including cases without contacts
# Merge cases without contacts
no_contacts = final_linelist$sciensano_request_ticket_number[which(!final_linelist$sciensano_request_ticket_number %in% no_dupli_TO2$sciensano_origin_ticket_number)]
dates_no_contacts = final_linelist$date[which(!final_linelist$sciensano_request_ticket_number %in% no_dupli_TO2$sciensano_origin_ticket_number)]

no_contacts_per_case = data.frame(cbind(as.character(as.Date(dates_no_contacts,format="%Y-%m-%d")),no_contacts,rep(0,length(dates_no_contacts))))
names(no_contacts_per_case) = names(contacts_per_case)

no_contacts_per_case$date = as.Date(no_contacts_per_case$date)
no_contacts_per_case$n = as.numeric(no_contacts_per_case$n)

contacts_per_case_all = rbind(contacts_per_case,no_contacts_per_case)

rep_contact_freq = contacts_per_case_all%>%group_by(date) %>%summarise(n=sum(n),
                                                                       cases=length(unique(sciensano_origin_ticket_number)))



# Contacts with 0 vs more than 0
###################################

contacts_per_case_all = contacts_per_case_all%>%mutate(yes_no = ifelse(n>0, 1, 0))

# summarise by day
prop_rep_contact = contacts_per_case_all%>%group_by(date,yes_no) %>%tally()
prop_rep_contact = prop_rep_contact %>%group_by(date) %>%spread(yes_no,n) %>%
  dplyr::rename("no" = `0`,
         "yes" = `1`)
prop_rep_contact$n = prop_rep_contact$no+prop_rep_contact$yes


# Mean proportion of cases listing at least 1 contact
mean(prop_rep_contact$yes/prop_rep_contact$n,na.rm=T)


# ALTERNATIVE WAY OF CALCULATING CONTACTS PER CASE
#######################################################

# BY AGE GROUP
age_labels3 = c("0-17","18-24","25-69","70+")
case_contact = case_contact %>% 
  mutate(age_cat.x3 = cut2(age.x, c(18,25,70)),
         age_cat.x3 = factor(age_cat.x3, labels=age_labels3),
         age_cat.x2 = cut2(age.x, c(10,20,30,40,50,60,70)),
         age_cat.x2 = factor(age_cat.x2, labels=age_labels))


# Create the variable mean of contact per day
period = as.Date("2020-08-01")
period_length = as.numeric(max(final_linelist$date- period))+1

two_weeks = seq(0, period_length+14, by=14)
#two_weeks_labels = period+two_weeks

two_weeks_labels = c("01-Aug","15-Aug","29-Aug","12-Sep","26-Sep","10-Oct","24-Oct","07-Nov","21-Nov")

weeks = seq(0, period_length+7, by=7)

contacts_per_case_alt_age = case_contact %>% filter(date.x>=period & !is.na(age_cat.x2))%>%
  group_by(date.x,  age_cat.x2, sciensano_request_ticket_number) %>% 
  summarise(n=(n()))%>%
  mutate(days = as.numeric(as.Date(date.x)-as.Date(period))+1) %>%
  mutate(periods_twoweeks = cut(days,two_weeks),
         #periods_twoweeks = factor(periods_twoweeks, labels=two_weeks_labels),
         periods_weeks = cut(days,weeks))

rep_contact_freq_alt_age <- contacts_per_case_alt_age%>%
  summarise(n = mean(n, na.rm=T)) %>%
  mutate(days = as.numeric(as.Date(date.x)-as.Date(period))+1) %>%
  mutate(periods_twoweeks = cut(days,two_weeks),
         #periods_twoweeks = factor(periods_twoweeks, labels=two_weeks_labels),
         periods_weeks = cut(days,weeks))

# OVERALL
contacts_per_case_alt = case_contact %>%  filter(date.x>=period)%>%
  group_by(date.x, sciensano_request_ticket_number) %>% 
  summarise(n=(n()))%>%
  mutate(days = as.numeric(as.Date(date.x)-as.Date(period))+1) %>%
  mutate(periods_twoweeks = cut(days,two_weeks),
         #periods_twoweeks = factor(periods_twoweeks, labels=two_weeks_labels),
         periods_weeks = cut(days,weeks))


# Create the variable mean of contact per day
rep_contact_freq_alt <- contacts_per_case_alt%>%
  summarise(n = mean(n, na.rm=T)) %>%
  mutate(days = as.numeric(as.Date(date.x)-as.Date(period))+1) %>%
  mutate(periods_twoweeks = cut(days,two_weeks),
         #periods_twoweeks = factor(periods_twoweeks, labels=two_weeks_labels),
         periods_weeks = cut(days,weeks))

# TABEL WITH MEDIAN AND IQR OF CONTACTS PER WEEK
mean_contacts_per_case_alt = case_contact %>%  filter(date.x>=period)%>%
  group_by(date.x, sciensano_request_ticket_number) %>% 
  summarise(n=(n()))%>%
  mutate(days = as.numeric(as.Date(date.x)-as.Date(period))+1) %>%
  mutate(periods_twoweeks = cut(days,two_weeks),
         #periods_twoweeks = factor(periods_twoweeks, labels=two_weeks_labels),
         periods_weeks = cut(days,weeks)) %>%
  group_by(periods_weeks) %>%
  summarise(median=median(n),
            lowerquantile = quantile(n,p=0.25),
            higherquantile = quantile(n,p=0.75),
            mean = mean(n),
            sd =sd(n),
            n_cases=length(n),
            se =sd/sqrt(n_cases),
            lower = mean - qt(1 - (0.05 / 2), n_cases - 1) * se,
            lower = ifelse(lower<0,0,lower),
            upper = mean + qt(1 - (0.05 / 2), n_cases - 1) * se
          ) %>%
  mutate(age_cat.x2="All",
         intervention = case_when(
           periods_weeks %in%c("(0,7]", "(7,14]","(14,21]","(21,28]") ~ 1,
           periods_weeks %in%c("(28,35]", "(35,42]","(42,49]","(49,56]") ~ 2,
           periods_weeks %in%c("(56,63]") ~ 3,
           periods_weeks %in%c("(63,70]", "(70,77]","(77,84]") ~ 4,
           periods_weeks %in%c("(84,91]") ~ 5,
        #   periods_weeks %in%c("(91,98]","(98,105]") ~ 6,
           TRUE ~ as.numeric(6)
         ))
         # intervention = case_when(
         #     periods_weeks %in%c("(0,7]", "(7,14]","(14,21]","(21,28]") ~ "schools closed & 5 close contacts allowed \n",
         #     periods_weeks %in%c("(28,35]", "(35,42]","(42,49]","(49,56]") ~ "schools open & 5 close contacts allowed \n",
         #     periods_weeks %in%c("(56,63]") ~ "schools open & limit close contacts suspended \n",
         #     periods_weeks %in%c("(63,70]", "(70,77]","(77,84]") ~ "schools open, bars closed & 3 close contacts allowed \n",
         #     periods_weeks %in%c("(84,91]") ~ "schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n",
         #     periods_weeks %in%c("(91,98]","(98,105]") ~ "schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n",
         #     TRUE ~ as.character(NA)
         # ),
         # intervention=factor(intervention, levels = c("schools closed & 5 close contacts allowed \n",
         #                                              "schools open & 5 close contacts allowed \n",
         #                                              "schools open & limit close contacts suspended \n",
         #                                              "schools open, bars closed & 3 close contacts allowed \n",
         #                                              "schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n",
         #                                              "schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n")))

# TABEL WITH MEDIAN AND IQR OF CONTACTS PER WEEK AGE
mean_contacts_per_case_alt_age = case_contact %>% filter(date.x>=period & !is.na(age_cat.x2))%>%
  group_by(date.x,  age_cat.x2, sciensano_request_ticket_number) %>% 
  summarise(n=(n()))%>%
  mutate(days = as.numeric(as.Date(date.x)-as.Date(period))+1) %>%
  mutate(periods_twoweeks = cut(days,two_weeks),
         #periods_twoweeks = factor(periods_twoweeks, labels=two_weeks_labels),
         periods_weeks = cut(days,weeks)) %>%
  group_by(periods_weeks, age_cat.x2) %>%
  summarise(median=median(n),
            lowerquantile = quantile(n,p=0.25),
            higherquantile = quantile(n,p=0.75),
            mean = mean(n),
            sd =sd(n),
            n_cases=length(n),
            se =sd/sqrt(n_cases),
            lower = mean - qt(1 - (0.05 / 2), n_cases - 1) * se,
            lower = ifelse(lower<0,0,lower),
            upper = mean + qt(1 - (0.05 / 2), n_cases - 1) * se
  ) %>%
  mutate(intervention = case_when(
    periods_weeks %in%c("(0,7]", "(7,14]","(14,21]","(21,28]") ~ 1,
    periods_weeks %in%c("(28,35]", "(35,42]","(42,49]","(49,56]") ~ 2,
    periods_weeks %in%c("(56,63]") ~ 3,
    periods_weeks %in%c("(63,70]", "(70,77]","(77,84]") ~ 4,
    periods_weeks %in%c("(84,91]") ~ 5,
   # periods_weeks %in%c("(91,98]","(98,105]") ~ 6,
    TRUE ~ as.numeric(6))
  )
    # 
    # intervention = case_when(
    # periods_weeks %in%c("(0,7]", "(7,14]","(14,21]","(21,28]") ~ "schools closed & 5 close contacts allowed \n",
    # periods_weeks %in%c("(28,35]", "(35,42]","(42,49]","(49,56]") ~ "schools open & 5 close contacts allowed \n",
    # periods_weeks %in%c("(56,63]") ~ "schools open & limit close contacts suspended \n",
    # periods_weeks %in%c("(63,70]", "(70,77]","(77,84]") ~ "schools open, bars closed & 3 close contacts allowed \n",
    # periods_weeks %in%c("(84,91]") ~ "schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n",
    # periods_weeks %in%c("(91,98]","(98,105]") ~ "schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n",
    # TRUE ~ as.character(NA)
  # ),
  # intervention=factor(intervention, levels = c("schools closed & 5 close contacts allowed \n",
  #                                              "schools open & 5 close contacts allowed \n",
  #                                              "schools open & limit close contacts suspended \n",
  #                                              "schools open, bars closed & 3 close contacts allowed \n",
  #                                              "schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n",
  #                                              "schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n")
#))
#knots = as.Date(c("2020-09-01","2020-09-29","2020-10-06","2020-10-26","2020-11-02"))

mean_contacts_per_case_alt = mean_contacts_per_case_alt[,names(mean_contacts_per_case_alt_age)]

mean_contacts_per_case_alt_age = rbind(mean_contacts_per_case_alt,mean_contacts_per_case_alt_age)


# Number of secondary cases
rep = case_case_contact %>% 
  group_by(id.x) %>%
  summarise(n=n())



## Serial interval distribution
#########################################################################
#The serial interval is defined as the delay between symptom onset between
# primary (source) cases and secondary cases.
## extract empirical data
serial_interval_empirical <- case_case_contact %>% 
  mutate(si_test = as.integer(dt_test1.y - dt_test1.x),
         si_report = as.integer(date.y - date.x),
         intervention = case_when(
           date.x>="2020-08-01" &date.x<"2020-09-06" ~ "period_1",
           date.x>="2020-09-06" &date.x<"2020-10-05" ~ "period_2",
           date.x>="2020-10-05" &date.x<"2020-10-12" ~ "period_3",
           date.x>="2020-10-12" &date.x<"2020-10-31" ~ "period_4",
           date.x>="2020-10-31" &date.x<"2020-11-07" ~ "period_5",
           date.x>="2020-11-07" &date.x<"2020-11-18" ~ "period_6",
            TRUE ~ "other")
         )

## remove negative delays and delays > 21 days
to_keep <- serial_interval_empirical$si_report > 0 & serial_interval_empirical$si_report < 22
serial_interval_empirical <- serial_interval_empirical[to_keep,]

si_week = serial_interval_empirical %>%  filter(date.x>=period)%>%
  mutate(days = as.numeric(as.Date(date.x)-as.Date(period))+1) %>%
  mutate(periods_twoweeks = cut(days,two_weeks),
         #periods_twoweeks = factor(periods_twoweeks, labels=two_weeks_labels),
         periods_weeks = cut(days,weeks)) %>%
  group_by(periods_weeks) %>%
  summarise(n_cases=n(),
            mean = mean(si_report),
            sd = sd(si_report),
            se =sd/sqrt(n_cases),
            lower = mean - qt(1 - (0.05 / 2), n_cases - 1) * se,
            lower = ifelse(lower<0,0,lower),
            upper = mean + qt(1 - (0.05 / 2), n_cases - 1) * se
            ) 

si_week_period = serial_interval_empirical %>%  filter(date.x>=period)%>%
  group_by(intervention) %>%
  summarise(n_cases=n(),
            mean = mean(si_report),
            sd = sd(si_report),
            se =sd/sqrt(n_cases),
            lower = mean - qt(1 - (0.05 / 2), n_cases - 1) * se,
            lower = ifelse(lower<0,0,lower),
            upper = mean + qt(1 - (0.05 / 2), n_cases - 1) * se
  ) 

# Transmission events by characteristics
###################################
freq_age <- data.frame(xtabs(~age_cat.x2 + age_cat.y2, transmissions_p_age))
freq_age_oth <- data.frame(xtabs(~age_cat.x3 + age_cat.y3, transmissions_p_age))

freq_age_aug <- data.frame(xtabs(~age_cat.x2 + age_cat.y2, case_case_contact_aug)) 
freq_age_sep <- data.frame(xtabs(~age_cat.x2 + age_cat.y2, case_case_contact_sep)) 
freq_age_oct <- data.frame(xtabs(~age_cat.x2 + age_cat.y2, case_case_contact_oct)) 
freq_age_nov <- data.frame(xtabs(~age_cat.x2 + age_cat.y2, case_case_contact_nov)) 

freq_age_oth_aug <- data.frame(xtabs(~age_cat.x3 + age_cat.y3, case_case_contact_aug)) 
freq_age_oth_sep <- data.frame(xtabs(~age_cat.x3 + age_cat.y3, case_case_contact_sep)) 
freq_age_oth_oct <- data.frame(xtabs(~age_cat.x3 + age_cat.y3, case_case_contact_oct)) 
freq_age_oth_nov <- data.frame(xtabs(~age_cat.x3 + age_cat.y3, case_case_contact_nov)) 

freq_post <- data.frame(xtabs(~zipcode.x + zipcode.y, case_case_contact)) 

freq_post_aug <- data.frame(xtabs(~zipcode.x + zipcode.y, case_case_contact_aug)) 
freq_post_sep <- data.frame(xtabs(~zipcode.x + zipcode.y, case_case_contact_sep)) 
freq_post_oct <- data.frame(xtabs(~zipcode.x + zipcode.y, case_case_contact_oct)) 
freq_post_nov <- data.frame(xtabs(~zipcode.x + zipcode.y, case_case_contact_nov)) 

###################################
# SUMMARIZE CLUSTERS
###################################

# summarize age by cluster size
# x$linelist%>%group_by(cluster_size2)%>%
#   summarise(age_m = mean(age,na.rm=T),
#             age_sd = sd(age,na.rm=T))
# 
# x$linelist%>%group_by(cluster_size2,gender_index_patient_nl)%>%
#   summarise(f_sex = n())
# 
# post = x$linelist%>%group_by(cluster_size2,zipcode)%>%
#   summarise(f_postal = n())

#################################################################################
# NUMBERS USED IN PAPER
#################################################################################
# TOTAL CASES THAT REACHED CALL CENTER
length(unique(sciensano_linelist$idc_pat))

# TOTAL CASES THAT REPORTED CONTACTS
length(unique(no_dupli_TO2$sciensano_origin_ticket_number))

# PERCENTAGE THAT REPORTED CONTACTS
length(unique(no_dupli_TO2$sciensano_origin_ticket_number))/length(unique(sciensano_linelist$idc_pat))

# TOTAL CONTACTS
length(unique(no_dupli_TO2$contactid_2))

# TOTAL CASES THAT REPORTED CONTACTS 
#length(unique(case_contact$sciensano_request_ticket_number))
length(unique(contacts_per_case_alt$sciensano_request_ticket_number))

# TOTAL CONTACTS THAT COULD BE LINKED TO CASE 
#length(unique(case_contact$contactid_2))
sum(contacts_per_case_alt$n)

# TOTAL TRANSMISSION EVENTS
transmissions_p = case_case_contact %>% filter(case_case_contact$date.x>="2020-08-01")
length(transmissions_p$id.x)


# TOTAL TRANSMISSION EVENTS WITH AGE
length(transmissions_p_age$id.x)

# MEAN CONTACTS MONTHS
no_dupli_TO2%>%mutate(
  month = format(date, "%m")) %>% 
  group_by(month,sciensano_origin_ticket_number) %>% 
  summarise(n=(n()))%>%
  summarise(
    mean=mean(n),
    sd=sd(n),
    median=median(n),
    loweriqr = quantile(n,0.25),
    upperiqr = quantile(n,0.75))
  
# count of all transmission events
#########################################

# Per time period overall
datas = list(freq_age, freq_age_aug, freq_age_sep, freq_age_oct, freq_age_nov)
datas_names = c("freq_age", "freq_age_aug", "freq_age_sep", "freq_age_oct", "freq_age_nov")

gen_sum = NULL
t = 0
for(i in datas){
  t = t+1
  d = i %>%
    mutate(within_gen = ifelse(age_cat.x2 == age_cat.y2, 1, 0))
  n_trans = sum(d$Freq)
  n_within = sum(d$Freq[d$within_gen==1])
  within_f = n_within/n_trans
  gen_sum = rbind(gen_sum, c(n_trans, n_within, within_f))
  assign(value = d, x = datas_names[t])
}

gen_sum = data.frame(gen_sum)
names(gen_sum) = c("n", "n_within", "within_f")
gen_sum

# # transmission from teenagers to older age groups
datas = list(freq_age, freq_age_aug, freq_age_sep, freq_age_oct, freq_age_nov)

t = 0
from_teen_sum = NULL
for(i in datas){
  t = t+1
  d = i %>%
    mutate(teen_trans = ifelse(age_cat.x2 == "10-19" & !age_cat.y2%in%c("0-9","10-19"), 1, 0))
 # print(head(d))
  n_trans = sum(d$Freq)
  n_teen = sum(d$Freq[d$teen_trans==1])
  teen_f = n_teen/n_trans
  from_teen_sum = rbind(from_teen_sum, c(n_trans, n_teen, teen_f))
  assign(value = d, x = datas_names[t])
}
from_teen_sum = data.frame(from_teen_sum)
names(from_teen_sum) = c("n", "n_from_teen", "from_teen_f")
from_teen_sum

# From_teen_sum for Sep Oct
sum(from_teen_sum$n_from_teen[c(3:4)])
sum(from_teen_sum$n[c(3:4)])
sum(from_teen_sum$n_from_teen[c(3:4)])/sum(from_teen_sum$n[c(3:4)])*100

# transmission events from younger to older generation (at least 2 and max 4 age groups difference)
datas = list(freq_age, freq_age_aug, freq_age_sep, freq_age_oct, freq_age_nov)

t = 0
to_teen_sum = NULL
for(i in datas){
  t = t+1
  d = i %>%
    mutate(old_to_teen = ifelse(age_cat.y2 == "10-19" & !age_cat.x2%in%c("0-9","10-19"), 1, 0))
  n_trans = sum(d$Freq)
  n_old_to_teen = sum(d$Freq[d$old_to_teen==1])
  old_to_teen_f = n_old_to_teen/n_trans
  to_teen_sum = rbind(to_teen_sum, c(n_trans, n_old_to_teen, old_to_teen_f))
  assign(value = d, x = datas_names[t])
}
to_teen_sum = data.frame(to_teen_sum)
names(to_teen_sum) = c("n", "n_to_teen", "to_teen_f")
to_teen_sum

# to_teen_sum for Sep Oct
sum(to_teen_sum$n_to_teen[c(3:4)])
sum(to_teen_sum$n[c(3:4)])
sum(to_teen_sum$n_to_teen[c(3:4)])/sum(to_teen_sum$n[c(3:4)])*100


# # transmission events from younger to older generation (at least 2 and max 4 age groups difference)
# 12+12+83+44+89+46+36+6+13
# 341/2360
# # transmission events from older to younger generation (at least 2 and max 4 age groups difference)
# 19+39+126+85+66+19+29+11+15
# 409/2360

# # transmission events in Sep-Oct
# case_case_contact_sepoct= case_case_contact%>%filter(date.x>"2020-09-02" &date.x<"2020-11-03")
# freq_age_sepoct <- data.frame(xtabs(~age_cat.x2 + age_cat.y2, case_case_contact_sepoct))
# sum(freq_age_sepoct$Freq) #2163
# # transmission from teenagers to older age groups, in Sep-Oct
# teenagerstoothers <- sum(freq_age_sepoct$Freq[freq_age_sepoct$age_cat.x2=="10-19"&freq_age_sepoct$age_cat.y2!="10-19"]) 
# teenagerstoothers
# teenagerstoothers/sum(freq_age_sepoct$Freq)
# # transmission from teenagers to older age groups, in Sep-Oct
# otherstoteenagers <- sum(freq_age_sepoct$Freq[freq_age_sepoct$age_cat.y2=="10-19"&freq_age_sepoct$age_cat.x2!="10-19"]) 
# otherstoteenagers
# otherstoteenagers/sum(freq_age_sepoct$Freq)
# 



# # transmission from teenagers to older age groups, in Sep
# teenagerstoothers <- sum(freq_age_sep$Freq[freq_age_sep$age_cat.x2=="10-19"&freq_age_sep$age_cat.y2!="10-19"]) 
# teenagerstoothers
# sum(freq_age_sep$Freq)
# teenagerstoothers/sum(freq_age_sep$Freq)
# # transmission from teenagers to older age groups, in Sep
# otherstoteenagers <- sum(freq_age_sep$Freq[freq_age_sep$age_cat.y2=="10-19"&freq_age_sep$age_cat.x2!="10-19"]) 
# otherstoteenagers
# otherstoteenagers/sum(freq_age_sep$Freq)
# 

#################################################################################
# PLOTS USED FOR PAPER
#################################################################################

# Transmission events by age - PAPER
###############################################################

plot_age_trans = ggplot(data = freq_age, aes(x=age_cat.x2, y=age_cat.y2, size=Freq)) + 
  geom_point(color = "navy", alpha = .5) +
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA) + 
  scale_size_continuous("Number of \ntransmissions",
                        range = c(1, 20),
                        breaks = c(1, 5, 10, 20, 30, 40, 50)) +
  labs(title = paste0("Transmissions across age (N=",sum(freq_age$Freq),")"),
       x = "Source case",
       y = "Infectee")
plot_age_trans

# Age Before September
size = 4
colour = "aquamarine3" # "navy"
alpha = 0.6
border_label_t = 2
border_freq = 1

# CHECK IF LABELS correct
# check = freq_age_aug%>%filter(!freq_age_aug$Freq<1)%>% 
#   mutate(label_t =round(Freq/sum(Freq),3)*100,
#          label_t = ifelse(label_t<border_label_t|Freq==border_freq,NA,label_t),
#          label_all =Freq/sum(Freq)*100)
# sum(check$label_all)


plot_age_trans_aug = freq_age_aug%>%filter(!freq_age_aug$Freq<1)%>% 
  mutate(label_t =round(Freq/sum(Freq),3)*100) %>%#,
#         label_t = ifelse(label_t<border_label_t|Freq==border_freq,NA,label_t))%>%
  ggplot(., aes(x=age_cat.x2, y=age_cat.y2, size=Freq)) + 
  geom_point(color = colour, alpha=alpha) +
  scale_color_gradient2() + 
  theme_bw()+
  scale_size_continuous("Number of \ntransmissions",
                        range = c(1, 20),
                        breaks = c(1, 5, 10, 20, 30, 40, 50)) +
  labs(title = paste0("Transmissions across age 1 Aug - 2 Sep '20 (N=",sum(freq_age_aug$Freq),")"),
       x = "Source case",
       y = "Infectee")+
  geom_text(size=size,aes(label=label_t),show.legend = FALSE)
plot_age_trans_aug

# Age After September
plot_age_trans_sep = freq_age_sep%>%filter(!freq_age_sep$Freq<1)%>% 
  mutate(label_t =round(Freq/sum(Freq),3)*100)%>%#,
#         label_t = ifelse(label_t<border_label_t|Freq==border_freq,NA,label_t)) %>%
  ggplot(., aes(x=age_cat.x2, y=age_cat.y2, size=Freq)) + 
  geom_point(color = colour, alpha = alpha) +
  theme_bw()+
  scale_size_continuous("Number of \ntransmissions",
                        range = c(1, 20),
                        breaks = c(1, 5, 10, 20, 30, 40, 50)) +
  labs(title = paste0("Transmissions across age 3 Sept - 7 Oct '20 (N=",sum(freq_age_sep$Freq),")"),
       x = "Source case",
       y = "Infectee")+
  geom_text(size=size, aes(label=label_t),show.legend = FALSE)
plot_age_trans_sep

# Age After October
plot_age_trans_oct = freq_age_oct%>%filter(!freq_age_oct$Freq<1)%>% 
  mutate(label_t =round(Freq/sum(Freq),3)*100) %>% #,
  #       label_t = ifelse(label_t<border_label_t|Freq==border_freq,NA,label_t))%>%
  ggplot(., aes(x=age_cat.x2, y=age_cat.y2, size=Freq)) + 
  geom_point(color = colour, alpha = alpha) +
  theme_bw()+
  scale_size_continuous("Number of \ntransmissions",
                        range = c(1, 20),
                        breaks = c(1, 5, 10, 20, 30, 40, 50)) +
  labs(title = paste0("Transmissions across age 8 Oct - 3 Nov '20 (N=",sum(freq_age_oct$Freq),")"),
       x = "Source case",
       y = "Infectee")+
  geom_text(size=size, aes(label=label_t),show.legend = FALSE)
plot_age_trans_oct

# Age After November
plot_age_trans_nov= freq_age_nov%>%filter(!freq_age_nov$Freq<1)%>% 
  mutate(label_t =round(Freq/sum(Freq),3)*100) %>%#,
     #    label_t = ifelse(label_t<border_label_t|Freq==border_freq,NA,label_t))%>%
  ggplot(., aes(x=age_cat.x2, y=age_cat.y2, size=Freq)) + 
  geom_point(color = colour, alpha = alpha) +
  theme_bw()+
  scale_size_continuous("Number of \ntransmissions",
                        range = c(1, 20),
                        breaks = c(1, 5, 10, 20, 30, 40, 50)) +
  labs(title = paste0("Transmissions across age 4 Nov - 30 Nov '20 (N=",sum(freq_age_nov$Freq),")"),
       x = "Source case",
       y = "Infectee")+
  geom_text(size=size, aes(label=label_t),show.legend = FALSE)
plot_age_trans_nov


# MEAN AND 95% CONFIDENCE INTERVALS OF CONTACTS PER CASE
###############################################################
two_weeks = seq(0, period_length+14, by=14)
weeks = seq(0, period_length+7, by=7)

two_weeks_labels = c("01-Aug","15-Aug","29-Aug","12-Sep","26-Sep","10-Oct","24-Oct","07-Nov","21-Nov")
weeks_labels = c("01-Aug","08-Aug","15-Aug","22-Aug","29-Aug","05-Sep","12-Sep","19-Sep","26-Sep",
                 "03-Oct","10-Oct","17-Oct","24-Oct","31-Oct","07-Nov","14-Nov","21-Nov","28-Nov")

mean_1824 = mean_contacts_per_case_alt_age%>%filter(age_cat.x2=="All")%>%select(periods_weeks,mean)
cols = viridis_pal()(length(mean_1824$periods_weeks))
knots = c("05-Sep","03-Oct","10-Oct","31-Oct","07-Nov")

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols2 = gg_color_hue(6)
#mycol = c( "lightcyan", "#00887d",  "#ee8f71","#7c260b" )

show_col(economist_pal()(9))
cols3 = economist_pal()(8)
cols3 = c("#00887d","#01a2d9","#76c0c1","#6794a7", "#ee8f71","#adadad")


plot_mean_contacts_case_fw = mean_contacts_per_case_alt_age %>% filter(!age_cat.x2=="0-9"&&!periods_weeks=="(0-7]") %>%
  ggplot(., aes(x=periods_weeks, y=mean,col=as.factor(intervention)))+geom_errorbar(aes(ymin=lower,ymax=upper), width=0,size=2) +
  geom_hline(yintercept=mean_1824$mean[5], colour=cols[6], lty=2, size=1)+
  geom_point(size=5)+
  geom_line()+
  scale_color_manual(values=cols3) +
  theme_bw()+ylab("Average number of contacts reported per case")+
  scale_x_discrete(labels= weeks_labels)+
  xlab("") + 
  ylim(0,11) +
  theme(axis.text = element_text(size=10),
        axis.title=element_text(size=15),
        legend.text= element_text(size=15),
        legend.title= element_text(size=15),
        strip.text.x = element_text(size = 20),
        plot.title = element_text(size=22),
        axis.text.x = element_text(angle=90, vjust=0),
        legend.position = "bottom") +
  ggtitle(paste0("Average contacts per reported case by age group"))+
  labs(colour = "Intervention")+
  facet_wrap(~age_cat.x2,2)
plot_mean_contacts_case_fw


##########################################################
# SAVE PLOTS
############################################################

pdf(file=paste0(OutputDirectoryPaper,"Fig4_20201218.pdf"), width=14, height=10)
multiplot(plot_age_trans_aug, plot_age_trans_oct,plot_age_trans_sep,  plot_age_trans_nov, cols = 2)
dev.off()

pdf(file=paste0(OutputDirectoryPaper,"Fig2_20201218.pdf"), width=14, height=8)
plot_mean_contacts_case_fw 
dev.off()

png(file=paste0(OutputDirectory,"trans_age_all_periods.png"), width=900, height=700)
multiplot(plot_age_trans_aug, plot_age_trans_oct,plot_age_trans_sep,  plot_age_trans_nov, cols = 2)
dev.off()

ggsave("mean_contacts_per_case_all_age_fw.png", plot =plot_mean_contacts_case_fw, device="png", path = OutputDirectory, width=18,height=10)
