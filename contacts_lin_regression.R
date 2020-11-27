####################################################
# BXL CONTACT TRACING ANALYSES                     #
# REGRESSION NPI -> CONTACTS & CONTACTS -> Rt      #
####################################################
# last update 261120 by Brecht Ingelbeen

#### 0. import and clean data: contacts and estimated Rt between 1 Aug and 12 Nov ####
# load dataframes
load(file = "R_Bxl_allages.Rdata") # saved at the end of the rt_estim.R script
load(file = "no_dupli_TO2_AugNov.Rdata") # saved below in this script
load(file = "mean_contacts_by_day_ex_nocontacts.Rdata") # saved below in this script
load(file = "casesMA7days.Rdata") # the 7 days moving average of reported cases in bxl
load(file = "contacttracingperformance.Rdata") # the number of reported cases for whom contacttracing could be successfully done 

# import contact data
no_dupli_TO2_AugNov <- no_dupli_TO2 # original contact database
no_dupli_TO2_AugNov$date <- as.Date(no_dupli_TO2_AugNov$date)
no_dupli_TO2_AugNov <- no_dupli_TO2_AugNov %>%
  filter(date>"2020-07-31" & date <"2020-11-13")
count(no_dupli_TO2_AugNov)

# add a variable for the different periods of NPI
# add periods with different sets of control measures
no_dupli_TO2_AugNov$NPI[no_dupli_TO2_AugNov$date<"2020-09-01"] <- "schools closed & 5 close contacts allowed"
no_dupli_TO2_AugNov$NPI[no_dupli_TO2_AugNov$date>"2020-08-30"&no_dupli_TO2_AugNov$date<"2020-09-30"] <- "schools open & 5 close contacts allowed"
no_dupli_TO2_AugNov$NPI[no_dupli_TO2_AugNov$date>"2020-09-29"&no_dupli_TO2_AugNov$date<"2020-10-06"] <- "schools open & limit close contacts suspended"
no_dupli_TO2_AugNov$NPI[no_dupli_TO2_AugNov$date>"2020-10-05"&no_dupli_TO2_AugNov$date<"2020-10-26"] <- "schools open, bars closed & 3 close contacts allowed"
no_dupli_TO2_AugNov$NPI[no_dupli_TO2_AugNov$date>"2020-10-25"&no_dupli_TO2_AugNov$date<"2020-11-02"] <- "schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed"
no_dupli_TO2_AugNov$NPI[no_dupli_TO2_AugNov$date>"2020-11-01"&no_dupli_TO2_AugNov$date<"2020-11-13"] <- "schools closed, mandatory teleworking, non-essential\nshops closed and all of the above"

# save dataframe
save(no_dupli_TO2_AugNov, file = "no_dupli_TO2_AugNov.Rdata")

#### 1. descriptive reported nr of contacts ####
# delay between contact and reporting
no_dupli_TO2_AugNov$date_last_contact_d <- as.Date(no_dupli_TO2_AugNov$date_last_contact)
no_dupli_TO2_AugNov$date_last_contact_d[grepl("/2020", no_dupli_TO2_AugNov$date_last_contact)==TRUE] <- dmy(no_dupli_TO2_AugNov$date_last_contact[grepl("/2020", no_dupli_TO2_AugNov$date_last_contact)==TRUE])
no_dupli_TO2_AugNov$date_last_contact_d[grepl("44", no_dupli_TO2_AugNov$date_last_contact)==TRUE] <- as.Date(as.numeric(no_dupli_TO2_AugNov$date_last_contact[grepl("44", no_dupli_TO2_AugNov$date_last_contact)==TRUE]), origin = "1899-12-30")
no_dupli_TO2_AugNov$date_last_contact_d[grepl("43", no_dupli_TO2_AugNov$date_last_contact)==TRUE] <- as.Date(as.numeric(no_dupli_TO2_AugNov$date_last_contact[grepl("43", no_dupli_TO2_AugNov$date_last_contact)==TRUE]), origin = "1899-12-30")
table(no_dupli_TO2_AugNov$date_last_contact_d, useNA = "always")
no_dupli_TO2_AugNov$contactreportingdelay <-  as.numeric(no_dupli_TO2_AugNov$date - no_dupli_TO2_AugNov$date_last_contact_d)
table(no_dupli_TO2_AugNov$contactreportingdelay, useNA = "always")
median(no_dupli_TO2_AugNov$contactreportingdelay[!is.na(no_dupli_TO2_AugNov$contactreportingdelay)])
quantile(no_dupli_TO2_AugNov$contactreportingdelay[!is.na(no_dupli_TO2_AugNov$contactreportingdelay)], 1/4)
quantile(no_dupli_TO2_AugNov$contactreportingdelay[!is.na(no_dupli_TO2_AugNov$contactreportingdelay)], 3/4)

# number of cases reporting at least 1 contact 
casesreportingcontacts_AugNov <- no_dupli_TO2_AugNov %>%
  group_by(sciensano_origin_ticket_number)%>%
  summarise(n=n())
head(casesreportingcontacts_AugNov)

# contacts per case
contacts_per_case = no_dupli_TO2_AugNov %>% 
  group_by(date, sciensano_origin_ticket_number) %>% 
  summarise(n=(n()))
contacts_per_case$mo <- month(contacts_per_case$date)

# Mean of contact per day - excluding cases that reported no contacts (assuming they were not contacted)
mean_contacts_by_day_ex_nocontacts <- contacts_per_case %>% 
  summarise(mean = mean(n), sd = sd(n))
head(mean_contacts_by_day_ex_nocontacts)
# save this dataframe
save(mean_contacts_by_day_ex_nocontacts, file = "mean_contacts_by_day_ex_nocontacts.Rdata") 

# Contact per mo
contacts_by_mo_ex_nocontacts <- contacts_per_case %>%
  group_by(mo) %>%
  summarise(mean = mean(n), median = median(n), quantile(n, 1/4), quantile(n, 3/4), sd(n))
head(contacts_by_mo_ex_nocontacts)

# Contact per start and end of each NPI period, adjusting for time till reporting of contacts (median 2 days)
contacts_per_case$NPI[contacts_per_case$date<"2020-09-03"&contacts_per_case$date>"2020-08-26"] <- "schools closed & 5 close contacts allowed end"
contacts_per_case$NPI[contacts_per_case$date>"2020-09-02"&contacts_per_case$date<"2020-09-10"] <- "schools open & 5 close contacts allowed start"
contacts_per_case$NPI[contacts_per_case$date>"2020-09-24"&contacts_per_case$date<"2020-10-02"] <- "schools open & 5 close contacts allowed end"
contacts_per_case$NPI[contacts_per_case$date>"2020-10-01"&contacts_per_case$date<"2020-10-09"] <- "schools open & limit close contacts suspended"
contacts_per_case$NPI[contacts_per_case$date>"2020-10-09"&contacts_per_case$date<"2020-10-17"] <- "schools open, bars closed & 3 close contacts allowed start"
contacts_per_case$NPI[contacts_per_case$date>"2020-10-20"&contacts_per_case$date<"2020-10-28"] <- "schools open, bars closed & 3 close contacts allowed end"
contacts_per_case$NPI[contacts_per_case$date>"2020-10-27"&contacts_per_case$date<"2020-11-04"] <- "schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed"
contacts_per_case$NPI[contacts_per_case$date>"2020-11-03"&contacts_per_case$date<"2020-11-11"] <- "schools closed, mandatory teleworking, non-essential\nshops closed and all of the above start"

contacts_by_NPI <- contacts_per_case %>%
  group_by(NPI) %>%
  summarise(number=n(), mean = mean(n), median = median(n), quantile(n, 1/4), quantile(n, 3/4), sd = sd(n))
contacts_by_NPI
contacts_by_NPI$lower <- contacts_by_NPI$mean - (1.96*contacts_by_NPI$sd/sqrt(contacts_by_NPI$number))
contacts_by_NPI$upper <- contacts_by_NPI$mean + (1.96*contacts_by_NPI$sd/sqrt(contacts_by_NPI$number))
contacts_by_NPI

# Contact per date
contacts_by_date <- contacts_per_case %>%
  group_by(date) %>%
  summarise(mean = mean(n), median = median(n), quantile(n, 1/4), quantile(n, 3/4), sd(n))
head(contacts_by_date)


#### 2. lin regression to evaluate effect of NPI on n contacts - total pop ####
contacttrend <- contacts_by_date
contacttrend$NPI[contacttrend$date<"2020-09-03"] <- "schools closed & 5 close contacts allowed \n"
contacttrend$NPI[contacttrend$date>"2020-09-02"&contacttrend$date<"2020-10-02"] <- "schools open & 5 close contacts allowed \n"
contacttrend$NPI[contacttrend$date>"2020-10-01"&contacttrend$date<"2020-10-08"] <- "schools open & limit close contacts suspended \n"
contacttrend$NPI[contacttrend$date>"2020-10-07"&contacttrend$date<"2020-10-28"] <- "schools open, bars closed & 3 close contacts allowed \n"
contacttrend$NPI[contacttrend$date>"2020-10-27"&contacttrend$date<"2020-11-04"] <- "schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n"
contacttrend$NPI[contacttrend$date>"2020-11-03"&contacttrend$date<"2020-11-13"] <- "schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n"

linregmodel_contactsNPI <- lm(mean ~ date + date*NPI, data=contacttrend) # the components, e.g. coefficients can be seen using names(linmodel) and linmodel$...
summary(linregmodel_contactsNPI)
linregmodel_contactsNPI_aug <- augment(linregmodel_contactsNPI)
tidy(linregmodel_contactsNPI)

# 95%CI 
confint <- predict(linregmodel_contactsNPI, interval = "confidence")
linregmodel_contactsNPI_aug <- merge(linregmodel_contactsNPI_aug, confint, by.x = ".fitted", by.y = "fit")

# plot fitted vs. observed
contactsNPI_plot <- ggplot(linregmodel_contactsNPI_aug) +
  geom_point(aes(x = date, y = mean, colour = factor(NPI, levels = c("schools closed & 5 close contacts allowed \n","schools open & 5 close contacts allowed \n","schools open & limit close contacts suspended \n","schools open, bars closed & 3 close contacts allowed \n","schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n","schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n"))), alpha = 0.5) +
  geom_line(aes(x = date, y = .fitted, colour = factor(NPI, levels = c("schools closed & 5 close contacts allowed \n","schools open & 5 close contacts allowed \n","schools open & limit close contacts suspended \n","schools open, bars closed & 3 close contacts allowed \n","schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n","schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n")))) +
  geom_line(aes(x = date, y = lwr, linetype = "dashed", colour = factor(NPI, levels = c("schools closed & 5 close contacts allowed \n","schools open & 5 close contacts allowed \n","schools open & limit close contacts suspended \n","schools open, bars closed & 3 close contacts allowed \n","schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n","schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n")))) +
  geom_line(aes(x = date, y = upr, linetype = "dashed", colour = factor(NPI, levels = c("schools closed & 5 close contacts allowed \n","schools open & 5 close contacts allowed \n","schools open & limit close contacts suspended \n","schools open, bars closed & 3 close contacts allowed \n","schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n","schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n")))) +
  theme_bw()+labs(title="", x = "", y="Average number of contacts reported per case") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(limits = c(0, 3.55)) 
contactsNPI_plot
ggsave(contactsNPI_plot, filename = "contactsNPI_plot.png", width = 11, height = 6)

# scale date to numeric - to improve interpretation of the equations (days sin2e Aug 1 instead of 18474 etc)
contacttrend_numericdate <- contacttrend
contacttrend_numericdate$date <- as.Date(contacttrend_numericdate$date)
contacttrend_numericdate <- subset(contacttrend_numericdate, date > 18474)
contacttrend_numericdate$date <- as.numeric(contacttrend_numericdate$date)-18474

linregmodel_contacttrend_numericdate <- lm(mean ~ date + date*NPI, data=contacttrend_numericdate) # the components, e.g. coefficients can be seen using names(linmodel) and linmodel$...
summary(linregmodel_contacttrend_numericdate)
linregmodel_contacttrend_numericdate_aug <- augment(linregmodel_contacttrend_numericdate)
tidy(linregmodel_contacttrend_numericdate)

# BY AGE
# add a variable age of the index case to the contact list
# age from final_linelist
linelistages <- subset(final_linelist, select = c("sciensano_request_ticket_number","age"))
linelistages$agegr[linelistages$age<10] <- "0-9yo"
linelistages$agegr[linelistages$age>9.99&linelistages$age<20] <- "10-19yo"
linelistages$agegr[linelistages$age>19.99&linelistages$age<30] <- "20-29yo"
linelistages$agegr[linelistages$age>29.99&linelistages$age<40] <- "30-39yo"
linelistages$agegr[linelistages$age>39.99&linelistages$age<50] <- "40-49yo"
linelistages$agegr[linelistages$age>49.99&linelistages$age<60] <- "50-59yo"
linelistages$agegr[linelistages$age>59.99&linelistages$age<70] <- "60-69yo"
linelistages$agegr[linelistages$age>69.99] <- "70+yo"

linelistages$largeagegr[linelistages$age<18] <- "0-17"
linelistages$largeagegr[linelistages$age>17.99&linelistages$age<25] <- "18-24"
linelistages$largeagegr[linelistages$age>24.99&linelistages$age<70] <- "25-69"
linelistages$largeagegr[linelistages$age>69.99] <- "70+"

# link cases and their contacts, to have age of index
indexcasecontacts <- merge(linelistages, contacts_per_case, by.x = "sciensano_request_ticket_number", by.y = "sciensano_origin_ticket_number")

# number of cases with age available
indexcasecontacts_age <- indexcasecontacts %>%
  filter(!is.na(age)) 
count(indexcasecontacts_age)

# total of contacts matched to cases with age available
sum(indexcasecontacts_age$n)

# mean contacts by agegr per date
contacts_by_age_date <- indexcasecontacts %>%
  group_by(largeagegr, date) %>%
  summarise(mean = mean(n), median = median(n), quantile(n, 1/4), quantile(n, 3/4), sd(n))
head(contacts_by_age_date)

#### 3. performance of contact tracing: % of reported cases from whom contacts could be obtained ####
# dataframes
# number of cases reporting at least 1 contact - still need to check whether the TO2 date corresponds to date when the index case was reported
firstcontactdate <- no_dupli_TO2 
firstcontactdate$date <- as.Date(firstcontactdate$date)
firstcontactdate <- firstcontactdate %>%
  group_by(sciensano_origin_ticket_number)%>%
  summarise(firstcontactdate = min(date))
number_of_cases_successful_contacttracing <- firstcontactdate %>%
  group_by(firstcontactdate) %>%
  summarise(ncasescontactraced = n())
# create moving 7 day avg for this number of cases with successful contact tracing 
contacttracing_ts <- zoo(number_of_cases_successful_contacttracing$ncasescontactraced, order.by = number_of_cases_successful_contacttracing$firstcontactdate)
contacttracing_MA7days <- rollapply(contacttracing_ts, width=7, FUN=mean, align='center')
contacttracing_MA7days_df <- data.frame(contacttracing_MA7days)
contacttracing_MA7days_df <- tibble::rownames_to_column(contacttracing_MA7days_df, "date")
contacttracing_MA7days_df$date <- as.Date(contacttracing_MA7days_df$date)

# add denominator (the number of reported cases each day) from casesallts_df
contacttracingperformance <- merge(contacttracing_MA7days_df, casesallts_df, by = "date")
head(contacttracingperformance)
contacttracingperformance$pct_contacttraced <- round(contacttracingperformance$contacttracing_MA7days*100/contacttracingperformance$casesallMA7days, 2)                                   
# save
save(contacttracingperformance, file = "contacttracingperformance.Rdata")
# plot
contacttracingperformance_AugNov <- subset(contacttracingperformance, date>"2020-07-31")
ggplot2::theme_set(theme_classic(base_size = 18))
contacttracingperformance_plot <- ggplot(contacttracingperformance, aes(date)) + 
  geom_line(aes(y = pct_contacttraced)) +  
  labs(title="", x = "", y="Pct of reported cases with succesful contact tracing (7-day moving average, %)") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank()) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_date(breaks = pretty_breaks(10))
contacttracingperformance_plot
ggsave(contacttracingperformance_plot, filename = "contacttracingperformance_plot.png", width = 9, height = 4)


#### 4. log normal regression of time series mean contacts and Rt ####

# combine dataframes
contactsRt <- merge (R_Bxl_allages, mean_contacts_by_day_ex_nocontacts, by = "date")
# also merge with a variable for the day by day performance of contact tracind
contactsRt <- merge (contactsRt, contacttracingperformance, by = "date")
str(contactsRt)

# add interaction term for testing strategy (all vs symptomatic only)
contactsRt$testing <- "all"
contactsRt$testing[contactsRt$date>"2020-09-20"] <- "only if symptoms"

# create a date counting days from Aug 1
contactsRt$days <- as.numeric(contactsRt$date)-18474

# add lag to contacts

# add lag to Rt

# take the log of R_e
contactsRt$logRe <- log(contactsRt$R_e_median)

# build regression model with Re as dependent variable and independent variables: mean n of contacts, contact tracing performance (%) and testing strategy (binary)
linregmodel_contactsRt <- lm(R_e_median ~ mean + date + date*mean + date*testing + date*pct_contacttraced, data=contactsRt) 
summary(linregmodel_contactsRt)
linregmodel_contactsRt_aug <- augment(linregmodel_contactsRt)
tidy(linregmodel_contactsRt)

# 95%CI 
confint <- predict(linregmodel_contactsRt, interval = "confidence")
linregmodel_contactsRt_aug <- merge(linregmodel_contactsRt_aug, confint, by.x = ".fitted", by.y = "fit")

# plot fitted vs. observed
linregmodel_contactsRt_plot <- ggplot(linregmodel_contactsRt_aug) +
  geom_point(aes(x = date, y = R_e_median, colour = factor(testing)), alpha = 0.5) +
  geom_line(aes(x = date, y = .fitted)) +
  geom_line(aes(x = date, y = lwr)) +
  geom_line(aes(x = date, y = upr)) +
  theme_bw()+labs(title="", x = "", y="Estimated effective R") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(limits = c(0, 2)) 
linregmodel_contactsRt_plot
ggsave(linregmodel_contactsRt_plot, filename = "linregmodel_contactsRt_plot.png", width = 11, height = 6)

# scale date to numeric - to improve interpretation of the equations (days since Aug 1 instead of 18474 etc)
linregmodel_contactsRt_days <- lm(R_e_median ~ mean + days + days*mean + days*testing, data=contactsRt) 
summary(linregmodel_contactsRt_days)
tidy(linregmodel_contactsRt_days)
