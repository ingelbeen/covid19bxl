####################################################
# BXL CONTACT TRACING ANALYSES                     #
# REGRESSION NPI -> CONTACTS                       #
####################################################
# last update 261120 by Brecht Ingelbeen
rm(list = ls())

# SET OUTPUT DIRECTORY
OutputDirectory <- "./report_outputs/"
OutputDirectoryPaper <- "./report_outputs/figures_paper/"
OutputDirectoryData <- "../paper/data/clean/regression/"


PACKAGES = c(
  "readxl", "writexl", "lubridate", "zoo", "ggplot2", "scales","generics","splines","boot",
  "epicontacts", "tidyverse", "formattable", "igraph", "viridis", "ggthemes", "ggpubr"
)

# install packages if needed
for (pack_name in PACKAGES) {
  if (!pack_name %in% rownames(installed.packages()))
    install.packages(pack_name)
}

# load package
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, scales, generics,splines,boot,
               epicontacts, tidyverse, formattable, igraph, Hmisc, viridis,ggthemes, ggpubr)

latest_date = "2020-11-12"

# LOAD DATA
Filename <- paste0("./data/clean/cleaned_datasets_linked_",latest_date,".RData")
load(Filename)

dir = "./data/clean/regression/regression_contacts/"
#### 0. import and clean data: contacts and estimated Rt between 1 Aug and 12 Nov ####
# load dataframes
load(file = paste0(dir,"R_Bxl_allages.Rdata")) # saved at the end of the rt_estim.R script
load(file = paste0(dir,"no_dupli_TO2_AugNov.Rdata")) # saved below in this script
load(file = paste0(dir,"mean_contacts_by_day_ex_nocontacts.Rdata")) # saved below in this script
load(file = paste0(dir,"casesMA7days.Rdata")) # the 7 days moving average of reported cases in bxl
load(file = paste0(dir,"contacttracingperformance.Rdata")) # the number of reported cases for whom contacttracing could be successfully done 

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

# Check normality in contacts
ggqqplot(mean_contacts_by_day_ex_nocontacts$n)
shapiro.test(mean_contacts_by_day_ex_nocontacts$n)

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

# Relative change contacts
(contacts_by_NPI[4, c(3,8,9)]-contacts_by_NPI[1,c(3,8,9)])/contacts_by_NPI[1,c(3,8,9)] # change period 2 vs 1
(contacts_by_NPI[6, c(3,8,9)]-contacts_by_NPI[7,c(3,8,9)])/contacts_by_NPI[7,c(3,8,9)] # change period 4 vs 3
(contacts_by_NPI[8, c(3,8,9)]-contacts_by_NPI[7,c(3,8,9)])/contacts_by_NPI[8,c(3,8,9)] # change period 5 vs 3



# Contact per date
contacts_by_date <- contacts_per_case %>%
  group_by(date) %>%
  summarise(mean = mean(n), median = median(n), quantile(n, 1/4), quantile(n, 3/4), sd(n))
head(contacts_by_date)


#### 2. lin regression to evaluate effect of NPI on n contacts - total pop ####
contacttrend <- contacts_by_date
contacttrend$day = c(1:length(contacttrend$date))
contacttrend$NPI[contacttrend$date<"2020-09-03"] <- "schools closed & 5 close contacts allowed \n"
contacttrend$NPI[contacttrend$date>"2020-09-02"&contacttrend$date<"2020-10-02"] <- "schools open & 5 close contacts allowed \n"
contacttrend$NPI[contacttrend$date>"2020-10-01"&contacttrend$date<"2020-10-08"] <- "schools open & limit close contacts suspended \n"
contacttrend$NPI[contacttrend$date>"2020-10-07"&contacttrend$date<"2020-10-28"] <- "schools open, bars closed & 3 close contacts allowed \n"
contacttrend$NPI[contacttrend$date>"2020-10-27"&contacttrend$date<"2020-11-04"] <- "schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n"
contacttrend$NPI[contacttrend$date>"2020-11-03"&contacttrend$date<"2020-11-13"] <- "schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n"
contacttrend %>% 
  mutate(NPI = factor(NPI),
         NPI = factor(NPI, levels = c("schools closed & 5 close contacts allowed \n","schools open & 5 close contacts allowed \n",
                                      "schools open & limit close contacts suspended \n",
                                      "schools open, bars closed & 3 close contacts allowed \n",
                                      "schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n",
                                      "schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n"
         ))
         )

contacttrend = contacttrend %>% 
  mutate(NPI2 = case_when(
  date>="2020-08-01" &date<"2020-09-03" ~ "1",
  date>="2020-09-03" &date<"2020-10-02" ~ "2",
  date>="2020-10-02" &date<"2020-10-08" ~ "3",
  date>="2020-10-08" &date<"2020-10-28" ~ "4",
  date>="2020-10-27" &date<"2020-11-04" ~ "5",
  date>="2020-11-04" &date<"2020-11-13" ~ "6",
  TRUE ~ "other"),
  NPI2 = as.factor(NPI2)
  )

# contacttrend = contacttrend %>%
#   mutate(NPI = ifelse(NPI == "schools closed & 5 close contacts allowed \n", 1, 0))

linregmodel_contactsNPI <- lm(mean ~ day + day*NPI2, data=contacttrend) # the components, e.g. coefficients can be seen using names(linmodel) and linmodel$...
summary(linregmodel_contactsNPI)
round(coef(linregmodel_contactsNPI),4)

linregmodel_contactsNPI_aug <- augment(linregmodel_contactsNPI)
tidy(linregmodel_contactsNPI)

# Calculate the times at which the interventions occurred
max1<-max(contacttrend$day[contacttrend$NPI == "schools closed & 5 close contacts allowed \n"])
max2<-max(contacttrend$day[contacttrend$NPI == "schools open & 5 close contacts allowed \n"])
max3<-max(contacttrend$day[contacttrend$NPI == "schools open & limit close contacts suspended \n"])
max4<-max(contacttrend$day[contacttrend$NPI == "schools open, bars closed & 3 close contacts allowed \n"])
max5<-max(contacttrend$day[contacttrend$NPI == "schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n"])
max6<-max(contacttrend$day[contacttrend$NPI == "schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n"])

# Calculate the individual intercepts
coef(linregmodel_contactsNPI)[1] + coef(linregmodel_contactsNPI)[3] + max1* (coef(linregmodel_contactsNPI)[2] + coef(linregmodel_contactsNPI)[8])
coef(linregmodel_contactsNPI)[1] + coef(linregmodel_contactsNPI)[4] + max2* (coef(linregmodel_contactsNPI)[2] + coef(linregmodel_contactsNPI)[9])
coef(linregmodel_contactsNPI)[1] + coef(linregmodel_contactsNPI)[5] + max3* (coef(linregmodel_contactsNPI)[2] + coef(linregmodel_contactsNPI)[10])
coef(linregmodel_contactsNPI)[1] + coef(linregmodel_contactsNPI)[6] + max4* (coef(linregmodel_contactsNPI)[2] + coef(linregmodel_contactsNPI)[11])
coef(linregmodel_contactsNPI)[1] + coef(linregmodel_contactsNPI)[7] + max5* (coef(linregmodel_contactsNPI)[2] + coef(linregmodel_contactsNPI)[12])

# Get the beta estimates, comparing period x with its previous period, by changing each time the reference level.Then we can just
# take for each particular period that interests us (e.g. period 4), the model that takes the previous period as reference.
m_ref2 <- lm(mean ~ day + day*relevel(NPI2, ref="2"), data=contacttrend) # the components, e.g. coefficients can be seen using names(linmodel) and linmodel$...
summary(m_ref2)

m_ref4 <- lm(mean ~ day + day*relevel(NPI2, ref="4"), data=contacttrend) # the components, e.g. coefficients can be seen using names(linmodel) and linmodel$...
summary(m_ref4)

# m_ref2 shows that period 4 difference significantly from the times when schools were open (period 2)
coef(m_ref2)[10]
confint(m_ref2)[10,]

# 95%CI 
confint <- predict(linregmodel_contactsNPI, interval = "confidence")
linregmodel_contactsNPI_aug <- merge(linregmodel_contactsNPI_aug, confint, by.x = ".fitted", by.y = "fit")

# plot fitted vs. observed
mycol = economist_pal()(8)
mycol = c("#00887d","#01a2d9","#76c0c1","#ee8f71","#6794a7", "#adadad")
linesize = 1
pointsize =2
linesize_i = 1.5
alphas = 0.4
linetype = 1
knots = as.Date(c("2020-09-01","2020-09-30","2020-10-07","2020-10-26","2020-11-02"))

# Plot continious line
contactsNPI_plot_con <- ggplot(linregmodel_contactsNPI_aug) +
  geom_point(aes(x = day, y = mean, colour = contacttrend$NPI_factor), alpha = 0.5) +
  geom_line(aes (x = day, y = .fitted, colour = contacttrend$NPI_factor))+
  geom_line(aes (x = day, y = lwr, colour = contacttrend$NPI_factor))+
  geom_line(aes (x = day, y = upr, colour = contacttrend$NPI_factor))+
  #geom_line(aes(x = day, y = lwr, linetype = "dashed", colour = factor(NPI, levels = c("schools closed & 5 close contacts allowed \n","schools open & 5 close contacts allowed \n","schools open & limit close contacts suspended \n","schools open, bars closed & 3 close contacts allowed \n","schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n","schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n")))) +
  #geom_line(aes(x = day, y = upr, linetype = "dashed", colour = factor(NPI, levels = c("schools closed & 5 close contacts allowed \n","schools open & 5 close contacts allowed \n","schools open & limit close contacts suspended \n","schools open, bars closed & 3 close contacts allowed \n","schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n","schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n")))) +
  theme_bw()+labs(title="", x = "", y="Average number of contacts reported per case") +
  theme(legend.title = element_blank()) +
  #scale_y_continuous(limits = c(0, 3.55)) 
  scale_y_continuous(limits = c(0, 3.5))
contactsNPI_plot_con

dates = as.Date(min(contacttrend$date)+c(0,14,31,45,61,75,92, 106))
dates = c("aug 01","aug 15","sep 01","sep 15","okt 01","okt 15", "nov 01")

contactsNPI_plot <- ggplot(linregmodel_contactsNPI_aug) +
  geom_point(aes(x = day, y = mean, alpha = 0.5),size=pointsize) +
  geom_line(aes(x = day, y = .fitted, col=NPI2), size=linesize) +
  geom_line(aes(x = day, y = lwr, col=NPI2), linetype = "dashed",size=linesize) +
  geom_line(aes(x = day, y = upr, col=NPI2), linetype = "dashed",size=linesize)+
  theme_bw()+labs(title="", x = "", y="Average number of contacts reported per case") +
  theme(legend.title = element_blank()) +
  theme(axis.text = element_text(size=15),
        axis.title=element_text(size=15),
        legend.text= element_text(size=15),
        legend.title= element_text(size=15),
        strip.text.x = element_text(size = 20),
        plot.title = element_text(size=22))+
  scale_color_manual(values=rep("black",6))+
    scale_x_continuous(breaks = c(1,15,32,46,62,75,92),
                       labels =dates
    )  +
  geom_vline(aes(xintercept=max1-1),colour = mycol[1],size=linesize_i, alpha=alphas, linetype=linetype)+
  geom_vline(aes(xintercept=max2-1),colour = mycol[2],size=linesize_i, alpha=alphas, linetype=linetype)+
  geom_vline(aes(xintercept=max3),colour = mycol[3],size=linesize_i, alpha=alphas, linetype=linetype)+
  geom_vline(aes(xintercept=max4-1),colour = mycol[4],size=linesize_i, alpha=alphas, linetype=linetype)+
  geom_vline(aes(xintercept=max5-1),colour = mycol[5],size=linesize_i, alpha=alphas, linetype=linetype)+
  scale_y_continuous(limits = c(0, 3.55)) +
  guides(alpha=F, col=F)
contactsNPI_plot

# contactsNPI_plot <- ggplot(linregmodel_contactsNPI_aug) +
#   geom_point(aes(x = day, y = mean, colour = factor(NPI, levels = c("schools closed & 5 close contacts allowed \n","schools open & 5 close contacts allowed \n","schools open & limit close contacts suspended \n","schools open, bars closed & 3 close contacts allowed \n","schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n","schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n"))), alpha = 0.5) +
#   geom_line(aes(x = day, y = .fitted, colour = factor(NPI, levels = c("schools closed & 5 close contacts allowed \n","schools open & 5 close contacts allowed \n","schools open & limit close contacts suspended \n","schools open, bars closed & 3 close contacts allowed \n","schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n","schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n")))) +
#   geom_line(aes(x = day, y = lwr, linetype = "dashed", colour = factor(NPI, levels = c("schools closed & 5 close contacts allowed \n","schools open & 5 close contacts allowed \n","schools open & limit close contacts suspended \n","schools open, bars closed & 3 close contacts allowed \n","schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n","schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n")))) +
#   geom_line(aes(x = day, y = upr, linetype = "dashed", colour = factor(NPI, levels = c("schools closed & 5 close contacts allowed \n","schools open & 5 close contacts allowed \n","schools open & limit close contacts suspended \n","schools open, bars closed & 3 close contacts allowed \n","schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n","schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n")))) +
#   theme_bw()+labs(title="", x = "", y="Average number of contacts reported per case") +
#   theme(legend.title = element_blank()) +
#   scale_y_continuous(limits = c(0, 3.55)) 
# contactsNPI_plot



# SAVE PLOT
pdf(file=paste0(OutputDirectoryPaper,"SuppFig2_20201218.pdf"), width=14, height=8)
contactsNPI_plot
dev.off()

ggsave(contactsNPI_plot, filename = paste0(OutputDirectory,"contactsNPI.png"), width = 11, height = 6)


# save dataframes
# save(no_dupli_TO2_AugNov, file = paste0(OutputDirectoryData,"no_dupli_TO2_AugNov.Rdata"))
# save(mean_contacts_by_day_ex_nocontacts, file = paste0(OutputDirectoryData,"mean_contacts_by_day_ex_nocontacts.Rdata")) 
# save(contacttrend,file=paste0(OutputDirectoryData,"contacts.Rdata"))
