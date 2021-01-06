################################################################################
# REGRESSION ANALYSES CONTACTS
################################################################################

# Date: 27 November 2020


rm(list = ls())

# SET OUTPUT DIRECTORY
OutputDirectory <- "./report_outputs/"
OutputDirectoryPaper <- "./report_outputs/figures_paper/"
OutputDirectoryData <- "../paper/data/clean/regression/"


PACKAGES = c(
  "readxl", "writexl", "lubridate", "zoo", "ggplot2", "scales","jtools","generics","nlstools","investr",
  "epicontacts", "tidyverse", "formattable", "igraph", "viridis", "ggthemes", "gam" 
)

# install packages if needed
for (pack_name in PACKAGES) {
  if (!pack_name %in% rownames(installed.packages()))
    install.packages(pack_name)
}

# load package
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, scales,jtools,generics, nlstools,investr,
               epicontacts, tidyverse, formattable, igraph, Hmisc, viridis,ggthemes, gam)

# load functions
source("./functions/multiplot.R")
# Load data
#Filename <- "./data/clean/cleaned_dataset.RData"

latest_date = "2020-11-12"

# Load data
Filename <- paste0("./data/clean/cleaned_datasets_linked_",latest_date,".RData")
rt = paste0("./data/clean/regression/regression_contacts/R_Bxl_allages.Rdata")
contacts = paste0("./data/clean/regression/regression_contacts/mean_contacts_by_day_ex_nocontacts.Rdata")
tracing = paste0("./data/clean/regression/regression_contacts/contacttracingperformance.Rdata")

load(Filename)
load(rt) ; rt = R_Bxl_allages
load(contacts) ; contacts = mean_contacts_by_day_ex_nocontacts
load(tracing) ; tracing = contacttracingperformance

names_datasets = c("contacttracingperformance","R_Bxl_allages", "mean_contacts_by_day_ex_nocontacts")

# Remove datasets with oldname
rm(list = ls()[which(ls() %in% names_datasets)])

# DEFINE VARIABLES
#################################################################
# Period
start = as.Date("2020-09-01")
end = as.Date("2020-11-12")

length_period = end-start
period = c(start+ c(0:length_period))
periodd = as.data.frame(period) %>%
  mutate(days = c(1:length(period)))%>%
  dplyr::rename(date = "period") 

# Build up dataset
###########################################################################
mean_pct = mean(tracing$pct_contacttraced, na.rm=T)
mean_rt = mean(rt$R_e_median, na.rm=T)
mean_cases = mean(tracing$casesallts)
mean_casesMA7 = mean(tracing$casesallMA7days)

d = left_join(contacts,tracing)
d2 = left_join(d,rt) %>%
  dplyr::rename(contact_mean = "mean",
         contact_sd = "sd",
         re_median = "R_e_median",
         re_lower = "R_e_q0025",
         Re_upper = "R_e_q0975",
         npi = "NPI",
         cases = "casesallts",
         cases_ma7 = "casesallMA7days",
         tracing_ma7 = "contacttracing_MA7days", 
         pct_traced = "pct_contacttraced"
  ) %>%
  mutate(testing = ifelse(date>"2020-10-20","only if symptoms", "All"),
         re_median_l = log(re_median),
         pct_traced_imp = ifelse(is.na(pct_traced), mean_pct,pct_traced), # impute NA with mean 
         re_median_imp = ifelse(is.na(re_median), mean_rt,re_median),# impute NA with mean 
         re_median_l_imp = log(re_median_imp),
         cases_imp = ifelse(is.na(cases), mean_cases, cases),
         cases_ma7_imp = ifelse(is.na(cases_ma7), mean_casesMA7,cases_ma7),
         contact_mean_exp = exp(contact_mean),
         contact_mean_l = log(contact_mean),
         contact_case_int_l = log(contact_mean*cases_imp),
         contact_case_int_n = (contact_mean*cases_imp)/100000, 
         month = as.character(month(date)),
         intervention = case_when(
           # date>="2020-08-01" &date<"2020-09-01" ~ "schools closed & 5 close contacts allowed \n",
           # date>="2020-09-01" &date<"2020-09-30" ~ "schools open & 5 close contacts allowed \n",
           # date>="2020-09-30" &date<"2020-10-07" ~ "schools open & limit close contacts suspended \n",
           # date>="2020-10-07" &date<"2020-10-26" ~ "schools open, bars closed & 3 close contacts allowed \n",
           # date>="2020-10-25" &date<"2020-11-02" ~ "schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n",
           # date>="2020-11-02" &date<"2020-11-13" ~ "schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n",
           # TRUE ~ "other")
           date>="2020-08-01" &date<"2020-09-01" ~ "1",
           date>="2020-09-01" &date<"2020-09-30" ~ "2",
           date>="2020-09-30" &date<"2020-10-07" ~ "3",
           date>="2020-10-07" &date<"2020-10-26" ~ "4",
           date>="2020-10-25" &date<"2020-11-02" ~ "5",
           date>="2020-11-02" &date<"2020-11-13" ~ "6",
           TRUE ~ "other")
         
  ) %>%
  filter(date>=start)

# d2$intervention = factor(d2$intervention, levels=c("schools closed & 5 close contacts allowed \n","schools open & 5 close contacts allowed \n",
#                             "schools open & limit close contacts suspended \n", "schools open, bars closed & 3 close contacts allowed \n",
#                             "schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n",
#                             "schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n"))
# 
ds = d2 

#dsf = ds%>%filter(intervention!="schools closed & 5 close contacts allowed \n")
dsf = ds%>%filter(intervention!="1")

# Exploratory plots
###########################################################################
p=ggplot(ds, aes(x=contact_mean)) + geom_histogram() +
  xlab("Mean number of contacts")+
  theme_bw()

p1=ggplot(ds, aes(x=re_median)) + geom_histogram()+
  xlab("Re")+
  theme_bw()

p2=ggplot(ds, aes(x=pct_traced)) + geom_histogram()+
  xlab("% contacts traced")+
  theme_bw()

p3=ggplot(ds, aes(x=contact_mean, y=re_median)) + geom_point(aes(col=intervention)) + 
  geom_smooth(method = "gam", formula = y ~ s(x)) +
  ylab("Re")+
  theme_bw()+
  xlab("Mean number of contacts per day")

p4=ggplot(ds, aes(x=pct_traced, y=re_median)) + geom_point(aes(col=intervention))+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  ylab("Re")+
  theme_bw()+
  xlab("% contacts traced")

p5=ggplot(ds, aes(x=pct_traced, y=contact_mean)) + geom_point(aes(col=intervention))+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  ylab("Mean number of contacts")+
  theme_bw()+
  xlab("% contacts traced")

p6 =ggplot(ds, aes(x=date, y=pct_traced)) + 
  geom_point()+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  ylab("% contacts traced")+
  theme_bw()+
  xlab("Date")

p7=ggplot(ds, aes(x=date, y=contact_mean)) + 
  geom_point()+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  ylab("Mean number of contacts per day")+
  theme_bw()+
  xlab("Date")


p8=ggplot(ds, aes(x=date, y=re_median)) + 
  geom_point()+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  ylab("Rt")+
  theme_bw()+
  xlab("Date")

p9=ggplot(ds, aes(x=date, y=cases)) + 
  geom_point()+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()+
  ylab("Cases")+
  xlab("Date")

p10=ggplot(ds, aes(x=pct_traced, y=cases)) + 
  geom_point(aes(col=intervention))+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()+
  theme_bw()+
  ylab("Cases")+
  xlab("% contacts traced")

ggplot(ds, aes(x=re_median, y=log(re_median))) + geom_point(aes(col=intervention))+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()+
  ylab("log(Re)")+
  xlab("Re")

ggplot(ds, aes(x=contact_mean, y=re_median)) + geom_point(aes(col=intervention))+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()+
  ylab("Re")+
  xlab("mean number of contacts")

ggplot(ds, aes(x=log(contact_mean), y=re_median)) + geom_point(aes(col=intervention))+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()+
  ylab("Re")+
  xlab("mean number of contacts")

ggplot(ds, aes(x=contact_mean, y=cases)) + geom_point(aes(col=intervention))+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()+
  ylab("cases")+
  xlab("mean number of contacts")

ggplot(ds, aes(x=cases, y=re_median)) + geom_point(aes(col=intervention))+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()+
  ylab("Re")+
  xlab("Cases")

ggplot(ds, aes(x=contact_case_int_l, y=re_median)) + geom_point(aes(col=intervention)) +
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()+
  ylab("Rt")+
  xlab("log(mean number of contacts*cases)")

ggplot(ds, aes(x=contact_case_int_n, y=re_median)) + geom_point(aes(col=intervention)) +
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()+
  ylab("Rt")+
  xlab("mean number of contacts*(cases/n)")

multiplot(p,p2,p1,p4,p3,p5,p6,p7,p8,p9,p10,cols=4)

multiplot(p4,p3,p5,p10,cols=2)

multiplot(p6,p7,p8,p9,cols=2)


###################################################################
# FIT NEGATIVE EXPONENTIAL
##################################################################
# So we will have to take the estimates of contact_mean and put them in a negative exponential model
# https://rpubs.com/mengxu/exponential-model
# Prepare a good inital state. Highest Rt recorded during study period is 1.5 (representing second wave). 
# Highest ever recorded Rt for Belgium was 2.6 (first wave)
# https://epistat.wiv-isp.be/covid/ (Sciensano dashboard)

# We therefore ensure theta must be lower than 2.6, and greater than zero. As alternative could opt for max(Rt)
theta.0 <- 2.6 * 1.1
#theta.0 <- max(ds$re_median) * 1.1

# Take initial values from a lm
model.0 <- lm(log(- re_median + theta.0) ~ contact_mean, data=ds)

alpha.0 <- -exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]

# For Exponential decay model
#const.0 <- exp(coef(model.0)[1])
#beta.0 <- -coef(model.0)[2]

start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
# Expontential decay model
#start <- list(const = const.0, beta = beta.0)

# Fit the non-linear model
nlc <- nls.control(maxiter = 1000)

# Exponential decay model with constant
model <- nls(re_median ~ alpha * exp(beta * contact_mean) + theta , data = ds, start = start, control=nlc)
# Exponential decay model
#model <- nls(re_median ~ const * (1-exp(-beta * contact_mean)), data = ds, start = start, control=nlc)
summary(model)

newdata = ds
# make new data with contacts is 0 to check assumptions model
#newdata$contact_mean[1] = 0
#newdata$contact_mean[nrow(newdata)] = 6

fit <- as_tibble(predFit(model, newdata=newdata, interval = "confidence", level= 0.9))
fit = fit %>% 
  mutate(re_median = ds$re_median,
         contact_mean=newdata$contact_mean,
         month = ds$month,
         intervention = ds$intervention
  )

# Plot Fit
# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 65, c = 100)[1:n]
# }
# cols = gg_color_hue(6)[2:6]

cols = economist_pal()(8)
cols = c("#00887d","#01a2d9","#76c0c1","#6794a7", "#ee8f71","#adadad")[2:6]

plot_nls = ggplot(fit, aes(contact_mean, re_median))+
  geom_hline(aes(yintercept=1), linetype=2) + 
  geom_point(aes(col=intervention, size=1)) +
  geom_line(aes(x=contact_mean, y=fit), size=2)+
  geom_line(aes(x=contact_mean, y=lwr), size=1, lty=2)+
  geom_line(aes(x=contact_mean, y=upr), size=1, lty=2)+
  theme_bw()+labs(title="Model fit: Rt ~ theta + alpha*exp(Beta*mean_contacts)", x = "Mean number of contacts", y="Rt (estimated)") +
  theme(legend.title = element_blank(),
        axis.text = element_text(size=15),
        axis.title=element_text(size=15),
        legend.text= element_text(size=15),
        strip.text.x = element_text(size = 20),
        plot.title = element_text(size=15))+
  scale_color_manual(values=cols)+
  labs(colour = "Intervention")+
  guides(size=F)
plot_nls
#ggsave("NLS_model_fit.png", plot =plot_nls, device="png", path = OutputDirectory, width=13,height=9)


# Fit similar model but with linear regression (by taking log contacts)
model.lm <- lm(re_median ~ contact_mean_l, data=ds) # Actually should plot a 5 day lag
summary(model.lm)

fit <- as_tibble(predFit(model.lm, newdata=as.data.frame(ds), interval = "confidence", level= 0.9))
fit = fit %>% 
  mutate(re_median = ds$re_median,
         contact_mean_l=ds$contact_mean_l,
         contact_mean=ds$contact_mean,
         month = ds$month,
         intervention = ds$intervention
  )

# # Plot Fit
plot_lm = ggplot(fit, aes(contact_mean, re_median))+
  geom_hline(aes(yintercept=1), linetype=2)+ 
  geom_point(aes(col=intervention, size=1)) +
  geom_line(aes(x=contact_mean, y=fit), size=2)+
  geom_line(aes(x=contact_mean, y=lwr), size=1, lty=2)+
  geom_line(aes(x=contact_mean, y=upr), size=1, lty=2)+
  theme_bw()+labs(title="Model fit: Rt ~ beta_1*log(mean_contact)", x = "Mean number of contacts", y="Rt (estimated)") +
  theme(legend.title = element_blank(),
        axis.text = element_text(size=15),
        axis.title=element_text(size=15),
        legend.text= element_text(size=15),
        strip.text.x = element_text(size = 20),
        plot.title = element_text(size=15))+
  scale_color_manual(values=cols)+
  guides(size=F)
plot_lm



# FIT EXP RELATIONSHIP CONTACTS*CASES
# Fit similar model but with linear regression (by taking log contacts*cases)
model.lm.case <- lm(re_median ~ contact_case_int_n, data=ds) # Actually should plot a 5 day lag
summary(model.lm.case)

fit.case <- as_tibble(predict(model.lm.case, newdata=as.data.frame(ds), interval = "confidence", level= 0.9))
fit.case = fit.case %>% 
  mutate(re_median = ds$re_median,
         contact_mean_l=ds$contact_mean_l,
         contact_mean=ds$contact_mean,
         contact_case_int_n = ds$contact_case_int_n,
         month = ds$month,
         intervention = ds$intervention
  )

# Plot Fit
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols = gg_color_hue(6)[2:6]

plot_lm.case = ggplot(fit.case, aes(contact_case_int_n, re_median)) + geom_point(aes(col=intervention, size=1)) +
  geom_line(aes(x=contact_case_int_n, y=fit), size=2)+
  geom_line(aes(x=contact_case_int_n, y=lwr), size=1, lty=2)+
  geom_line(aes(x=contact_case_int_n, y=upr), size=1, lty=2)+
  theme_bw()+labs(title="Model fit: Rt ~ alpha + beta_1*log(mean_contact*cases)", x = "log(Mean number of contacts*cases)", y="Rt (estimated)") +
  theme(legend.title = element_blank(),
        axis.text = element_text(size=15),
        axis.title=element_text(size=15),
        legend.text= element_text(size=15),
        strip.text.x = element_text(size = 20),
        plot.title = element_text(size=15),
        legend.position = c(0.75,0.2))+
  scale_color_manual(values=cols)+
  guides(size=F)
plot_lm.case



#################################################
# FUNCTION TO GET ESTIMATES FROM LM AND PLOT FIT
#################################################

# function to get fit
get_fit <- function(model, lag=lag,data=data, out=out, out_log=0) {
  md <- augment(model)
  if(length(which(names(md)%in%".rownames"))==0){
    md$".rownames" = c(1:length(model$fitted.values))
  }
  md <- md %>%
  dplyr::rename(days = ".rownames") %>%
    mutate(days=as.numeric(days))
  md = left_join(md,periodd)
  re_median = ifelse(out_log==1,exp(md[,which(names(md)==out)]),md[,which(names(md)==out)])[[1]]
  md = md %>%
    mutate(re_median=re_median,
           testing2 = ifelse(date<"2020-10-20", "All","Symptomatic only"))
  tidy(model)
  # 95%CI 
  confint = data.frame(predict(model, interval = "confidence", data=data[lag:nrow(data),]))
  if(out_log==1){ 
    confint = confint %>%
      mutate(fit_e = exp(fit),
             lwr_e = exp(lwr),
             upr_e= exp(upr)
      )
  }else{
    confint = confint %>%
      mutate(fit_e = fit,
             lwr_e = lwr,
             upr_e= upr
      )
  }
  md <- merge(md, confint, by.x = ".fitted", by.y = "fit")
  return(md)
}

# Function to plot fit
plot_m= function(model_data, name="model_name"){
  model_data %>%
    ggplot(.) +
    geom_point(aes(x = date, y = re_median, colour = factor(testing2)), alpha = 0.5, size=3) +
    geom_line(aes(x = date, y = fit_e), size=1) +
    geom_line(aes(x = date, y = lwr_e),lty=2) +
    geom_line(aes(x = date, y = upr_e), lty=2) +
    theme_bw()+labs(title="", x = "", y="Estimated effective R") +
    theme(legend.title = element_blank(),
          axis.text = element_text(size=15),
          axis.title=element_text(size=15),
          legend.text= element_text(size=15),
          strip.text.x = element_text(size = 20),
          plot.title = element_text(size=15),
          legend.position = "bottom")+
    ggtitle(paste0("Model fit \n",name))+
    scale_y_continuous(limits = c(0, 2)) 
} 

#################################################
# FIT LINEAR MODEL WITH LOG CONTACTS
#################################################

# Fit models
m1 <- lm(re_median_imp ~ contact_mean_l + pct_traced_imp + testing, data=ds) #to make models comparable with lag model, remove first date
summ(m1)

m2 <- lm(re_median_imp ~ lag(contact_mean_l,k=5) + lag(pct_traced_imp,k=5) + testing, data=ds) #
summ(m2)

m3 <- lm(re_median_imp ~ lag(contact_mean_l,k=5) + lag(pct_traced_imp,k=5), data=ds) #
summ(m3)

m4 <- lm(re_median_imp ~ lag(contact_mean_l,k=5)*lag(cases_imp,k=5) + lag(pct_traced_imp,5) + testing , data=ds) #
summ(m4)

m5 <- lm(re_median_imp ~ lag(contact_mean_l,k=5)*bs(lag(date,k=5)) + lag(pct_traced_imp,5) + testing , data=ds) #
summ(m5)



# GET AIC
AIC(m1,m2,m3, m4, m5)

# PLOT RESIDUALS
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(m1) 
plot(m2)
plot(m4)
plot(m5)



# GET ESTIMATES (Define periods (if lag in model, then use period_lag))
# length_lag = 5
# 
# period_lag = period[c((length_lag+1):length(period))]
# period_nlag = period[2:length(period)]

md1 = get_fit(m1,lag=1,data=ds, out="re_median_imp", out_log=0)
md2 = get_fit(m2,lag=5,data=ds, out="re_median_imp", out_log=0)
md3 = get_fit(m3,lag=5,data=ds, out="re_median_imp", out_log=0)
md4 = get_fit(m4,lag=5,data=ds, out="re_median_imp", out_log=0)
md5 = get_fit(m5,lag=5,data=ds, out="re_median_imp", out_log=0)


# PLOT ESTIMATES (Define title of the plot by defining the model)
p_m1 = plot_m(md1, name = "Re ~ b0 + log(m_contacts) + pct_t + c_test")
p_m2 = plot_m(md2, name = "Re ~ b0 + log(lag(m_contacts,5)) + lag(pct_t, 5) + c_test")
p_m3 = plot_m(md3, name = "Re ~ b0 + log(lag(m_contacts,5)) + lag(pct_t,5)")
p_m4 = plot_m(md4, name = "Re ~ b0 + log(lag(m_contacts,5)) + lag(pct_t,5) + c_test + lag(cases,5)")
p_m5 = plot_m(md5, name = "Re ~ b0 + log(lag(m_contacts,5))*bs(lag(date,5)) + lag(pct_t,5) + c_test + lag(cases,5)")

p_m1
p_m2
p_m3
p_m4
p_m5

multiplot(p_m1, p_m2, p_m3, p_m4, cols=2)

# GET ESTIMATES
#To interpet the amount of change in the original metric of the outcome, 
#a one percent increase in the dependent variable increases (or decreases) the dependent variable by 
#(coefficient/100) units# Thus, for a one percent increase in the average daily number of contacts (mean contacts), 
# the average Rt increases by coef(m4)[2]/100 = 0.0102.
coef(m4)[2]
coef(m4)[2]/2

confint(m4)

# NOW WE WANT TO HAVE ESTIMATE OF BETA_1 WHEN OTHER VARIABLES ARE SET AT VALUE OF 6 SEPTEMBER
# new_data = ds%>%select(date,re_median,pct_traced_imp, testing, cases_imp,  contact_mean_l) %>% filter(date=="2020-10-06")
# new_data = new_data[rep(seq_len(nrow(new_data)), each = length(ds$date)),] %>%
#   mutate(date=ds$date)
# 
# predict(m4, new_data)


# SAVE PLOTS

# Descriptives
pdf(file=paste0(OutputDirectory,"lin_rt_models_fit.png"), width=8, height=6)
multiplot(p_m1, p_m2, p_m3, p_m4, cols=2)
dev.off()

# png(file=paste0(OutputDirectory,"variable_corr.png"), width=700, height=500)
# multiplot(p4,p3,p5,p10,cols=2)
# dev.off()

pdf(file=paste0(OutputDirectoryPaper,"SuppFig6_20201218.pdf"), width=7, height=10)
multiplot(p,p2,p1,p6,p7,p8, cols=2)
dev.off()


# FINAL FIT
pdf(file=paste0(OutputDirectoryPaper,"SuppFig4_20201218.pdf"), width=8, height=6)
plot_lm
#plot_nls
dev.off()

# ggsave("lin_rt_model_fit_final.png", plot =p_m4, device="png", path = OutputDirectory, width=14,height=9)
ggsave("neg_exp_lin_model_fit.png", plot =plot_nls, device="png", path = OutputDirectory, width=12,height=9)
ggsave("log_lin_model_fit_alt.png", plot =plot_lm, device="png", path = OutputDirectory, width=12,height=9)

