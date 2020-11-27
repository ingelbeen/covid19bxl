####################################################
# BXL CONTACT TRACING ANALYSES                     #
# Rt ESTIMATION                                    #
####################################################
# last update 261120 by Brecht Ingelbeen

#### 0. import and clean data and packages ####
# required packages
pacman::p_load(tidyverse, openxlsx, readxl,lubridate,purrr,dplyr,ggplot2, scales)
# devtools::install_github("mrc-ide/EpiEstim")
library(EpiEstim)
# import case data
casesBE <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/COVID19_Bel/COVID19_Bxl/COVID19BE_CASES_AGESEX.csv") # the latest one is here : https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv
casesBE$date <- as.Date(casesBE$DATE)
# regroup age groups
casesBE$AGEGROUP <- as.character(casesBE$AGEGROUP)
casesBE$agegr <- "70+"
casesBE$agegr[casesBE$AGEGROUP=="0-9"] <- "0-9"
casesBE$agegr[casesBE$AGEGROUP=="10-19"] <- "10-19"
casesBE$agegr[casesBE$AGEGROUP=="20-29"] <- "20-29"
casesBE$agegr[casesBE$AGEGROUP=="30-39"] <- "30-39"
casesBE$agegr[casesBE$AGEGROUP=="40-49"] <- "40-49"
casesBE$agegr[casesBE$AGEGROUP=="50-59"] <- "50-59"
casesBE$agegr[casesBE$AGEGROUP=="60-69"] <- "60-69"

# summarize all Bxl cases
casesBXL_all <- casesBE %>%
  filter(!is.na(date)&date>"2020-07-20"&date<"2020-11-24"&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(date) %>%
  summarise(n=sum(CASES))
casesBXL_all

#### 1. R_e calculation - Uncertainty method
### Serial Interval
### -- mean 4.7 (95% CrI: 3.7, 6.0)
### -- sd 2.9 (95% CrI: 1.9, 4.9)
sens_configs <- 
  make_config(
    list(
      mean_si = 4.7, std_mean_si = 0.7,
      min_mean_si = 3.7, max_mean_si = 6.0,
      std_si = 2.9, std_std_si = 0.5,
      min_std_si = 1.9, max_std_si = 4.9,
      n1 = 1000,
      n2 = 100,
      seed = 123456789
    )
  )

# Estimate on data 1 Aug 2020 -12 Nov 2020 

casesBXL_all$t_start= 1:nrow(casesBXL_all)
casesBXL_all$t_end= casesBXL_all$t_start + 6


#Estimate R - Inset/change NO_antal, SE_antal, DK_antal, FI_antal manually
Rt_nonparam_si <- 
  estimate_R(
    casesBXL_all$n, 
    method = "uncertain_si",
    config = sens_configs
  )

Rt_nonparam_si$R

### inspect R_e estimate
plot(Rt_nonparam_si, legend = FALSE)

## Posterior sample R_e estimate
sample_windows <- seq(length(Rt_nonparam_si$R$t_start))

posterior_R_t_post <- 
  map(
    .x = sample_windows,
    .f = function(x) {
      
      posterior_sample_obj <- 
        sample_posterior_R(
          R = Rt_nonparam_si,
          n = 1000, 
          window = x
        )
      
      posterior_sample_estim <- 
        data.frame(
          window_index = x,
          window_t_start = Rt_nonparam_si$R$t_start[x],
          window_t_end = Rt_nonparam_si$R$t_end[x],
          date_point = Rt_nonparam_si$dates[[Rt_nonparam_si$R$t_end[[x]]]],
          R_e_median = median(posterior_sample_obj),
          R_e_q0025 = quantile(posterior_sample_obj, probs = 0.025),
          R_e_q0975 = quantile(posterior_sample_obj, probs = 0.975)
        )
      
      return(posterior_sample_estim)
      
    }
  ) %>% 
  reduce(bind_rows)

# merge with case data
casesBXL_all_minus_one_week_for_R_estim <- subset(casesBXL_all, date > "2020-07-27")
R_Bxl_allages <- cbind(casesBXL_all_minus_one_week_for_R_estim, posterior_R_t_post)
# add NPI to colour background
R_Bxl_allages$NPI[R_Bxl_allages$date<"2020-09-01"] <- "schools closed & 5 close contacts allowed \n"
R_Bxl_allages$NPI[R_Bxl_allages$date>"2020-08-30"&R_Bxl_allages$date<"2020-09-30"] <- "schools open & 5 close contacts allowed \n"
R_Bxl_allages$NPI[R_Bxl_allages$date>"2020-09-29"&R_Bxl_allages$date<"2020-10-06"] <- "schools open & limit close contacts suspended \n"
R_Bxl_allages$NPI[R_Bxl_allages$date>"2020-10-05"&R_Bxl_allages$date<"2020-10-26"] <- "schools open, bars closed & 3 close contacts allowed \n"
R_Bxl_allages$NPI[R_Bxl_allages$date>"2020-10-25"&R_Bxl_allages$date<"2020-11-02"] <- "schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n"
R_Bxl_allages$NPI[R_Bxl_allages$date>"2020-11-01"&R_Bxl_allages$date<"2020-11-13"] <- "schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n"

# limit to study period
R_Bxl_allages <- subset(R_Bxl_allages, date > "2020-08-01" & date <"2020-11-18")

# plot age estimates
ggplot2::theme_set(theme_classic(base_size = 18))
R_plot <- ggplot(R_Bxl_allages, aes(date)) + 
  geom_rect(fill = "#ffa3a3", alpha = 0.01, # colour hsl(0, 100%, 82%)
            xmin = as.Date("2020-08-01"), xmax = as.Date("2020-09-06"), ymin = 0, ymax = 2) + # applying a five day delay
  geom_rect(fill = "#ffffa3", alpha = 0.01, 
            xmin = as.Date("2020-09-06"), xmax = as.Date("2020-10-05"), ymin = 0, ymax = 2) +
  geom_rect(fill = "#a3ffa3", alpha = 0.01, 
            xmin = as.Date("2020-10-05"), xmax = as.Date("2020-10-12"), ymin = 0, ymax = 2) +
  geom_rect(fill = "#a3ffff", alpha = 0.01, 
            xmin = as.Date("2020-10-12"), xmax = as.Date("2020-10-31"), ymin = 0, ymax = 2) +
  geom_rect(fill = "#a3a3ff", alpha = 0.01, 
            xmin = as.Date("2020-10-31"), xmax = as.Date("2020-11-07"), ymin = 0, ymax = 2) +
  geom_rect(fill = "#ffa3ff", alpha = 0.01, 
            xmin = as.Date("2020-11-07"), xmax = as.Date("2020-11-18"), ymin = 0, ymax = 2) +
#  geom_ribbon(aes(ymin = R_e_q0025,
#                  ymax = R_e_q0975), fill = "grey", alpha = 0.6) +   
  geom_line(aes(y = R_e_q0025), colour = "dark grey", linetype = "dashed") + 
  geom_line(aes(y = R_e_q0975), colour = "dark grey", linetype = "dashed") + 
  geom_line(aes(y = R_e_median)) + 
  labs(title="", x = "", y="Estimated effective reproduction number") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank()) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_date(breaks = pretty_breaks(10)) +
  geom_vline(xintercept=as.Date("2020-10-21"), linetype = "dashed", colour = "red")


R_plot
ggsave(R_plot, filename = "R_plot_SI4.9.png", width = 9, height = 4)
# save dataset
save(R_Bxl_allages, file = "R_Bxl_allages.Rdata")
