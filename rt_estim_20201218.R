####################################################
# BXL CONTACT TRACING ANALYSES                     #
# Rt ESTIMATION                                    #
####################################################
# last update 261120 by Brecht Ingelbeen
rm(list=ls())

# SET OUTPUT DIRECTORY
OutputDirectory <- "./report_outputs/"
OutputDirectoryPaper <- "./report_outputs/figures_paper/"
OutputDirectoryData <- "../paper/data/clean/regression/"

# required packages
pacman::p_load(tidyverse, openxlsx, readxl,lubridate,purrr,dplyr,ggplot2, scales)

# devtools::install_github("mrc-ide/EpiEstim")
library(EpiEstim)

#### 0. import and clean data and packages ####
# import case data
#casesBE <- read.csv("C:/Users/bingelbeen/OneDrive - ITG/nCoV2019/COVID19_Bel/COVID19_Bxl/COVID19BE_CASES_AGESEX.csv") # the latest one is here : https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv
casesBE <- read.csv("./data/clean/COVID19BE_CASES_AGESEX.csv")

latest_date = as.Date("2020-11-24")

# regroup age groups
casesBE <- casesBE %>% 
  mutate(AGEGROUP = as.character(AGEGROUP),
         agegr = case_when(
           AGEGROUP=="0-9" ~ "0-9",
           AGEGROUP=="10-19" ~ "10-19",
           AGEGROUP=="20-29" ~ "20-29",
           AGEGROUP=="30-39" ~ "30-39",
           AGEGROUP=="40-49" ~ "40-49",
           AGEGROUP=="50-59" ~ "50-59",
           AGEGROUP=="60-69" ~ "60-69",
           AGEGROUP%in%c("70-79","80-89","90+") ~ "70+",
           TRUE ~ as.character(NA))
         ) %>%
  dplyr::rename(date = "DATE") %>% 
  mutate(date = as.Date(date), format="%Y-%m-%d")

# summarize all Bxl cases
casesBXL_all <- casesBE %>%
  filter(!is.na(date)&date>"2020-07-20"&date<=latest_date&!is.na(AGEGROUP)&PROVINCE=="Brussels") %>%
  group_by(date) %>%
  summarise(n=sum(CASES)) %>%
# Estimate on data 1 Aug 2020 - latest date  
  mutate(t_start= c(1:length(n)),
         t_end= t_start + 6
         )
casesBXL_all

#### 1. R_e calculation - Uncertainty method
### Serial Interval
### -- mean 5.19 (95% CI: 4.37, 6.02) # from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7448781/
### -- sd 2.9 (95% CrI: 1.9, 4.9)
sens_configs <- 
  make_config(
    list(
      mean_si = 5.19, std_mean_si = 0.7,
      min_mean_si = 4.37, max_mean_si = 6.02,
      std_si = 2.9, std_std_si = 0.5,
      min_std_si = 1.9, max_std_si = 4.9,
      n1 = 1000,
      n2 = 100,
      seed = 123456789
    )
  )



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
knots = as.Date(c("2020-09-01","2020-09-30","2020-10-07","2020-10-26","2020-11-02"))

casesBXL_all_minus_one_week_for_R_estim <- subset(casesBXL_all, date > "2020-07-27")
R_Bxl_allages <- cbind(casesBXL_all_minus_one_week_for_R_estim, posterior_R_t_post)

# add NPI to colour background
R_Bxl_allages <- R_Bxl_allages %>%
  mutate(NPI = case_when(
    date < knots[1] ~ "schools closed & 5 close contacts allowed \n",
    date >= knots[1] & date < knots[2] ~ "schools open & 5 close contacts allowed \n",
    date >= knots[2] & date < knots[3]  ~ "schools open & limit close contacts suspended \n",
    date >= knots[3] & date < knots[4]  ~ "schools open, bars closed & 3 close contacts allowed \n",
    date >= knots[4] & date < knots[5]  ~ "schools open, bars+restaurants closed, curfew, indoor\nsports prohibited & 1 close contacts allowed \n",
    date >= knots[5] & date < "2020-11-13" ~ "schools closed, mandatory teleworking, non-essential\nshops closed and all of the above \n",
    TRUE ~ as.character(NA))
  )
    

# limit to study period
R_Bxl_allages <- subset(R_Bxl_allages, date > "2020-08-01" & date <="2020-11-13")

# descriptive Rt
Rt <- R_Bxl_allages %>%
  summarise(min(R_e_median), max(R_e_median))
Rt 

# Dates of min and max
R_Bxl_allages$date[which(R_Bxl_allages$R_e_median ==min(R_Bxl_allages$R_e_median))]
R_Bxl_allages$date[which(R_Bxl_allages$R_e_median ==max(R_Bxl_allages$R_e_median))]


# When Rt blow 1
min(R_Bxl_allages$date[R_Bxl_allages$R_e_median<1 & R_Bxl_allages$date>knots[2]])

# Compute relative changes at start and end of each period
######################################################################################

# Take values of interest
Rt_intperiods = R_Bxl_allages %>% filter(date %in% (knots-1)) 
Rt_intperiods

Rt_change = NULL
for(i in 4:nrow(Rt_intperiods)){
  change = (Rt_intperiods[i,c(9:11)]-Rt_intperiods[(3),c(9:11)])/Rt_intperiods[(3),c(9:11)]
  Rt_change = rbind(Rt_change, change)
}
Rt_change

# Plot Rt 
###############################################################################
mycol <- c( "lightcyan", "yellow", "red",  "navy")
show_col(economist_pal()(9))

mycol = economist_pal()(8)
mycol = c("#00887d","#01a2d9","#76c0c1","#ee8f71","#6794a7", "#adadad")

# ggplot default colours
mycol2 = c("#ffa3a3","#ffffa3","#a3ffa3","#a3ffff","#a3a3ff","#ffa3ff")
  
alphas=0.3
sizes=2
linetype =1

ggplot2::theme_set(theme_classic(base_size = 18))
R_plot <- ggplot(R_Bxl_allages, aes(date)) + 
  geom_vline(xintercept=knots[1], colour = mycol[1], size=sizes, alpha=alphas, linetype=linetype)+
  geom_vline(xintercept=knots[2], colour = mycol[2], size=sizes,alpha=alphas, linetype=linetype)+
  geom_vline(xintercept=knots[3], colour = mycol[3], size=sizes,alpha=alphas, linetype=linetype)+
  geom_vline(xintercept=knots[4], colour = mycol[4], size=sizes,alpha=alphas, linetype=linetype)+
  geom_vline(xintercept=knots[5], colour = mycol[5], size=sizes,alpha=alphas, linetype=linetype)+
#   geom_rect(fill = mycol[1], alpha = 0.01, # colour hsl(0, 100%, 82%)
#             xmin = as.Date("2020-08-01"), xmax = as.Date("2020-09-01"), ymin = 0, ymax = 2) + # applying a five day delay
#   geom_rect(fill = mycol[2], alpha = 0.01, 
#             xmin = as.Date("2020-09-01"), xmax = as.Date("2020-09-30"), ymin = 0, ymax = 2) +
#   geom_rect(fill = mycol[3], alpha = 0.01, 
#             xmin = as.Date("2020-09-30"), xmax = as.Date("2020-10-07"), ymin = 0, ymax = 2) +
#   geom_rect(fill = mycol[4], alpha = 0.01, 
#             xmin = as.Date("2020-10-07"), xmax = as.Date("2020-10-26"), ymin = 0, ymax = 2) +
#   geom_rect(fill = mycol[5], alpha = 0.01, 
#             xmin = as.Date("2020-10-26"), xmax = as.Date("2020-11-02"), ymin = 0, ymax = 2) +
#   geom_rect(fill = mycol[6], alpha = 0.01, 
#             xmin = as.Date("2020-11-02"), xmax = as.Date("2020-11-30"), ymin = 0, ymax = 2) +
# #  geom_ribbon(aes(ymin = R_e_q0025,
#                  ymax = R_e_q0975), fill = "grey", alpha = 0.6) +  
geom_vline(xintercept=as.Date("2020-10-21"), linetype = "dashed", colour = "red",size=1)+
geom_hline(yintercept = 1,linetype=2)+
  geom_line(aes(y = R_e_q0025), colour = "dark grey", linetype = "dashed",size=1) + 
  geom_line(aes(y = R_e_q0975), colour = "dark grey", linetype = "dashed",size=1) + 
  geom_line(aes(y = R_e_median),size=1) + 
  labs(title="", x = "", y="Estimated reproduction number") +
  theme_bw() +
  # theme(panel.background=element_rect(colour = NA, fill = "white"),
  #       legend.title = element_blank()) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_date(breaks = pretty_breaks(10)) +
  theme(axis.text = element_text(size=15),
        axis.title=element_text(size=15),
        legend.text= element_text(size=15),
        legend.title= element_text(size=15),
        strip.text.x = element_text(size = 20),
        plot.title = element_text(size=22))
R_plot

# save dataset
save(R_Bxl_allages, file = paste0(OutputDirectoryData,"R_Bxl_allages.Rdata"))

# Save plot
pdf(file=paste0(OutputDirectoryPaper,"Fig3_20201218.pdf"), width=10, height=7)
R_plot
dev.off()

#ggsave(R_plot, filename = paste0(OutputDirectory,"R_plot_SIsystreview.png"), width = 11, height = 7)


