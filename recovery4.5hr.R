# different factors 4.5 hr post.


library(vascr)
library(tidyverse)
library(ggplot2)

data1<- vascr_import("ECIS", 
                       raw="ECIS_260910_MFT_1_ogd4.5rescue1restarted.abp",
                       model="ECIS_260910_MFT_1_ogd4.5rescue1restarted_RbA.csv", experiment="exp1")


data1key = tribble(~SampleID, ~Row, ~ Column, ~ Sample, 
                   # no wash, mid 131 - penstrep, constituents
                      1, "A", "2 3", "OGD - Spiked with 2% FBS and 1g/L glucose", 1, "H", "7", "OGD - Spiked with 2% FBS and 1g/L glucose",
                      2, "A", "4 5 6", "OGD - Oxygen recovery",
                      3, "B", "2 3", "OGD - Spiked with 1g/L glucose", 3, "H", "8", "OGD - Spiked with 1g/L glucose",
                      4, "B", "4 5 6", "OGD - Spiked with 2% FBS",
                  
                   # Old media removed, replaced
                      5, "C", "1 2 3", "OGD - Fresh 131", # with PS, constituents
                      6, "D", "1 2 3", "OGD - 2% FBS + glutamax", 
                      7, "E", "1 2 3", "OGD - 2% FBS + glutamax + 1g/L glucose",
                      8, "F", "1 2 3", "OGD - 1g/L glucose",
                      9, "G", "2 3", "OGD - 2% FBS + Insulin", 9, "H", "3", "OGD - 2% FBS + Insulin",
                      10, "C", "4 5 6", "OGD - 1g/L glucose + insulin",
                      11, "D", "4 5 6", "OGD - Insulin",
                      12, "E", "4 5 6", "OGD - BHB",
                      13, "F", "4 5 6", "OGD - 2% FBS + BHB",
                      14, "G", "4 5 6", "OGD - BHB + 1g/L glucose",
                      15, "H", "4 5 6", "OGD - 2% FBS + 1g/L glucose + insulin",
                   
                   # base media - just 131 and hepes
                      16, "A", "7 8 9", "basal - Oxygen recovery",
                      17, "A", "10 11 12", "basal - Spiked with 2% FBS and glucose",
                      18, "B", "7 8 9", "basal - Spiked with 2% FBS",
                      19, "B", "10 11 12", "basal - Spiked with 1g/L glucose",
                   
                      20, "C", "7 8 9", "basal - Fresh 131", # 131 with hepes ps, constituents
                      21, "D", "7 8 9", "basal - 2% FBS + glutamax",
                      22, "E", "7 8 9", "basal - 2% FBS + glutamax + 1g/L glucose",
                      23, "F", "7 8 9", "basal - 1g/L glucose",
                      24, "G", "7 8 9", "basal - 2% FBS + Insulin",
                      25, "C", "10 11 12", "basal - 1g/L glucose + insulin", 
                      26, "D", "10 11 12", "basal - Insulin",
                      27, "E", "10 11 12", "basal - BHB",
                      28, "F", "10 11 12", "basal - 2% FBS + BHB",
                      29, "G", "10 11 12", "basal - BHB + 1g/L glucose",
                      30, "H", "10 11 12", "basal - BHB + 1g/L glucose + insulin")



data1_labeled<- vascr:::vascr_apply_map(data1, data1key)

# deprivationtime = data1_labeled %>% vascr_zero_time(68.767) %>% 
  # vascr_subset(unit = "Rb") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
  # vascr_resample_time(500) %>% 
  # vascr_normalise(-1, divide = TRUE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
  # vascr_subset(time = c(-4,48))

recovery <- data1_labeled %>% vascr_zero_time(25.95907) %>% 
  vascr_subset(unit = "Rb") %>% 
  vascr_resample_time(500) %>% 
# vascr_normalise(-1, divide = FALSE) %>% 
  vascr_subset(time = c(-2,100))


###### plotting recovery
recovery %>% vascr_subset(sampleid = c(1:4)) %>% vascr_exclude (well = c("H7", "H8")) %>% 
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() +theme_bw()# +ylim(0, 1.3)

recovery %>% vascr_subset(sampleid = c(2,3,4,1)) %>% vascr_exclude (well = c("H7", "H8")) %>% 
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() +theme_bw()# +ylim(0, 1.3)


recovery %>% vascr_subset(sampleid = c(1:4)) %>% vascr_exclude (well = c("H8")) %>% 
  vascr_summarise(level = "well") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() +theme_bw()# +ylim(0, 1.3)

### what if I normalise to the same time 

recovery %>% vascr_subset(sampleid = c(2,16)) %>% vascr_exclude (well = "H8") %>% 
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() #+ ylim(0, 1000)



recovery %>% vascr_subset(sampleid = c(16:19)) %>% vascr_exclude (well = "H8") %>% 
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() + theme_bw() #+ ylim(0, 1000)

recovery %>% vascr_normalise(-1, divide = FALSE) %>% vascr_subset(sampleid = c(16,19, 18,17)) %>% vascr_exclude (well = "H8") %>% 
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() + theme_bw() #+ ylim(0, 1000)

recovery %>% vascr_normalise(-1, divide = FALSE) %>% vascr_subset(sampleid = c(5:8)) %>% 
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() + theme_bw() #+ ylim(0, 1000)


recovery %>% vascr_subset(sampleid = c(20,5)) %>% 
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() + theme_bw() #+ ylim(0, 1000)



recovery %>% vascr_subset(sampleid = c(20:23)) %>% 
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() + theme_bw() #+ ylim(0, 1000)



recovery %>% vascr_normalise(-1, divide = FALSE) %>%vascr_subset(sampleid = c(8,10 )) %>% vascr_exclude(well="F1") %>% 
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() + theme_bw() #+ ylim(0, 1000)

recovery %>% vascr_normalise(-1, divide = FALSE) %>%vascr_subset(sampleid = c(7, 15)) %>% vascr_exclude(well="E1") %>% 
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() + theme_bw() #+ ylim(0, 1000)


recovery %>% vascr_subset(sampleid = c(13, 7, 6)) %>% vascr_exclude(well="E1") %>% 
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() + theme_bw() #+ ylim(0, 1000)


# oxygen
recovery %>% vascr_subset(sampleid = c(2)) %>% vascr_exclude(well="E1") %>% 
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() + theme_bw() + ylim(0, 3.2) +labs(y="Rb (not normalised)")

# oxygen basal
recovery %>% vascr_subset(sampleid = c(16)) %>% vascr_exclude(well="E1") %>% 
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() + theme_bw() + ylim(0, 3.2) +labs(y="Rb (not normalised)")


5 and 20

recovery %>% vascr_subset(sampleid = c(2,5)) %>% vascr_exclude(well="E1") %>% 
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() + theme_bw() + ylim(0, 3.2) +labs(y="Rb (not normalised)")

recovery %>% vascr_subset(sampleid = c(16,20)) %>% vascr_exclude(well="E1") %>% 
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() + theme_bw() + ylim(0, 3.2) +labs(y="Rb (not normalised)")

# start
start <- data1_labeled %>% vascr_zero_time(25.95907) %>% 
  vascr_subset(unit = "Rb") %>% 
  vascr_resample_time(500) %>% 
  vascr_normalise(-22, divide = TRUE) %>% 
  vascr_subset(time = c(-22,10))

start %>% vascr_subset(sampleid = c(5,20)) %>% 
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() + theme_bw()

