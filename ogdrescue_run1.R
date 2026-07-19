# Run 1 - OGD for 24 hours, rescue. Both containing drugs. Previously protective drugs, and lots of reactive oxygen species mitochondrial stress related


library(vascr)
library(tidyverse)
library(ggplot2)

rescue1<- vascr_import("ECIS", #importing raw data and modeled data
                    raw="ECIS_260708_MFT_1_CG_ogdrescue1.abp",
                    model="ECIS_260708_MFT_1_CG_ogdrescue1_RbA.csv", experiment="exp1")
rescue1_key = tribble(~SampleID, ~Row, ~ Column, ~ Sample, #add treatment to dataframe by creating a map,
                   1, "A", "1 2 3", "1uM Ebselen",
                   2, "A", "4 5 6", "10uM Ebselen",
                   3, "B", "1 2 3", "1uM Doxycycline",
                   4, "B", "4 5 6", "10uM Doxycycline",
                   5, "C", "1 2 3", "1nM Melatonin",
                   6, "C", "4 5 6", "10nM Melatonin",
                   7, "D", "1 2 3", "0.2uM Cilostazol",
                   8, "D", "4 5 6", "2uM Cilostazol",
                   9, "E", "1 2 3", "Low VPA", 
                   10, "E", "4 5 6", "High VPA", 
                   11, "F", "1 2 3", "Low Rapamycin", 
                   12, "F", "4 5 6", "High Rapamycin", 
                   13, "G", "1 2 3", "0.1mM BHB", 
                   14, "G", "4 5 6", "1mM BHB",
                   
                   15, "A", "7 8 9", "1uM Apocynin",
                   16, "A", "10 11 12", "10uM Apocynin",
                   17, "B", "7 8 9", "1nM Exendin-4",
                   18, "B", "10 11 12", "10nM Exendin-4",
                   19, "C", "7 8 9", "1uM Riluzole",
                   20, "C", "10 11 12", "10uM Riluzole",
                   21, "D", "7 8 9", "0.1uM Pravastatin",
                   22, "D", "10 11 12", "1uM Pravastatin",
                   23, "E", "7 8 9", "1uM Sapropterin", 
                   24, "E", "10 11 12", "10uM Sapropterin", 
                   25, "G", "7 8 9", "1mM LiCl", 
                   26, "G", "10 11 12", "10mM LiCl",
                   
                   101, "F", "7 8 9", "Vehicle 0.1% H2O, DMSO", 
                   102, "F", "10 11 12", "Vehicle 1% H2O",
                   
                   103, "H", "7 8 9", "1g/L glucose", 
                   104, "H", "10 11 12", "2g/L glucose", 
                   
                   105, "H", "1 2 4", "EGM growth curve"
                   )


rescue1_labeled<- vascr:::vascr_apply_map(rescue1, rescue1_key)

ogdrescue1_plotdata = rescue1_labeled %>% vascr_zero_time(64.463) %>% 
  vascr_subset(unit = "Rb") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
  vascr_resample_time(500) %>% 
  vascr_normalise(-1, divide = TRUE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
  vascr_subset(time = c(-4,48))

ogdrescue1_plot<- ogdrescue1_plotdata %>% 
  vascr_subset(sampleid = c(101, 102,1:12)) %>%
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() 
ogdrescue1_plot

library(plotly)
ggplotly(rescue1_plot)

reperrescue1_plotdata<- rescue1_labeled %>% vascr_zero_time(89.1) %>% 
  vascr_subset(unit="Rb") %>% 
  vascr_resample_time(500) %>% 
  vascr_normalise(-2, divide = FALSE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
  vascr_subset(time = c(-4,22)) 

reperrescue1_plot<- reperrescue1_plotdata %>% 
  vascr_subset(sampleid = c(101, 102,1:12)) %>%
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() 

reperrescue1_plot

ggplotly(reperrescue1_plot)


# right side of plate
ogdrescue1_plotdata %>% 
  vascr_subset(sampleid = c(15:24, 101)) %>%
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() 

ogdrescue1_plotdata %>% 
  vascr_subset(sampleid = c(7:12, 101)) %>%
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() 


##### glucose
ogdrescue1_plotdata %>% 
  vascr_subset(sampleid = c(103, 104,102)) %>%
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() 

# BHB
ogdrescue1_plotdata %>% 
  vascr_subset(sampleid = c(13:14,102)) %>%
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() + ylim(0, 1.25)

ogdrescue1_plotdata %>% 
  vascr_subset(sampleid = c(25:26, 102)) %>%
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() + ylim(0, 1.25)

# vehicles check
ogdrescue1_plotdata %>% 
  vascr_subset(sampleid = c(102, 101)) %>%
  vascr_summarise(level = "wells") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() 


# plate effect check
ogdrescue1_plotdata %>% 
  vascr_subset(sampleid = c(11, 12, 101)) %>%
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() + geom_vline(xintercept=-0.5)


# drug
ogdrescue1_plotdata %>% 
  vascr_subset(sampleid = c(19:20, 101)) %>%
  vascr_summarise(level = "experiment") %>% 
  vascr_plot_line() 
