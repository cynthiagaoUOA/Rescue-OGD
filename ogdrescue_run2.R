
library(vascr)
library(tidyverse)
library(ggplot2)

rescue2<- vascr_import("ECIS", #importing raw data and modeled data
                       raw="ECIS_260713_MFT_1_CG_ogdrescue2.abp",
                       model="ECIS_260713_MFT_1_CG_ogdrescue2_RbA.csv", experiment="exp2")
rescue2_key = tribble(~SampleID, ~Row, ~ Column, ~ Sample, #add treatment to dataframe by creating a map,
                      1, "E", "7 8 9", "1uM Ebselen",
                      2, "E", "10 11 12", "10uM Ebselen",
                      3, "G", "1 2 3", "1uM Doxycycline",
                      4, "G", "4 5 6", "10uM Doxycycline",
                      5, "D", "7 8 9", "1nM Melatonin",
                      6, "D", "10 11 12", "10nM Melatonin",
                      7, "C", "7 8 9", "0.2uM Cilostazol",
                      8, "C", "10 11 12", "2uM Cilostazol",
                      9, "F", "7 8 9", "Low VPA", 
                      10, "F", "10 11 12", "High VPA", 
                      11, "G", "7 8 9", "Low Rapamycin", 
                      12, "G", "10 11 12", "High Rapamycin", 
                      13, "A", "7 8 9", "0.1mM BHB", 
                      14, "A", "10 11 12", "1mM BHB",
                      
                      15, "D", "1 2 3", "1uM Apocynin",
                      16, "D", "4 5 6", "10uM Apocynin",
                      17, "B", "1 2 3", "1nM Exendin-4",
                      18, "B", "4 5 6", "10nM Exendin-4",
                      19, "F", "1 2 3", "1uM Riluzole",
                      20, "F", "4 5 6", "10uM Riluzole",
                      21, "B", "7 8 9", "0.1uM Pravastatin",
                      22, "B", "10 11 12", "1uM Pravastatin",
                      23, "E", "1 2 3", "1uM Sapropterin", 
                      24, "E", "4 5 6", "10uM Sapropterin", 
                      25, "A", "1 2 3", "1mM LiCl", 
                      26, "A", "4 5 6", "10mM LiCl",
                      
                      101, "C", "4 5 6", "Vehicle 0.1% H2O, DMSO", 
                      102, "C", "7 8 9", "Vehicle 1% H2O",
                      
                      103, "H", "7 8 9", "2g/L glucose", 
                      104, "H", "10 11 12", "1g/L glucose", 
                      
                      105, "H", "2 4 5", "EGM growth curve"
)


rescue2_labeled<- vascr:::vascr_apply_map(rescue2, rescue2_key)

ogdrescue2_plotdata = rescue2_labeled %>% vascr_zero_time(68.767) %>% 
  vascr_subset(unit = "Rb") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
  vascr_resample_time(500) %>% 
  vascr_normalise(-1, divide = TRUE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
  vascr_subset(time = c(-4,48))

# plotting
# vehicles
ogdrescue2_plotdata %>% 
  vascr_subset(sampleid = c(101, 102)) %>%
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() 

ogdrescue2_plotdata %>% 
  vascr_subset(sampleid = c(101, 25:26)) %>%
  vascr_summarise(level = "experiment") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() 
