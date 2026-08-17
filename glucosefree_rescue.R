# Run where glucose-free was done with drugs without hypoxia, Can use for n=1 supplementary maybe
# n=1

library(vascr)
library(tidyverse)
library(ggplot2)

gfrescue<- vascr_import("ECIS", #importing raw data and modeled data
                       raw="ECIS_260720_MFT_1_CG_ogdrescue3.abp",
                       model="ECIS_260720_MFT_1_CG_ogdrescue3_RbA.csv", experiment="exp1")
gfrescuekey = tribble(~SampleID, ~Row, ~ Column, ~ Sample, 
                      
                      1, "D", "10 11 12", "1uM Ebselen",
                      2, "D", "7 8 9", "10uM Ebselen",
                      3, "C", "4 5 6", "1uM Doxycycline",
                      4, "C", "1 2 3", "10uM Doxycycline",
                      5, "F", "10 11 12", "1nM Melatonin",
                      6, "F", "7 8 9", "10nM Melatonin",
                      7, "C", "10 11 12", "0.2uM Cilostazol",
                      8, "C", "7 8 9", "2uM Cilostazol",
                      9, "B", "10 11 12", "Low VPA", 
                      10, "B", "7 8 9", "High VPA", 
                      11, "E", "10 11 12", "Low Rapamycin", 
                      12, "E", "7 8 9", "High Rapamycin", 
                      13, "A", "10 11 12", "0.1mM BHB", 
                      14, "A", "7 8 9", "1mM BHB",
                      
                      15, "B", "4 5 6", "1uM Apocynin",
                      16, "B", "1 2 3", "10uM Apocynin",
                      17, "E", "4 5 6", "1nM Exendin-4",
                      18, "E", "1 2 3", "10nM Exendin-4",
                      19, "F", "4 5 6", "1uM Riluzole",
                      20, "F", "1 2 3", "10uM Riluzole",
                      21, "G", "10 11 12", "0.1uM Pravastatin",
                      22, "G", "7 8 9", "1uM Pravastatin",
                      23, "G", "4 5 6", "1uM Sapropterin", 
                      24, "G", "1 2 3", "10uM Sapropterin", 
                      25, "A", "4 5 6", "1mM LiCl", 
                      26, "A", "1 2 3", "10mM LiCl",
                      
                      101, "D", "4 5 6", "Vehicle 0.1% H2O, DMSO", 
                      102, "D", "1 2 3", "Vehicle 1% H2O",
                      
                      103, "H", "7 8 9", "1g/L glucose", 
                      104, "H", "10 11 12", "2g/L glucose")



gfrescue_labeled<- vascr:::vascr_apply_map(gfrescue, gfrescuekey)

gfrescue_plotdata = gfrescue_labeled %>% vascr_zero_time(72.722) %>% 
  vascr_subset(unit = "Rb") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
  vascr_resample_time(500) %>% 
  vascr_normalise(-1, divide = TRUE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
  vascr_subset(time = c(-4,48))

#plotting
# eb and doxy
gfrescue_plotdata %>% 
  vascr_subset(sampleid = c(101, 1:4)) %>%
  vascr_summarise(level = "experiment") %>% 

# melatonin, cilostazol
gfrescue_plotdata %>% 
  vascr_subset(sampleid = c(101, 5:8)) %>%
  vascr_summarise(level = "experiment") %>% 
  vascr_plot_line() + xlim(-4,30)          

# VPA
gfrescue_plotdata %>% 
  vascr_subset(sampleid = c(101, 9:10)) %>%
  vascr_summarise(level = "experiment") %>% 
  vascr_plot_line() + xlim(-4,30)    

# rapa
gfrescue_plotdata %>% 
  vascr_subset(sampleid = c(101, 11:12)) %>%
  vascr_summarise(level = "experiment") %>% 
  vascr_plot_line() + xlim(-4,30)    

# BHB, 
gfrescue_plotdata %>% 
  vascr_subset(sampleid = c(102, 13:14)) %>%
  vascr_summarise(level = "experiment") %>% 
  vascr_plot_line() + xlim(-4,30)    

# apocyin
gfrescue_plotdata %>% 
  vascr_subset(sampleid = c(101, 15:16)) %>%
  vascr_summarise(level = "experiment") %>% 
  vascr_plot_line() + xlim(-4,30)    

# exendin
gfrescue_plotdata %>% 
  vascr_subset(sampleid = c(101, 17:18)) %>%
  vascr_summarise(level = "experiment") %>% 
  vascr_plot_line() + xlim(-4,30)    

# riluzole
gfrescue_plotdata %>% 
  vascr_subset(sampleid = c(101, 19:20)) %>%
  vascr_summarise(level = "experiment") %>% 
  vascr_plot_line() + xlim(-4,30)    

# pravastatin
gfrescue_plotdata %>% 
  vascr_subset(sampleid = c(101, 21:22)) %>%
  vascr_summarise(level = "experiment") %>% 
  vascr_plot_line() + xlim(-4,30)    

# sapro
gfrescue_plotdata %>% 
  vascr_subset(sampleid = c(101, 23:24)) %>%
  vascr_summarise(level = "experiment") %>% 
  vascr_plot_line() + xlim(-4,30)    


# licl
gfrescue_plotdata %>% 
  vascr_subset(sampleid = c(102, 25:26)) %>%
  vascr_summarise(level = "experiment") %>% 
  vascr_plot_line() + xlim(-4,30)   

# glucose controls
gfrescue_plotdata %>% 
  vascr_subset(sampleid = c(103,104, 101)) %>%
  vascr_summarise(level = "experiment") %>% 
  vascr_plot_line() + xlim(-4,48)   +geom_vline(xintercept=15.8)
