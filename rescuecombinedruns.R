# combined runs
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

ogdrescue1_timezero = rescue1_labeled %>% vascr_zero_time(64.463)

# run 2
rescue2<- vascr_import("ECIS", 
                       raw="ECIS_260713_MFT_1_CG_ogdrescue2.abp",
                       model="ECIS_260713_MFT_1_CG_ogdrescue2_RbA.csv", experiment="exp2")
rescue2_key = tribble(~SampleID, ~Row, ~ Column, ~ Sample, 
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
                      
                      103, "H", "7 8 9", "1g/L glucose", 
                      104, "H", "10 11 12", "2g/L glucose", 
                      
                      105, "H", "2 4 5", "EGM growth curve"
)


rescue2_labeled<- vascr:::vascr_apply_map(rescue2, rescue2_key)

ogdrescue2_timezero = rescue2_labeled %>% vascr_zero_time(68.767) 


rescue3<- vascr_import("ECIS", raw="ECIS_260809_MFT_1_CG_ogdrescue3corrected (1).abp", model="ECIS_260809_MFT_1_CG_ogdrescue3corrected (1)_RbA.csv", experiment="exp3")

rescue3_key = tribble(~SampleID, ~Row, ~ Column, ~ Sample, 
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
                      13, "H", "10 11 12", "0.1mM BHB", 
                      14, "H", "7 8 9", "1mM BHB",
                      
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
                      25, "A", "10 11 12", "1mM LiCl", 
                      26, "A", "7 8 9", "10mM LiCl",
                      
                      102, "D", "4 5 6", "Vehicle 1% H2O", 
                      101, "D", "1 2 3", "Vehicle 0.1% H2O, DMSO",
                      
                      103, "A", "1 2 3", "1g/L glucose", 
                      104, "A", "4 5 6", "2g/L glucose",
                      200, "H", "6", "no glucose recovery")

rescue3_labeled<- vascr:::vascr_apply_map(rescue3, rescue3_key)

ogdrescue3_timezero = rescue3_labeled %>% vascr_zero_time(74.763912)

# testing third run vehicle
# ogdrescue3_timezero %>%   vascr_subset(unit = "Rb") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
#   vascr_resample_time(500) %>% 
#   vascr_normalise(-1, divide = TRUE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
#   vascr_subset(time = c(-4,48)) %>%  vascr_subset(sampleid = c(101, 102)) %>%
#   vascr_summarise(level = "summary") %>% 
#   vascr_plot_line() 






####
combinedplotdata<- rbind(ogdrescue2_timezero, ogdrescue1_timezero, ogdrescue3_timezero) %>% 
  vascr_subset(unit = "Rb") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
  vascr_resample_time(500) %>% 
  vascr_normalise(-2, divide = TRUE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
  vascr_subset(time = c(-4,48))

# plotting combinedruns

# vehicles - not too different, good
combinedplotdata %>% 
  vascr_subset(sampleid = c(101, 102)) %>%
  vascr_summarise(level = "experiment") %>% 
  vascr_plot_line() +theme_bw() + facet_wrap(~Experiment)

# first couple hours# first couple hours# first couple hours
combinedplotdata %>%   vascr_subset(time = c(-3,10)) %>% 
  vascr_subset(sampleid = c(102, 25:26)) %>%
  vascr_summarise(level = "summary") %>% 
  vascr_plot_line() 



# glucose
combinedplotdata %>% vascr_subset(unit = "Rb") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
  vascr_resample_time(500) %>% 
  vascr_normalise(-2, divide = TRUE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
  vascr_subset(time = c(-4,48)) %>% vascr_subset(sampleid= c("103")) %>% 
  vascr_summarise(level= "summary") %>% 
  vascr_plot_line() +theme_bw() + ylim(0, 1.3)

glucose <-combinedplotdata %>% vascr_subset(unit = "Rb") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
  vascr_resample_time(500) %>% 
  vascr_normalise(-2, divide = TRUE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
  vascr_subset(time = c(-4,48)) %>% vascr_subset(sampleid= c("103", "104")) 

t.test(Value ~ Sample, data = glucose)


plot_ogd(drug=c(103:104), ylim=c(0.5, 1.5), time=c(-2, 48))



# rest
combinedplotdata %>% 
  vascr_subset(sampleid = c(103:104, 101)) %>%
  vascr_summarise(level = "summary") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() +ylim(0, 1.3)

cm<- rbind(ogdrescue2_timezero, ogdrescue1_timezero) %>% 
  vascr_subset(unit = "Cm") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
  vascr_resample_time(500) %>% 
  vascr_normalise(-1, divide = TRUE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
  vascr_subset(time = c(-4,48))

cm %>% 
  vascr_subset(sampleid = c(103:104, 101)) %>%
  vascr_summarise(level = "summary") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() +ylim(0, 1.3)

combinedplotdata %>% 
  vascr_subset(sampleid = c(13:14,102)) %>%
  vascr_summarise(level = "summary") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() +ylim(0, 1.3) + xlim(-2, 7) +theme_bw()

# recovery after 24hr in ogd. 



recoverycombined %>% 
  vascr_subset(sampleid = c(101,102)) %>%
  vascr_summarise(level = "well") %>% #summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line() + xlim(22, 48) + theme_bw()+geom_vline(xintercept = 24)




### recoveryzero
ogdrecovery1_timezero = rescue1_labeled %>% vascr_zero_time(88.463)
ogdrecovery2_timezero = rescue2_labeled %>% vascr_zero_time(93) 
ogdrecovery3_timezero = rescue3_labeled %>% vascr_zero_time(98)  %>% vascr_exclude(well=c("D3", "D6"))

recoverycombined<- rbind(ogdrecovery1_timezero, ogdrecovery2_timezero, ogdrecovery3_timezero) %>% 
  vascr_subset(unit = "Rb") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
  vascr_resample_time(500) %>% 
  vascr_normalise(-2, divide = FALSE) 

# recoverycombined %>% 
#   vascr_subset(sampleid = c(101,102)) %>%
#   vascr_summarise(level = "summary") %>% 
#   vascr_plot_line() + xlim(-2, 24) + theme_bw()
# 
# recoverycombined %>% 
#   vascr_subset(sampleid = c(25:26,102)) %>%
#   vascr_summarise(level = "summary") %>% 
#   vascr_plot_line() + xlim(-2, 24) + theme_bw()

ogdrecovery3_timezero %>%  vascr_subset(unit = "Rb") %>% #only looking at Rb atm. Need to repeat code from here for alpha, Cm, etc
  vascr_resample_time(500) %>% 
  vascr_normalise(-2, divide = FALSE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
  vascr_subset(time = c(-4,48)) %>% vascr_subset(sampleid= c("200", "101")) %>% 
  vascr_summarise(level= "experiment") %>% 
  vascr_plot_line()




# plot function -----------------------------------------------------------
plot_ogd<- function(data= combinedplotdata, drug, vehicle=101, time = c(-2, 7), ylim= c(0.25, 1.3)){ 
  
  library(stringr)
  drugdf<- data %>% vascr:::vascr_subset(sampleid= drug)
  drugname = str_extract(drugdf$Sample[1], "\\S+$")
  
  subset <- data %>% vascr:::vascr_subset(sampleid= c(drug,vehicle), time= time) %>% vascr_summarise(level="summary")
  plot <- subset %>% vascr_plot_line() + 
    theme_bw() +
    scale_fill_manual(values= c("#0CB702", "darkviolet", "grey35"))+ 
    scale_color_manual(values= c("#0CB702", "darkviolet", "darkgrey")) + ylim(ylim)+
    labs(title=drugname )+ geom_vline(xintercept=0, linetype="dashed")
  
  return(plot)
}

# drugs in ogd
eb<- plot_ogd(drug = c(1:2)) # ebselen, toxic

dox<- plot_ogd(drug = c(3:4)) # doxycycline
mel<- plot_ogd(drug = c(5:6)) # melatonin
cilo<- plot_ogd(drug = c(7,8)) # cilostazol
vpa<- plot_ogd(drug = c(9:10))    # VPA
rapa<- plot_ogd(drug = c(11:12)) # rapamycin
BHB<- plot_ogd(drug = c(13:14), vehicle= 102) # BHB
apo<- plot_ogd(drug = c(15:16)) # apocynin
exe<- plot_ogd(drug = c(17:18)) # exendin
ril<- plot_ogd(drug = c(19:20)) # riluzole
prav<- plot_ogd(drug = c(21:22)) # prava
sap<- plot_ogd(drug = c(23:24)) # sapropterin
licl<- plot_ogd(drug = c(25, 26), vehicle = 102) 

#previously protective basally
dox + vpa + rapa & theme(legend.position="none")

# new evidence of protection
BHB + exe + licl + mel + cilo+prav + ril & theme(legend.position="none")

apo + sap & theme(legend.position="none")

#plot_ogd(drug= 200) #recovery no glucose looksy


### recovery to normox and glucose--------------------------------------------------------

plot_recovery<- function(data= recoverycombined, drug, vehicle=101, time = c(-2, 10), ylim= c(-0.1, 4)){ 
  
  library(stringr)
  drugdf<- data %>% vascr:::vascr_subset(sampleid= drug)
  drugname = str_extract(drugdf$Sample[1], "\\S+$")
  
  subset <- data %>% vascr:::vascr_subset(sampleid= c(drug,vehicle), time= time) %>% vascr_summarise(level="summary")
  plot <- subset %>% vascr_plot_line() + 
    theme_bw() +
    scale_fill_manual(values= c("deepskyblue", "darkviolet", "grey35"))+ 
    scale_color_manual(values= c("deepskyblue", "darkviolet", "darkgrey")) + ylim(ylim)+
    labs(title=drugname ) + geom_vline(xintercept=0, linetype="dashed")
  
  return(plot)
}

# drugs in ogd
ebselen<- plot_recovery(drug = c(1:2)) # ebselen
doxycycline<- plot_recovery(drug = c(3:4)) # doxycycline
melatonin<- plot_recovery(drug = c(5:6)) # melatonin
cilostazol<- plot_recovery(drug = c(7,8)) # cilostazol
valproic<- plot_recovery(drug = c(9:10))    # VPA
rapamycin<- plot_recovery(drug = c(11:12)) # rapamycin    
ketone<- plot_recovery(drug = c(13:14), vehicle= 102) # BHB
apocynin<- plot_recovery(drug = c(15:16)) # apocynin
exendin<- plot_recovery(drug = c(17:18)) # exendin
riluzole<- plot_recovery(drug = c(19:20)) # riluzole
pravastatin<- plot_recovery(drug = c(21:22)) # prava
sapropterin<- plot_recovery(drug = c(23:24)) # sapropterin
lithium <- plot_recovery(drug = c(25, 26), vehicle = 102) #licl


doxycycline + valproic + rapamycin & theme(legend.position="none")

ketone + exendin + lithium + melatonin + cilostazol+pravastatin + riluzole & theme(legend.position="none")


apocynin + sapropterin & theme(legend.position="none")



# sanity check ------------------------------------------------------------

expplotogd<- function(data= combinedplotdata, drug, vehicle=101, time = c(-2, 7), ylim= c(0.25, 1.3)){ 
  
  library(stringr)
  drugdf<- data %>% vascr:::vascr_subset(sampleid= drug)
  drugname = str_extract(drugdf$Sample[1], "\\S+$")
  
  subset <- data %>% vascr:::vascr_subset(sampleid= c(drug,vehicle), time= time) %>% vascr_summarise(level="experiment")
  plot <- subset %>% vascr_plot_line() + 
    theme_bw() +
    scale_fill_manual(values= c("#0CB702", "darkviolet", "grey35"))+ 
    scale_color_manual(values= c("#0CB702", "darkviolet", "darkgrey")) + ylim(ylim)+
    labs(title=drugname )+ geom_vline(xintercept=0, linetype="dashed") +facet_wrap(~Experiment)
  
  return(plot)
}


sanityrecovery<- function(data= recoverycombined, drug, vehicle=101, time = c(-2, 10), ylim= c(-0.1, 4)){ 
  
  library(stringr)
  drugdf<- data %>% vascr:::vascr_subset(sampleid= drug)
  drugname = str_extract(drugdf$Sample[1], "\\S+$")
  
  subset <- data %>% vascr:::vascr_subset(sampleid= c(drug,vehicle), time= time) %>% vascr_summarise(level="experiment")
  plot <- subset %>% vascr_plot_line() + 
    theme_bw() +
    scale_fill_manual(values= c("deepskyblue", "darkviolet", "grey35"))+ 
    scale_color_manual(values= c("deepskyblue", "darkviolet", "darkgrey")) + ylim(ylim)+
    labs(title=drugname ) + geom_vline(xintercept=0, linetype="dashed")+facet_wrap(~Experiment)
  
  return(plot)
}




expplotogd(drug=c(3:4))
sanityrecovery(drug=c(3:4))

expplotogd(drug=c(13:14))
sanityrecovery(drug=c(13:14))

expplotogd(drug=c(11:12))
sanityrecovery(drug=c(11:12))
