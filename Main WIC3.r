
# USE F1 for help
{#create folders
  cc.dir.val = "../data"
  if(length(grep(pattern = "data",x = dir("../")))==0){#checks if 'data' folder exists
    dir.create(path=cc.dir.val)
  }
  
  #create an results folder to save indicators graphs
  cc.dir.val = "../results"
  if(length(grep(pattern = "results",x = dir("../")))==0){
    dir.create(path=cc.dir.val)
  }
  
  #Here you will paste results
  cc.dir.val = "../results/wic3proj"
  if(length(grep(pattern = "wic3proj",x = dir("../results")))==0){
    dir.create(path=cc.dir.val)
  }
  
  #create an output folder - model output
  cc.dir.val = "../data/output"
  if(length(grep(pattern = "output",x = dir("../data")))==0){
    dir.create(path=cc.dir.val)
  }
  
  #Here you will find the input files for running SSPs.. (starting with SSP2)
  cc.dir.val = "../data/output/input_wic3proj"
  if(length(grep(pattern = "input_wic3proj",x = dir("../data/output")))==0){
    dir.create(path=cc.dir.val)
  }
  
  #sorry for additional layer [need this to separate original output files in my system]
  cc.dir.val = "../data/output/output_wic3proj"
  if(length(grep(pattern = "output_wic3proj",x = dir("../data/output")))==0){
    dir.create(path=cc.dir.val)
  }
  
}

source("scen_w09_ssp2_emort_efert_emig.r") 
source("scen_w10_ssp2_emort_efert_emig1ukr.r") 

#SSP1
source("scen_w01_ssp1_emort_efert_emig.r") #30th May #age-specific used... education specific coming up
source("scen_w05_ssp1_emort_efert_emig1ukr.r") #
#mort - low
#fert - low/low or low/low10
#mig - med - no change scen_emort_efert_emig1.r
#edu - sdg

#SSP3
source("scen_w03_ssp3_emort_efert_emig.r") #30th May #age-specific used... education specific coming up
source("scen_w07_ssp3_emort_efert_emig1ukr.r") #
#mort - high
#fert - high/high
#mig - low (.5)
#edu - cer

#SSP4
source("scen_w04_ssp4_emort_efert_emig.r") #30th May #age-specific used... education specific coming up
source("scen_w08_ssp4_emort_efert_emig1ukr.r") #
#mort - high/medium
#fert - high/low
#mig - med
#edu - cer-10%/GET

#SSP5
source("scen_w02_ssp5_emort_efert_emig.r") #30th May #age-specific used... education specific coming up
source("scen_w06_ssp5_emort_efert_emig1ukr.r") #
#mort - low
#fert - low/low or low/low10
#mig - high
#edu - sdg

#SSP2_0mig
source("scen_x01_ssp2_emort_efert_0mig.r") #30th May #age-specific used... education specific coming up
source("scen_x02_ssp2_emort_efert_emig2x.r") #30th May #age-specific used... education specific coming up

# source("scen_x03_ssp2_emort_efert_0mig.r")#Wildermeesch

#Phillip
# source("scen_x04_ssp2_emort0_efert0irf_0mig.r")#Instant replacement fertility

#Marina -  SSP2_SDG and SSP2_SDGGET.
source("scen_x11_ssp2sdg_emort_efert_emig1ukr.r")     # #age-specific used... education specific coming up
source("scen_x12_ssp2sdgget_emort_efert_emig1ukr.r")     # #age-specific used... education specific coming up

## rmd files!

#tfr - edu-specific tfr
#sx ...
#migration ...??
#education ...: mys25..

# Ssps?? [global + country]
# population toString.default
# mys
# prop65+


#MYS
#run the following once to get official duration data  ...
if(T) source("mys wicproj3, V1.r") #saves file save(mysdt.col,file = "../results/mys SSP1-5.rda")
# fmys(iscen,save.oecd)# to calculate and save the mys in scen folder

##File management
#save for Rob also in 
#This file also contains codes for creating graphs
"Report cntry files rmd.r"


#Create annual interpolation by age*sex*country [Samir]






