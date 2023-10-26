{#setup
  rm(list=ls())
  #checking for required packages and installing if missing
  required.packages <- c("tidyverse","readxl","mdpop","data.table","MortCast","optimParallel")
  new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  data("UNlocations",package= "wpp2019" )   
  #these loads all required packages
  invisible(lapply(required.packages, library, character.only = TRUE))
  
  iscen = "e09baselineSSP2" #there will be only one baseline.. with iscen_text = "_emort1_efert1_emig0"
  iscen_text = "_emort_efert_emig1ukr"
  iscen_fullname = paste(iscen,iscen_text,sep="")
  
  baseline = F #baseline T to generate efert and emort
  
  usescen = paste0("e03baselineCovid","_emortC_efert_0mig")#when baseline = F
  
  efert = F #estimate differential fertility OR not
  iruneduasfr= F #whether to run the eduasfr optimization 
  newfert = F#if T then some information here
  
  emort = F # Now efert and iruneudasfr both are not needed 
  irunedult =  F #whether to run the edult optmization
  newmort = F#if T then some information here
  
  newedu = F#if T then some information here
  
  emig = T #this is THE FIRST TIME WE ARE BRINGING THE MIGRATION 
  newmig = T # use Dilek's input for the first time
  newmigtype = "AGE"
  corrmigC = T #C=country
  net0 = 2060
  iMigcorr_u15 = T
}#setup

#r statespace
source("statespace.r")#empty if baseline is F, then the statespace can already take the baseline value
source("fillstatespace.r")

#transitions
source("main wic3, V9 mortdiff.r")
# See "Report WIC3.Rmd"