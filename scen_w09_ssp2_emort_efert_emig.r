{#setup
  rm(list=ls())
  #checking for required packages and installing if missing
  required.packages <- c("tidyverse","data.table")
  new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # data(UNlocations,package = "wpp2022")
  #these loads all required packages
  invisible(lapply(required.packages, library, character.only = TRUE))
               
  #naming of scenario (very important)
  baseline = F #baseline T to generate efert and emort
  
  iscen = "w09ssp2" #s for ssps 
  iscen_text = "_emort_efert_emig" #e indicates education differential values
  iscen_fullname = paste(iscen,iscen_text,sep="")
  
  #SSP2 input
  usescen = "input_wic3proj" #e04
  
  newfert = F# Mediumif T then some information here
  newmort = F#if T then some information here
  newedu = F#if T then some information here
  
  #migration needs to be adjusted...
  emig = T # check this out
  newmig = T # use Dilek's input 
  newmigtype = "AGE"
  corrmigC = T #C=country
  net0 = 2060
  iMigcorr_u15 = T
}
  
#r statespace
source("statespace.r")#empty if baseline is F, then the statespace can already take the baseline value

source("fillstatespace.r")

#transitions
source("projection.r")
# See "Report WIC3.Rmd"