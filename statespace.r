# #common functions
# source("function fertility calc.r") #fnrr1
# source("function smortality calc, un2022.R")  
# source("fune0toltWPP2022.r")#new

{
  
  #regions is loaded
  load(file="../data/output/input_wic3proj/regions200.RData")
  length(regions)#200
  #recoding country names different sources
  cntcode <- countrycode::codelist #this package is amazing
  
  
  nreg = length(regions)#200
  
  ages = seq(-5,120,by=5)#for newborns "alive" at the beginning of the time interval 
  nage = length(ages) #
  agecodes = ages/5
  
  sexnames = c("f","m") 
  nsex = length(sexnames)
  sexcodes = 1:2
  
  initime = 2020
  fintime = 2100
  ts = 5 #timestep
  nper = (fintime-initime)/ts
  pers = seq(initime,fintime,by=ts)
  neduper = nper+3 #we 
  edupers = seq(initime,fintime,by=ts)
  
  fertages = seq(10,45,by=5)#during the next five years
  nfertage = length(fertages) #Later for cohort - need 8
  nfertsex = 1
  
  educodes = paste("e",1:6,sep="")
  nedu = length(educodes)
  eduages = seq(10,25,by=5) #transitions from these ages
  neduage = length(eduages)
  edunames = educodes #?? name them
  
  #input data
  ntrans = 6 #sx, asfr, srb, edu, immigration, emigration
  input.vars = c("pop","sx","asfr","eapr","imr","emr","srb")
  vardims = data.frame( npers =c(nper,rep(nper-1,6)),
                        nedus = c(nedu,nedu,nedu,nedu-1,nedu,nedu,1),
                        nsexs=c(nsex,nsex,nfertsex,nsex,nsex,nsex,1),
                        nages = c(nage,nage,nfertage,neduage,nage,nage,1)
  ) 
  
  #output vars
  output.vars <- c("pop",
                   "deaths",
                   "emi",#
                   "imm",#
                   "births",
                   "tr")#education??
  
  original_input_output =  c("popdt","sxdt","propdt","emrdt", "imrdt","asfrdt","srbdt")
  idvar_order <- c("sex","Time","region","edu","edumom")
}#definitions


{#create folders
  #each scenario will be saved in separate folder
  #create an output/SCEN folder
  cc.dir.scen = paste("../data/output/output_wic3proj",iscen_fullname,sep="/")
  if(length(grep(pattern = iscen_fullname,x = dir("../data/output/output_wic3proj")))==0){
    dir.create(path=cc.dir.scen)
  }
  #all the output of this scenario will be saved h
  
  path_scen = paste(cc.dir.scen,"/",sep="")
  dir(path_scen)
}#create folders  

#create empty files or use from others
#for alternative scenarios

#load the old xxxdts and the results
print("Load Baseline Files")

#to copy from the following folder
cc.dir.usescen = paste("../data/output",usescen,sep="/")
dir(cc.dir.usescen)

ifiles <- grep(pattern="dt.RData",dir(cc.dir.usescen,full.names = T),value=T)
#check #single files only
print(ifiles)  


# #change some files [Later]
# if(exists("usescen_fert") && usescen_fert != usescen){
#   ifiles[grep("asfrdt",ifiles)] <- gsub(pattern = usescen,usescen_fert,ifiles[grep("asfrdt",ifiles)])
#   }
# 
# if(exists("usescen_mort") && usescen_mort != usescen){
#   ifiles[grep("sxdt",ifiles)] <- gsub(pattern = usescen,usescen_mort,ifiles[grep("sxdt",ifiles)])
# }

#8 files loaded
# rm(list=ls(pattern = ".RData"))
for(ifile in ifiles) {
  ifile.nm = gsub(".RData","",strsplit(ifile,"/")[[1]][5])
  assign(ifile.nm,get(load(file=ifile)))
}

id.cols <- names(popdt)[1:5]

#save csv file to check the data
dttosave <-grep("dt$",ls(),value = T)
for(ifile in dttosave) {
  xx <- get(ifile)
  xx[,value:=NA]
  # write.csv(xx[,value:=NA],file = paste0(path_scen,ifile,".csv"))
  write.csv(xx[,value:=NA],file = paste0(cc.dir.usescen,"/",ifile,".csv"))
}

#check the input
popdt[agest==5]
asfrdt[agest==15]


#in fillstatespace.r let us fill the data!!

# We will use the following later
# if(iscen == "ssp1"){
#   
#   #fert = low - 20% lower by 2020-2040 and 25% lower by 2040-2060 {??} [done]
#   #low fert
#   #fertfact.high = c(seq(1,1.2,length.out=5),seq(1.2,1.25,length.out=5)[-1],rep(1.25,8))
#   
#   #mort = low [done]
#   #updated above (usescen_mort)
#   
#   #mig= medium [done]
#   #No  change
#   
#   #edu = sdg [done]
#   
#   } else if(iscen == "ssp3"){#ssp1
# 
#     #fertfact.high = c(seq(1,1.2,length.out=5),seq(1.2,1.25,length.out=5)[-1],rep(1.25,8))
#     
#   } else if (iscen == "ssp4"){
#     #update HiFert countries with High mortality
#     usescen_mort = "baselineHmort_emort1high_efert_emig0"##statespace  
#     ifile <- gsub(pattern = usescen,usescen_mort,ifiles[grep("sxdt",ifiles)])
#     mortH <- get(load(file=ifile))
#     sxdt[mortH[region%in%reghf],on = setdiff(id.cols,"tob"), sx:=i.sx]
#   }
  
