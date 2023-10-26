
# Setup -------------------------------------------------------------------
# unlink(scen_foldres[2],recursive = T)
{
  {
    gc()
    rm(list=ls())
    
    #checking for required packages and installing if missing
    required.packages <- c("tidyverse","data.table","knitr")#,"ggrepel","countrycode"
    new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    #these loads all required packages
    invisible(lapply(required.packages, library, character.only = TRUE))
    iversion = "V13"
  }#setup
  
  source("funstack.r") #two functions to plot graphs

  # regions 
  load(file="../data/output/input_wic3proj/regions200.RData")
  sort(regions)

  unlo <- get(data("UNlocations",package="wpp2022") )
  setDT(unlo)
  unlo <- unlo[,.(name,country_code,reg_code,reg_name,area_code,area_name)]
  unlo[reg_name=="Australia/New Zealand",reg_name:="Australia New Zealand"
  ][name=="Australia/New Zealand",name:="Australia New Zealand"
    ][area_name=="Northern America",reg_name:="Northern America"]


  fertages = seq(10,45,by=5)#during the next five years
  educodes = paste("e",1:6,sep="")
  nedu = length(educodes)
  eduages = seq(10,25,by=5) #transitions from these ages
  neduage = length(eduages)
  edunames = educodes #?? name them

  #what do you want?
  ifert = T
  imort = T
  imig = T
  iprop = T#edu
  # imyscalc = F
  # imys = T
  # if(iSSPdb) imys = F
  #get the scens from above
  id.cols <- c("Time","sex","edu","agest","region")

  #list of scenarios
  scen_foldres <- dir(pattern="","../data/output/output_wic3proj/",full.names = T)
  scendt <- data.table(iscen = c(1:length(scen_foldres)),
                         scen = gsub("../data/output/output_wic3proj/","",scen_foldres),
                         name = c("SSP2noukr"))
  iscen.sel = c(1)# select scenarios to plot
  iscen <- scendt[iscen%in%iscen.sel,scen]
  iscen.nm <- scendt[iscen%in%iscen.sel,name]
  names(iscen.nm) = iscen
  nscen <- length(iscen)
  #path
  cc.dir.scen = paste("../data/output/output_wic3proj/",iscen,sep="/")  
  path_scen = paste(cc.dir.scen,"/",sep="")
}#setup
####single country output!!
# cctoname(108)
# ireg = "reg108"
# Read Data ---------------------------------------------------------------
{
#main result file [collect data] and prepare popavg for asfr calculation later

  for(i in 1:nscen) {
  
  if(i == 1) {final.col <- NULL; popf.cnt.asfr.all.col <- NULL} 
  
  iscenX =paste(iscen[i],"",sep="")
  
  #result file
  ifiles <- grep(paste("res_",iscen[i],sep=""),dir(path_scen[i]),value = T)
  mtime.files <- file.info(file=paste(path_scen[i],ifiles,sep=""))$mtime
  latest.file <- max(mtime.files)
  
  # final
  final.temp <- list(get(load(file=paste(path_scen[i],ifiles[match(latest.file,mtime.files)],sep=""))))
  final.temp[[1]][pop==-999,pop:=-0.00001]#correct this in the projection
  names(final.temp) = iscen[i]
  final.col <- append(final.col,final.temp)
  
  
  # final.temp[[1]][region=="reg524"&Time==2020][,by=.(agest),.(sum(pop))]#[,sum(V1)]#28,999
   
   
  
  
  
  # TFr based on births
  ## sex specific births gathered
  births.cnt          <- copy(final.temp[[1]])[pop==0,pop:=.001][
                              agest%in%fertages[-1],.(region,Time,edu,agest,births)][
                              ,.(births=sum(.SD$births)),by=.(region,Time,edu,agest)]
  # births.cnt[region==ireg&Time==2015,sum(births)]
  
  #collect [female population] and births from births.cnt
  popf.cnt.asfr <- copy(final.temp[[1]])[pop==0,pop:=.001
                        ][agest%in%fertages[-1]&sex=="f",.(region,Time,edu,agest,births,pop)
                        ][births.cnt,births:=i.births,on=.(region,Time,edu,agest)]
  popf.cnt.asfr.endyr <- copy(popf.cnt.asfr)[,Time:=Time-5]
  popf.cnt.asfr       <- popf.cnt.asfr[Time<2100,][popf.cnt.asfr.endyr,`:=`(popavg=  ((pop*0.5+i.pop*0.5)),pop1=i.pop),on=.(region,Time,agest,edu)]
  
  # popf.cnt.asfr[region==ireg&Time==2015,]
  popf.cnt.asfr.all   <- popf.cnt.asfr[,.(asfr=sum(.SD$births)/sum(.SD$popavg)*200),by=.(region,Time,agest)][,
                              scen:=iscen.nm[iscenX]]
  popf.cnt.asfr.all.col <- rbind(popf.cnt.asfr.all.col,popf.cnt.asfr.all)
  # popf.cnt.asfr.all.col[region==ireg&Time==2015][,.(tfr=sum(.SD$asfr)/200),by=.(scen)]
  
  }
  
  final1 <- NULL
  for(list.nm in names(iscen.nm))
    final1 <- rbind(final1,final.col[[list.nm]][,scen:=iscen.nm[list.nm]])
  
  
  # final1[region=="reg524"&scen=="SSP2"&agest ==-5&Time==2020][,by=.(Time),.(sum(pop))]
  
  
  
  # #check for negative numbers
  # if(nrow(final1[pop<0,])>0) print("Some negative pop numbers")
  # measure.vars = c("pop","births","pop1","emi","imm","deaths")
  # final1 <-copy(final1)[,lapply(.SD,function(icol) {icol[icol<0]<-0; icol}),.SDcols = measure.vars]
  #check
  if(nrow(final1[pop<0,])>0) stop("Some negative pop numbers")
  # final1[deaths<0,]
  
  
  # asfrdt ------------------------------------------------------------------
if(ifert){#asfrdt

for(i in 1:nscen) {
  if(i == 1) asfrdt.col <- NULL
  iscenX =paste(iscen[i],"",sep="")
  ##stack graphs
  ifiles <- grep("asfrdt",dir(path_scen[i]),value = T)
  mtime.files <- file.info(file=paste(path_scen[i],ifiles,sep=""))$mtime
  latest.file <- max(mtime.files)
  # final
  asfrdt.temp <- list(get(load(file=paste(path_scen[i],ifiles[match(latest.file,mtime.files)],sep=""))))
  # asfrdt.temp[[1]][pop==-999,pop:=-0.00001]#correct this in the rpojection
  # names(asfrdt.temp) = iscen[i]
  asfrdt.col <- rbind(asfrdt.col,asfrdt.temp[[1]][,scen:=iscen.nm[iscen[i]]])
}

for(i in 1:nscen) {
  if(i == 1) srbdt.col <- NULL
  iscenX =paste(iscen[i],"",sep="")
  ##stack graphs
  ifiles <- grep("srbdt",dir(path_scen[i]),value = T)
  mtime.files <- file.info(file=paste(path_scen[i],ifiles,sep=""))$mtime
  latest.file <- max(mtime.files)
  # final
  srbdt.temp <- list(get(load(file=paste(path_scen[i],ifiles[match(latest.file,mtime.files)],sep=""))))
  # srbdt.temp[[1]][pop==-999,pop:=-0.00001]#correct this in the rpojection
  # names(srbdt.temp) = iscen[i]
  srbdt.col <- rbind(srbdt.col,srbdt.temp[[1]][,scen:=iscen.nm[iscen[i]]])
}

}#fert


# propdt ------------------------------------------------------------------
if(iprop){#asfrdt
    
    for(i in 1:nscen) {
      if(i == 1) propdt.col <- NULL
      iscenX =paste(iscen[i],"",sep="")
      ##stack graphs
      ifiles <- grep("propdt",dir(path_scen[i]),value = T)
      mtime.files <- file.info(file=paste(path_scen[i],ifiles,sep=""))$mtime
      latest.file <- max(mtime.files)
      # final
      propdt.temp <- list(get(load(file=paste(path_scen[i],ifiles[match(latest.file,mtime.files)],sep=""))))
      # propdt.temp[[1]][pop==-999,pop:=-0.00001]#correct this in the rpojection
      # names(propdt.temp) = iscen[i]
      propdt.col <- rbind(propdt.col,propdt.temp[[1]][,scen:=iscen.nm[iscen[i]]])
    }
    
  }#propdt
# lifetable ---------------------------------------------------------------
if(imort){
#life table [wic3]

  
# e0s
#Later  
if(F){
  load(file = "../data/mortality/wic1 and wpp2022.rda")
  #....
}

#Sx for 2015, 2030, 2060, 2100
sxdt.col <- NULL
for(i in 1:nscen) {
  iscenX =paste(iscen[i],"",sep="")
  ifiles <- grep("sxdt",dir(path_scen[i]),value = T)
  mtime.files <- file.info(file=paste(path_scen[i],ifiles,sep=""))$mtime
  latest.file <- max(mtime.files)
  sxdt.temp <- list(get(load(file=paste0(path_scen[i],ifiles[match(latest.file,mtime.files)]))))
  sxdt.col <- rbind(sxdt.col,sxdt.temp[[1]][,scen:=iscen.nm[iscen[i]]])
}#i

#check!!
sxdt.col[,table(scen)]
sxdt.col[scen=="WIC2022_AGE_0mig"&region=="reg524"&sex=="m"&Time==2020]%>%spread(edu,sx)
sxdt.col[Time==2020&region=="reg100"&sex=="f"&edu=="e4"]%>%spread(scen,sx)

}#imort

# migration ---------------------------------------------------------------

  
  
if(imig){#emrdt for 2015, 2030, 2060, 2100
  print("mig data for only five SSPs")
for(i in 1:nscen) {
  if(i == 1) emrdt.col <- NULL
  iscenX =paste(iscen[i],"",sep="")
  
  ##stack graphs
  ifiles <- grep("emrdt",dir(path_scen[i]),value = T)
  mtime.files <- file.info(file=paste(path_scen[i],ifiles,sep=""))$mtime
  latest.file <- max(mtime.files)
  # final
  emrdt.temp <- list(get(load(file=paste(path_scen[i],ifiles[match(latest.file,mtime.files)],sep=""))))
  
  if(i==6) emrdt.temp[[1]][,emr:=0][,emr0:=0]
  # asfrdt.temp[[1]][pop==-999,pop:=-0.00001]#correct this in the rpojection
  # names(asfrdt.temp) = iscen[i]
  emrdt.col <- rbind(emrdt.col,
                      emrdt.temp[[1]][,scen:=iscen.nm[iscen[i]]])
  }
  
#Dilek's data copied from fillstatespace.r



#imrdt for 2015, 2030, 2060, 2100
for(i in 1:nscen) {
  if(i == 1) imrdt.col <- NULL
  iscenX =paste(iscen[i],"",sep="")
  ##stack graphs
  ifiles <- grep("imrdt",dir(path_scen[i]),value = T)
  mtime.files <- file.info(file=paste(path_scen[i],ifiles,sep=""))$mtime
  latest.file <- max(mtime.files)
  # final
  imrdt.temp <- list(get(load(file=paste(path_scen[i],ifiles[match(latest.file,mtime.files)],sep=""))))
  if(i==6) imrdt.temp[[1]][,imr:=0][,imr0:=0]
  imrdt.col <- rbind(imrdt.col,imrdt.temp[[1]][,scen:=iscen.nm[iscen[i]]])
  }
migdt <- rbind(emrdt.col[,var:="emr"][][, setnames(.SD, c("emr","emr0"), c("value","value0"))],
               imrdt.col[,var:="imr"][, setnames(.SD, c("imr","imr0"), c("value","value0"))])
vars = setdiff(names(final.col[[1]]),id.cols)
}

  # WIC1/2 wpp --------------------------------------------------------------
  
  if(F){


  
  #mig_val
  # ifiles <- dir("df[1-5]",path = "../wic1/ToGuy_V9/",full.names = T)
  
  
  
  #mig  
  ifiles <- dir("df[1-5]net",path = "../wic1/ToGuy_V9/",full.names = T)
  dfwic1 <- sapply(ifiles,function(x ){read.csv(x)} ,simplify = F)
  dfwic1 <- do.call("rbind",dfwic1)
  dfwic1$scen = row.names(dfwic1)
  row.names(dfwic1) <- 1:nrow(dfwic1)
  # df1wic1 <- read_csv("../wic1/A_POPDISTR_R_valbp_7scen.csv")
  setDT(dfwic1)
  dfwic1[,scen:=paste0("SSP",substr(scen,20,20),"_WIC2013")]
  
  wic1netflow <- dfwic1[scen=="SSP2_WIC2013"&year>=2020&isono<900&sexno==0&eduno==0&ageno==0][
    ,by=.(year,scen),.(netflow=sum(abs(net))/1000)
    ][,setnames(.SD,c("year"),c("Time"))]
  
  #to compare with UN net for 2015-2020
  wic1net_cnt2015 <- dfwic1[scen=="SSP2_WIC2013"&
                          year==2015&isono<900&sexno==0&eduno==0&ageno==0][
    ,by=.(isono,year,scen),.(net=sum(net)/1000)
  ][,setnames(.SD,c("year","isono"),c("Time","region"))][,region:=paste0("reg",region)]
  
  # save(wic1net_cnt2015,file="../Validation/V12 extra/wic1net_cnt2015.rda")
  
  #pop
  ifiles <- dir("df[1-5]pop",path = "../wic1/ToGuy_V9/",full.names = T)
  dfwic1 <- sapply(ifiles,function(x ){read.csv(x)} ,simplify = F)
  dfwic1 <- do.call("rbind",dfwic1)
  dfwic1$scen = row.names(dfwic1)
  row.names(dfwic1) <- 1:nrow(dfwic1)
  # df1wic1 <- read_csv("../wic1/A_POPDISTR_R_valbp_7scen.csv")
  setDT(dfwic1)
  dfwic1[,scen:=paste0("SSP",substr(scen,20,20))]
  
  # ../wic1/ToGuy_V9/df1pop.csv.130
  
  wic1pop <- dfwic1[isono==900&sexno==0&eduno==0&ageno==0][
    ,by=.(year,scen),.(pop=sum(pop))][,setnames(.SD,c("year"),c("Time"))]
  wic1pop[scen=="SSP2"]
  wic1cntpop<- dfwic1[sexno==0&eduno==0&ageno==0][
    ,by=.(year,scen,isono),.(pop=sum(pop))][,setnames(.SD,c("year","isono"),c("Time","region"))]
  wic1cntpop<- wic1cntpop%>%filter(scen%in%c("SSP2","SSP3") & Time>=2015 & region<900 )%>%
    mutate(region=paste0("reg",region),scen=paste(scen,"WIC2013",sep = "_"))
  
  
  #wic2 data
  load(file="../data/population/popprojFull_AGES_fertsmoothed_SSP2_2018-12-17_185418.RData")
  setDT(df1wic2)
  vars.wic <- names(df1wic2)[c(8,11,13,14,18,20,21)]
  setnames(df1wic2,"period","Time")
  
  wic2netflow <-df1wic2[Time<2100][,by=.(Time,region),.(netflow=abs(sum(inmig)-sum(outmig)))
                                   ][,by=.(Time),.(netflow=sum(netflow/1000))][,scen:="SSP2_WIC2018"] 
  
  
  #world
  df1wic2[,lapply(.SD,sum,na.rm=T),.SDcols = vars.wic,by=.(Time,region)]
  totpop.df1wic2 = round(df1wic2[,lapply(.SD,sum,na.rm=T),.SDcols = vars.wic,by=.(Time)],0)
  
  
  #wpp popas.wpp
  load(file="../results/popas.wpp.rda")
  pop.wpp2022 <- popas.wpp[cc==900&wpp=="wpp2022_AG"][,.(pop=sum(.SD$pop)),by=.(Time)]
  
  data("popAge5dt",package = "wpp2022")
  data("popprojAge5dt",package = "wpp2022")
  selnames <- names(popAge5dt)
  popas.wpp2022 <- rbind(popAge5dt,copy(popprojAge5dt)[,..selnames])

  popas.wpp2022 <- popas.wpp2022[,.(year,country_code,age,popM,popF)][
    ,setnames(.SD,c("country_code","age","year"),c("ccode","agest","Time"))][
      ,agest:=(tstrsplit(x = agest,split = "[+-]",keep=1,fixed=F))][
        ,agest:=as.numeric(agest)][,region:=paste0("reg",ccode)][region%in%regions]
  popas.wpp2022 <- melt(data = popas.wpp2022,measure.vars = c("popM","popF"),variable.name = "sex",value.name = "pop")
  popas.wpp2022[,sex:=tolower(gsub("pop","",sex))]
  
  #pop2020
  popdt.total <- copy(popas.wpp2022)[Time==2020,.(pop=sum(pop,na.rm = T)),by=.(region)]
  popdt.total[order(pop)]

  pop2020_reg900 <- 7804973.773
  pop2020.regions <- copy(popas.wpp2022)[Time==2020,.(pop=sum(pop,na.rm = T))][,pop]
  pop2020_reg900/pop2020.regions
  popAge5dt[country_code==900]
  
  
  totpop.cnt.df1wic2 <- df1wic2[,lapply(.SD,sum,na.rm=T),.SDcols = vars.wic,by=.(Time,region)][,
                                                                                               region:=gsub("cc","reg",region)]
  totpop.cnt.wpp2022 <- popas.wpp2022[,.(pop=sum(.SD$pop)),by=.(Time,region)
  ][,wpp:="wpp2022"]
  
  
  # totpop.cnt.col <- rbind(totpop.cnt.df1wic2[,.(region,Time,pop)][,scen:="WIC2018"],
  #                         totpop.cnt.wpp2019[,.(region,Time,pop)][,scen:="WPP2019"])
  # save(totpop.cnt.col,file='../data/wic2 and wpp2019 totpop.rda') #for individual country checking in main wic3, V9..r          
  totpop.cnt.col <- rbind(wic1cntpop,
                          totpop.cnt.df1wic2[region%in%regions][,.(region,Time,pop)][,scen:="SSP2_WIC2018"],
                          totpop.cnt.wpp2022[,.(region,Time,pop)][,scen:="WPP2022_AG"])
  save(totpop.cnt.col,file='../data/wic1_2 and wpp2022 totpop.rda') #for individual country checking in main wic3, V9..r          
  
}
  # totpop.cnt.col
  load(file='../data/wic3proj/wic1_2 and wpp2022 totpop.rda') #for individual country checking in main wic3, V9..r          
  

  
  ## summary table
final.summ.col <- NULL
vars = setdiff(setdiff(names(final.col[[1]]),id.cols),c("pop1","scen"))

final.summ.col = NULL
for(i in 1:nscen){
  #edutran = zero; emi = imi; pop-deaths+births = pop(t+5)
  final.summ.temp <-final.col[[i]][,lapply(.SD,sum,na.rm=T),.SDcols = vars,by=.(Time)][,
                    `:=`(pop1=pop-deaths, pop=pop-births)] #births are already in 'pop'
  final.summ.col <- rbind(final.summ.col,final.summ.temp[,scen:=iscen.nm[iscen[i]]])
}

final.summ.col[Time==2020]
final.summ.col[Time==2025]

final.summ.col[,.(Time,pop,scen)]%>%spread(scen,pop)

final.summ.births.col = NULL
for(i in 1:nscen){
  #edutran = zero; emi = imi; pop-deaths+births = pop(t+5)
  final.summ.temp <-final.col[[i]][agest==-5][,lapply(.SD,sum,na.rm=T),.SDcols = vars,by=.(Time)]
  final.summ.births.col <- rbind(final.summ.births.col,final.summ.temp[,scen:=iscen.nm[iscen[i]]])
}
final.summ.births.col[,.(Time,pop,scen)]%>%spread(scen,pop)


final.summ.cnt.col <- NULL
for(i in 1:nscen){
  #edutran = zero; emi = imi; pop-deaths+births = pop(t+5)
  final.summ.cnt.temp <-final.col[[i]][,lapply(.SD,sum,na.rm=T),.SDcols = vars,by=.(Time,region)
                                       ][,`:=`(pop1=pop-deaths, pop=pop-births)] #births are already in 'pop'
  final.summ.cnt.col <- rbind(final.summ.cnt.col,final.summ.cnt.temp[,scen:=iscen.nm[iscen[i]]])
}
#check
final.summ.cnt.col[region=="reg356"&Time==2020]

final.summ.cntage.col <- NULL
for(i in 1:nscen){
  #edutran = zero; emi = imi; pop-deaths+births = pop(t+5)
  final.summ.cntage.temp <-final.col[[i]][,lapply(.SD,sum,na.rm=T),.SDcols = vars,by=.(Time,region,agest)][,`:=`(pop1=pop-deaths, pop=pop-births)] #births are already in 'pop'
  final.summ.cntage.col <- rbind(final.summ.cntage.col,
                                 final.summ.cntage.temp[,scen:=iscen.nm[iscen[i]]])
}

final.summ.cntagesex.col <- NULL
for(i in 1:nscen){
  #edutran = zero; emi = imi; pop-deaths+births = pop(t+5)
  final.summ.cntagesex.temp <-final.col[[i]][,lapply(.SD,sum,na.rm=T),.SDcols = vars,by=.(Time,region,agest,sex)][,`:=`(pop1=pop-deaths, pop=pop-births)] #births are already in 'pop'
  final.summ.cntagesex.col <- rbind(final.summ.cntagesex.col,
                                 final.summ.cntagesex.temp[,scen:=iscen.nm[iscen[i]]])
}


totpop.cnt.col <- rbind(totpop.cnt.col,
                        final.summ.cnt.col[,.(region,Time,pop,scen)])


# mysdt ------------------------------------------------------------------

#Later - day2
if(F&&imys){#mysdt
  for(i in 1:nscen) {
    # source("mys wicproj3, V1.r") #updated with new data from MP for 2023
    if(i == 1) mysdt.col <- NULL
    iscenX =paste(iscen[i],"",sep="")
    if(imyscalc) {source("fmys.r");fmys(iscen = iscenX)}
    mysdt.temp =  get(load(file=paste("../results/MYS_",iscenX,".rda",sep="")))
    mysdt.col <- rbind(mysdt.col,mysdt.temp[,scen:=iscen.nm[iscenX]])  }
}#mys



# regional aggregation ----------------------------------------------------
#Later
#run it once
if(F){#Regional aggregation
  # load(file="../results/popas.wpp.rda")
  # pop.wpp2022 <- popas.wpp[cc==900&wpp=="wpp2022_AG"][,.(pop=sum(.SD$pop)),by=.(Time)]
  
  data("popAge5dt",package = "wpp2022")#jan1
  data("popprojAge5dt",package = "wpp2022")
  selnames <- names(popAge5dt)
  popas.wpp2022 <- rbind(popAge5dt,copy(popprojAge5dt)[,..selnames])
  
  popas.wpp2022 <- popas.wpp2022[,.(year,country_code,age,popM,popF)][
    ,setnames(.SD,c("country_code","age","year"),c("ccode","agest","Time"))][
      ,agest:=(tstrsplit(x = agest,split = "[+-]",keep=1,fixed=F))][
        ,agest:=as.numeric(agest)][,region:=paste0("reg",ccode)]
  popas.wpp2022 <- melt(data = popas.wpp2022,measure.vars = c("popM","popF"),variable.name = "sex",value.name = "pop")
  popas.wpp2022[,sex:=tolower(gsub("pop","",sex))]
  
  popdt.total <- copy(popas.wpp2022)[Time==2010&region=="reg156",.(pop=sum(pop,na.rm = T)),by=.(region,ccode)]
  
  
  #pop2020
  popdt.total <- copy(popas.wpp2022)[Time==2020,.(pop=sum(pop,na.rm = T)),by=.(region,ccode)]
  popdt.total[order(pop)]
  
  pop2020_reg900 <- popdt.total[ccode==900,pop] #7,804,974
  pop2020.200c <- copy(popas.wpp2022)[Time==2020 & region%in%regions,sum(pop,na.rm = T)]#7,802,030
  pop2020.200c/pop2020_reg900#99.962%
  pop2020.200c-pop2020_reg900# -2944.122
  
  corr_worldpop <- pop2020_reg900/pop2020.200c
  # correction factor [subregion, age, sex]
  
  
  # unlo[name=="Northern America"]
  # unlo[location_type==4,name] #northern america is missing
  
  popas.wpp2022[unlo[,setnames(.SD,"country_code","ccode")],
                on=.(ccode),`:=`(reg_name=reg_name,area_name=area_name)]
  unregs <- unlo[,unique(reg_name)][-1]
  unregs.cc <- unlo[name%in%unregs,.(name,country_code)]
  unareas <- unlo[,unique(area_name)][-1]
  
  #Australia NZ is repeated twice
  popreg2020 <- popas.wpp2022[Time==2020&ccode > 900&ccode!=1836,by=.(ccode),.(totpop=sum(pop))
  ][unlo[,setnames(.SD,"country_code","ccode")],
    on=.(ccode),`:=`(reg_name=name,area_name=name)]
  # popreg2020[area_name %in% unareas & ccode <=935,sum(totpop)]
  # popreg2020[,sum(totpop)]
  #sub_reg
  poptotreg.2020 <-popreg2020[reg_name %in% unregs,by=.(reg_name) ,sum(totpop)]
  poptotreg.2020[,sum(V1)]#7804974
  cc200 <- as.numeric(gsub("reg","",regions))
  poptotreg.2020.cc <- popas.wpp2022[Time==2020&ccode%in%cc200,
                                     by=.(reg_name),.(totpop=sum(pop))
  ][poptotreg.2020,on=.(reg_name),cratio:=V1/totpop]
  poptotreg.2020.cc[,sum(totpop)]#7,802,030
  save(poptotreg.2020.cc,file="../data/poptotreg.2020.cc.rda")
  write.csv(poptotreg.2020.cc,file="../data/poptotreg.2020.cc.csv")
}
# wpp2022_regcode <- readxl::read_xlsx("../data/codeWPP22.xlsx")
# setDT(wpp2022_regcode)
# wpp2022_regcode[,region:=paste0("reg",ccode)]
}#read data


# World ------------------------------------------------------------------
if(T){
  # stop("482 World")
  icnt = "World"
  ireg = "World"
  
  totpop.glo <- copy(totpop.cnt.col)[region%in%regions&Time>2015][,.(pop=sum(pop)),by=.(scen,Time)]
  totpop.glo%>%spread(scen,pop)
  scens.glo <- totpop.glo[,unique(scen)]
  ggpopline <- totpop.glo[,Time:=as.numeric(Time)]%>%
  ggplot(aes(x=Time,y=pop/1000000,linetype=scen,shape=scen, col=scen))+
    geom_line()+geom_point()+
    ylab("Population (in billions)")+xlab("Year")+
    ggtitle(paste("Population projection for the ", icnt,sep=""))
    ggpopline

   # ggsave("../results/World total pop SSPs.png",last_plot(),width = 7, height=5)
   ggsave(file =  paste0("../results/wic3proj/",icnt,"_Report_pop", iversion,".png"),
          last_plot(),width = 6,height = 4)
  
   
    
#pyramid
   #ssp2
   scen.sel = iscen.nm[1]
   ggpyr2020<-funpyrwrapper(figval = copy(final1)[scen==scen.sel],
                            ivar="pop",
                            iTime=2020,
                            iiscen=scen.sel,#can be deleted
                            iscale=1000)
   
   #multiple year
   scen.sel = iscen.nm[1]
    ggpyrX <- funpyrwrapper(figval = copy(final1)[scen==scen.sel],
                            ivar="pop",
                            iTime=c(2020,2050,2100),
                            iiscen=scen.sel,
                            iscale=1000)
    
  ggsave(paste0("../results/wic3proj/World pyramid multiple time ",iversion," ",scen.sel,".png"),
           last_plot(),width = 10, height=8)
  
  
# stack 
  figdt <- copy(final1)[scen==scen.sel][,by=.(Time,sex,edu,agest,scen),.(pop=sum(pop))]
  
  ggpopstack <- funstack(
    figval =  figdt, 
    iage = 0:120, #-5:100
    isex = c("m","f")[1:2],
    ivar = "pop",
    #select regions
    iregions =icnt ,#regions.nm,#,#"Nepal" all china
    icnt = icnt, #region name
    #cohort
    itob = 9999, #not to choose any cohort
    # itob = 2000 #those born in 2000 #maybe a bar
    # if we want prop data
    ipropgraph = F,
    # # ipers = 2010:2095
    iiscen = scen.sel,
    iscale = 1000#millions
  ) 
  
  ggsave(paste0("../results/wic3proj/World stack",iversion," ",scen.sel,".png"),
         last_plot(),width = 10, height=8)
  
}#World

