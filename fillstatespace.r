dtfiles <- grep("dt$",ls(),value=T)
# [1] "asfrdt" "emrdt"  "imrdt"  "popdt"  "propdt" "srbdt"  "sxdt"  

# popdt - no change required

# New Mort ----------------------------------------------------------------
if(newmort){ 
 if(newmorttype%in%c("ssp1","ssp5")){
   #done at the starting level usescen
   stop("prepare files for high/low scenario")
 } else if (newmorttype%in%c("ssp4")) {
   #high fer [HIGH]
   stop("prepare files for high/low scenario")
   sxdt.high <-  get(load(paste0("../data/output/e06baselineHmortCovid_emorthighC_efert_0mig/sxdt.RData")))##statespace
   #replace for high fertility countries
   sxdt[sxdt.high[region%in%reghf], on = .(region,Time,sex,edu,agest), sx:=i.sx]
   rm(sxdt.high)
 } else if(newmorttype%in%c("ssp3")) {
   stop("prepare files for high/low scenario")
   #done at the starting level
 } else {
 }
 
 
}#newmort
# New Fert ----------------------------------------------------------------
if(newfert) {
  print("asfrdt changes required")
  
  
   if(newferttype %in% c("ssp1","ssp5")){#newfert type
      #fert = low - 20% lower by 2020-2040 and 25% lower by 2040-2060 {??} [done]
      #low fert
      # fertfact.high = c(seq(1,1.2,length.out=5),seq(1.2,1.25,length.out=5)[-1],rep(1.25,8))
      fertfact.low = c(seq(1,0.8,length.out=5)[-1],
                       seq(0.8,.75,length.out=5)[-1],
                       rep(0.75,8))
      fertfact.low <- data.table(Time = pers[-length(pers)],ifact=fertfact.low)
      asfrdt[fertfact.low,on=.(Time),asfr:=asfr*ifact]
    }  else if (newferttype == "ssp3"){#newfert type
      #fert = high - 20% higher by 2035-2040 and 25% higher by 2055-2060 {??} [done]
      #low fert
      print("ssp3 high fertility")
      fertfact.high = c(seq(1,1.2,length.out=5)[-1],
                        seq(1.2,1.25,length.out=5)[-1],
                        rep(1.25,8))
      fertfact.high <- data.table(Time = pers[-length(pers)],ifact=fertfact.high)
      asfrdt[fertfact.high,on=.(Time),asfr:=asfr*ifact]
    }  else if (newferttype == "ssp4"){#newfert type
      #High/Low  fert =  +- 20% lower by 2020-2040 and +-25% by 2040-2060 {??} [done]
      
      #High (HiFert)
      fertfact.high = c(seq(1,1.2,length.out=5)[-1],
                        seq(1.2,1.25,length.out=5)[-1],
                        rep(1.25,8))
      fertfact.high <- data.table(Time = pers[-length(pers)],
                                  ifact=fertfact.high)
      asfrdt.hf <- copy(asfrdt)[region%in%reghf][fertfact.high,on=.(Time),asfr:=asfr*ifact]
      
      #Low (LoFert)
      fertfact.low = c(seq(1,0.8,length.out=5)[-1],
                       seq(0.8,.75,length.out=5)[-1],
                       rep(0.75,8))
      fertfact.low <- data.table(Time = pers[-length(pers)],ifact=fertfact.low)
      asfrdt.lf <- copy(asfrdt)[!(region%in%reghf)][fertfact.low,on=.(Time),asfr:=asfr*ifact]
    
      asfrdt <- rbind(asfrdt.lf,asfrdt.hf)
    } else if (newferttype == "kcfert"){
      ##CHANGES###
      # stop("Update new fertility")
      
      # we copied all the input files in a new input folder called input_kcfert
      # we changed asfrdt.csv in the new input folder and saved it as asfrdt_kcfert
      # also deleted two columsn (not needed)
      asfrdt.kcfert <-read.csv("../data/output/input_kcfert/asfrdt_kcfert.csv")
      rm(asfrdt)
      asfrdt <- setDT(asfrdt.kcfert)
      rm(asfrdt.kcfert)
      
      # asfrdt[region=="reg524"]
      
    }
   
  }#newfert  
# srbdt - no change required

# New Edu -----------------------------------------------------------------
if(newedu) {
  stop("prepare files")  
  
  if(newedutype%in%c("ssp1","ssp5")){
      # stop("...")
      #edu = sdg [done]
      # propdt
      ##edu (turn these into EAPRs) - problem with the data
      #run it once
      if(T){
        
      #SSP1 update CHECK
       #RUN THIS ONCE
        # stop("..")
        if(F) source("ssp1 edu.r")
        # eapr5.tgt.col.185
        load(file="../data/education/eapr5.tgt.col.185.rda")
        # eaprs.tgt.col.185 #e1:e4
        load(file="../data/education/eaprs.tgt.col.185.rda")
        
        # eaprs.tgt.col.185[time==2050&sex=="f"&ccode==cntocc("Ethiopia")&scen=="tgt3"]%>%spread(edu,value)
        
        cumprop.tgt3.col.185 <- rbind(eaprs.tgt.col.185,eapr5.tgt.col.185)[scen=="tgt3"
        ][,cumprop:=cumprod(.SD$value),by=.(ccode,sex,time,agest)
        ][,.(ccode,sex,edu,tob,agest,time,cumprop)]
        
        # cumprop.tgt3.col.185[time==2050&sex=="f"&ccode==cntocc("Ethiopia")]%>%spread(edu,cumprop)
        
        # cumprop.tgt3.col.185[cumprop!=1&edu=="e1"]
        
        # cumprop.tgt3.col.185[ccode==231&sex=="m"&time==2050]
        iprop.final.col <- cumprop.tgt3.col.185%>%spread(edu,cumprop)%>%as.data.table()
        iprop.final.col <-copy(iprop.final.col)[,`:=`(e6=e5,e5=e4-e5,e4=e3-e4,e3=e2-e3,e2=e1-e2,e1=1-e1)]
        # iprop.final.col[ccode==231&sex=="m"&time==2050]
        
        #proxy
        edu.approx.agbl <- read.csv("../data/education/16countries_approximation AG BB.csv")
        setDT(edu.approx.agbl)
        #or simply use it from the last time
        # cntocc("Channel Islands") is excluded in this round
        
        
        #subset(edup,period==2020 & age == 15 &sex == "female",select = c(edu,cc356))
        edu.approx1 <- edu.approx.agbl[cc.nos.approx==1,.(country_code,cc.approx1)
        ][,setnames(.SD,c("cc.approx1"),c("ccode"))]
        edu.approx1<-  merge(edu.approx1[,ccode:=as.character(ccode)],iprop.final.col)
        edu.approx1<- edu.approx1[,ccode:=NULL][,setnames(.SD,c("country_code"),c("ccode"))][time>=2020]
        
        # head(edu.approx1)
        # with(edu.approx1,table(time))
        #edu.approx3
        edu.approx3 <- edu.approx.agbl[cc.nos.approx==3][,cc.nos.approx:=NULL]
        edu.approx3 <-melt(edu.approx3,id.vars = "country_code",value.name = "ccode")
        edu.approx3[,variable:=NULL]
        edu.approx3<-  merge(edu.approx3[,ccode:=as.character(ccode)],iprop.final.col)
        edu.approx3<- edu.approx3[,ccode:=NULL][,setnames(.SD,c("country_code"),c("ccode"))][time>=2020]
        edu.approx3<- melt(edu.approx3,id.vars = c("ccode","sex","tob","agest","time"),variable.name = "edu")
        edu.approx3 <- edu.approx3[,.(value=mean(value,na.rm=T)),by=.(ccode,sex,tob,agest,time,edu)]%>%spread(edu,value)%>%as.data.table()
        
        iprop.final.col.wproxy <- rbind(iprop.final.col,edu.approx1,edu.approx3)
        
        input.prop = copy(iprop.final.col.wproxy)[time>2020&agest%in%15:30
                                                  ][,region:=paste0("reg",ccode)][,ccode:=NULL]
        input <- melt(input.prop,measure.vars = edunames,variable.name = "edu",value.name = "eduprop") 
        
        id.cols.here <- intersect(id.cols,names(propdt))
        propdt<-propdt[Time >2020][,prop:=-999][input[,setnames(.SD,"time","Time")],  prop:=eduprop, on = id.cols.here]
        
        #SSP1/5 = average(Medium + Target)
        propdt.get <- get(load("../data/output/e04baselineSSP2_emort_efert_emig1/propdt.RData"))
        
        propdt[propdt.get,on=setdiff(id.cols,"tob"),prop:=(prop+i.prop)/2]
        rm(propdt.get)
        }
      #check for what?
      # input%>%filter(region==ireg,sex=="f",agest ==15&Time==2015)
      
    } else if (newedutype=="ssp3"){
      print("SSP3 - CER")
      
     if(F){ 
      
      funCER <- function(idf){
        # idf <<- idf
        idf0 <- as.data.frame(idf[Time==2020])
            #str(idf)
        idf.col <- idf0
        for(ipr in seq(2025,2100,by=5)){
          #stop()
          idf.next <- idf0%>%mutate(Time=Time+5)
          idf.next[2:4,names.eapr] <- idf0[1:3,names.eapr] #
          idf.next[,names.eapr] <- pmax(as.matrix(idf0[,names.eapr]),as.matrix(idf.next[,names.eapr]))
          idf.col <- rbind(idf.col,idf.next)
          idf0 <- idf.next  
        }
        return(idf.col)  
      }#end function
      
      
      #+add 2020 education distribution!!
      names.eapr = edunames[-length(edunames)]
      eapr2020 <- popdt[Time==2020&agest%in%15:30,setnames(.SD,"pop","value")][,by=.(region,Time,sex,agest),eapr(.SD)]
      eapr2020p <- propdt[,setnames(.SD,"prop","value")][,by=.(region,Time,sex,agest),eapr(.SD)]
      eaprs <- rbind(eapr2020,eapr2020p)%>%spread(edu,value)%>%as.data.table()
      rm(eapr2020,eapr2020p)
      eaprs.cer <- eaprs[,by=.(region,sex),funCER(idf=.SD)]
      eaprs.cer <- data.frame(eaprs.cer)
      idf.cum <- data.frame(eaprs.cer)%>%mutate(e1=1)
      for(i in 2:6) {
        idf.cum[,edunames[i]] = idf.cum[,edunames[i-1]]*eaprs.cer[,edunames[i-1]]
      }
      idf.cum[,edunames[1:5]] <- idf.cum[,edunames[1:5]] - idf.cum[,edunames[2:6]]
      
      setDT(idf.cum)
      #now save these values
      save(idf.cum,file="../data/education/cer edu_proj_WIC3.rda")
    }#run_once  
      
      input <- get(load("../data/education/cer edu_proj_WIC3.rda"))
      input <- melt(data = input,id.vars = c("region","Time","sex","agest"),
                    variable.name = "edu")
      #check for what?
      id.cols.here <- intersect(id.cols,names(propdt))
      propdt[,prop:=-999][input,  prop:= value, on = id.cols.here]
      
    } else if (newedutype=="ssp4") {
      # stop("SSP4 - CER/GET")
      #RUN THIS ONCE
      
      if(F){
        #SSP3
        cer <-  get(load(usescen_cer))
        #2020
        propdt2020 <- popdt[Time==2020&agest%in%15:30
                            ][,by=.(region,Time,sex,agest),prop:=prop.table(.SD$pop)
                              ][,pop:=NULL]
        cer <- rbind(propdt2020,cer)[,setnames(.SD,"prop","cer")]
        rm(propdt2020)
        
        #SSP2
        get <- copy(propdt)[,setnames(.SD,old = 'prop',new = 'get')]
        #head(get)
        #30Apr2019: decided to use CER-10%/GET for all scenarios
        ssp4d0 <- cer[get,on = setdiff(id.cols,"tob"),get:=i.get][Time==2020,get:=cer]
        rm(cer,get)
        #LoFert and HiFert (and Rich-OECD) countries (CER-10%/GET):
        #the education attainment progression ratio (EAPR) is reduced by 10% of the ratios in the CER scenario (CER-10%) 
        #for the transitions: from no education to incomplete primary, incomplete primary to completed primary and
        #from completed primary to completed lower secondary.
        #As a result proportion without education increases while proportion with at least some primary,
        #completed primary and lower secondary declines.
        #The GET transition ratio is assumed for transitions from Lower secondary to upper secondary and 
        #upper secondary to post-secondary/tertiary which aims to produce larger group of elites in these countries.
        
        #The assumption for the high income countries is different because we consider their differences in current education
        #composition and education requirements in job market. High income countries have already had larger proportion of
        #high educated population, and people with secondary or lower education have already been disadvantageous position in 
        #finding decent jobs. We assume the progression ratios to be same as in CER for categories no education,
        #incomplete primary, completed primary, and completed lower secondaryprimary and CER-20% for the transitions from 
        #lower secondary complted to upper secondary completed and from upper secondary complted to post-secondary/tertiary 
        # (CER/CER-20%). The idea is to generate a smaller portion of highly educated elites."
        e4s <- edunames[1:4]
        #from "fn_step10 V9, V2_AG.R"
        age.ult.edu <- c(15,15,20,25,30)
        names(age.ult.edu) <- edunames[-1]
        age.ult.edu.dtt <-as.data.table(data.frame(edu=edunames[-1],agest = age.ult.edu))
        cumprop <- function (prop) {
          # prop <<- prop
          
          x <- rev(cumsum(rev(prop$value)))
          n <- length(x)
          prop$value <- x
          return(prop)
          
          # xx <-round(CER.ssp4[Time==2020&agest==30,sspcumval],3)
          # yy <- c(diff(-xx),xx[length(xx)])
          # cumsum(rev(rev(yy)))
          # CER.ssp4 <- CER.ssp4[,by=.(Time,agest,tob),sspval:=  c(diff(-.SD$sspcumval),.SD$sspcumval[length(.SD$sspcumval)])]
          
        }
        
        f.ssp4d <- function(df1,CERx = -.1,iby) {
           # df1 <<- df1
           # iby <<- iby
           # CERx <<- CERx
           # stop("...")
          
          #start with 0.9 for all and change the ones that will be reached gradually 
           # CER.ssp4 <- df1[,.(Time,edu,agest,tob,cer)
           #                ][edu%in%edunames[5:6],edu:="e4"
           #                  ][,by=.(Time,edu,agest,tob),.(cer=sum(.SD$cer))
           #                      ][,ssp4c:=1]
           
           CER.ssp4 <- df1[,.(Time,edu,agest,tob,cer)
           ][,by=.(Time,agest,tob),cumcer:=rev(cumsum(rev(.SD$cer)))
           ][,ssp4c:=1][edu%in%e4s]
           
           # CER.ssp4[Time==2020,.(agest,edu,cumcer)]%>%spread(edu,cumcer)
           
           # #this should be 1 [2020 and the edu related to the tob]
           # CER.ssp4[copy(age.ult.edu.dtt)[,tob:=2020-agest-5],on=.(edu,tob),ssp4c:=1]
           # 
          
          # CER2020.2035%>%spread(edu,cer)
          #ult.age correction
          for(ipr in 2:3){
            CER.ssp4[copy(age.ult.edu.dtt)[,Time:=2020+5*(ipr-1)],on=.(Time,edu,agest),ssp4c:=1+CERx/3 * (ipr-1)]
            }
           
           for(ipr in 4:17){
             CER.ssp4[copy(age.ult.edu.dtt)[,Time:=2020+5*(ipr-1)],on=.(Time,edu,agest),ssp4c:=1+CERx]
           }
           #connected with the tob
           CER.ssp4[unique(copy(CER.ssp4)[ssp4c<1,.(edu,tob,ssp4c)]),on=.(edu,tob),ssp4c:=i.ssp4c]
          # CER.ssp4[copy(age.ult.edu.dtt)[,tob:=2020-agest-5],on=.(edu,tob),ssp4c:=1]
          
          #not changing the 2020 values #last values are  all 0.9
          CER.ssp4[Time==2020,ssp4c:=1][Time>2050&edu!="e1",ssp4c:=0.9][,sspcumval:=cumcer*ssp4c]
          
          CER.ssp4[Time==2025&agest==15]
                   
          # xx <-round(CER.ssp4[Time==2020&agest==30,sspcumval],3)
          # yy <- c(diff(-xx),xx[length(xx)])
          # cumsum(rev(rev(yy)))
          
          CER.ssp4 <- CER.ssp4[,by=.(Time,agest,tob),sspval:=  c(diff(-.SD$sspcumval),.SD$sspcumval[length(.SD$sspcumval)])]
          
          #check
          CER.ssp4[,by=.(Time,agest),.(ss =sum(.SD$sspval))]%>%spread(agest,ss)
          
          #Final
          CER.ssp4 <- CER.ssp4[,.(Time,edu,agest,tob,sspval,sspcumval)]
          
          if(F){ 
            ggeapr <- merge(age.ult.edu.dtt,CER2020.2100)%>%
              ggplot(aes(x=Time,y=sspval,col=edu,shape=edu))+
              geom_line()+geom_point()+ylab("EAPR")+
              ggtitle(paste("PROP",sep=""))
            print(ggeapr)
          }
          
          #GET (proportions)
          get0 <- df1%>%select(Time,agest,edu,get)
          
          eapr.get0 <- get0[,setnames(.SD,"get","value")
                            ][,by=.(Time,agest),eapr(.SD)
                              ][edu%in%edunames[4:5]][,edu:=paste0(edu,"x")]%>%spread(edu,value)%>%as.data.table()
          
          cumprop.e4.cer <- CER.ssp4[edu=="e4",.(Time,agest,tob,sspcumval)]
          prop.get0.SSP4 <- merge(cumprop.e4.cer,eapr.get0)[,`:=`(e4=sspcumval*(1-e4x),
                                                                  e5=sspcumval*e4x,
                                                                  e6=sspcumval*e4x*e5x)
                                  ][,e5:=e5-e6][,.(Time,agest,tob,e4,e5,e6)]
          
          prop.get0.SSP4 <-melt(prop.get0.SSP4,measure.vars = edunames[4:6],variable.name = "edu",value.name = "sspval")
          
          prop.ssp4 <- rbind(CER.ssp4[edu!="e4",.(Time,agest,tob,edu,sspval)],
                             prop.get0.SSP4)
          
          # prop.ssp4[,by=.(Time,agest,tob),sum(sspval)]#%>%spread(edu,sspval)
          
          cumprop.ssp4 <- prop.ssp4[,setnames(.SD,"sspval","value")][,by=.(Time,agest),cumprop(.SD)] 
          
          if(T){ 
            ggprop <- merge(age.ult.edu.dtt,cumprop.ssp4)%>%
              ggplot(aes(x=Time,y=value,col=edu,shape=edu))+
              geom_line()+geom_point()+ylab("EAPR")+
              ggtitle(paste("PROP ",cctoname(gsub("reg","",iby$region))," ",iby$sex,sep=""))
            
            print(ggprop)
            #prop of e5 is increasing as the prop of e6 is declining
            }
          
          setDT(prop.ssp4)
          return(prop.ssp4)#ssp4d0
        }#end of f.ssp4d.hf 
        
        
        
        #using data.frame and data.table together.. need to work on changing to data.table
        pdf("../data/education/ssp4 check.pdf")
          ssp4d1 <-ssp4d0[,tob:=Time-agest][,by=.(region,sex),f.ssp4d(.SD,iby=.BY)]
          #check
          # 
          # ssp4d1[sspval<0]
          # cntocc("Zambia")
          # ssp4d1[sex=="f"&region=="reg894"&Time==2075]%>%spread(edu,sspval)
          # ssp4d1[sex=="f"&region=="reg894"&Time==2075,by=.(agest),sum(.SD$sspval)]
          # ssp4d1[,by=.(region,sex,Time,agest,tob),sum(sspval)][is.na(V1)][,summary(V1)]#%>%spread(edu,sspval)
        dev.off()
        # ssp4d1[,setnames(.SD,"sspval","prop")]
        #done #?? graphic checking - remaining
        save(ssp4d1,file = "../data/education/ssp4d1 edu new.rda")
      }#wic3 CER-10%calc + GET
      
      
    # ssp4d1
    load(file = "../data/education/ssp4d1 edu new.rda")
      propdt[ssp4d1[,Time:=as.numeric(Time)],on = setdiff(id.cols,"tob"),prop:=sspval]  
        
    }  else if(newedutype == "sdg"){
      
      {
        # eapr5.tgt.col.185
        load(file="../data/education/eapr5.tgt.col.185.rda")
        # eaprs.tgt.col.185 #e1:e4
        load(file="../data/education/eaprs.tgt.col.185.rda")
        
        # eaprs.tgt.col.185[time==2050&sex=="f"&ccode==cntocc("Ethiopia")&scen=="tgt3"]%>%spread(edu,value)
        
        cumprop.tgt3.col.185 <- rbind(eaprs.tgt.col.185,eapr5.tgt.col.185)[scen=="tgt3"
        ][,cumprop:=cumprod(.SD$value),by=.(ccode,sex,time,agest)
        ][,.(ccode,sex,edu,tob,agest,time,cumprop)]
        
        # cumprop.tgt3.col.185[time==2050&sex=="f"&ccode==cntocc("Ethiopia")]%>%spread(edu,cumprop)
        
        # cumprop.tgt3.col.185[cumprop!=1&edu=="e1"]
        
        # cumprop.tgt3.col.185[ccode==231&sex=="m"&time==2050]
        iprop.final.col <- cumprop.tgt3.col.185%>%spread(edu,cumprop)%>%as.data.table()
        iprop.final.col <-copy(iprop.final.col)[,`:=`(e6=e5,e5=e4-e5,e4=e3-e4,e3=e2-e3,e2=e1-e2,e1=1-e1)]
        # iprop.final.col[ccode==231&sex=="m"&time==2050]
        
        #proxy
        edu.approx.agbl <- read.csv("../data/education/16countries_approximation AG BB.csv")
        setDT(edu.approx.agbl)
        #or simply use it from the last time
        # cntocc("Channel Islands") is excluded in this round
        
        
        #subset(edup,period==2020 & age == 15 &sex == "female",select = c(edu,cc356))
        edu.approx1 <- edu.approx.agbl[cc.nos.approx==1,.(country_code,cc.approx1)
        ][,setnames(.SD,c("cc.approx1"),c("ccode"))]
        edu.approx1<-  merge(edu.approx1[,ccode:=as.character(ccode)],iprop.final.col)
        edu.approx1<- edu.approx1[,ccode:=NULL][,setnames(.SD,c("country_code"),c("ccode"))][time>=2020]
        
        # head(edu.approx1)
        # with(edu.approx1,table(time))
        #edu.approx3
        edu.approx3 <- edu.approx.agbl[cc.nos.approx==3][,cc.nos.approx:=NULL]
        edu.approx3 <-melt(edu.approx3,id.vars = "country_code",value.name = "ccode")
        edu.approx3[,variable:=NULL]
        edu.approx3<-  merge(edu.approx3[,ccode:=as.character(ccode)],iprop.final.col)
        edu.approx3<- edu.approx3[,ccode:=NULL][,setnames(.SD,c("country_code"),c("ccode"))][time>=2020]
        edu.approx3<- melt(edu.approx3,id.vars = c("ccode","sex","tob","agest","time"),variable.name = "edu")
        edu.approx3 <- edu.approx3[,.(value=mean(value,na.rm=T)),by=.(ccode,sex,tob,agest,time,edu)]%>%spread(edu,value)%>%as.data.table()
        
        iprop.final.col.wproxy <- rbind(iprop.final.col,edu.approx1,edu.approx3)
        
        input.prop = copy(iprop.final.col.wproxy)[time>2020&agest%in%15:30
        ][,region:=paste0("reg",ccode)][,ccode:=NULL]
        input <- melt(input.prop,measure.vars = edunames,variable.name = "edu",value.name = "eduprop") 
        
        id.cols.here <- intersect(id.cols,names(propdt))
        propdt<-propdt[Time >2020][,prop:=-999][input[,setnames(.SD,"time","Time")],  prop:=eduprop, on = id.cols.here]
        
      }
        
    }else if(newedutype == "none"){
      #if sx and asfr are not edu specific then propdt does not matter
      # propdt[,by=.(region,Time,sex,agest),prop:= ]      
    }#newedutype
  }#newedu
# New Mig -----------------------------------------------------------------
if(newmig){
  if(newmigtype == "AGE"){
    #emrdt [updated in e04 and saved]
    #imrdt 
    
    # mig.a.Cratio
    load(file=paste(cc.dir.usescen,"mig.a.Cratio.RData",sep="/"))
  }
  
  if(grepl("emig1ukr",iscen_fullname)) {
     #read
     
     ireg.ukr = "reg804"
     
     migukr <- read.csv("../data/migration/Ukr_ref_longwar_Samir_WICPROJ_srj.csv") 
     setDT(migukr)
     
     migukr <- migukr[,Time:=Time-2][,X:=NULL][,region:=NULL
                    ][,region:= countrycode::countrycode(dest_cnt,origin="country.name",destination = "un")
                      ][,region:=paste0("reg",region)
                        ][,destregion:=NULL]
     region.migukr = migukr[,unique(region)]
     cols = c("refugees_em","refugees_em_familyRe","refugees_im_return")
     migukr[,(cols):=copy(migukr)[,..cols]/1000]

     migukr.toUkr = copy(migukr)[,by=.(Time,agest,sex),
                                .(ref_em=sum(refugees_em+refugees_em_familyRe),
                                  ref_im= sum(refugees_im_return))][,region:="reg804"]

     migukr <- migukr[is.na(refugees_em),refugees_em:=0]
     migukr <- migukr[is.na(refugees_em_familyRe),refugees_em_familyRe:=0]
     migukr <- migukr[is.na(refugees_im_return),refugees_im_return:=0]
     
     migukr[,by=Time,.(sum(refugees_em),sum(refugees_em_familyRe)
                                     ,sum(refugees_im_return))] #in numbers convert to 1000
     
     
     #refugee return rate
     ref.return <- migukr[,by=Time,.(sum(refugees_em_familyRe,refugees_im_return))] #in numbers convert to 1000
     ref.return <- ref.return[,]
     
     prop.ret = c(0,ref.return$V1[-1]/c(ref.return$V1[1],ref.return$V1[1]-cumsum(ref.return$V1[-1]))[-length(ref.return$V1)])
     ref.return <- ref.return[,propret:=prop.ret]
     
     # ref.return[,ref.return.rate := c(4500/(ref.return$V1[1]+4500),ref.return$V1[-1]/ref.return$V1[-length(ref.return$V1)])]
     # 
     # xx<-copy(migukr)[Time==2020,by=.(agest,sex),.(ref=sum(refugees_em),fam=sum(refugees_em_familyRe))]
     # xx%>%
     #   ggplot(aes(x=agest,y=ref,col=sex))+geom_line()+geom_line(aes(y=fam),linetype=2)
     # 
     # #refugees
     # xx<-copy(migukr)[Time==2020,by=.(agest,sex),.(ref=sum(refugees_em,refugees_em_familyRe))]
     # xx%>%ggplot(aes(x=agest,y=ref,col=sex))+geom_line()
     # 
     # yy<-copy(migukr)[,by=.(Time,agest,sex),.(ret=sum(refugees_im_return))]
     # yy%>%ggplot(aes(x=agest,y=ret,col=as.character(Time)))+geom_line()+facet_wrap(~sex)
   }  
 } #newmig
