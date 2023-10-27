# Projection 
icheck = T #to collect the sums at each stage (sx_propedu, mig,migC,migfinal,)

iage.u15corr = 25:35

#add
final = copy(popdt)[region%in%regions][,`:=`(births=0,pop1=0,emi=0,imm=0,edutran=0,deaths=0)]

#prepare a separate final DT for Ukraine refugees [2020 population is that of the countries where refugrees went]
#will use later
if(F&&grepl("1ukr",iscen_fullname)) final.ukr = copy(final)[,pop:=0][region%in%region.migukr&Time <=2050]

#check again
# if(round(popdt[pop>0&Time==2020,sum(pop)],0)!=7802030) print("World pop not matching line 65")
# popdt[pop==0&region=="reg196"&Time==2020,][order(agest)]

# iper = 2020 
ireg = regions
{ #to run for all regions at the same time
 for (iper in seq(initime,fintime-ts,ts)){
    print(iper)
    # stop()
# sx and edu -------------------------------------------------------------
     {
    #pop1
    final.temp = copy(final)[Time == iper&region%in%ireg]# chose an many regions you want
    #checks
    # final.temp[agest==15&region=="reg524"]
    # final.temp[,sum(pop),by=.(agest)]
    # final.temp[,sum(pop)]
    #this way sx is not saved as a separate file [births -5 is empty]
    # final.temp[is.nan(pop),pop:=0.001] #??check this out
    
    #for the first period, there are no refugees at the beginning
    #will use later
    if(F&&grepl("1ukr",iscen_fullname)&&iper>2020) {
      final.temp.ukr = copy(final.ukr)[Time == iper]# chose an many regions you want
      final.temp.ukr[,sum(pop),by=.(agest)]
      #this way sx is not saved as a separate file [births -5 is empty]
      final.temp.ukr[is.nan(pop),pop:=0.001] #??check this out
    }
    
    final.temp <- final.temp[sxdt,on=id.cols,`:=`(pop1=pop*sx,deaths=pop*(1-sx))]
    # final.temp[,sum(deaths)]
    # final.temp[is.na(pop1)]
    # final.temp[region=="reg108"&agest==0]
    # final.temp[pop1 < 0 ]
    
    # time and age as initime+5, corrected to match initime
    ieduprop <- copy(propdt)[Time==iper+5 & region%in%ireg][
      ,`:=`(Time=Time-5,agest=agest-5)][prop<0,prop:=0.001][
      ,.(edu=edu,prop=prop.table(prop)),by=setdiff(id.cols,"edu")]

    # xx <- final.temp[sex=="f"&agest==15,.(edu,pop)][,prop:=prop.table(.SD$pop)]
    
    #agr pop 
    pop1agr <- copy(final.temp)[agest%in%10:25][,.(pop1=sum(pop1)),by=setdiff(id.cols,"edu")]
    #merge
    ieduprop[pop1agr,on=setdiff(id.cols,"edu"),`:=`(pop1=pop1*prop)]
    #check (0.9999??)
    #bring pop1edu into pop1 (update)    
    final.temp[ieduprop,on=id.cols,`:=`(pop1=i.pop1,edutran = pop1-i.pop1)]
    vars = names(final.temp)[6:12]
    rm(ieduprop,pop1agr)
    
    #for the first period no refugees - so need to apply education transitions
    if(F&&grepl("1ukr",iscen_fullname)&&iper>2020) {
      final.temp.ukr <- final.temp.ukr[sxdt[region==ireg.ukr],on=setdiff(id.cols,"region"),`:=`(pop1=pop*sx,deaths=pop*(1-sx))]
      
      # final.temp[,sum(deaths)]
      # final.temp[is.na(pop1)]
      # final.temp[region=="reg108"&agest==0]
      
      # final.temp[pop1 < 0 ]
      #eduprop [new transitions]
      # time and age as initime+5, corrected to match initime
      ieduprop <- copy(propdt)[Time==iper+5 & region%in%ireg.ukr][
        ,`:=`(Time=Time-5,agest=agest-5)][prop<0,prop:=0.001][
          ,.(edu=edu,prop=prop.table(prop)),by=setdiff(id.cols,"edu")][,`:=`(Time=NULL,region=NULL)]
      
      # xx <- final.temp[sex=="f"&agest==15,.(edu,pop)][,prop:=prop.table(.SD$pop)]
      
      #agr pop 
      pop1agr <- copy(final.temp.ukr)[agest%in%10:25][,.(pop1=sum(pop1)),by=setdiff(id.cols,"edu")]
      #merge
      ieduprop = merge(pop1agr,ieduprop,allow.cartesian=TRUE)
      
      ieduprop[,`:=`(pop1=pop1*prop)]
      #check (0.9999??)
      #bring pop1edu into pop1 (update)    
      final.temp.ukr[ieduprop,on=id.cols,`:=`(pop1=i.pop1,edutran = pop1-i.pop1)]
      rm(ieduprop,pop1agr)
    }
    
    }

  #check (edutran = 0)
  if(icheck) {
    final.summ <-final.temp[,lapply(.SD,sum),.SDcols = vars,by=.(Time)][,pop1:=pop+births-deaths][,stage:="sx_eduprop"]
    # final.summ;print(final.summ)
    }

# migration ---------------------------------------------------------------

  #emig means use given education-specific migration rate
  #Here in any case - we need to adjust for scenarios + net0 
  if(grepl("emig",iscen_text)){
    print("Migration is running")
    {
      # icc = "reg108"
      # copy(emrdt)[agest>-5&agest<80&region==icc&Time==iper]%>%#spread(edu,imr)
      #   ggplot(aes(agest,emr,col=sex,shape = edu))+geom_line()+geom_point()
    final.temp[copy(emrdt)[,agest:=agest-5], #2020-2025, age at 2025, so to match with pop1 (with age at 2015)
               on=.(Time,region,sex,edu,agest),`:=`(emr=i.emr,emi=pop1*i.emr)]#end of the period (to be applied, age is not there)
    #check - different sex specific emr for AG but same emr0
    # emrdt[agest==50&region=="reg524"]
    # final.temp[agest>=0,sum(emi,na.rm=T)]
    
    #check for (0)
    # final.temp[agest==20&region=="reg840"]
    # final.temp[is.na(emi)][,unique(region)]
    # 
    # final.temp[agest==20&region=="reg524"]
    # imrdt[agest==20&region=="reg524"]

    #row #check for empty data.table
    final.temp[is.na(pop1)]
    
    final.temp.glo <- copy(final.temp)[,.(pop1=sum(.SD$pop1)),by=.(sex,edu,agest)]
    
    #check
    final.temp[,sum(pop1)]#7.5187billion [newborns no added yet]
    final.temp.glo[agest==20]
    
    final.temp[final.temp.glo,on=.(sex,edu,agest),row := i.pop1 - pop1]
    final.temp[copy(imrdt)[,agest:=agest-5], #2015-2020, age at 2020, so to match with pop1 (with age at 2015)
               on=.(Time,region,sex,edu,agest),`:=`(imr=i.imr,imm=row*i.imr)]
    #check
    if(icheck) { 
      final.summX <-final.temp[,lapply(.SD,sum),.SDcols = vars,by=.(Time)][,pop1:=pop+births-deaths][,stage:="mig"]
      final.summ = rbind(final.summ,final.summX)
    }
    
    
    
    #check
    final.temp[,sum(imm)]#V12?96997.12  V1199.217mil
    # xx0 <- copy(final.temp)[region=="reg840"&agest==20]
    #match the overall migration to country rate
}#aseC rate
    
    ################################################################################
    #check
    # icc = "reg108"
    # copy(final.temp)[agest>-5&region==icc,by=.(agest,sex),.(row=sum(row))]%>%
    #   ggplot(aes(agest,row,col=sex))+geom_line()
    # 
    # copy(final.temp)[agest>-5&region==icc,by=.(agest,sex),.(imm=sum(imm))]%>%
    #   ggplot(aes(agest,imm,col=sex))+geom_line()
    # 
    # copy(imrdt)[agest>-5&agest<80&region==icc&Time==iper]%>%#spread(edu,imr)
    #   ggplot(aes(agest,imr,col=sex,shape = edu))+geom_line()+geom_point()
    # 
    # 
    # copy(emrdt)[agest==0&region==icc&Time==iper]
    
    ################################################################################
    
    
    {
      if(iMigcorr_u15){#rate for 25-35 f
        migrate.U15fcorr <- final.temp[sex=="f"&agest%in%iage.u15corr,by=.(region),
                                       .(emr=.SD[,sum(emi)/sum(pop)],imr=.SD[,sum(imm)/sum(row)])]
        mig.a.U15 <- copy(mig.a.Cratio)[migrate.U15fcorr,on=.(region),
                                        `:=`(emr=i.emr*emiCratio,imr=i.imr*immCratio)]
        #update U15 - no edu or sex difference
        # copy(emrdt)[agest==0&region==icc&Time==iper]
        emrdt <- emrdt[mig.a.U15[,Time:=iper],on=.(region,Time,agest),emr:=i.emr]
        imrdt <- imrdt[mig.a.U15[,Time:=iper],on=.(region,Time,agest),imr:=i.imr]
      }
      # imrdt[region==icc&agest==5&Time==iper]
      # copy(emrdt)[agest==5&region==icc&Time==iper]
      
      #recalculate the migration [<15 will change]
      final.temp[copy(emrdt)[,agest:=agest-5], #2020-2025, age at 2025, so to match with pop1 (with age at 2015)
                 on=.(Time,region,sex,edu,agest),`:=`(emr1=i.emr,emi=pop1*i.emr)]#end of the period (to be applied, age is not there)
      final.temp[copy(imrdt)[,agest:=agest-5], #2015-2020, age at 2020, so to match with pop1 (with age at 2015)
                 on=.(Time,region,sex,edu,agest),`:=`(imr1=i.imr,imm=row*i.imr)]
      
      # test2 <- final.temp[region==icc&agest%in%0:10,by=.(region),.(emi=.SD[,sum(emi)],
      #                                                  imm=.SD[,sum(imm)])]
      
      # final.temp[region==icc&agest%in%0]
      # test2 <- copy(final.temp)[agest%in%0:75,by=.(region,sex,agest),
      #                     .(emr=.SD[,sum(emi)/sum(pop)],imr=.SD[,sum(imm)/sum(row)])]
      # ggy <- copy(test2)[agest>-5&agest<75&region==icc]%>%#spread(edu,imr)
      #   ggplot(aes(agest,imr,col=sex))+geom_line()+geom_point()+ggtitle("after")
      
    } #U15 age specific rate
    final.summ
    
    
    if(exists("corrmigC")){#WITHOUT OUTLIERS
      
      final.temp[copy(emrdt)[,agest:=agest-5], #2015-2020, age at 2020, so to match with pop1 (with age at 2015)
                 on=.(region,sex,edu,agest),emi0:=pop1*emr0]#end of the period (to be applied, age is not there)
      #age120
      final.temp[is.na(emi0),emi0:=0] #to make it nonzero
      
      final.temp[agest>=0,sum(emi0,na.rm = T)]#
      final.temp[agest>=0,sum(emi,na.rm = T)]#
      
      final.temp[copy(imrdt)[,agest:=agest-5], #2015-2020, age at 2020, so to match with pop1 (with age at 2015)
                 on=.(region,sex,edu,agest),imm0:=row*imr0]
      final.temp[is.na(imm0),imm0:=0] #to make it nonzero
      
      final.temp[agest>=0,sum(imm0,na.rm = T)]#
      final.temp[agest>=0,sum(imm,na.rm = T)]#
      
      # final.temp[region=="reg175",sum(imm)]
      # imrdt[region=="reg175",sum(imr)]
      # final.temp[agest>=0&region=="reg524",sum(imm,na.rm = T)]#251[2095] k per five years  
      # final.temp[agest>=0&region=="reg524",sum(emi,na.rm = T)]#810[2095] k per five years  
      #matching to country flows
      final.temp.corrmigC <- copy(final.temp)[,by=.(region),
                                    .(emicorrC=sum(emi0,na.rm = T)/sum(emi,na.rm = T),
                                      immcorrC=sum(imm0,na.rm = T)/sum(imm,na.rm = T))]

      #check correction factors
     
      #correction
      final.temp <- final.temp[final.temp.corrmigC,on=.(region),
                               `:=`(emi=emi*emicorrC,imm=imm*immcorrC)]
      
      
      final.temp[agest>=0,sum(emi0,na.rm = T)]#new78.065 old66.15 V1166.276 million per five years
      final.temp[agest>=0,sum(emi,na.rm = T)]#new78.065
      
      final.temp[copy(imrdt)[,agest:=agest-5], #2015-2020, age at 2020, so to match with pop1 (with age at 2015)
                 on=.(region,sex,edu,agest),imm0:=row*imr0]
      final.temp[is.na(imm0),imm0:=0] #to make it nonzero
      
      final.temp[agest>=0,sum(imm0,na.rm = T)]#new77.147old65.83 V111765.959 million per five years  
      final.temp[agest>=0,sum(imm,na.rm = T)]#new77.147 V1193.43 million per five years  
      
      
      # final.temp.corrmigC[order(emicorrC)]
      # final.temp.corrmigC[order(immcorrC)]
      
      # cctoname(316) #Guam
      
      # final.temp[agest>=0&region=="reg524",sum(imm,na.rm = T)]#142[2060] k per five years  
      # final.temp[agest>=0&region=="reg524",sum(emi,na.rm = T)]#606[2060] k per five years  

      final.temp[is.na(imm)]
      final.temp[is.na(emi)]#no NA
    
      # final.temp[agest>=0,sum(emi,na.rm = T)]#95.42 million per five years - same ase emi0    
      # final.temp[agest>=0,sum(imm,na.rm = T)]#93.814 million per five years - same ase imm0 
      if(icheck) { final.summX <-final.temp[agest>-5][,lapply(.SD,sum),.SDcols = vars,by=.(Time)][,pop1:=pop+births-deaths][,stage:="migC"]
      final.summ = rbind(final.summ,final.summX)
      } 
      
      
    } #Country rate
    
    {
      
    corr.imm <- copy(final.temp)[,.(corr = sum(emi)/sum(imm)),by=.(sex,edu,agest)][is.nan(corr),corr := 1]
    corr.imm[is.na(corr)]
    corr.imm[corr!=1]
    
    final.temp[corr.imm,on=.(sex,edu,agest),imm:=imm*corr]
    final.temp[,sum(imm)]#66276.5mil (why less??)
    final.temp[,sum(emi)]#66276.5
    }#imm = emi [global]
    
    
    if(exists("net0")&&iper>=net0){#correct the emi0 and imm0
      
      # STEPS
      
      #1 RC_rate_csae_50ratio [Dilek!!]
      #2 missing countries
      #3 rate_c
      #4 U15 correction (linked with mother's avgrate 2539 mig.a.Cratio)
      #5 TAiwan proxied by Singapore
      #6 emr*pop1          [end-age CHECK]
      #7 imr*row_pop1
      #8 #4 - correct for CWR  0-4, 5-9, 10-14 (2015) and compare with CWR of iper
      #9  rates adjusted to C
      #10  glo_imm by sae is adjusted to glo_emi sae [Check Nicola]
      #11   net0 by 2100 - average of imm and emi  
      
      if(F){#country flow net0
        final.temp.cnt <- final.temp[agest>=0,by=.(region),
                                   .(imm0=sum(.SD$imm,na.rm = T),emi0=sum(.SD$emi,na.rm = T))]

        final.temp.cnt<-final.temp.cnt[,netcorr:=(imm0-emi0)/((2100-net0)/5)*(iper/5-net0/5 + 1)
        ][,imm0c:=imm0-netcorr/2][,emi0c:=emi0+netcorr/2][,imm0c:=imm0c/imm0][,emi0c:=emi0c/emi0]
        
        #check - before convergence
        # final.temp[agest>=0&region=="reg524",sum(imm0,na.rm = T)]#145[2060] k per five years  
        # final.temp[agest>=0&region=="reg524",sum(emi0,na.rm = T)]#606[2060] k per five years  
        
        final.temp <- final.temp[final.temp.cnt,on=.(region),`:=`(emi=emi*emi0c,imm=imm*imm0c)]
        
        #check
        final.temp.cnt <- final.temp[agest>=0,by=.(region),
                                     .(imm0=sum(.SD$imm,na.rm = T),emi0=sum(.SD$emi,na.rm = T))]
      } else {#edu specific flows net0
        final.temp.cnt <- final.temp[agest>=0,by=.(region,edu),
                                     .(imm0=sum(.SD$imm,na.rm = T),emi0=sum(.SD$emi,na.rm = T))]
        
        final.temp.cnt<-final.temp.cnt[,netcorr:=(imm0-emi0)/((2100-net0)/5)*(iper/5-net0/5 + 1)
        ][,imm0c:=imm0-netcorr/2][,emi0c:=emi0+netcorr/2][,imm0c:=imm0c/imm0][,emi0c:=emi0c/emi0]
        
        #check - before convergence
        # final.temp[agest>=0&region=="reg524",sum(imm0,na.rm = T)]#145[2060] k per five years  
        # final.temp[agest>=0&region=="reg524",sum(emi0,na.rm = T)]#606[2060] k per five years  
        
        final.temp <- final.temp[final.temp.cnt,on=.(region,edu),`:=`(emi=emi*emi0c,imm=imm*imm0c)]
        # final.temp[agest>=0&region=="reg524",sum(imm,na.rm = T)]#171[2060] k per five years  
        # final.temp[agest>=0&region=="reg524",sum(emi,na.rm = T)]#577[2060] k per five years  
        
        #check
        final.temp.cnt <- final.temp[agest>=0,by=.(region),
                                     .(imm0=sum(.SD$imm,na.rm = T),emi0=sum(.SD$emi,na.rm = T))]
        
      }
      
      if(icheck) {
        final.summX <-final.temp[agest>-5][,lapply(.SD,sum),.SDcols = vars,by=.(Time)
        ][,pop1:=pop+births-deaths][,stage:="mig_glocorr"]
        final.summ = rbind(final.summ,final.summX)
      }
      
      #after convergence
      # final.temp[agest>=0&region=="reg524",sum(imm0,na.rm = T)]#549[2095] k per five years  
      # final.temp[agest>=0&region=="reg524",sum(emi0,na.rm = T)]#549[2095] k per five years 
      # stop("532")
      corr.imm.net0 <- copy(final.temp)[,.(corr = sum(emi)/sum(imm)),by=.(sex,edu,agest)][is.nan(corr),corr := 1]
      corr.imm.net0[is.na(corr)]
      corr.imm.net0[corr!=1]
      
      final.temp[corr.imm.net0,on=.(sex,edu,agest),imm:=imm*corr]
      final.temp[,sum(imm)]#66276.5mil (why less??)
      final.temp[,sum(emi)]#66276.5
      
    }#net0

    
    
    # copy(final.temp)[region=="reg524",.(sum(emi),sum(imm))]
    # xx1 <- copy(final.temp)[region=="reg840"&agest==20]
    # corr.imm%>%spread(edu,corr)
    # copy(final.temp)[region=="reg840",.(sum(imm),sum(emi))] #emigration is so low!!
    # copy(final.temp)[,.(sum(imm),sum(emi))] #emigration is so low!!
    
    vars.del = c("emr","row","imr","emr1","imr1","emi0","imm0")
    final.temp[,(vars.del):=NULL]
    
    final.temp[,pop1:=pop1-emi+imm]
    
    #Refugee corrections for Ukraine until 2045-2050
    if(F&&iper <= 2045&&grepl("1ukr",iscen_text)){
      
      #place holder for ukraine refugees
      # if(iper == 2020) final.temp[,popukr:=0] 
       # stop("478")
      
      #save ratio pop to pop1
      
      #region = destination here
      #em and im is for Ukraine so for others it would be opposite
      
      # •	In Ukraine's refugee scenario [9mil], 50% of refugees will return [4.5mil] after the war's end by 2025.
      # •	Fam_renunion… 1.17mil

      # migukr[,by=Time,.(sum(refugees_em),sum(refugees_em_familyRe),sum(refugees_im_return))] #in numbers convert to 1000
      
      # •	Out of the remaining refugees, 75% return [37,500, without mortality + fertility] within 30 years [… , …  , …] after the war and 25% [12,500] will never return.
      # •	We maintain the refugee population separately for each country (how many?).
      # •	For the returnees, we will apply the fertility and mortality of Ukraine.
      # •	In the second period, 2025-2030, more will join for family reunions [what %]. They will be counted as refugees [what if they join the 25% and never be counted as refugees] from the second period.
      # •	Out of the remaining refugees (remaining 50% + those who emigrated for family reunions), 75% will start to come back to Ukraine within 30 years.
                                       
      
      #need to distribute by educational distribution of Ukrainian??
      
      #refugee leaving (only in 2020)
      #first step
      if(iper == 2020) {
         
           #survival already applied (taking mid-year)
            #to destination
        final.temp.ukr = copy(final.ukr)[Time == iper]# empty for 2020
        
         
        ukr.eduprop <- copy(final.temp)[region==ireg.ukr][,by=.(sex,agest),prop1 := prop.table(.SD$pop1)
                                        ][!(is.nan(prop1))][,.(Time,sex,edu,agest,prop1)]
        final.ukr.refu <- merge(ukr.eduprop,
            migukr)[,`:=`(ref.im = prop1*(refugees_em_familyRe+ refugees_em),
                          ref.em = prop1*refugees_im_return)][,.(region,Time,sex,agest,edu,ref.em,ref.im)]
        final.ukr.refu[region=="reg40"&agest==20]
        # final.ukr.refu[,.(sum(ref.em),sum(ref.im))]
        
        #update popukr in destination
        final.temp.ukr[final.ukr.refu,on=id.cols,pop1:=ref.im-ref.em]
        final.temp.ukr[,sum(pop1)] #5671k outside ukr
            
        #Ukraine pop1 change
        migukr.toUkr = copy(final.ukr.refu)[,region:=ireg.ukr][,by=id.cols,
                                        .(ref.im=sum(ref.em),
                                          ref.em= sum(ref.im))]
            # migukr.toUkr[,.(sum(ref.im),sum(ref.em))]
            # final.temp[region=="reg804"&agest==20]
        final.temp[migukr.toUkr,on=id.cols,`:=`(pop1=pop1+ref.im-ref.em)]
        
        # final.temp[region=="reg804"&agest==20]
        # final.temp[region=="reg40"&agest==20]
        # final.temp[,sum(popukr)] #5671

      } else {
        
        #update popukr in destination
        final.temp.ukr[ref.return,on=.(Time),`:=`(pop1=pop1*(1-propret),emi=pop1*propret)]
        final.temp.ukr[,sum(pop1)] #5671k outside ukr
        
        #Ukraine pop1 change
        migukr.toUkr = copy(final.temp.ukr)[,region:=ireg.ukr][,by=id.cols,
                                                               .(ref.im=sum(emi))]
        # migukr.toUkr[,.(sum(ref.im),sum(ref.em))]
        # final.temp[region=="reg804"&agest==20]
        final.temp[migukr.toUkr,on=id.cols,`:=`(pop1=pop1+ref.im)]
        
      }
      
      
         
      if(icheck) {
        final.summX <-final.temp[agest>-5][,lapply(.SD,sum),.SDcols = vars,by=.(Time)
        ][,stage:="mig_ukrcorr_x"]
        final.summ = rbind(final.summ,final.summX)
      }
      
      } 

    # copy(final.temp)[,.(corr = sum(emi)/sum(imm))]
    if(icheck) {
      final.summX <-final.temp[agest>-5][,lapply(.SD,sum),.SDcols = vars,by=.(Time)
                                                    ][,stage:="migfinal"]
      final.summ = rbind(final.summ,final.summX)
    }
  }#emig   
 
# final.summ
# births ------------------------------------------------------------------
 {#efert = F #run with given asfrs 
          
      birthsx <- final.temp[sex=="f"][,`:=`(emi=NULL,imm=NULL,edutran=NULL,sex=NULL)][,#this are still empty
              `:=`(popavg=.5*(.SD$pop+shift(.SD$pop1,1,NA,"lag"))),by = .(region,edu)] [,
              `:=`(pop1=NULL,pop=NULL,deaths=NULL)][agest%in%15:49,][asfrdt,#age 15 at the start of iper
               on=setdiff(id.cols,"sex"),#get the asfr 
              `:=`(births=(popavg*ts)*(i.asfr/1000))][#calculate births
                ,popavg:=NULL][srbdt, #bring srb
               on=.(region,Time),`:=`(m=births*i.srb/(1+i.srb),f=births*1/(1+i.srb))][,births:=NULL]
      
      birthsx <- melt(birthsx,id=setdiff(id.cols,"sex"),variable.name = "sex",value.name = "births")
      final.temp[birthsx,on=id.cols,`:=`(births=i.births)]
      
      #total births (by edu and sex) will be added to the -5 at final.temp initime
      birthstot = birthsx[,.(pop=sum(births)),by=setdiff(id.cols,"agest")][,agest:=-5]
      #update births
      final.temp[copy(birthstot),on=id.cols,`:=`(pop=i.pop)]
      
     if(F&&iper<=2045 && grepl("1ukr",iscen_text)){ #ukraine
          # stop("line 830 1ukr - check for birthstot update")
         if(iper == 2020){
          
           
          #ratio to generate popf(2020) value for a given pop1f - to get the exposure      
          ratio_pop_pop1 <- copy(final.temp)[sex=="f"&region==ireg.ukr
                                             ][,ratio_pop_pop1:=pop/pop1 
                                               ][!(is.nan(ratio_pop_pop1))]
          
          # final.temp[agest==20]
          
          birthsx.ukr <- copy(final.temp.ukr)[sex=="f"&agest%in%10:49
              ][,.(region,Time,edu,agest,births,pop,pop1)
                ][ratio_pop_pop1,on=.(Time,edu,agest),pop:=pop1*ratio_pop_pop1
            ][,`:=`(popavg=.5*(.SD$pop+shift(.SD$pop1,1,NA,"lag"))),by = .(region,edu)
          ][,`:=`(pop1=NULL,pop=NULL)][agest%in%15:49,
          ][asfrdt[region==ireg.ukr],#age 15 at the start of iper
            on=setdiff(id.cols,c("sex","region")),#get the asfr 
            `:=`(births=(popavg*ts)*(i.asfr/1000))][#calculate births
              ,popavg:=NULL][srbdt[region==ireg.ukr], #bring srb
                             on=.(Time),`:=`(m=births*i.srb/(1+i.srb),f=births*1/(1+i.srb))][,births:=NULL]
         } else {
           birthsx.ukr <- copy(final.temp.ukr)[sex=="f"&agest%in%10:49
           ][,.(region,Time,edu,agest,births,pop,pop1)
           ][,`:=`(popavg=.5*(.SD$pop+shift(.SD$pop1,1,NA,"lag"))),by = .(region,edu)
           ][,`:=`(pop1=NULL,pop=NULL)][agest%in%15:49,
           ][asfrdt[region==ireg.ukr],#age 15 at the start of iper
             on=setdiff(id.cols,c("sex","region")),#get the asfr 
             `:=`(births=(popavg*ts)*(i.asfr/1000))][#calculate births
               ,popavg:=NULL][srbdt[region==ireg.ukr], #bring srb
                              on=.(Time),`:=`(m=births*i.srb/(1+i.srb),f=births*1/(1+i.srb))][,births:=NULL]
           }
          
          birthsx.ukr <- melt(birthsx.ukr,id=setdiff(id.cols,"sex"),variable.name = "sex",value.name = "births")
          #add the births to ukr mothers to the births column
          final.temp.ukr[birthsx.ukr,on=id.cols,`:=`(births=i.births)]
          
          #total births (by edu and sex) will be added to the -5 at final.temp initime
          birthstot.ukr = birthsx.ukr[,.(pop=sum(births)),by=setdiff(id.cols,"agest")][,agest:=-5]
          #update births 
          final.temp.ukr[copy(birthstot.ukr),on=id.cols,`:=`(pop=i.pop)]
          # final.temp[region=="reg40"&agest==-5]
          #birthsx.ukr[,sum(births)]#347
      }
   
    }#end efert

  # final.summ <-final.temp[region=="reg356",lapply(.SD,sum),.SDcols = vars,by=.(Time)][,pop1:=pop+births-pop1]
  if(icheck) {final.summX <-final.temp[,lapply(.SD,sum),.SDcols = vars,by=.(Time)][,pop1:=pop+births-deaths][,stage:="births"]
  final.summ = rbind(final.summ,final.summX)
 } 

  #project newborns
  birthstot <- birthstot[sxdt,on=id.cols,`:=`(pop1=pop*sx,deaths=pop*(1-sx))]
  # sxdt[region==ireg&Time==iper]
  
  #add pop1 deaths 
  final.temp[copy(birthstot),on=id.cols,`:=`(pop1=i.pop1,deaths=i.deaths)]
  
  #check for negative numbers
  if(nrow(final.temp[pop<0,])>0) print(final.temp[pop<0,])
  measure.vars = c("pop","births","pop1","emi","imm","deaths")
  final.temp <-final.temp[,(measure.vars):=lapply(.SD, function(icol) {icol[icol<0]<-0; icol}),.SDcols = measure.vars]
  #check
  if(nrow(final.temp[pop<0,])>0) stop("Some negative pop numbers")

  #add in final total births initime age -5
  final[copy(birthstot),on=id.cols,`:=`(pop=i.pop)] #pop1 will be updated later
  #Update the final with pop1, births, mig, edu transition
  final[copy(final.temp)[,`:=`(pop=NULL,pop1=NULL)],on=id.cols,
        `:=`(deaths=i.deaths,births=i.births,imm=i.imm,emi=i.emi,edutran=i.edutran)] #new
  #End of the period age and Time
  #prepare for the next year [5+]
  final.temp.end<-copy(final.temp)[,`:=`(Time=Time+ts,agest=agest+ts,pop=pop1,pop1=NULL)]
  final.temp.end[agest>=120,agest:=120][,pop:=sum(pop),by=id.cols]

  #add end of the year population to the final (t+5)
  final[final.temp.end,on=id.cols,`:=`(pop = i.pop)]
  
  if(F&&iper<=2045 && grepl("1ukr",iscen_text)){ #ukraine
    birthstot.ukr <- birthstot.ukr[sxdt[region==ireg.ukr],on=setdiff(id.cols,"region"),`:=`(pop1=pop*sx,deaths=pop*(1-sx))]
    final.temp.ukr[copy(birthstot.ukr),on=id.cols,`:=`(pop1=i.pop1,deaths=i.deaths)]
    final.ukr[copy(birthstot.ukr),on=id.cols,`:=`(pop=i.pop)] #pop1 will be updated later
    final.ukr[copy(final.temp.ukr)[,`:=`(pop=NULL,pop1=NULL)],on=id.cols,
          `:=`(deaths=i.deaths,births=i.births,imm=i.imm,emi=i.emi,edutran=i.edutran)] #new
    
    #End of the period age and Time
    #prepare for the next year [5+]
    final.temp.ukr.end<-copy(final.temp.ukr)[,`:=`(Time=Time+ts,agest=agest+ts,pop=pop1,pop1=NULL)]
    final.temp.ukr.end[agest>=120,agest:=120][,pop:=sum(pop),by=id.cols]
    
    #add end of the year population to the final
    # stop(1005)
    final.ukr[final.temp.ukr.end,on=id.cols,`:=`(pop = i.pop)]
    
    # final.temp.ukr[,table(agest)]
    # final.ukr[Time==iper+5&region=="reg40"&agest==40]
    
    if(iper == 2045) { #adding remaining ukr pop to 
      print("end of 2045-2050 added ukr refugees to the general pop")
      final[final.temp.ukr.end,on=id.cols,`:=`(pop = pop + i.pop)]
    }
    
  }

  if(icheck) {
    final.summX <-final.temp[,lapply(.SD,sum),.SDcols = vars,by=.(Time)][,pop:=pop-births][,pop1:=pop+births-deaths][,stage:="nb_deaths"]
    final.summ = rbind(final.summ,final.summX)
  }
  
  if(icheck) final.summX <-final[Time==2020][,lapply(.SD,sum),.SDcols = vars,by=.(Time)][,pop:=pop-births][,pop1:=pop+births-deaths][,stage:="births"]
  
  }#loop of iper
}#PROJ

{#SAVE RESULTS

final <- final[pop==-999,pop:=-0.00001]
save(final,file=paste(path_scen,"res_",iscen_fullname,as.numeric(Sys.time()),".RData",sep=""))

vars = setdiff(names(final),id.cols)
final.summ.temp <-final[agest>-5&pop>=0,lapply(.SD,sum,na.rm=T),.SDcols = vars,by=.(Time)][
  ,`:=`(pop=pop)][,pop1:=NULL] #births are already in 'pop'

# DT::datatable(data = round(final.summ.temp,0))

if(final.summ.temp[,sum(emi)]==0){
  print(round(final.summ.temp[,edutran:=NULL][,emi:=NULL][,imm:=NULL],0))
} else {
  print(round(final.summ.temp[,edutran:=NULL],0))
}

if(F&&grepl("1ukr",iscen_text)){ #ukraine
  final.ukr <- final.ukr[pop==-999,pop:=-0.00001]#for year 2050 births
  save(final.ukr,file=paste(path_scen,"res_ukr_",iscen_fullname,as.numeric(Sys.time()),".RData",sep=""))
  if(username=="kc") save(final,file=paste(pdrive_path_scen,"res_ukr",iscen_fullname,as.numeric(Sys.time()),".RData",sep=""))
  
  save(final,file=paste(path_scen,"res_woukr_",iscen_fullname,as.numeric(Sys.time()),".RData",sep=""))
  # stop("1287")
  final[Time==2025&agest==25&region=="reg40"]
  final.wukr <- copy(final)[final.ukr[Time<2050],on=id.cols,`:=`(pop=pop+i.pop,
                                  births=births+i.births,
                                  pop1=pop1+i.pop1,
                                  edutran=edutran+i.edutran,
                                  deaths=deaths+i.deaths)]
  final.wukr[Time==2025&agest==25&region=="reg40"]
  
  
  # final.ukr[Time==2020&agest==20]
  save(final.wukr,file=paste(path_scen,"res_",iscen_fullname,as.numeric(Sys.time()),".RData",sep=""))
  
  # [region=="reg108"]
  final.summ.temp <-final.ukr[agest>-5&pop>=0,lapply(.SD,sum,na.rm=T),.SDcols = vars,by=.(Time)][
    ,`:=`(pop=pop)][,pop1:=NULL] #births are already in 'pop'
  print("get absolute edu transitions")
  DT::datatable(data = round(final.summ.temp,0))
  if(final.summ.temp[,sum(emi)]==0){
    print(round(final.summ.temp[,edutran:=NULL][,emi:=NULL][,imm:=NULL],0))
  } else {
    print(round(final.summ.temp[,edutran:=NULL],0))
  }
}

#save dttosave
for(ifile in dttosave) {
  xxx<-get(ifile)
  save(xxx,file=paste(path_scen,ifile,".RData",sep=""))
}  

}

# See "Report WIC3.Rmd"
