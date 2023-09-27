### BC health model
# (c) Dylan G. Clark
# 2022



library(shiny)
library(chron)
library(lubridate)
library(timeSeries)
library(timeDate)
library(tseries)
library(zyp)
library(ggplot2)
library(data.table)
library(plyr)
library(dplyr)
library(zoo)
library(shinycssloaders)
library(shinydashboard)
library(extrafont)
library(showtext)
library(scales)
library(packrat)
library(rsconnect)
library(tidyr)
library(htmlwidgets)
library(htmltools)
library(ggridges)
library(shinyWidgets)
library(rtrend)
library(reactable)
library(reactablefmtr)
library(viridis)
library(stringr)
library(rgdal)
library(cowplot)
library(DT)
library(janitor)

library(simmer)
library(simmer.plot)
library(TruncatedNormal)
library(writexl)
library(gridExtra)
library(parallel)
library(progress)






##################################################### Presets
#----
###################   Load datasets

# setwd("D:/R/BC/Health/HealthModel/HealthModel/")

levelMonths<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
levelOrderDecade<-(c(2020:2090))
GeoS<-c("Population centres","Health regions","Province")
Sectors<-c("Health system", "Infrastructure","Food","Labour")

warningCriteria<-read.csv("data/WarningCriteria.csv",header=T)
PC<-read.csv("data/PopCentres.csv", header=T)
PopC<-as.list(unique(PC$PCNAME))
HR<-as.list(unique(PC$HA_Name))


##################  Additional objects

font_add(family="Montserrat Medium", regular="www/font/Montserrat-Regular.ttf")
font_add(family="Oswald", "www/font/Oswald-Regular.ttf")
font_add(family="Oswald Light", "www/font/Oswald-Light.ttf")
showtext_auto()


# ----

######################################################################################################## Shiny server (data visualization)

shinyServer(function(input, output,session) {
  

  ################ Health simulation modeler
  #######################################################################

  
  ################################## Generate health buttons
  ######
  
  output$inResource <- renderUI({
    selectInput("Resource", "Select resource for analysis",
                choices<-c("ED bed","quarter ED nurse","tenth ED doc","Triage nurse","ICU bed","Non-ICU bed","Fast Track bed","911Dispatcher","fire","Ambulance","police","coroner","Waiting room chair"),
                selected="ED Bed")
  })
  
    output$IHA<-renderUI({radioButtons(inputId = "HA",label="Select which health authority this is for",choices=(c("Vancouver Coastal Health"=0,"Vancouver Island Health" = 1,"Fraser Health"=2,"Interior Health"=3,"Northern Health"=4)),inline=F)})
    output$IHWInput<-renderUI({radioButtons(inputId = "HWInput",label="Select how you want to model a heatwave",choices=(c("No heatwaves"=0,"Default heatwave (2021 event) *use this for historic run*" = 1,"Upload a timeseries"=2)),selected=1,inline=F)})
    output$IHWUpload<-renderUI({
            req(input$HWInput)
            if(input$HWInput==2){fileInput(inputId ="HWTimeline",label="Upload heatwave timeseries csv",multiple = F,accept=".csv")
              }else{}})
  
    output$InEDBeds<-renderUI({numericInput(inputId = "nEDBeds",label="Number of ED beds",value=ifelse(input$CatchmentPop==50,14,ifelse(input$CatchmentPop==200,58,ifelse(input$CatchmentPop==100,30,(ceiling(input$CatchmentPop*0.32))))))})
    output$InEDNurse<-renderUI({numericInput(inputId = "nEDNurse",label="Number of ED nurses",value=ifelse(input$CatchmentPop==100,6,ifelse(input$CatchmentPop==200,14,(round(input$CatchmentPop*0.08,0)))))})
    output$Indoc<-renderUI({numericInput(inputId = "ndoc",label="Number of ED doctors",value=ifelse(input$CatchmentPop==200,5,ifelse(input$CatchmentPop>50,(round(input$CatchmentPop*0.03,0)),1)))})
    output$InTriageNurse<-renderUI({numericInput(inputId = "nTriageNurse",label="Number of ED triage nurses",value=(round(input$CatchmentPop*0.0201,0)))})
        ## 21 ICU beds per 100,000 and assuming that 30% of beds are occupied by non-ed admissions
    output$InICUbed<-renderUI({numericInput(inputId = "nICUbed",label="Number of ICU beds",value=ifelse(input$CatchmentPop==200,10,ifelse(input$CatchmentPop==100,7,(round(input$CatchmentPop*0.07,0)))))})
        ## 296 non-ICU acute beds per 100,000 and assuming that 50% of beds are occupied by non-ed admissions and 15% are long-term use
    output$InNonICUbed<-renderUI({numericInput(inputId = "nNonICUbed",label="Number of non-ICU beds",value=ifelse(input$CatchmentPop==50,25,ifelse(input$CatchmentPop==100,64,ifelse(input$CatchmentPop==200,118,(round(input$CatchmentPop*0.613,0))))))})   
    output$InFTBeds<-renderUI({numericInput(inputId = "nFTBeds",label="Number of fast-track beds",value=ifelse(input$CatchmentPop==50,0,ifelse(input$CatchmentPop==200,15,(round(input$CatchmentPop*0.06,0)))))})
    output$InFTdocs<-renderUI({numericInput(inputId = "nFTdocs",label="Number of fast-track doctors",value=round(input$CatchmentPop*0.01,0))})
    output$InFTnurse<-renderUI({numericInput(inputId = "nFTnurse",label="Number of fast-track nurses",value=ifelse(input$CatchmentPop==50,0,ifelse(input$CatchmentPop==100,2,(round(input$CatchmentPop*0.02,0)))))})
    output$InPolice<-renderUI({numericInput(inputId = "nPolice",label="Number of police units",value=(round(input$CatchmentPop*0.05,0)))})
    output$InFire<-renderUI({numericInput(inputId = "nFire",label="Number of fire medical units",value=(round(input$CatchmentPop*0.02,0)))})
    output$InAmbulance<-renderUI({numericInput(inputId = "nAmbulance",label="Number of Ambulances",value=(round(input$CatchmentPop*0.04,0)))})
    output$InDispatcher<-renderUI({numericInput(inputId = "nDispatcher",label="Number of 911 dispatchers",value=(round(input$CatchmentPop*0.03,0)))})
    output$InCoroner<-renderUI({numericInput(inputId = "nCoroner",label="Number of coroners",value=ifelse((round(input$CatchmentPop*0.009,0))>0,(round(input$CatchmentPop*0.009,0)),1))})
    
    #output$IEDstay<-renderUI({sliderInput(inputId = "EDstay",label="Average ED time for treatment (minutes)",min=60,max=500,value=420,round=TRUE)})
    #output$IMedstay<-renderUI({sliderInput(inputId = "Medstay",label="Average non-ICU time for treatment (minutes)",min=1440,max=14400,value=2320,round=TRUE)})
    #output$IICUstay<-renderUI({sliderInput(inputId = "ICUstay",label="Average ICU time for treatment (days)",min=3,max=15,value=7,round=TRUE)})
    #output$IPostICUstay<-renderUI({sliderInput(inputId = "PostICUstay",label="Average non-ICU time for treatment after ICU discharge (minutes)",min=2880,max=28800,value=5760,round=TRUE)})
    output$IAmbRespT<-renderUI({sliderInput(inputId = "AmbRespT",label="Average ambulance response time once available (minutes)",min=5,max=30,value=15,round=TRUE)})
    output$IFireRespT<-renderUI({ sliderInput(inputId = "FireRespT",label="Average fire response time once available (minutes)",min=3,max=15,value=7,round=TRUE)})
    output$ITransToHospSIREN<-renderUI({sliderInput(inputId = "TransToHospSIREN",label="Average ambulance transport time to hospital if using lights/sirens (minutes)",min=5,max=15,value=10,round=TRUE)})
    output$ITransToHosp<-renderUI({sliderInput(inputId = "TransToHosp",label="Average ambulance transport time to hospital without using lights/sirens (minutes)",min=5,max=25,value=15,round=TRUE)})
  #####
  ################################# Health simulator and graphics
  #####
    UploadHW<-reactive({
      req(input$HWInput)
      req(input$HWTimeline)
      if(input$HWInput==2){
        f<-input$HWTimeline
        ext<-tools::file_ext(f$datapath)
        req(f)
        validate(need(ext=="csv","Please upload a csv file"))
        
        UploadHW<-read.csv(f$datapath)
      }else{UploadHW<-NULL}
    })  
    
    DefaultHW<-read.csv("data/DefaultHeatwave.csv",header=T)
    
    
    DHW<-reactive({
      req(input$HWInput)
      req(input$HA)
        if(input$HWInput==0){
          DHW<-NULL
        }else if (input$HWInput==1){
          DHW<-DefaultHW %>%
              dplyr::filter(DefaultHW$HA==input$HA)
        }else if (input$HWInput==2){
          UploadHW<-UploadHW()
          DHW<-UploadHW %>%
              dplyr::filter(UploadHW$HA==input$HA)
        }
    })
    
    HWDays<-reactive({
      req(DHW)
      req(input$HWInput)
      if(input$HWInput!=0){
      DHW<-DHW()
      HWDays<-as.numeric(max(DHW$Day))
      }else{HwDays<-0}
    })

    HWStartTimes<-reactive({
      req(DHW)
      req(input$RunTime)
      req(HWDays)
      req(input$HWInput)
      if(input$HWInput!=0){
        HWDays<-HWDays()
        RT<-input$RunTime
        #HWStart<-sample(seq(from=1440,to=((RT-HWDays-10)*1440),by=1440),size=1,replace=F)
        HWStart<-(14)*1440
        HWStartTimes<-(seq(from=HWStart,length.out=(HWDays*3),by=480))
      }else{HWStartTimes<-NULL}
    })
    
    HWStopTimes<-reactive({
      req(HWDays)
      req(HWStartTimes)
      req(input$HWInput)
      if(input$HWInput!=0){
        HWDays<-HWDays()
        HWStartTimes<-HWStartTimes()
        HWStopTimes<-seq(from=(HWStartTimes[1]+479),length.out=(HWDays*3),by=480)
      }else{HWStopTimes<-NULL}
    })
    
    N_HW_Gen<-reactive({
      req(HWDays)
      n<-HWDays()
      N_HW_Gen<-n*3
    })
    
    
    
    l_WI<-reactive({
      req(DHW)
      req(N_HW_Gen)
      req(input$CatchmentPop)
      Pop<-input$CatchmentPop
      DHW<-DHW()
      PopRatio<-(1/(as.numeric(DHW[1,6])/(1000*Pop)))
      Ngen<-N_HW_Gen()
      L<-lapply(1:nrow(DHW),function(i){(DHW[i,10:12])})
      L<-as.numeric(unlist(L))
      L<-replace(L, L==0, 0.01)
      l_WI<-((8*60)/(L*PopRatio))
      
    })
    
    l_EMS<-reactive({
      req(DHW)
      req(N_HW_Gen)
      req(input$CatchmentPop)
      Pop<-input$CatchmentPop
      DHW<-DHW()
      PopRatio<-(1/(as.numeric(DHW[1,6])/(1000*Pop)))
      Ngen<-N_HW_Gen()
      L<-lapply(1:nrow(DHW),function(i){(DHW[i,7:9])})
      L<-as.numeric(unlist(L))
      L<-replace(L, L==0, 0.01)
      l_EMS<-((8*60)/(L*PopRatio))
    })
    
    #####

    
     
  Hmodel<<-eventReactive(input$RunHModel,{
    
      
          nIterations<-reactive(input$iterations)
              
          env<-mclapply(1:nIterations(),function(i){
            #library(simmer)
            #library(shiny)

                env<-simmer()
                  
        
                ######## Health simmer setup
                      #####
                isolate({
                  PopSize<-reactive({input$CatchmentPop})
                  PopSize<-PopSize()
                  
                    ### Heatwave inputs
                  HWRun<-reactive(input$HWInput)
                  HWRun<-HWRun()

                if(HWRun!=0){ 
                      Ngen<-N_HW_Gen()
                      HWStopT<-HWStopTimes()
                      HWStartT<-HWStartTimes()
                      l_WI<-l_WI()
                      l_EMS<-l_EMS()
                      HW_WI<-lapply(1:Ngen,function(i){rpois(n=10000,lambda=l_WI[i])})
                      HW_EMS<-lapply(1:Ngen,function(i){rpois(n=10000,lambda=l_EMS[i])})
                }else{}
                  
                    
                    ### Other inputs      
                        BinMCI<-reactive(input$mci)      
                        MCI<-if(BinMCI()==TRUE){1}else{0}
                        
                        CoolD<-reactive(input$dischargeCooling)
                        CoolDecanting<-if(CoolD()==TRUE){1}else{0}
                        
                        Ss<-reactive(input$SurgeStaff)
                        SurgeStaff<-if(Ss()==TRUE){1}else{0}
                
                        
                        ## Hospital resources
                        nEDbed<-reactive(input$nEDBeds)
                        nICUbed<-reactive(input$nICUbed)
                        nNonICUbed<-reactive(input$nNonICUbed)
                        nEDdoc<-reactive({input$ndoc*10})
                        nTriageNurse<-reactive(input$nTriageNurse)
                        nEDnurse<-reactive({input$nEDNurse*4})
                        nWaitingRoom<-PopSize
                        nMorgue<-Inf
                        nLabImag<-4
                        nFTbeds<-reactive(input$nFTBeds)
                        nFTdoc<-reactive({input$nFTdocs*10})
                        nFTnurse<-reactive({input$nFTnurse*5})
                        nPolice<-reactive(input$nPolice)
                        nFire<-reactive(input$nFire)
                        nAmbulance<-reactive(input$nAmbulance)
                        nDispatcher<-reactive(input$nDispatcher)
                        nCoroner<-reactive(input$nCoroner)
                      
                        
                        nRunDays<-reactive(input$RunTime)
                        nRunDays<-nRunDays()

                        
                        
                        ## Time and calendar functions
                        start_day<-function()sample(1:7,1)
                        
                        t_minute<-function(t) as.numeric(t)
                        t_hour<-function(t) t_minute(t) * 60
                        t_day<-function(t) t_hour(t) * 24
                        DayOfWeek<-function(t) t_day(t)+start_day
                        
                        
                        ## Triage distributions non-HW
                        triageX<-c(1:5)
                        ambulanceTriageProb<-c(0.023,0.255,0.315,0.409,0)
                        walkinTriageProb<-c(0.005,0.158,0.491,0.315,0.029)
                        
                        ## Triage distributions HW
                        HWambulanceTriageProb<-c(0.035,0.303,0.334,0.328,0)
                        HWwalkinTriageProb<-c(0.014,0.183,0.457,0.305,0.031)
                        
                        ## ICU distributions for Non-HW pt
                        ICUdays<-seq(from=1440, to=28800, by=1440)
                        ICUdaysProb<-c(0.284,0.174,0.103,0.069,0.051,0.04,0.034,0.027,0.022,0.02,0.018,0.017,0.015,0.015,0.013,0.013,0.013,0.13,0.012,0.012)
                        
                        
                        #              Patient set up
                        
                        ## daily patients
                        
                        dis_WI<-rnorm(n=10000,mean=(0.93*PopSize),sd=(0.93*PopSize*0.01))
                        #+-1%
                        dis_EMS<-rnorm(n=10000,mean=(0.21*PopSize),sd=(0.21*PopSize*0.01))
                        #+-1%
                        
                        ## 2.18 deaths per 100,000 per day * 42.9% in hospital - is equal to (2.18*0.429/93)
                        probdeathAll_WI<-(PopSize*0.01005613)
                        ## 2.18 deaths per 100,000 per day * 57.1% in hospital - is equal to (2.18*0.571/21) - multiple this by the number of deaths that EHS treats
                        probdeathAll_EMS<-(PopSize*0.05927524*0.5)
                        
                       
                        
                        #Prehospital response timing 
                        tAmbulanceResponse<-reactive(input$AmbRespT)
                        tFireResponse<-reactive(input$FireRespT)
                        tTransportToHosSiren<-reactive(input$TransToHospSIREN)
                        tTransportToHos<-reactive(input$TransToHosp)
                        
                        tAmbulanceResponse<-tAmbulanceResponse()
                        tFireResponse<-tFireResponse()
                        tTransportToHosSiren<-tTransportToHosSiren()
                        tTransportToHos<-tTransportToHos()
                        
                        FirstResponder<-c("fire","Ambulance")
                        
                        nAmbulance<-nAmbulance()
                        nFire<-nFire()
                        nPolice<-nPolice()
                        nCoroner<-nCoroner()
                        nDispatcher<-nDispatcher()
                        nEDnurse<-nEDnurse()
                        nTriageNurse<-nTriageNurse()
                        nEDbed<-nEDbed()
                        nEDdoc<-nEDdoc()
                        nICUbed<-nICUbed()
                        nICUbed<-nICUbed
                        nNonICUbed<-nNonICUbed()
                        nNonICUbed<-nNonICUbed
                        nFTbeds<-nFTbeds()
                        nFTnurse<-nFTnurse()
                        nFTdoc<-nFTdoc()
                })
                
                ## Non-heatwave hourly distribution
                
                
                  #Walk in
                    WI_0_3<-rpois(n=10000,lambda=(60/(.017*(sample(size=1,dis_WI)))))
                    WI_3_6<-rpois(n=10000,lambda=(60/(.011*(sample(size=1,dis_WI)))))
                    WI_6_9<-rpois(n=10000,lambda=(60/(.027*(sample(size=1,dis_WI)))))
                    WI_9_12<-rpois(n=10000,lambda=(60/(.061*(sample(size=1,dis_WI)))))
                    WI_12_15<-rpois(n=10000,lambda=(60/(.063*(sample(size=1,dis_WI)))))
                    WI_15_18<-rpois(n=10000,lambda=(60/(.058*(sample(size=1,dis_WI)))))
                    WI_18_21<-rpois(n=10000,lambda=(60/(.056*(sample(size=1,dis_WI)))))
                    WI_21_24<-rpois(n=10000, lambda=(60/(.041*(sample(size=1,dis_WI)))))
                   
                  #EMS arrival 
                    EMS_0_6<-rpois(n=10000,lambda=(60/(.027*(sample(size=1,dis_EMS)))))
                    EMS_6_9<-rpois(n=10000,lambda=(60/(.023*(sample(size=1,dis_EMS)))))
                    EMS_9_12<-rpois(n=10000,lambda=(60/(.047*(sample(size=1,dis_EMS)))))
                    EMS_12_15<-rpois(n=10000,lambda=(60/(.055*(sample(size=1,dis_EMS)))))
                    EMS_15_18<-rpois(n=10000,lambda=(60/(.056*(sample(size=1,dis_EMS)))))
                    EMS_18_21<-rpois(n=10000,lambda=(60/(.054*(sample(size=1,dis_EMS)))))
                    EMS_21_24<-rpois(n=10000, lambda=(60/(.046*(sample(size=1,dis_EMS)))))
                
                        ## Small towns (i.e. if population size is less than 75k people) *Not used in paper*
                        WI_0_6<-rpois(n=10000,lambda=(60/(.014*(sample(size=1,dis_WI)))))
                        WI_6_12<-rpois(n=10000,lambda=(60/(.044*(sample(size=1,dis_WI)))))
                        WI_12_18<-rpois(n=10000,lambda=(60/(.06*(sample(size=1,dis_WI)))))
                        WI_18_24<-rpois(n=10000,lambda=(60/(.048*(sample(size=1,dis_WI)))))
                        
                        EMS_6_12<-rpois(n=10000,lambda=(60/(.025*(sample(size=1,dis_EMS)))))
                        EMS_12_18<-rpois(n=10000,lambda=(60/(.049*(sample(size=1,dis_EMS)))))
                        EMS_18_24<-rpois(n=10000,lambda=(60/(.051*(sample(size=1,dis_EMS)))))
                
                
                      #####              
                      
                      ### Fast Track trajectory
                      #####
                      FastTrack<-trajectory() %>%
                        seize("Fast Track bed",1)%>%
                        set_attribute("Door to ED bed wait for lowest severity",function()ifelse(get_attribute(env,"triage score")>3.9,as.numeric(as.numeric(now(env)-(get_attribute(env,"ED arrival time")))),NA))%>%
                        seize("fifth Fast Track nurse",1)%>%
                        timeout(5)%>%
                        release_all("fifth Fast Track nurse")%>%
                        timeout(20)%>%
                        seize("fifteenth Fast Track doc",1)%>%
                        timeout(5)%>%
                        release_all("fifteenth Fast Track doc")%>%
                        timeout(function()rpois(n=1,lambda=120))%>%
                        seize("fifth Fast Track nurse")%>%
                        timeout(function()rpois(n=1,lambda=30))%>%
                        seize("fifteenth Fast Track doc")%>%
                        release_all("fifth Fast Track nurse")%>%
                        timeout(5)%>%
                        release_all("fifteenth Fast Track doc")%>%
                        timeout(15)%>%
                        release("Fast Track bed",1)
                      
                      
                      #####            
                      
                      
                      ### Code Blue trajectories (in hospital)
                      #####
                      CodeBlue<-trajectory() %>%
                        set_prioritization(values=c(5,6,FALSE))%>%
                        seize("quarter ED nurse", 4) %>%
                        branch(option=function() ifelse(get_attribute(env, "SALT?")==0,1,2),continue=c(TRUE,TRUE),
                               trajectory("Run code blue") %>%
                                 seize("quarter ED nurse",4) %>%
                                 seize("tenth ED doc", 10) %>%
                                 timeout(25) %>%
                                 release("quarter ED nurse",4) %>%
                                 release_all("tenth ED doc"),
                               
                               trajectory("MCI1")%>%
                                 timeout(5)) %>%
                        
                        timeout(5) %>%
                        release_all("quarter ED nurse")%>%
                        timeout(10) %>%
                        seize("Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==0,1,0))%>%
                        seize("Heatwave Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==1,1,0))%>%
                        set_attribute("Heatwave deaths",function()get_server_count(env,"Heatwave Morgue"))%>%
                        set_attribute("Total deaths",function()get_server_count(env,"Morgue")+get_server_count(env,"Heatwave Morgue"))%>%
                        release_all("ED bed")
                        
                
                
                
                        CodeBlue2<-trajectory() %>%
                          set_prioritization(values=c(5,6,FALSE))%>%
                          seize("Morgue",1)%>%
                          set_attribute("Total deaths",function()get_server_count(env,"Morgue"))
                        
                      #####
                      
                      
                      ####### Hospital trajectory (ED and acute wards)
                      #####
                        
                        EDAcute<-trajectory()%>%  
                          timeout(5)%>%
                          
                            ### patient dies in ED?
                                    branch(option=function() 
                                      ifelse(get_attribute(env, "Patient dies")==1 & get_attribute(env, "dieED")==1,1,0), continue=FALSE,
                                      trajectory("branch1")%>%
                                        release_all("ED bed")%>%
                                        simmer::join(CodeBlue)) %>%
                          
                            ### Start pt assessment
                                    
                                    set_prioritization(values=function(){
                                      TS<-get_attribute(env,"triage score")
                                      Prior<-ifelse(TS>3,1,ifelse(TS==3,0,ifelse(TS<3,0,0)))
                                      Preemt<-Prior+1
                                      c(Prior,Preemt,FALSE)
                                    })%>%
                                    timeout(function()ifelse(get_attribute(env,"triage score")>3.9,rpois(n=1,lambda=20),
                                                             ifelse(get_attribute(env,"triage score")==3,rpois(n=1,lambda=15),
                                                                    ifelse(get_attribute(env,"triage score")==2,rpois(n=1,lambda=5),1)))) %>%
                                seize("quarter ED nurse",function() get_attribute(env,"ED nurses siezed"))%>%
                          
                                    timeout(function()ifelse(get_attribute(env,"triage score")>3.9,rpois(n=1,lambda=10),
                                                             ifelse(get_attribute(env,"triage score")==3,rpois(n=1,lambda=15),
                                                                    ifelse(get_attribute(env,"triage score")==2,rpois(n=1,lambda=5),1)))) %>%
                                    timeout(1)%>%
                                seize("tenth ED doc",10,continue=T,reject = trajectory("Waiting on ED doc")%>%
                                            timeout(1)%>%
                                            simmer::rollback(amount=3,times=Inf))%>%
                                    timeout(function()ifelse(get_attribute(env,"triage score")>3.9,rpois(n=1,lambda=10),
                                                             ifelse(get_attribute(env,"triage score")==3,rpois(n=1,lambda=15)*1.2,
                                                                    ifelse(get_attribute(env,"triage score")==2,rpois(n=1,lambda=20),30)))) %>%
                                    
                               release_all("tenth ED doc")%>%
                                    seize("tenth ED doc",function() get_attribute(env,"ED doctors siezed"),continue=T,reject=trajectory("Waiting on ED doc")%>%
                                            timeout(1)%>%
                                            simmer::rollback(amount=3,times=Inf))%>%
                             
                          ### Waiting on labs and imaging for tx
                          
                                    set_attribute("triage score", function()ifelse(get_attribute(env,"triage score")==2,sample(size=1,rep(c(1,2), c(0.75,9.25)),replace=T),get_attribute(env,"triage score")))%>%
                                    set_attribute("triage score", function()ifelse(get_attribute(env,"triage score")==3,sample(size=1,rep(c(2,3), c(0.04,9.6)),replace=T),get_attribute(env,"triage score")))%>%
                                    set_prioritization(values=function(){
                                      TS<-get_attribute(env,"triage score")
                                      Prior<-(5-TS)
                                      Preemt<-(Prior+1)
                                      c(Prior,Preemt,FALSE)
                                    })%>%
                                    timeout(function()ifelse(get_attribute(env,"triage score")>3.9,rpois(n=1,lambda=7)^1.2,ifelse(get_attribute(env,"triage score")==3,rpois(n=1,lambda=10)^1.2,rpois(n=1,lambda=30)^1.3))) %>%
                                    release_all("tenth ED doc")%>%
                          
                          ### Cooling HW patients
                                    timeout(function()ifelse(get_attribute(env,"triage score")<2.1 & get_attribute(env,"Heatwave patient")==1 & CoolDecanting==0 ,rpois(n=1,lambda=350)^1.1,0))%>%
                                    timeout(function()ifelse(get_attribute(env,"triage score")==3 & get_attribute(env,"Heatwave patient")==1 & CoolDecanting==0 ,rpois(n=1,lambda=460),0))%>%
                                    timeout(function()ifelse(get_attribute(env,"triage score")>3 & get_attribute(env,"Heatwave patient")==1 & CoolDecanting==0 ,rpois(n=1,lambda=400),0))%>%
                          
                                    timeout(function()ifelse(get_attribute(env,"triage score")<2.1 & get_attribute(env,"Heatwave patient")==1 & CoolDecanting==1 ,rpois(n=1,lambda=350)^1.1,0))%>%
                                    timeout(function()ifelse(get_attribute(env,"triage score")==3 & get_attribute(env,"Heatwave patient")==1 & CoolDecanting==1 ,rpois(n=1,lambda=90),0))%>%
                                    timeout(function()ifelse(get_attribute(env,"triage score")>3 & get_attribute(env,"Heatwave patient")==1 & CoolDecanting==1 ,rpois(n=1,lambda=30),0))%>%

                          ###

                          
                          ### Start pt tx
                                    seize("tenth ED doc",10,continue=T,reject = trajectory("Waiting on ED doc")%>%
                                            timeout(1)%>%
                                            simmer::rollback(amount=3,times=Inf))%>%
                                    timeout(function()ifelse(get_attribute(env,"triage score")>3.9,rpois(n=1,lambda=7)^1.2,ifelse(get_attribute(env,"triage score")>2.1,rpois(n=1,lambda=5)^1.4,rpois(n=1,lambda=10)^1.4))) %>%
                                    release_all("tenth ED doc")%>%
                                    timeout(function()ifelse(get_attribute(env,"triage score")>3.9,rpois(n=1,lambda=5)^1.2,ifelse(get_attribute(env,"triage score")>2.1,rpois(n=1,lambda=20),rpois(n=1,lambda=8)^1.4))) %>%
                                    set_attribute("ED Boarding (start clock)",function()as.numeric(as.numeric(now(env))-45))%>%
                        
                                      set_prioritization(values=function(){
                                        TS<-get_attribute(env,"triage score")
                                        Prior<-(5-TS)
                                        Preemt<-(Prior+1)
                                        c(Prior,Preemt,FALSE)
                                      })%>%
                          
                          ### ED transfer/discharge
                                    branch(option=function() ifelse(get_attribute(env, "triage score")<1.1,1,0), continue=FALSE,
                                           trajectory("ICU")%>%
                                             seize("tenth ED doc",5,continue=T,reject = trajectory("Waiting on ED doc")%>%
                                                     timeout(1)%>%
                                                     simmer::rollback(amount=3,times=Inf))%>%
                                             timeout(1)%>%
                                             seize("ICU bed",1,reject=trajectory("Waiting for ICU bed") %>%
                                                     log_("Waiting for ICU bed")%>%
                                                     timeout(5)%>%
                                                     simmer::rollback(amount=3,times=Inf),continue=TRUE)%>%
                                             set_attribute("ICU admit time",function()as.numeric(as.numeric(now(env))))%>%
                                                      
                                                      #check if HW is done?
                                                     set_capacity("Ambulance",value=nAmbulance)%>%
                                                     set_capacity("fire",value=nFire)%>%
                                                     set_capacity("quarter ED nurse",value=nEDnurse)%>%
                                                     set_capacity("tenth ED doc",value=nEDdoc)%>%
                                                     set_capacity("Non-ICU bed",value=nNonICUbed)%>%
                                             
                                             release_all("tenth ED doc")%>%
                                             release_all("quarter ED nurse")%>%
                                             release_all("ED bed")%>%
                                             set_attribute("Time in ED for highest severity",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ED admit time")))))%>%
                                             set_attribute("ED boarding time for ICU",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ED Boarding (start clock)")))))%>%
                                             timeout(90)%>%
                                             branch(option=function()
                                               ifelse(get_attribute(env, "Patient dies")==1 & get_attribute(env, "dieHosWard")==1,1,0), continue=FALSE,
                                               trajectory()%>%
                                                 timeout(function()rpois(n=1,lambda=240))%>%
                                                 release_all("ICU bed")%>%
                                                 simmer::join(CodeBlue2)) %>%
                                             timeout(function()sample(size=1,rep(ICUdays, round(100*ICUdaysProb)),replace=T)) %>%
                                             timeout(300)%>%
                                             seize("Non-ICU bed",1)%>%
                                             set_attribute("Patient ICU LOS",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ICU admit time")))))%>%
                                             release("ICU bed",1)%>%
                                             timeout(function()rpois(n=1,lambda=920))%>%
                                             timeout(function()sample(size=1,rgamma(n=1000,shape=2,scale=1000))) %>%
                                             set_attribute("ALC start",function()as.numeric(as.numeric(now(env))))%>%
                                             timeout(function()as.numeric(as.numeric(get_attribute(env,"Patient ICU LOS"))*0.20))%>%
                                             release("Non-ICU bed",1)%>%
                                             set_attribute("ALC",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ALC start")))))
                                           
                                    )%>%
                                    
                                    branch(option=function() ifelse(get_attribute(env, "triage score")==2,1, 
                                                                    ifelse(sample(size=1,rep(c(0,1), c(10,0)),replace=T)==1 & get_attribute(env, "triage score")==3,1,0)), continue=FALSE,
                                           trajectory("Non-ICU")%>%
                                             release_all("tenth ED doc")%>%
                                             timeout(function()sample(size=1,rpois(n=1000,lambda=50),replace=T)^1.5)%>%
                                             timeout(1)%>%
                                             seize("Non-ICU bed",1,continue=T,reject = trajectory("Waiting on Non-ICU bed")%>%
                                                     timeout(5)%>%
                                                     release_all("tenth ED doc")%>%
                                                     seize("tenth ED doc",1)%>%
                                                     simmer::rollback(amount=6,times=Inf))%>%
                                             set_attribute("Non-ICU admit time",function()as.numeric(as.numeric(now(env))))%>%
                                             release_all("tenth ED doc")%>%
                                             release_all("quarter ED nurse")%>%
                                             release_all("ED bed")%>%
                                             set_attribute("Time in ED for highest severity",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ED admit time")))))%>%
                                             set_attribute("ED boarding time for Non-ICU",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ED Boarding (start clock)")))))%>%
                                             timeout(1)%>%
                                             branch(option=function()
                                               ifelse(get_attribute(env, "Patient dies")==1 & get_attribute(env, "dieHosWard")==1,1,0), continue=FALSE,
                                               trajectory()%>%
                                                 timeout(function()rpois(n=1,lambda=120))%>%
                                                 release_all("Non-ICU bed")%>%
                                                 simmer::join(CodeBlue2)) %>%
                                             timeout(function()sample(size=1,rgamma(n=1000,shape=4,scale=850),replace=T)) %>%
                                             set_attribute("Patient Non-ICU ward LOS",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"Non-ICU admit time")))))%>%
                                             set_attribute("ALC start",function()as.numeric(as.numeric(now(env))))%>%
                                             timeout(function()as.numeric(as.numeric(get_attribute(env,"Patient Non-ICU ward LOS"))*0.15))%>%
                                             
                                             timeout(function()ifelse(get_attribute(env,"triage score")<2.1 & get_attribute(env,"Heatwave patient")==1 & CoolDecanting==0 ,rpois(n=1,lambda=350)^1.1,0))%>%
                                             timeout(function()ifelse(get_attribute(env,"triage score")==3 & get_attribute(env,"Heatwave patient")==1 & CoolDecanting==0 ,rpois(n=1,lambda=460),0))%>%
                                             
                                             timeout(function()ifelse(get_attribute(env,"triage score")<2.1 & get_attribute(env,"Heatwave patient")==1 & CoolDecanting==1 ,0,0))%>%
                                             timeout(function()ifelse(get_attribute(env,"triage score")==3 & get_attribute(env,"Heatwave patient")==1 & CoolDecanting==1 ,0,0))%>%
                                             timeout(function()ifelse(get_attribute(env,"triage score")>3 & get_attribute(env,"Heatwave patient")==1 & CoolDecanting==1 ,0,0))%>%
                                             
                                             release("Non-ICU bed",1)%>%
                                             set_attribute("ALC",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ALC start")))))
                                    ) %>%
                                    
                                    branch(option=function() ifelse(get_attribute(env, "triage score")==3,1,0), continue=FALSE,
                                             trajectory("ED discharge")%>%
                                               
                                               release_all("tenth ED doc")%>%
                                               release_all("quarter ED nurse")%>%
                                               release_all("ED bed")%>%
                                               set_attribute("Time in ED for middle severity",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ED admit time")))))
                                             
                                      )%>%
                                    branch(option=function() ifelse(get_attribute(env, "triage score")>3,1,0), continue=FALSE,
                                              trajectory("ED discharge")%>%
                                                         
                                              release_all("tenth ED doc")%>%
                                              release_all("quarter ED nurse")%>%
                                                         release_all("ED bed")%>%
                                                         set_attribute("Time in ED for lowest severity",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ED admit time")))))
                                                       
                                                )
                        
                      #####
                        
                      
                      ### Hospital arrival
                      #####
                      
                          Hospital<-trajectory("Hospital") %>%
                            
                     ### Patient arrival
                                  log_("ED arrival")%>%
                                  set_attribute("Patients leave AMA",0)%>%
                                    handle_unfinished(
                                      trajectory() %>%
                                      log_("resolving unfinished")%>%
                                      timeout(1440))%>%
                                  set_attribute("ED arrival time",function()as.numeric(now(env)))%>%
                                  branch(option=function()ifelse(get_attribute(env, "mode of arrival")==0,1,0), continue=TRUE,
                                    trajectory()%>%
                                        log_("triaging patient")%>%
                                        seize("Triage nurse", 1, continue=TRUE,
                                              reject=trajectory("waiting room before triage") %>%
                                                log_("Waiting for triage")%>%
                                                seize("Waiting room chair",1) %>%
                                                timeout(1) %>%
                                                release("Waiting room chair",1)%>%
                                                simmer::rollback(amount=6,times=Inf))%>%
                                        timeout(function()rpois(1,lambda=10))%>%
                                        release_all("Triage nurse"))%>%
                                    timeout(function()rpois(1,lambda=2))%>%
                                    set_attribute(keys="Wait to triage patient",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ED arrival time")))))%>%
                                    log_("patient triaged")%>%
                    ### Test if die in waiting room
                                  branch(option=function() 
                                    ifelse(get_attribute(env, "Patient dies")==1 & get_attribute(env, "dieWaitingRoom")==1,1,0), continue=FALSE,
                                    trajectory()%>%
                                      log_("patient dies in waiting room")%>%
                                      release_all("Ambulance")%>%
                                      set_attribute("Ambulance off-load time",function()ifelse(get_attribute(env,"mode of arrival")==1,as.numeric(as.numeric(now(env)-(get_attribute(env,"ED arrival time")))),0))%>%
                                      simmer::join(CodeBlue)) %>%
                                  ####
                                  
                                  seize("Waiting room chair",1) %>%
                                  
                                  renege_in(t=function()ifelse(get_attribute(env,"mode of arrival")==0 & get_attribute(env, "triage score")>3.9, sample(size=1,replace=T,x=rgamma(n=1000,shape=1.8,scale=200)),10000),keep_seized = FALSE,
                                    out=trajectory()%>%
                                              set_attribute("Patients leave AMA",1)%>%
                                              log_("leaving AMA")%>%
                                              release_all("Ambulance")%>%
                                              release_all("Waiting room chair"))%>%
                                  
                  ### Fast Track trajectory ??
                                  set_attribute(keys="FT?",values=function()ifelse(get_attribute(env,"triage score")>3.9 & get_attribute(env, "fragility")==0 & get_queue_count(env,"Fast Track bed")<5 & PopSize>50 &
                                                                                     (((as.numeric(now(env))/1440)-as.numeric(now(env))%/%1440)*24)>9 & (((as.numeric(now(env))/1440)-as.numeric(now(env))%/%1440)*24)<19,
                                                                                   1,0)) %>%
                                  
                                  branch(option=function() 
                                    ifelse(get_attribute(env, "FT?")==1,1,0), continue=FALSE,
                                    trajectory("FT")%>%
                                      log_("going to FT")%>%
                                      renege_abort()%>%
                                      timeout(10)%>%
                                      set_attribute("Patients leave AMA",0)%>%
                                      set_attribute("Door to ED bed wait for FT bed",function()ifelse(get_attribute(env,"triage score")>3.9,as.numeric(as.numeric(now(env)-(get_attribute(env,"ED arrival time")))),NA))%>%
                                      release_all("Ambulance")%>%
                                      set_attribute("Ambulance off-load time",function()ifelse(get_attribute(env,"mode of arrival")==1,as.numeric(as.numeric(now(env)-(get_attribute(env,"ED arrival time")))),0))%>%
                                      release_all("Waiting room chair")%>%
                            simmer::join(FastTrack)) %>%
                                      
                 ### ED admission
                                  timeout(1)%>%
                                  timeout(function()ifelse(get_attribute(env,"triage score")>3, rpois(n=1,lambda=60),0))%>%
                                  timeout(function()ifelse(get_attribute(env,"triage score")>3 & get_attribute(env,"mode of arrival")==0 & as.numeric(get_queue_count(env,"quarter ED nurse"))>1,rpois(n=1,lambda=120),0))%>%
                                  timeout(function()ifelse(get_attribute(env,"triage score")==3 & get_attribute(env,"mode of arrival")==0 & as.numeric(get_queue_count(env,"quarter ED nurse"))>5,rpois(n=1,lambda=40),0))%>%
                          
                                  seize("ED bed",1, continue= TRUE,
                                        reject = trajectory("Waiting on ED bed")%>%
                                                timeout(1)%>%
                                                simmer::rollback(amount=3,times=Inf))%>%
                                                ### Test if die in ED
                                  renege_abort()%>%
                                  log_("admitted to ED")%>%
                                  set_attribute("Patients leave AMA",0)%>%
                                  set_attribute("ED admit time",function()as.numeric(now(env)))%>%
                                  release_all("Waiting room chair")%>%
                                  set_prioritization(values=function(){
                                      TS<-get_attribute(env,"triage score")
                                      Prior<-(5-TS)
                                      Preemt<-(Prior+1)
                                      c(Prior,Preemt,FALSE)
                                    })%>%
                                  set_attribute("Door to ED bed wait for highest severity",function()ifelse(get_attribute(env,"triage score")<2.1,as.numeric(as.numeric(now(env)-(get_attribute(env,"ED arrival time")))),NA))%>%
                                  set_attribute("Door to ED bed wait for lowest severity",function()ifelse(get_attribute(env,"triage score")>3.9,as.numeric(as.numeric(now(env)-(get_attribute(env,"ED arrival time")))),NA))%>%
                                  release_all("Ambulance")%>%
                                  set_attribute("Ambulance off-load time",function()ifelse(get_attribute(env,"mode of arrival")==1,as.numeric(as.numeric(now(env)-(get_attribute(env,"ED arrival time")))),NA))%>%
                          simmer::join(EDAcute)
                          
                      #####
                      
                      
                      ### PreHospital trajectory
                      #####
                      
                      HighAcuity<-trajectory()%>%
                        #log_("High acuity EMS")%>%
                        branch(option=function() ifelse(get_attribute(env, "DOA")==1 & get_attribute(env, "SALT?")==0,1,
                                                      ifelse(get_attribute(env, "DOA")==1 & get_attribute(env, "SALT?")==1,2,
                                                        ifelse(get_attribute(env, "diePreHos")==1 & get_attribute(env, "SALT?")==1,3,
                                                             ifelse(get_attribute(env, "diePreHos")==1 & get_attribute(env, "SALT?")==0,4,5)))),continue=c(FALSE,FALSE,FALSE,TRUE,TRUE),
                               
                                trajectory("DOA no MCI") %>%
                                        #log_("code blue")%>%
                                         ## roll fire to determine DOA and hold police at scene
                                         seize("fire",1,continue=FALSE,
                                               reject=trajectory()%>%
                                                 #log_("no fire available for DOA")%>%
                                                 seize("police",1)%>%
                                                 timeout(60)%>%
                                                 seize("coroner",1)%>%
                                                 release_all("police")%>%
                                                 timeout(function()rpois(n=1,lambda=240))%>%
                                                 seize("Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==0,1,0))%>%
                                                 seize("Heatwave Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==1,1,0))%>%
                                                 set_attribute("Heatwave deaths",function()get_server_count(env,"Heatwave Morgue"))%>%
                                                 set_attribute("Total deaths",function()get_server_count(env,"Morgue")+get_server_count(env,"Heatwave Morgue"))%>%
                                                 release_all("coroner"))%>%
                                         #log_("DOA")%>% 
                                         seize("police",1)%>%
                                         release_all("fire")%>%
                                         timeout(60)%>%
                                         seize("coroner",1)%>%
                                         release_all("police")%>%
                                         timeout(function()rpois(n=1,lambda=240))%>%
                                         seize("Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==0,1,0))%>%
                                         seize("Heatwave Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==1,1,0))%>%
                                         set_attribute("Heatwave deaths",function()get_server_count(env,"Heatwave Morgue"))%>%
                                         set_attribute("Total deaths",function()get_server_count(env,"Morgue")+get_server_count(env,"Heatwave Morgue"))%>%
                                         release_all("coroner")%>%
                                        log_("DOA to morgue"),
                                 
                              
                                 trajectory("DOA and MCI")%>%
                                         ## dispatcher determines over the phone that no resources are deployed immediately
                                          log_("Dispatercher SALT triage")%>%
                                          seize("family with body",1)%>%
                                          seize("police",1,continue=FALSE,
                                                reject= trajectory()%>%
                                                    timeout(5)%>%
                                                    simmer::rollback(amount=2,check=function()get_queue_count(env,"police")<5))%>%
                                          timeout(30)%>%
                                          release("police",1)%>%
                                          seize("coroner",1)%>%
                                          timeout(function()rpois(n=1,lambda=120))%>%
                                           seize("Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==0,1,0))%>%
                                           seize("Heatwave Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==1,1,0))%>%
                                           set_attribute("Heatwave deaths",function()get_server_count(env,"Heatwave Morgue"))%>%
                                           set_attribute("Total deaths",function()get_server_count(env,"Morgue")+get_server_count(env,"Heatwave Morgue"))%>%
                                          release_all("coroner"),
                                                 
                                  trajectory("Die prehos w/ provider and MCI")%>%
                                          simmer::select(resources=FirstResponder,policy="first-available",id=0)%>%
                                          seize_selected(amount=1,id=0)%>%
                                          timeout(min(rpois(n=1, lambda=tAmbulanceResponse),rpois(n=1, lambda=tFireResponse)))%>%
                                          seize("Ambulance",amount=function()ifelse(get_selected(env,id=0)=="Ambulance",0,1))%>%
                                          set_attribute("Ambulance response time",function() as.numeric(as.numeric(now(env)-(get_attribute(env,"911 call time")))))%>%
                                          timeout(function()rpois(n=1, lambda=10))%>%
                                          release_all("fire")%>%
                                          timeout(function()rpois(n=1, lambda=tTransportToHosSiren))%>%
                                          timeout(15)%>%
                                          seize("Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==0,1,0))%>%
                                          seize("Heatwave Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==1,1,0))%>%
                                          set_attribute("Heatwave deaths",function()get_server_count(env,"Heatwave Morgue"))%>%
                                          set_attribute("Total deaths",function()get_server_count(env,"Morgue")+get_server_count(env,"Heatwave Morgue"))%>%
                                          release_all("Ambulance"),
                                        
                                  trajectory("Run code and no MCI")%>%
                                          simmer::select(resources=FirstResponder,policy="first-available",id=0)%>%
                                          seize_selected(amount=1,id=0)%>%
                                          timeout(min(rpois(n=1, lambda=tAmbulanceResponse),rpois(n=1, lambda=tFireResponse)))%>%
                                          seize("Ambulance",amount=function()ifelse(get_selected(env,id=0)=="Ambulance",0,1))%>%
                                          set_attribute("Ambulance response time",function() as.numeric(as.numeric(now(env)-(get_attribute(env,"911 call time")))))%>%
                                          timeout(function()rpois(n=1, lambda=10))%>%
                                          release("fire",amount=function()ifelse(get_selected(env,id=0)=="fire",1,0))%>%
                                          timeout(function()rpois(n=1, lambda=tTransportToHosSiren))%>%
                                              set_prioritization(values=function(){
                                               TS<-get_attribute(env,"triage score")
                                               Prior<-(4)
                                               Preemt<-(Prior+1)
                                               c(Prior,Preemt,FALSE)
                                             })%>%
                                          simmer::join(Hospital),
                                          
                                 trajectory("1B High acuity not dead")%>%
                                   #log_("not dead but sick")%>%
                                   simmer::select(FirstResponder,policy="first-available",id=1)%>%
                                   seize_selected(amount=1,id=1)%>%
                                   #log_(function()paste0("ambulance queue: ", as.character(get_queue_count(env,"Ambulance"))))%>%
                                   #log_(function() paste0("seized: ",as.character(get_selected(env,id=1))))%>%
                                   timeout(min(rpois(n=1, lambda=tAmbulanceResponse),rpois(n=1, lambda=tFireResponse)))%>%
                                   seize("Ambulance",amount=function()ifelse(get_selected(env,id=1)=="Ambulance",0,1))%>%
                                   set_attribute("Ambulance response time",function() as.numeric(as.numeric(now(env)-(get_attribute(env,"911 call time")))))%>%
                                   release("fire",amount=function()ifelse(get_selected(env,id=1)=="fire",1,0))%>%
                                   timeout(function()rpois(n=1, lambda=tTransportToHosSiren))%>%
                                   set_prioritization(values=function(){
                                     TS<-get_attribute(env,"triage score")
                                     Prior<-(4)
                                     Preemt<-(Prior+1)
                                     c(Prior,Preemt,FALSE)
                                   })%>%
                                   simmer::join(Hospital))
                      
                      
                      
                      LowAcuity<-trajectory()%>%
                        #log_("not too sick")%>%
                        simmer::select(FirstResponder,"first-available",id=2)%>%
                        seize_selected(amount=1,id=2)%>%
                        #log_(function()paste0("ambulance queue: ", as.character(get_queue_count(env,"Ambulance"))))%>%
                        #log_(function() paste0("seized: ",as.character(get_selected(env,id=2))))%>%
                        timeout(min(rpois(n=1, lambda=tAmbulanceResponse),rpois(n=1, lambda=tFireResponse)))%>%
                        seize("Ambulance",amount=function()ifelse(get_selected(env,id=2)=="Ambulance",0,1))%>%
                        set_attribute("Ambulance response time",function() as.numeric(as.numeric(now(env)-(get_attribute(env,"911 call time")))))%>%
                        release("fire",amount=function()ifelse(get_selected(env,id=2)=="fire",1,0))%>%
                        timeout(function()rpois(n=1, lambda=tTransportToHos))%>%
                        set_prioritization(values=function(){
                          TS<-get_attribute(env,"triage score")
                          Prior<-(2)
                          Preemt<-(Prior+1)
                          c(Prior,Preemt,FALSE)
                        })%>%
                        simmer::join(Hospital)
                      
                      
                      
                      PreHospital<-trajectory() %>%
                        ### Patient arrival
                        #log_("911 call")%>%
                        set_attribute("911 call time",function()as.numeric(now(env)))%>%
                        seize("911Dispatcher",1)%>%
                        timeout(3)%>%
                        release("911Dispatcher",1)%>%
                        set_attribute("Ambulances available",function()get_capacity(env,"Ambulance")-get_server_count(env,"Ambulance"))%>%
                        branch(option=function() ifelse(get_attribute(env, "triage score")<2.1 | get_attribute(env, "DOA")==1,1,2),continue=c(FALSE,FALSE),
                               trajectory("HighAcuity911") %>%
                                 simmer::join(HighAcuity),
                               trajectory("LowAcuity911")%>%
                                 simmer::join(LowAcuity))
                      
                      
                      
                      
                      #####
                      
                      ### Heatwave patient initial path and attributes
                      #####
                      
                      HW_WalkInPatient<-trajectory() %>%

                        #set staffing and resource surging
                        set_capacity("Ambulance",value=function()ifelse(SurgeStaff==1,ceiling(nAmbulance*1.1),nAmbulance))%>%
                        set_capacity("fire",value=function()ifelse(SurgeStaff==1,ceiling(nFire*1.1),nFire))%>%
                        set_capacity("quarter ED nurse",value=function()ifelse(SurgeStaff==1,ceiling(nEDnurse*1.1),nEDnurse))%>%
                        set_capacity("tenth ED doc",value=function()ifelse(SurgeStaff==1,ceiling(nEDdoc*1.1),nEDdoc))%>%
                        set_capacity("Non-ICU bed",value=function()ifelse(SurgeStaff==1,ceiling(nNonICUbed*1.1),nNonICUbed))%>%
                        
                        
                        # MCI protocol on (automatic) or off
                        set_attribute(keys="SALT?", values=function()ifelse(MCI==1 & (get_queue_count(env,"ED bed")/get_capacity(env,"ED bed"))>.9,1,0)) %>%
                        set_attribute(keys="Hour_of_day",values=function() floor((now(env) %% 1440)/60)) %>%

                        
                        #set non-heatwave attribute
                        set_attribute(keys="Heatwave patient", values=1)%>%
                        
                        #set arrival attribute (by walk in =0 or by ambulance =1)
                        set_attribute(keys="mode of arrival", values=0)%>%
                        
                        #set triage score (informed by arrival attribute)
                        set_attribute(keys="triage score",values=function() sample(size=1,rep(triageX, round(100*HWwalkinTriageProb)),replace=T)) %>%
                        set_prioritization(values=function(){
                          TS<-get_attribute(env,"triage score")
                          Prior<-(5-TS)
                          Preemt<-(Prior+1)
                          c(Prior,Preemt,FALSE)
                        })%>%
                        
                        #set fragility attribute for FT (low co-morbidity/age =0 or high-risk =1)
                        set_attribute(keys="fragility",values=function()
                          rbinom(n=1,size=1,prob=0.4))%>%
                        
                        #set resource usage
                        set_attribute(keys="ED nurses siezed", values=function()
                          ifelse(get_attribute(env, "triage score")<1.1,4,
                                 ifelse(get_attribute(env, "triage score")<2.1,2,1))) %>%
                        
                        set_attribute(keys="ED doctors siezed", values=function()
                          ifelse(get_attribute(env, "triage score")==1, 10,
                                 ifelse(get_attribute(env, "triage score")==2, 5,
                                        ifelse(get_attribute(env, "triage score")>2, 1, 1)))) %>%
                        
                        #set dead or alive outcome
                        set_attribute(keys="Patient dies", values=function() rbinom(n=1,size=1,prob=0.45)) %>%
                        
                        #death distributions
                        set_attribute(keys="dieWaitingRoom", values=function()
                          ifelse(get_attribute(env,"Patient dies")==1,rbinom(n=1,size=1,prob=0.1),0)) %>%
                        set_attribute(keys="dieED", values=function()
                          ifelse(get_attribute(env,"Patient dies")==1,rbinom(n=1,size=1,prob=0.35),0)) %>%
                        set_attribute(keys="dieHosWard", values=function()
                          ifelse(get_attribute(env,"Patient dies")==1,rbinom(n=1,size=1,prob=1),0))%>%
                        
                        #log_(function() paste0("ED bed queue: ",as.character(get_queue_count(hospital,"ED bed"))))%>%
                        #log_(function() paste0("Triage score: ",as.character(get_attribute(hospital,"triage score"))))
                        
                        simmer::join(Hospital)
                      
                      
                      HW_EMSPatient<-trajectory() %>%
                        # MCI protocol on (automatic) or off
                        set_attribute(keys="SALT?", values=function()ifelse(MCI==1 & (get_queue_count(env,"Ambulance")/get_capacity(env,"Ambulance"))>0.1,1,0)) %>%
                        set_attribute(keys="Hour_of_day",values=function() floor((now(env) %% 1440)/60)) %>%
                        
                        #set non-heatwave attribute
                        set_attribute(keys="Heatwave patient", values=1)%>%
                        
                        #set arrival attribute (by walk in =0 or by ambulance =1)
                        set_attribute(keys="mode of arrival", values=1)%>%
                        
                        #set triage score (informed by arrival attribute)
                        set_attribute(keys="triage score",values=function() sample(size=1,rep(triageX, round(100*HWambulanceTriageProb)),replace=T)) %>%
                        set_prioritization(values=function(){
                          TS<-get_attribute(env,"triage score")
                          Prior<-(5-TS)
                          Preemt<-(Prior+1)
                          c(Prior,Preemt,FALSE)
                        })%>%
                        
                        #set fragility attribute for FT (low co-morbidity/age =0 or high-risk =1)
                        set_attribute(keys="fragility",values=function()
                          rbinom(n=1,size=1,prob=0.7))%>%
                        
                      
                        
                        #set resource usage
                        set_attribute(keys="ED nurses siezed", values=function()
                          ifelse(get_attribute(env, "triage score")==1,4,
                                 ifelse(get_attribute(env, "triage score")==2,2,1))) %>%
                        
                        set_attribute(keys="ED doctors siezed", values=function()
                          ifelse(get_attribute(env, "triage score")==1, 10,
                                 ifelse(get_attribute(env, "triage score")==2, 5,
                                        ifelse(get_attribute(env, "triage score")>2, 1, 1)))) %>%
                        
                        #set dead or alive outcome
                        set_attribute(keys="Patient dies", values=function()
                          rbinom(n=1,size=1,prob=0.45)) %>%
                        
                        
                        #death distributions
                        set_attribute(keys="DOA", values=function()
                          ifelse(get_attribute(env,"Patient dies")==1,rbinom(n=1,size=1,prob=0.5),0)) %>%
                        set_attribute(keys="diePreHos", values=function()
                          ifelse(get_attribute(env,"Patient dies")==1,rbinom(n=1,size=1,prob=0.2),0)) %>%
                        set_attribute(keys="dieWaitingRoom", values=function()
                          ifelse(get_attribute(env,"Patient dies")==1,rbinom(n=1,size=1,prob=0.05),0)) %>%
                        set_attribute(keys="dieED", values=function()
                          ifelse(get_attribute(env,"Patient dies")==1,rbinom(n=1,size=1,prob=0.35),0)) %>%
                        set_attribute(keys="dieHosWard", values=function()
                          ifelse(get_attribute(env,"Patient dies")==1,rbinom(n=1,size=1,prob=1),0)) %>%
                        
                        
                        #set if death needs coroner
                        set_attribute(keys="Coroner case",function() 
                          ifelse(get_attribute(env, "DOA")==1, 1, 0)) %>%
                        
                        simmer::join(PreHospital)
                      
                      #####  
                      
                      ### Non-heatwave patient initial path and attributes
                      #####
                      
                      WalkInPatient<-trajectory() %>%
                        # MCI protocol on (automatic) or off
                        set_global(keys="MCI switch", values=MCI) %>%
                        set_attribute(keys="SALT?", values=0) %>%
                        set_attribute(keys="Hour_of_day",values=function() floor((now(env) %% 1440)/60)) %>%
                        
                        #set non-heatwave attribute
                        set_attribute(keys="Heatwave patient", values=0)%>%
                        
                        
                        #set arrival attribute (by walk in =0 or by ambulance =1)
                        set_attribute(keys="mode of arrival", values=0)%>%
                        
                        #set triage score (informed by arrival attribute)
                        set_attribute(keys="triage score",values=function() sample(size=1,rep(triageX, round(100*walkinTriageProb)),replace=T)) %>%
                        set_prioritization(values=function(){
                                TS<-get_attribute(env,"triage score")
                                Prior<-(5-TS)
                                Preemt<-(Prior+1)
                                c(Prior,Preemt,FALSE)
                              })%>%
                        
                        #set fragility attribute for FT (low co-morbidity/age =0 or high-risk =1)
                        set_attribute(keys="fragility",values=function()
                          rbinom(n=1,size=1,prob=0.65))%>%
                        
                        #set resource usage
                        set_attribute(keys="ED nurses siezed", values=function()
                          ifelse(get_attribute(env, "triage score")<1.1,4,
                                 ifelse(get_attribute(env, "triage score")<2.1,2,1))) %>%
                        
                        set_attribute(keys="ED doctors siezed", values=function()
                          ifelse(get_attribute(env, "triage score")<2, 10,
                                 ifelse(get_attribute(env, "triage score")==2, 2,
                                        ifelse(get_attribute(env, "triage score")>2, 1, 1)))) %>%
                        
                        #set dead or alive outcome
                        set_attribute(keys="Patient dies", values=function()ifelse(get_attribute(env, "triage score")==1,rbinom(n=1,size=1,prob=0.212),
                                                                                   ifelse(get_attribute(env, "triage score")==2,rbinom(n=1,size=1,prob=0.05),
                                                                                          ifelse(get_attribute(env, "triage score")==3,rbinom(n=1,size=1,prob=0.01),0))))%>%
                        
                        #death distributions
                        set_attribute(keys="dieWaitingRoom", values=function()
                          ifelse(get_attribute(env,"Patient dies")==1,rbinom(n=1,size=1,prob=0.005),0))%>%
                        set_attribute(keys="dieED", values=function()
                          ifelse(get_attribute(env,"Patient dies")==1,rbinom(n=1,size=1,prob=0.25),0)) %>%
                        set_attribute(keys="dieHosWard", values=function()
                          ifelse(get_attribute(env,"Patient dies")==1,rbinom(n=1,size=1,prob=1),0)) %>%
                        
                        #log_(function() paste0("ED bed queue: ",as.character(get_queue_count(hospital,"ED bed"))))%>%
                        #log_(function() paste0("Triage score: ",as.character(get_attribute(hospital,"triage score"))))
                      
                        
                        simmer::join(Hospital)
                      
                      
                      EMSPatient<-trajectory() %>%
                        # MCI protocol on (automatic) or off
                        set_global(keys="MCI switch", values=MCI) %>%
                        set_attribute(keys="SALT?", values=0) %>%
                        set_attribute(keys="Hour_of_day",values=function() floor((now(env) %% 1440)/60)) %>%
                        
                        #set non-heatwave attribute
                        set_attribute(keys="Heatwave patient", values=0)%>%
                        
                        #set arrival attribute (by walk in =0 or by ambulance =1)
                        set_attribute(keys="mode of arrival", values=1)%>%
                        
                        #set triage score (informed by arrival attribute)
                        set_attribute(keys="triage score",values=function() sample(size=1,rep(triageX, round(100*ambulanceTriageProb)),replace=T)) %>%
                        set_prioritization(values=function(){
                                TS<-get_attribute(env,"triage score")
                                Prior<-(5-TS)
                                Preemt<-(Prior+1)
                                c(Prior,Preemt,FALSE)
                              })%>%
                        
                        #set fragility attribute for FT (low co-morbidity/age =0 or high-risk =1)
                        set_attribute(keys="fragility",values=function()
                          rbinom(n=1,size=1,prob=0.7))%>%
                      
                        
                        #set dead or alive outcome
                        set_attribute(keys="Patient dies", values=function()ifelse(get_attribute(env, "triage score")==1,rbinom(n=1,size=1,prob=0.212),
                                                                                   ifelse(get_attribute(env, "triage score")==2,rbinom(n=1,size=1,prob=0.09),
                                                                                          ifelse(get_attribute(env, "triage score")==3,rbinom(n=1,size=1,prob=0.01),0))))%>%
                        
                        #set resource usage
                        set_attribute(keys="ED nurses siezed", values=function()
                          ifelse(get_attribute(env, "triage score")<1.1,4,
                                 ifelse(get_attribute(env, "triage score")<2.1,2,1))) %>%
                        
                        set_attribute(keys="ED doctors siezed", values=function()
                          ifelse(get_attribute(env, "triage score")<2, 10,
                                 ifelse(get_attribute(env, "triage score")==2, 2,
                                        ifelse(get_attribute(env, "triage score")>2, 1, 1)))) %>%
                        
                        
                        
                        #death distributions
                        set_attribute(keys="DOA", values=function()
                          ifelse(get_attribute(env,"Patient dies")==1,rbinom(n=1,size=1,prob=0.35),0)) %>%
                        set_attribute(keys="diePreHos", values=function()
                          ifelse(get_attribute(env,"Patient dies")==1,rbinom(n=1,size=1,prob=0.1),0)) %>%
                        set_attribute(keys="dieWaitingRoom", values=function()
                          ifelse(get_attribute(env,"Patient dies")==1,rbinom(n=1,size=1,prob=0.005),0)) %>%
                        set_attribute(keys="dieED", values=function()
                          ifelse(get_attribute(env,"Patient dies")==1,rbinom(n=1,size=1,prob=0.25),0)) %>%
                        set_attribute(keys="dieHosWard", values=function()
                          ifelse(get_attribute(env,"Patient dies")==1,rbinom(n=1,size=1,prob=1),0))%>%
                        

                        #set if death needs coroner
                        set_attribute(keys="Coroner case",function() 
                          ifelse(get_attribute(env, "diePreHos")==1 && get_attribute(env,"DOA")==1, rbinom(n=1,size=1,prob=0.25), 0)) %>%
                        
                        simmer::join(PreHospital)
                      
                      #####          
                      
                      
                      ### Model resources
                      #####

                      env%>%
                        add_resource(name="Ambulance", capacity=nAmbulance, queue_size=(nAmbulance*10),preemptive=F,preempt_order = "lifo") %>%
                        add_resource(name="fire", capacity=nFire, queue_size=(nFire*10),preemptive=F,preempt_order = "lifo") %>%
                        add_resource(name="police", capacity=nPolice, queue_size=(nPolice*5),preemptive=F,preempt_order = "lifo") %>%
                        add_resource(name="coroner", capacity=nCoroner, queue_size=(nCoroner*20),preemptive=F,preempt_order = "lifo") %>%
                        add_resource(name="family with body", capacity=Inf, queue_size=Inf,preemptive=F,preempt_order = "lifo") %>%
                        add_resource(name="911Dispatcher", capacity=nDispatcher, queue_size=(nDispatcher*5),preemptive=F,preempt_order = "lifo")
                        
                      
                      
                      env%>%
                        add_resource(name="Waiting room chair", capacity=nWaitingRoom, queue_size=(nWaitingRoom),preemptive=F,preempt_order = "lifo") %>%
                        add_resource(name="Morgue", capacity=nMorgue, queue_size=Inf,preemptive=F) %>%
                        add_resource(name="Heatwave Morgue", capacity=nMorgue, queue_size=Inf,preemptive=F) %>%
                        add_resource(name = "quarter ED nurse",capacity = nEDnurse, queue_size = Inf,preemptive = F,preempt_order = "lifo") %>%
                        add_resource(name = "Triage nurse",capacity = nTriageNurse, queue_size = Inf,preemptive = F,preempt_order = "lifo") %>%
                        add_resource(name = "ED bed",capacity = nEDbed, queue_size = (nWaitingRoom),preemptive =F,preempt_order = "lifo") %>%
                        add_resource(name = "tenth ED doc",capacity = nEDdoc, queue_size = Inf,preemptive = F,preempt_order = "lifo") %>%
                        add_resource(name="ICU bed", capacity=nICUbed, queue_size=Inf,preemptive=F,preempt_order = "lifo") %>%
                        add_resource(name="Non-ICU bed", capacity=nNonICUbed, queue_size=(nNonICUbed/2),preemptive=F,preempt_order = "lifo")%>%
                        
                        
                        add_resource(name="Fast Track bed", capacity=nFTbeds, queue_size=(nFTbeds*2),preemptive=F,preempt_order = "lifo") %>%
                        add_resource(name="fifth Fast Track nurse", capacity=nFTnurse, queue_size=Inf,preemptive=F,preempt_order = "lifo") %>%
                        add_resource(name="fifteenth Fast Track doc", capacity=nFTdoc, queue_size=Inf,preemptive=F,preempt_order = "lifo")
                      
                      print("ping")
                      
                      #####
                      
                      
                      ##### Heatwave patient generators
                      #####
                                      
   
                      if(HWRun!=0){
                                lapply(1:Ngen,function(i){
                                   PatientName_WI<-paste0("WI_HW patient ",i,"- ")
                                   PatientName_EMS<-paste0("EMS_HW patient ",i,"- ")
                                   
                                      add_generator(env,name_prefix=PatientName_WI,trajectory=HW_WalkInPatient, 
                                                    from_to(start_time=HWStartT[i],stop_time=HWStopT[i], dist=function()sample(HW_WI[[i]],size=1,replace=F),arrive=T),
                                                    priority=0,preemptible=0,restart=F,mon=2)
                                      add_generator(env,name_prefix=PatientName_EMS,trajectory=HW_EMSPatient, 
                                                    from_to(start_time=HWStartT[i],stop_time=HWStopT[i], dist=function()sample(HW_EMS[[i]],size=1,replace=F),arrive=T),
                                                    priority=0,preemptible=0,restart=F,mon=2)
                                 })  
                               } 
              
                      
                      #####
                      
                      ### Non-heatwave patient generators
                      #####
                      
                      if(PopSize>50){
                      
                            env%>%
                              add_generator("patient [0-3]",trajectory=WalkInPatient, 
                                            from_to(t_hour(0),t_hour(3),every = t_day(1), dist=function()sample(WI_0_3,size=1,replace=F),arrive=T),
                                            priority=0,preemptible=0,restart=F,mon=2) %>%
                              
                              add_generator("patient [3-6]",trajectory=WalkInPatient, 
                                            from_to(t_hour(3),t_hour(6),every = t_day(1), dist=function()sample(WI_3_6,size=1,replace=F),arrive=T),
                                            priority=0,preemptible=0,restart=F,mon=2) %>%
                              
                              add_generator("patient [6-9]",trajectory=WalkInPatient, 
                                            from_to(t_hour(6),t_hour(9),every = t_day(1), dist=function()sample(WI_6_9,size=1,replace=F),arrive=T),
                                            priority=0,preemptible=0,restart=F,mon=2) %>%
                              
                              add_generator("patient [9-12]",trajectory=WalkInPatient, 
                                            from_to(t_hour(9),t_hour(12),every = t_day(1), dist=function()sample(WI_9_12,size=1,replace=F),arrive=F),
                                            priority=0,preemptible=0,restart=F,mon=2) %>%
                              
                              add_generator("patient [12-15]",trajectory=WalkInPatient, 
                                            from_to(t_hour(12),t_hour(15),every = t_day(1), dist=function()sample(WI_12_15,size=1,replace=F),arrive=F),
                                            priority=0,preemptible=0,restart=F,mon=2) %>%
                              
                              add_generator("patient [15-18]",trajectory=WalkInPatient, 
                                            from_to(t_hour(15),t_hour(18),every = t_day(1), dist=function()sample(WI_15_18,size=1,replace=F),arrive=F),
                                            priority=0,preemptible=0,restart=F,mon=2) %>%
                              
                              add_generator("patient [18-21]",trajectory=WalkInPatient, 
                                            from_to(t_hour(18),t_hour(21),every = t_day(1), dist=function()sample(WI_18_21,size=1,replace=F),arrive=F),
                                            priority=0,preemptible=0,restart=F,mon=2) %>%
                              
                              add_generator("patient [21-24]",trajectory=WalkInPatient, 
                                            from_to(t_hour(21),t_hour(24),every = t_day(1), dist=function()sample(WI_21_24,size=1,replace=F),arrive=F),
                                            priority=0,preemptible=0,restart=F,mon=2)
                            
                            
                            env%>%
                              
                              add_generator("EMS patient [0-6]",trajectory=EMSPatient, 
                                            from_to(t_hour(0),t_hour(6),every = t_day(1), dist=function()sample(EMS_0_6,size=1,replace=F),arrive=T),
                                            priority=0,preemptible=0,restart=F,mon=2) %>%
                              
                              add_generator("EMS patient [6-9]",trajectory=EMSPatient, 
                                            from_to(t_hour(6),t_hour(9),every = t_day(1), dist=function()sample(EMS_6_9,size=1,replace=F),arrive=T),
                                            priority=0,preemptible=0,restart=F,mon=2) %>%
                              
                              add_generator("EMS patient [9-12]",trajectory=EMSPatient, 
                                            from_to(t_hour(9),t_hour(12),every = t_day(1), dist=function()sample(EMS_9_12,size=1,replace=F),arrive=F),
                                            priority=0,preemptible=0,restart=F,mon=2) %>%
                              
                              add_generator("EMS patient [12-15]",trajectory=EMSPatient, 
                                            from_to(t_hour(12),t_hour(15),every = t_day(1), dist=function()sample(EMS_12_15,size=1,replace=F),arrive=F),
                                            priority=0,preemptible=0,restart=F,mon=2) %>%
                              
                              add_generator("EMS patient [15-18]",trajectory=EMSPatient, 
                                            from_to(t_hour(15),t_hour(18),every = t_day(1), dist=function()sample(EMS_15_18,size=1,replace=F),arrive=F),
                                            priority=0,preemptible=0,restart=F,mon=2) %>%
                              
                              add_generator("EMS patient [18-21]",trajectory=EMSPatient, 
                                            from_to(t_hour(18),t_hour(21),every = t_day(1), dist=function()sample(EMS_18_21,size=1,replace=F),arrive=F),
                                            priority=0,preemptible=0,restart=F,mon=2) %>%
                              
                              add_generator("EMS patient [21-24]",trajectory=EMSPatient, 
                                            from_to(t_hour(21),t_hour(24),every = t_day(1), dist=function()sample(EMS_21_24,size=1,replace=F),arrive=F),
                                            priority=0,preemptible=0,restart=F,mon=2)
                      
                      }else{
                        
                        env%>%
                          add_generator("patient [0-6]",trajectory=WalkInPatient, 
                                        from_to(t_hour(0),t_hour(3),every = t_day(1), dist=function()sample(WI_0_6,size=1,replace=F),arrive=T),
                                        priority=0,preemptible=0,restart=F,mon=2) %>%
                          
                          add_generator("patient [6-12]",trajectory=WalkInPatient, 
                                        from_to(t_hour(6),t_hour(9),every = t_day(1), dist=function()sample(WI_6_12,size=1,replace=F),arrive=T),
                                        priority=0,preemptible=0,restart=F,mon=2) %>%
                          
                          add_generator("patient [12-18]",trajectory=WalkInPatient, 
                                        from_to(t_hour(9),t_hour(12),every = t_day(1), dist=function()sample(WI_12_18,size=1,replace=F),arrive=F),
                                        priority=0,preemptible=0,restart=F,mon=2) %>%
                          
                          add_generator("patient [18-24]",trajectory=WalkInPatient, 
                                        from_to(t_hour(12),t_hour(15),every = t_day(1), dist=function()sample(WI_18_24,size=1,replace=F),arrive=F),
                                        priority=0,preemptible=0,restart=F,mon=2)
                        
                        
                        env%>%
                          
                          add_generator("EMS patient [0-6]",trajectory=EMSPatient, 
                                        from_to(t_hour(0),t_hour(6),every = t_day(1), dist=function()sample(EMS_0_6,size=1,replace=F),arrive=T),
                                        priority=0,preemptible=0,restart=F,mon=2) %>%
                          
                          add_generator("EMS patient [6-12]",trajectory=EMSPatient, 
                                        from_to(t_hour(6),t_hour(9),every = t_day(1), dist=function()sample(EMS_6_12,size=1,replace=F),arrive=T),
                                        priority=0,preemptible=0,restart=F,mon=2) %>%
                          
                          add_generator("EMS patient [12-18]",trajectory=EMSPatient, 
                                        from_to(t_hour(9),t_hour(12),every = t_day(1), dist=function()sample(EMS_12_18,size=1,replace=F),arrive=F),
                                        priority=0,preemptible=0,restart=F,mon=2) %>%
                          
                          add_generator("EMS patient [18-24]",trajectory=EMSPatient, 
                                        from_to(t_hour(12),t_hour(15),every = t_day(1), dist=function()sample(EMS_18_24,size=1,replace=F),arrive=F),
                                        priority=0,preemptible=0,restart=F,mon=2)
                        
                          
                      }
                      
                      # END patient generators
                      #####
        
            
            sim_out<-env%>%
              simmer::run(nRunDays*1440)
                   
            wrap(sim_out)
            
            })
        })
  
  

  
  output$Simmer1<-renderPlot({
    req(Hmodel)
    
    nRunDays<-input$RunTime
    eT<-nRunDays*1440
    breaks<-seq(0,eT,by=1440)
    
    resourceSelect<-input$Resource
    resources<-get_mon_resources(Hmodel())
    
    plot(resources,metric="usage", resourceSelect, steps=T) +
      scale_x_continuous(breaks=breaks)+
      xlab("Model time (in minutes)")+
      theme(plot.background = element_rect(fill = "#F5FBFA")) +
      theme(panel.background = element_rect(fill = "#F5FBFA")) +
      theme(text = element_text(family="Montserrat Medium", color = "#545F66")) +
      theme(plot.title = element_text(family = "Oswald", color = "black", size = 16, hjust = 0),
            plot.title.position = "plot") +
      theme(plot.subtitle = element_text(family = "Oswald Light", color = "#000000", size = 12)) +
      theme(plot.caption = element_text(family = "Montserrat Medium", colour = "#545F66", size = 8, hjust = 0),
            plot.caption.position = "plot") +
      theme(plot.tag = element_text(family = "Oswald", color = "black", size = 10, hjust = 1)) +
      theme(axis.text = element_text(size = 10)) +
      theme(panel.grid.minor.x = element_blank()) +
      theme(axis.line.x = element_line(colour = "#545F66")) +
      theme(axis.line.y = element_blank()) +
      theme(axis.ticks = element_blank()) +
      theme(legend.position = "bottom")+
      theme(legend.key = element_rect(fill = "#F5FBFA", colour = NA))+
      theme(strip.background = element_blank())+
      theme(legend.background = element_rect(fill = "#F5FBFA"))
    
  })
  
  output$TableKPI<-renderReactable({
      req(Hmodel)
      req(input$iterations)
      nI<-input$iterations
    
      Log1<-get_mon_arrivals(Hmodel())
      Log2<-as.data.frame(get_mon_attributes(Hmodel()))
      Log2<-Log2[,2:5]
      colnames(Log2)<-c("name","key","value","replication")
      Log2$value<-as.numeric(Log2$value)
      Log2<-reshape2::dcast(Log2,name+replication~key,mean,fill=NaN)
      Log<-merge(Log1,Log2,by=c("name","replication"))
  
      df<-Log%>%
        arrange(start_time)
      
          for(i in (1:nrow(df))){
              st<-df[i,3]
            if(i!=1){last<-df[(i-1),3]}else{last<-0}
            if(exists("tsince")){
              tsince<-c(tsince,(st-last)*nI)
            }else{
              tsince<-(st-last)*nI
              }
            }
      df$TimeSinceLastPt<-tsince
      df<-df[,c(3,8,11,25,26,32,38,39,40,48,49)]
      
      df_base<-df %>%
        filter(start_time < 18720)
      df_base<-as.data.frame(colMeans(df_base,na.rm = T))
      
      df_HW<-df %>%
        filter(start_time > 18720 & start_time < 28800)
      df_HW<-as.data.frame(colMeans(df_HW,na.rm = T))
      
      DF<-cbind(df_base,df_HW)
      
      DF<-DF%>%
      dplyr::mutate_if(is.numeric, round, digits=2)
      
      DF<-DF[-1,]
      colnames(DF)<-c("Baseline period mean (day 1 to 14)","Heatwave period mean (day 14 to 20)")
      rownames(DF)<-c("Mean ALC time (minutes)","Mean ambulance response time (minutes)","Mean ED boarding time for ICU pts (minutes)","Mean ED boarding time for non-ICU pts (minutes)","Ratio of heatwave to non-heatwave pts","Ratio of pts who die","Mean ICU LOS (minutes)","Mean non-ICU ward LOS (minutes)","Mean triage wait time (minutes)","Mean time between patients (minutes)")
  
  reactable(DF,
            theme=reactableTheme(style=list(fontFamily="Montserrat,sans-serif")),
            fullWidth = T,
            defaultColDef = colDef(align="left", headerStyle=list(background="#b4cbce"),minWidth = 200),
            bordered=F, striped = T, highlight=T,showSortable = T,
            minRows = 1, showPageSizeOptions = T,defaultPageSize=20
            )

  })
  
  output$Simmer2<-renderPlot({
    req(Hmodel)
    
    nRunDays<-input$RunTime
    eT<-nRunDays*1440
    breaks<-seq(0,eT,by=1440)
    
    arrive<-get_mon_arrivals(Hmodel())
    
    plot(arrive,metric="waiting_time")+
      scale_x_continuous(breaks=breaks)+
      xlab("Model time (in minutes)")+
      theme(plot.background = element_rect(fill = "#F5FBFA")) +
      theme(panel.background = element_rect(fill = "#F5FBFA")) +
      theme(text = element_text(family="Montserrat Medium", color = "#545F66")) +
      theme(plot.title = element_text(family = "Oswald", color = "black", size = 16, hjust = 0),
            plot.title.position = "plot") +
      theme(plot.subtitle = element_text(family = "Oswald Light", color = "#000000", size = 12)) +
      theme(plot.caption = element_text(family = "Montserrat Medium", colour = "#545F66", size = 8, hjust = 0),
            plot.caption.position = "plot") +
      theme(plot.tag = element_text(family = "Oswald", color = "black", size = 10, hjust = 1)) +
      theme(axis.text = element_text(size = 10)) +
      theme(panel.grid.minor.x = element_blank()) +
      theme(axis.line.x = element_line(colour = "#545F66")) +
      theme(axis.line.y = element_blank()) +
      theme(axis.ticks = element_blank()) +
      theme(legend.position = "bottom")+
      theme(legend.key = element_rect(fill = "#F5FBFA"))+
      theme(strip.background = element_blank())
    
  })
  
  output$Simmer3<-renderPlot({
    req(Hmodel)
    
    nRunDays<-input$RunTime
    eT<-nRunDays*1440
    breaks<-seq(0,eT,by=1440)
    
    arrival<-get_mon_arrivals(Hmodel())
    plot(arrival,metric="flow_time")+
      scale_x_continuous(breaks=breaks)+
      xlab("Model time (in minutes)")+
      theme(plot.background = element_rect(fill = "#F5FBFA")) +
      theme(panel.background = element_rect(fill = "#F5FBFA")) +
      theme(text = element_text(family="Montserrat Medium", color = "#545F66")) +
      theme(plot.title = element_text(family = "Oswald", color = "black", size = 16, hjust = 0),
            plot.title.position = "plot") +
      theme(plot.subtitle = element_text(family = "Oswald Light", color = "#000000", size = 12)) +
      theme(plot.caption = element_text(family = "Montserrat Medium", colour = "#545F66", size = 8, hjust = 0),
            plot.caption.position = "plot") +
      theme(plot.tag = element_text(family = "Oswald", color = "black", size = 10, hjust = 1)) +
      theme(axis.text = element_text(size = 10)) +
      theme(panel.grid.minor.x = element_blank()) +
      theme(axis.line.x = element_line(colour = "#545F66")) +
      theme(axis.line.y = element_blank()) +
      theme(axis.ticks = element_blank()) +
      theme(legend.position = "bottom")+
      theme(legend.key = element_rect(fill = "#F5FBFA"))+
      theme(strip.background = element_blank())
    
  })
  
  output$Simmer4<-renderPlot({
    req(Hmodel)
    nRunDays<-input$RunTime
    eT<-nRunDays*1440
    breaks<-seq(0,eT,by=1440)
    
    
    attribute<-Hmodel()
    attribute<-get_mon_attributes(Hmodel())
    
    attribute<-attribute%>%
      dplyr::filter(attribute$key %in% c("Ambulance off-load time","Ambulance response time","Door to ED bed wait for highest severity","Door to ED bed wait for lowest severity","Door to ED bed wait for FT bed",
          "Time in ED for lowest severity","Time in ED for middle severity","Time in ED for highest severity","Wait to triage patient","ED boarding time for ICU","ED boarding time for Non-ICU","ALC"))
    
    plot(attribute)+
      scale_x_continuous(breaks=breaks)+
      xlab("Model time (in minutes)")+
      ylab("Minutes")+
      ylim(0,1000)+
      theme(plot.background = element_rect(fill = "#F5FBFA")) +
      theme(panel.background = element_rect(fill = "#F5FBFA")) +
      theme(text = element_text(family="Montserrat Medium", color = "#545F66")) +
      theme(plot.title = element_text(family = "Oswald", color = "black", size = 16, hjust = 0),
            plot.title.position = "plot") +
      theme(plot.subtitle = element_text(family = "Oswald Light", color = "#000000", size = 12)) +
      theme(plot.caption = element_text(family = "Montserrat Medium", colour = "#545F66", size = 8, hjust = 0),
            plot.caption.position = "plot") +
      theme(plot.tag = element_text(family = "Oswald", color = "black", size = 10, hjust = 1)) +
      theme(axis.text = element_text(size = 10)) +
      theme(panel.grid.minor.x = element_blank()) +
      theme(axis.line.x = element_line(colour = "#545F66")) +
      theme(axis.line.y = element_blank()) +
      theme(axis.ticks = element_blank()) +
      theme(legend.position = "bottom")
    
  })
  
  output$Simmer4.5<-renderPlot({
    req(Hmodel)
    nRunDays<-input$RunTime
    eT<-nRunDays*1440
    breaks<-seq(0,eT,by=1440)
    
    
    attribute<-Hmodel()
    attribute<-get_mon_attributes(Hmodel())
    
    attribute<-attribute%>%
      dplyr::filter(attribute$key %in% c("ED boarding time for ICU","ED boarding time for Non-ICU","Patient ICU LOS","Patient Non-ICU ward LOS"))
    
    plot(attribute)+
      scale_x_continuous(breaks=breaks)+
      xlab("Model time (in minutes)")+
      ylab("Minutes")+
      ylim(0,15000)+
      theme(plot.background = element_rect(fill = "#F5FBFA")) +
      theme(panel.background = element_rect(fill = "#F5FBFA")) +
      theme(text = element_text(family="Montserrat Medium", color = "#545F66")) +
      theme(plot.title = element_text(family = "Oswald", color = "black", size = 16, hjust = 0),
            plot.title.position = "plot") +
      theme(plot.subtitle = element_text(family = "Oswald Light", color = "#000000", size = 12)) +
      theme(plot.caption = element_text(family = "Montserrat Medium", colour = "#545F66", size = 8, hjust = 0),
            plot.caption.position = "plot") +
      theme(plot.tag = element_text(family = "Oswald", color = "black", size = 10, hjust = 1)) +
      theme(axis.text = element_text(size = 10)) +
      theme(panel.grid.minor.x = element_blank()) +
      theme(axis.line.x = element_line(colour = "#545F66")) +
      theme(axis.line.y = element_blank()) +
      theme(axis.ticks = element_blank()) +
      theme(legend.position = "bottom")
    
  })
  
  output$Simmer5<-renderPlot({
    req(Hmodel)
    nRunDays<-input$RunTime
    eT<-nRunDays*1440
    breaks<-seq(0,eT,by=1440)
    
    
    attribute<-Hmodel()
    attribute<-get_mon_attributes(Hmodel())
    
    attribute<-attribute%>%
      dplyr::filter(attribute$key %in% c("Ambulances available","Heatwave patient", "Heatwave deaths", "Patients leave AMA", "Total deaths", "SALT?"))
    
    plot(attribute)+
      scale_x_continuous(breaks=breaks)+
      xlab("Model time (in minutes)")+
      ylab("Count")+
      theme(plot.background = element_rect(fill = "#F5FBFA")) +
      theme(panel.background = element_rect(fill = "#F5FBFA")) +
      theme(text = element_text(family="Montserrat Medium", color = "#545F66")) +
      theme(plot.title = element_text(family = "Oswald", color = "black", size = 16, hjust = 0),
            plot.title.position = "plot") +
      theme(plot.subtitle = element_text(family = "Oswald Light", color = "#000000", size = 12)) +
      theme(plot.caption = element_text(family = "Montserrat Medium", colour = "#545F66", size = 8, hjust = 0),
            plot.caption.position = "plot") +
      theme(plot.tag = element_text(family = "Oswald", color = "black", size = 10, hjust = 1)) +
      theme(axis.text = element_text(size = 10)) +
      theme(panel.grid.minor.x = element_blank()) +
      theme(axis.line.x = element_line(colour = "#545F66")) +
      theme(axis.line.y = element_blank()) +
      theme(axis.ticks = element_blank()) +
      theme(legend.position = "bottom")
    
  })

  output$TableText<-renderReactable({
    req(DF_Dist)
    df<-DF_Dist()
    
    reactable(as.data.frame(df),
              theme=reactableTheme(style=list(fontFamily="Montserrat,sans-serif")),
              fullWidth = T,
              defaultColDef = colDef(align="left", headerStyle=list(background="#b4cbce"),minWidth = 200,filterable=T),
              
              bordered=F, striped = T, highlight=T,showSortable = T,
              minRows = 1, showPageSizeOptions = T, pageSizeOptions = c(10,20,50),defaultPageSize=10)
    
  })
  
  output$downloadTable<-downloadHandler(
    filename=function(){
      paste0(input$CatchmentPop,"_",input$HA,".csv")
    },
    content=function(file){
            Log1<-get_mon_arrivals(Hmodel())
            Log2<-as.data.frame(get_mon_attributes(Hmodel()))
            Log2<-Log2[,2:5]
            colnames(Log2)<-c("name","key","value","replication")
            Log2$value<-as.numeric(Log2$value)
            Log2<-reshape2::dcast(Log2,name+replication~key,mean,fill=NaN)
            Log<-merge(Log1,Log2,by=c("name","replication"))
      write.csv(Log,file)
    })
  
#### Distribution panels
      DF_Dist<-reactive({
    req(Hmodel)
    Log1<-get_mon_arrivals(Hmodel())
    Log2<-as.data.frame(get_mon_attributes(Hmodel()))
    Log2<-Log2[,2:5]
    colnames(Log2)<-c("name","key","value","replication")
    Log2$value<-as.numeric(Log2$value)
    Log2<-reshape2::dcast(Log2,name+replication~key,mean,fill=NaN)
    Log<-merge(Log1,Log2,by=c("name","replication"))
    janitor::clean_names(Log)

  })
  
  #Triage score & deaths
      output$Dist1<-renderPlot({
        req(DF_Dist)
        df<-DF_Dist()
        
        #simulation period
        for(i in (1:nrow(df))){
          actTime<-df[i,3]
          if(actTime<18720){v<-"baseline"}else if(actTime>18720 & actTime<28800){v<-"heatwave"}else{v<-"post_heatwave"}
          if(exists("vec")){
            vec<-c(vec,v)
          }else{
            vec<-v
          }
        }
        df$simulation_period<-vec
        
        heatwave_non<-c(
          'baseline' = "Baseline period",
          'heatwave' = "Heatwave period",
          'post_heatwave'="Post heatwave")
        
        ggplot(df, aes(x=triage_score, fill=simulation_period))+
          geom_histogram()+
          facet_wrap(~factor(simulation_period),scales = "free",labeller=as_labeller(heatwave_non))+
          xlab("Triage score") +
          ylab("Count of patients")+
          labs(title="Distribution of CTAS Triage Scores")+
          theme(plot.margin = unit(c(1, 1, .5, 1), "cm")) +
          theme(plot.background = element_rect(fill = "#F5FBFA")) +
          theme(panel.background = element_rect(fill = "#F5FBFA")) +
          theme(text = element_text(family="Montserrat Medium", color = "#545F66")) +
          theme(plot.title = element_text(family = "Oswald", color = "black", size = 16, hjust = 0),
                plot.title.position = "plot") +
          theme(plot.subtitle = element_text(family = "Oswald Light", color = "#000000", size = 12)) +
          theme(plot.caption = element_text(family = "Montserrat Medium", colour = "#545F66", size = 8, hjust = 0),
                plot.caption.position = "plot") +
          theme(plot.tag = element_text(family = "Oswald", color = "black", size = 10, hjust = 1)) +
          theme(axis.text = element_text(size = 10)) +
          theme(panel.grid.minor.x = element_blank()) +
          theme(axis.line.x = element_line(colour = "#545F66")) +
          theme(axis.line.y = element_blank()) +
          theme(axis.ticks = element_blank()) +
          theme(legend.position = "none")+
          theme(strip.background = element_blank())
    
      })
  
  #Triage time by triage level
      output$Dist2<-renderPlot({
        req(DF_Dist)
        df<-DF_Dist()
      ####
        # Edits to df ####
        df$ed_admit_time<-as.numeric(df$ed_admit_time)
        df$ed_arrival_time<-as.numeric(df$ed_arrival_time)
        df<-df %>%
          mutate(ed_door_to_bed_all=rowMeans(.[,c("door_to_ed_bed_wait_for_ft_bed","door_to_ed_bed_wait_for_highest_severity","door_to_ed_bed_wait_for_lowest_severity")],na.rm=T))
        
        df<-df %>%
          mutate(time_in_ed_all=rowMeans(.[,c("time_in_ed_for_highest_severity","time_in_ed_for_lowest_severity","time_in_ed_for_middle_severity")],na.rm=T))
        
        #simulation period
        for(i in (1:nrow(df))){
          actTime<-df[i,3]
          if(actTime<18720){v<-"baseline"}else if(actTime>18720 & actTime<28800){v<-"heatwave"}else{v<-"post_heatwave"}
          if(exists("vec")){
            vec<-c(vec,v)
          }else{
            vec<-v
          }
        }
        df$simulation_period<-vec
        
        #ward status
        for(i in (1:nrow(df))){
          if(!is.na(df[i,34])){ws<-"ICU"
          }else if(!is.na(df[i,37])){ws<-"Non-ICU ward"}else{ws<-"Not admitted to ward"}
          if(exists("vec2")){
            vec2<-c(vec2,ws)
          }else{
            vec2<-ws
          }
        }
        df$ward_status<-as.factor(vec2)
      #####
      
        heatwave_non<-c(
          'baseline' = "Baseline period",
          'heatwave' = "Heatwave period",
          'post_heatwave'="Post heatwave")
        
        ggplot(df, aes(x=triage_score, y=time_in_ed_all))+
          geom_jitter(aes(color=ward_status))+
          facet_wrap(~factor(simulation_period),scales = "fixed",labeller=as_labeller(heatwave_non))+
          xlab("Triage score") +
          ylab("Time in ED (minutes)")+
          labs(title="Distribution of time in ED by triage scores and ward admission status")+
          theme(plot.margin = unit(c(1, 1, .5, 1), "cm")) +
          theme(plot.background = element_rect(fill = "#F5FBFA")) +
          theme(panel.background = element_rect(fill = "#F5FBFA")) +
          theme(text = element_text(family="Montserrat Medium", color = "#545F66")) +
          theme(plot.title = element_text(family = "Oswald", color = "black", size = 16, hjust = 0),
                plot.title.position = "plot") +
          theme(plot.subtitle = element_text(family = "Oswald Light", color = "#000000", size = 12)) +
          theme(plot.caption = element_text(family = "Montserrat Medium", colour = "#545F66", size = 8, hjust = 0),
                plot.caption.position = "plot") +
          theme(plot.tag = element_text(family = "Oswald", color = "black", size = 10, hjust = 1)) +
          theme(axis.text = element_text(size = 10)) +
          theme(panel.grid.minor.x = element_blank()) +
          theme(axis.line.x = element_line(colour = "#545F66")) +
          theme(axis.line.y = element_blank()) +
          theme(axis.ticks = element_blank()) +
          theme(legend.position = "right")+
          theme(strip.background = element_blank())
        
      })
      
  #Ambulance response time by triage level
      output$Dist3<-renderPlot({
    req(DF_Dist)
    df<-DF_Dist()
    ####
    # Edits to df ####
    df$ed_admit_time<-as.numeric(df$ed_admit_time)
    df$ed_arrival_time<-as.numeric(df$ed_arrival_time)
    df<-df %>%
      mutate(ed_door_to_bed_all=rowMeans(.[,c("door_to_ed_bed_wait_for_ft_bed","door_to_ed_bed_wait_for_highest_severity","door_to_ed_bed_wait_for_lowest_severity")],na.rm=T))
    
    df<-df %>%
      mutate(time_in_ed_all=rowMeans(.[,c("time_in_ed_for_highest_severity","time_in_ed_for_lowest_severity","time_in_ed_for_middle_severity")],na.rm=T))
    
    #simulation period
    for(i in (1:nrow(df))){
      actTime<-df[i,3]
      if(actTime<18720){v<-"baseline"}else if(actTime>18720 & actTime<28800){v<-"heatwave"}else{v<-"post_heatwave"}
      if(exists("vec")){
        vec<-c(vec,v)
      }else{
        vec<-v
      }
    }
    df$simulation_period<-vec
    
    #ward status
    for(i in (1:nrow(df))){
      if(!is.na(df[i,34])){ws<-"ICU"
      }else if(!is.na(df[i,37])){ws<-"Non-ICU ward"}else{ws<-"Not admitted to ward"}
      if(exists("vec2")){
        vec2<-c(vec2,ws)
      }else{
        vec2<-ws
      }
    }
    df$ward_status<-as.factor(vec2)
    #####
    
    heatwave_non<-c(
      'baseline' = "Baseline period",
      'heatwave' = "Heatwave period",
      'post_heatwave'="Post heatwave")
    
    ggplot(df, aes(x=as.factor(triage_score),y=ambulance_response_time))+
      geom_jitter(aes())+
      facet_wrap(~factor(simulation_period),scales = "free",labeller=as_labeller(heatwave_non))+
      xlab("Triage score") +
      ylab("Ambulance response time (minutes)")+
      labs(title="Distribution of ambulance response time by triage score and simulation period")+
      theme(plot.margin = unit(c(1, 1, .5, 1), "cm")) +
      theme(plot.background = element_rect(fill = "#F5FBFA")) +
      theme(panel.background = element_rect(fill = "#F5FBFA")) +
      theme(text = element_text(family="Montserrat Medium", color = "#545F66")) +
      theme(plot.title = element_text(family = "Oswald", color = "black", size = 16, hjust = 0),
            plot.title.position = "plot") +
      theme(plot.subtitle = element_text(family = "Oswald Light", color = "#000000", size = 12)) +
      theme(plot.caption = element_text(family = "Montserrat Medium", colour = "#545F66", size = 8, hjust = 0),
            plot.caption.position = "plot") +
      theme(plot.tag = element_text(family = "Oswald", color = "black", size = 10, hjust = 1)) +
      theme(axis.text = element_text(size = 10)) +
      theme(panel.grid.minor.x = element_blank()) +
      theme(axis.line.x = element_line(colour = "#545F66")) +
      theme(axis.line.y = element_blank()) +
      theme(axis.ticks = element_blank()) +
      theme(legend.position = "right")+
      theme(strip.background = element_blank())
    
  })
  
  
  #Time in ED by triage score
  
  #Time in ICU
  
  #Time in non-ICU ward
  
  #Time between patients by time of day
  
  
  #END server
  
})