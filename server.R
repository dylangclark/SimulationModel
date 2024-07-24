### BC health model
# (c) Dylan G. Clark
# 2024



library(shiny)
library(chron)
library(zyp)
library(ggplot2)
library(data.table)
library(plyr)
library(dplyr)
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

# setwd("D:/R/BC/Health/SimulationModel/")

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
    output$IHWInput<-renderUI({radioButtons(inputId = "HWInput",label="Select how you want to model a heatwave",choices=(c("No heatwaves"=0,"Default heatwave (2021 event) *use this for historic run*" = 1)),selected=1,inline=F)})

  
    output$InEDBeds<-renderUI({numericInput(inputId = "nEDBeds",label="Number of ED beds",value=30)})
    output$InEDNurse<-renderUI({numericInput(inputId = "nEDNurse",label="Number of ED nurses",value=7)})
    output$Indoc<-renderUI({numericInput(inputId = "ndoc",label="Number of ED doctors",value=3)})
    output$InTriageNurse<-renderUI({numericInput(inputId = "nTriageNurse",label="Number of ED triage nurses",value=2)})
        ## 21 ICU beds per 100,000 and assuming that 15% of beds are occupied by non-ed admissions
    output$InICUbed<-renderUI({numericInput(inputId = "nICUbed",label="Number of ICU beds",value=8)})
        ## 296 non-ICU acute beds per 100,000 and assuming that 30% of beds are occupied by non-ed admissions and 5% are long-term use
    output$InNonICUbed<-renderUI({numericInput(inputId = "nNonICUbed",label="Number of non-ICU beds",value=107)})   
    output$InFTBeds<-renderUI({numericInput(inputId = "nFTBeds",label="Number of fast-track beds",value=ifelse(input$CatchmentPop==50,0,ifelse(input$CatchmentPop==200,15,(round(input$CatchmentPop*0.06,0)))))})
    output$InFTdocs<-renderUI({numericInput(inputId = "nFTdocs",label="Number of fast-track doctors",value=round(input$CatchmentPop*0.01,0))})
    output$InFTnurse<-renderUI({numericInput(inputId = "nFTnurse",label="Number of fast-track nurses",value=ifelse(input$CatchmentPop==50,0,ifelse(input$CatchmentPop==100,2,(round(input$CatchmentPop*0.02,0)))))})
    output$InPolice<-renderUI({numericInput(inputId = "nPolice",label="Number of police units",value=5)})
    output$InFire<-renderUI({numericInput(inputId = "nFire",label="Number of fire medical units",value=2)})
    output$InAmbulance<-renderUI({numericInput(inputId = "nAmbulance",label="Number of Ambulances",value=12)})
    output$InDispatcher<-renderUI({numericInput(inputId = "nDispatcher",label="Number of 911 dispatchers",value=3)})
    output$InCoroner<-renderUI({numericInput(inputId = "nCoroner",label="Number of coroners",value=1)})
    
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
    
    DefaultHW<-read.csv("data/DefaultHeatwave.csv",header=T)
    
    DHW<-reactive({
      req(input$HWInput)
      req(input$HA)
        if(input$HWInput==0){
          DHW<-NULL
        }else if (input$HWInput==1){
          DHW<-DefaultHW %>%
              dplyr::filter(DefaultHW$HA==input$HA)
        }else {}
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
        HWStart<-(30)*1440
        HWStartTimes<-seq(from=HWStart,length.out=(HWDays*3),by=480)
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
      PopRatio<-(100000/as.numeric(DHW[1,6]))
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
      PopRatio<-(100000/(as.numeric(DHW[1,6])))
      L<-lapply(1:nrow(DHW),function(i){(DHW[i,7:9])})
      L<-as.numeric(unlist(L))
      L<-replace(L, L==0, 0.01)
      l_EMS<-((8*60)/(L*PopRatio))
    })
    
    #####

    
     
  Hmodel<<-eventReactive(input$RunHModel,{
    
    #select how many simulation runs (monte carlos) to run
          nIterations<-reactive(input$iterations)
     
    #create simulation environment         
          env<-mclapply(1:nIterations(),function(i){

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
                      HW_WI<-lapply(1:Ngen,function(i){rnorm(n=10000,mean=l_WI[i],sd=(mean=l_WI[i]*0.001))})
                      HW_EMS<-lapply(1:Ngen,function(i){rnorm(n=10000,mean=l_EMS[i]*.9,sd=(mean=l_EMS[i]*0.001))})
                }else{}
                  
                    
        ### Adaptation options  
                        BinMCI<-reactive(input$mci)      
                        MCI<-if(BinMCI()==TRUE){1}else{0}
                        
                        CoolD<-reactive(input$dischargeCooling)
                        CoolDecanting<-if(CoolD()==TRUE){1}else{0}
                        
                        Ss<-reactive(input$SurgeStaff)
                        SurgeStaff<-if(Ss()==TRUE){1}else{0}
                
                        
          ## Health system resources
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
                      
           ## Run time selection
                        nRunDays<-reactive(input$RunTime)
                        nRunDays<-nRunDays()

                        
                        
           ## Time and calendar functions
                        start_day<-1
                        
                        t_minute<-function(t) as.numeric(t)
                        t_hour<-function(t) t_minute(t) * 60
                        t_day<-function(t) t_hour(t) * 24
                        DayOfWeek<-function(t) t_day(t)+start_day
                        
                        
          ## Triage distributions non-HW patients
                        triageX<-c(1:5)
                        ambulanceTriageProb<-c(0.015,0.25,0.350,0.40,0.001)
                        walkinTriageProb<-c(0.0025,0.165,0.4190,0.3250,0.0727)
                        
          ## Triage distributions HW patients
                        HWambulanceTriageProb<-c(0.14,0.25,0.30,0.33,0.005)
                        HWwalkinTriageProb<-c(0.08,0.15,0.455,0.365,0.3)
                        
                        
                        
                        
        #### Patient set up
                        
               ## Non-HW patients per day
                        
                        dis_WI<-(1.0*100)
                        #+-1%
                        dis_EMS<-(0.36*100)
                        #dis_EMS<-rnorm(n=10000,mean=(0.3233*100),sd=(0.3233*100*0.01))
                        #+-1%

                        
              #Prehospital response timing (if resource is immediately available)
                        
                        tAmbulanceResponse<-10
                        tFireResponse<-7
                        tTransportToHosSiren<-10
                        tTransportToHos<-12
                        
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
                
                  #Walk in non-HW patients
                    WI_0_3<-rnorm(n=10000,mean=(180/(0.0504*dis_WI)),sd=(180/(.0504*dis_WI))*0.01)
                    WI_3_6<-rnorm(n=10000,mean=(180/(0.0336*dis_WI)),sd=(180/(.0336*dis_WI))*0.01 )
                    WI_6_9<-rnorm(n=10000,mean=(180/(0.0858*dis_WI)),sd=(180/(.0858*dis_WI))*0.01)
                    WI_9_12<-rnorm(n=10000,mean=(180/(0.1821*dis_WI)),sd=(180/(.1821*dis_WI))*0.01)
                    WI_12_15<-rnorm(n=10000,mean=(180/(0.1900*dis_WI)),sd=(180/(.1900*dis_WI))*0.01)
                    WI_15_18<-rnorm(n=10000,mean=(180/(0.1769*dis_WI)),sd=(180/(.1769*dis_WI))*0.01)
                    WI_18_21<-rnorm(n=10000,mean=(180/(0.1647*dis_WI)),sd=(180/(.1647*dis_WI))*0.01)
                    WI_21_24<-rnorm(n=10000,mean=(180/(0.1164*dis_WI)),sd=(180/(.1164*dis_WI))*0.01)
                   
                  #EMS arrival non-HW patients
                    EMS_0_6<-rnorm(n=10000,mean=(180/(0.0798*dis_EMS)),sd=(180/(.0798*dis_EMS))*0.01)
                    EMS_6_9<-rnorm(n=10000,mean=(180/(0.07850*dis_EMS)),sd=(180/(.0785*dis_EMS))*0.01)
                    EMS_9_12<-rnorm(n=10000,mean=(180/(0.1469*dis_EMS)),sd=(180/(.1469*dis_EMS))*0.01)
                    EMS_12_15<-rnorm(n=10000,mean=(180/(0.1703*dis_EMS)),sd=(180/(.1703*dis_EMS))*0.01)
                    EMS_15_18<-rnorm(n=10000,mean=(180/(0.1686*dis_EMS)),sd=(180/(.1686*dis_EMS))*0.01)
                    EMS_18_21<-rnorm(n=10000,mean=(180/(0.1474*dis_EMS)),sd=(180/(.1474*dis_EMS))*0.01)
                    EMS_21_24<-rnorm(n=10000,mean=(180/(0.1287*dis_EMS)),sd=(180/(.1287*dis_EMS))*0.01)
              
                
                
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
                          set_attribute("IPAWait",function()as.numeric(as.numeric(now(env))-get_attribute(env,"ED arrival time")))%>%
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
                        set_prioritization(c(6,7,FALSE))%>%
                        seize("quarter ED nurse", 4)%>%
                        branch(option=function() ifelse(get_attribute(env, "SALT?")==0,1,2),continue=c(FALSE,FALSE),
                               trajectory("Run code blue") %>%
                                 seize("quarter ED nurse",4)%>%
                                 seize("tenth ED doc",10)%>%
                                 timeout(25)%>%
                                 release("quarter ED nurse",4)%>%
                                 release_all("tenth ED doc")%>%
                                 seize("Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==0,1,0))%>%
                                 seize("Heatwave Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==1,1,0))%>%
                                 set_attribute("Heatwave deaths",function()get_server_count(env,"Heatwave Morgue"))%>%
                                 set_attribute("Total deaths",function()get_server_count(env,"Morgue")+get_server_count(env,"Heatwave Morgue"))%>%
                                 timeout(5)%>%
                                 release_all("quarter ED nurse")%>%
                                 release_all("tenth ED doc")%>%
                                 release_all("ED bed"),
                               
                               trajectory("MCI1")%>%
                                 timeout(5)%>%
                                 release_all("quarter ED nurse") %>%
                                 release_all("tenth ED doc")%>%
                                 seize("Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==0,1,0))%>%
                                 seize("Heatwave Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==1,1,0))%>%
                                 set_attribute("Heatwave deaths",function()get_server_count(env,"Heatwave Morgue"))%>%
                                 set_attribute("Total deaths",function()get_server_count(env,"Morgue")+get_server_count(env,"Heatwave Morgue"))%>%
                                 release_all("ED bed")
                               )
                        

                        CodeBlue2<-trajectory() %>%
                          set_prioritization(c(6,7,FALSE))%>%
                          seize("Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==0,1,0))%>%
                          seize("Heatwave Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==1,1,0))%>%
                          set_attribute("Heatwave deaths",function()get_server_count(env,"Heatwave Morgue"))%>%
                          set_attribute("Total deaths",function()get_server_count(env,"Morgue")+get_server_count(env,"Heatwave Morgue"))%>%
                          release_all("ED bed")
                      #####
                      
                      
                      ####### Hospital trajectory (ED and acute wards)
                      #####
                        
                        EDAcute<-trajectory()%>%  

                          
                            ### patient dies in ED?
                                    branch(option=function() 
                                      ifelse(get_attribute(env, "Patient dies")>0 & get_attribute(env, "dieED")==1,1,0), continue=FALSE,
                                      trajectory("branch1")%>%
                                        release_all("ED bed")%>%
                                        release_all("Waiting room chair")%>%
                                        simmer::join(CodeBlue)) %>%
                          
                          ##Set prioritization
                                set_prioritization(values=function(){
                                  TS<-get_attribute(env,"triage score")
                                  Prior<-(5-TS)
                                  Preempt<-(Prior+1)
                                  c(Prior,Preempt,FALSE)
                                })%>%
                          
                          ### Set number of ED hallway beds
                                set_capacity("ED bed", value=function()ifelse(get_capacity(env,"ED bed")>nEDbed & get_seized(env, "ED bed")<(nEDbed-4),nEDbed,get_capacity(env,"ED bed")))%>%
                                set_capacity("ED bed", value=function()ifelse(get_queue_count(env,"ED bed")>30 & get_capacity(env,"ED bed")==nEDbed,(nEDbed+10),nEDbed))%>%
                          
                          ### Start pt assessment
                                      #The higher the number, the higher the priority. Prempt is the minimum priority number that can prempt this patient
                                set_prioritization(values=function(){
                                  TS<-get_attribute(env,"triage score")
                                  if(TS==5){Prior<-3}else{Prior<-(5-TS)}
                                  Preempt<-(Prior+1)
                                  c(Prior,Preempt,FALSE)
                                })%>%
                                    timeout(function()ifelse(get_attribute(env,"triage score")>3.9,rpois(n=1,lambda=8),
                                                             ifelse(get_attribute(env,"triage score")==3,rpois(n=1,lambda=7),
                                                                    ifelse(get_attribute(env,"triage score")==2,rpois(n=1,lambda=1),0)))) %>%
                          log_("seizing nurse", tag="EDNurseSeize")%>%          
                          seize("quarter ED nurse",function() get_attribute(env,"ED nurses siezed"))%>%

                                    timeout(function()ifelse(get_attribute(env,"triage score")>3.9,rpois(n=1,lambda=5),
                                                             ifelse(get_attribute(env,"triage score")==3,rpois(n=1,lambda=9),
                                                                    ifelse(get_attribute(env,"triage score")==2,rpois(n=1,lambda=2),0)))) %>%
                          
                          log_("waiting on dr",tag="EDDrSeize")%>%
                          seize("tenth ED doc",5)%>%
                          set_attribute("IPAWait",function()as.numeric(as.numeric(now(env))-get_attribute(env,"ED arrival time")))%>%        
                          log_("w/ doc for initial assessment")%>% 
                          timeout(function()ifelse(get_attribute(env,"triage score")>3.9,rpois(n=1,lambda=5),
                                                             ifelse(get_attribute(env,"triage score")==3,rpois(n=1,lambda=12),
                                                                    ifelse(get_attribute(env,"triage score")==2,rpois(n=1,lambda=10),10)))) %>%
                            
                               release_all("tenth ED doc",tag="DrMonitoring")%>%
                                    seize("tenth ED doc",function() get_attribute(env,"ED doctors seized"))%>%
                                        
                                release_all("quarter ED nurse")%>%
                                seize("quarter ED nurse",1)%>%
                             
                          ### Waiting on labs and imaging for tx
                          log_("waiting on labs")%>%
                                     set_prioritization(values=function(){
                                      TS<-get_attribute(env,"triage score")
                                      if(TS==5){Prior<-2}else{Prior<-(5-TS)}
                                      Preempt<-(Prior+1)
                                      c(Prior,Preempt,FALSE)
                                    })%>%
                                
                                    timeout(function()ifelse(get_attribute(env,"triage score")==5,0,
                                                             ifelse(get_attribute(env,"triage score")==4,rpois(n=1,lambda=7)^1.2,
                                                                    ifelse(get_attribute(env,"triage score")==3,rpois(n=1,lambda=12)^1.5,
                                                                           ifelse(get_attribute(env,"triage score")==2,rpois(n=1,lambda=30)^1.2,
                                                                                  ifelse(get_attribute(env,"triage score")==1,rpois(n=1,lambda=7)^1.5,0)))))) %>%
                                    release_all("tenth ED doc")%>%
                                    release_all("quarter ED nurse")%>%
                                    seize("quarter ED nurse",function() get_attribute(env,"ED nurses siezed"))%>%
                          
                          
                          ###

                          log_("doc treating pt",tag="EDDrTx")%>%
                          ### Start pt tx
                                    seize("tenth ED doc",8)%>%
                                    timeout(function()ifelse(get_attribute(env,"triage score")==5,rpois(n=1,lambda=4)^1.2,
                                                              ifelse(get_attribute(env,"triage score")==4,rpois(n=1,lambda=7)^1.1,
                                                                ifelse(get_attribute(env,"triage score")==3,rpois(n=1,lambda=7)^1.3,
                                                                    ifelse(get_attribute(env,"triage score")==2,rpois(n=1,lambda=7)^1.7,0))))) %>%
                                    release_all("tenth ED doc")%>%
                          log_("doc done txing")%>%
                                       set_prioritization(values=function(){
                                        TS<-get_attribute(env,"triage score")
                                        Prior<-(5-TS)
                                        Preemt<-(Prior+1)
                                        c(Prior,Preemt,FALSE)
                                      })%>%
                          
                          ### Cooling HW patients
                          timeout(function()ifelse(get_attribute(env,"triage score")<2.1 & get_attribute(env,"Heatwave patient")==1 & CoolDecanting==0 ,rnorm(n=1,mean=5),0))%>%
                          timeout(function()ifelse(get_attribute(env,"triage score")==3 & get_attribute(env,"Heatwave patient")==1 & CoolDecanting==0 ,rnorm(n=1,mean=120),0))%>%
                          timeout(function()ifelse(get_attribute(env,"triage score")>3 & get_attribute(env,"Heatwave patient")==1 & CoolDecanting==0 ,rnorm(n=1,mean=90),0))%>%
                          
                          timeout(function()ifelse(get_attribute(env,"triage score")<2.1 & get_attribute(env,"Heatwave patient")==1 & CoolDecanting==1 ,rnorm(n=1,mean=5),0))%>%
                          timeout(function()ifelse(get_attribute(env,"triage score")==3 & get_attribute(env,"Heatwave patient")==1 & CoolDecanting==1 ,rnorm(n=1,mean=45),0))%>%
                          timeout(function()ifelse(get_attribute(env,"triage score")>3 & get_attribute(env,"Heatwave patient")==1 & CoolDecanting==1 ,rnorm(n=1,mean=5),0))%>%
                          
                          log_("discharging/admitting")%>%
                          
                          ### ED transfer/discharge
                                    branch(option=function() ifelse(get_attribute(env, "triage score")==1,1,0), continue=FALSE,
                                           trajectory("ICU")%>%
                                             set_attribute("ED Boarding (start clock)",function()as.numeric(as.numeric(now(env))-45))%>%
                                             log_("going to ICU",tag="ICUAdmit")%>%
                                             timeout(function()ifelse(get_attribute(env,"Heatwave patient")==1 & CoolDecanting==1 ,rpois(n=1,lambda=9)^1.1,0))%>%
                                             seize("tenth ED doc",2)%>%
                                             timeout(function()sample(size=1,rgamma(n=1000,shape=4,scale=50),replace=T))%>%
                                             set_capacity("ICU bed", value=function()ifelse(get_capacity(env,"ICU bed")>nICUbed & get_seized(env, "ICU bed")<(nICUbed-4),nICUbed,get_capacity(env,"ICU bed")))%>%
                                             set_capacity("ICU bed", value=function()ifelse(get_queue_count(env,"ICU bed")>1 & get_capacity(env,"ICU bed")==nICUbed,(nICUbed+5),nICUbed))%>%
                                             
                                             
                                             log_("going to ICU",tag="ICUAdmit2")%>%
                                             seize("ICU bed",1)%>%
                                             set_attribute("ICU admit time",function()as.numeric(as.numeric(now(env))))%>%
                                                      
                                             
                                             release_all("tenth ED doc")%>%
                                             release_all("quarter ED nurse")%>%
                                             release_all("ED bed")%>%
                                             set_attribute("Time in ED for highest severity",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ED admit time")))))%>%
                                             set_attribute("ED boarding time for ICU",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ED Boarding (start clock)")))))%>%
                                             set_attribute("X_ED boarding time",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ED Boarding (start clock)")))))%>%
                                             branch(option=function()
                                               ifelse(get_attribute(env, "Patient dies")>0 & get_attribute(env, "dieHosWard")==1,1,0), continue=FALSE,
                                               trajectory()%>%
                                                 timeout(function()rpois(n=1,lambda=240))%>%
                                                 release_all("ICU bed")%>%
                                                 simmer::join(CodeBlue2))%>%
                                             timeout(function()sample(size=1,rgamma(n=1000,shape=4,scale=1100),replace=F)) %>%
                                             log_("ICU stepdown",tag="ICUdischarge")%>%
                                             seize("Non-ICU bed",1)%>%
                                             set_attribute("Non-ICU admit time",function()as.numeric(as.numeric(now(env))))%>%
                                             set_attribute("Patient ICU LOS",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ICU admit time")))))%>%
                                             release_all("ICU bed")%>%
                                             timeout(function()sample(size=1,rgamma(n=1000,shape=2.5,scale=2500))) %>%
                                             renege_in(t=function()ifelse(rbinom(n=1,size=1,prob=0.05)==1,sample(size=1,replace=F,x=rgamma(n=1000,shape=1.8,scale=1000)),Inf),keep_seized = FALSE,
                                                       out=trajectory()%>%
                                                         set_attribute("Patients leave AMA",1)%>%
                                                         set_attribute("Patient Non-ICU ward LOS",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"Non-ICU admit time")))))%>%
                                                         release_all("Non-ICU bed"))%>%
                                             
                                             set_attribute("Patient Non-ICU ward LOS",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"Non-ICU admit time")))))%>%
                                             set_attribute("ALC start",function()as.numeric(as.numeric(now(env))))%>%
                                             timeout(function()sample(size=1,rgamma(n=1000,shape=2,scale=900)))%>%
                                             release_all("Non-ICU bed")%>%
                                             set_attribute("ALC",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ALC start")))))
                                           
                                    )%>%
                                    
                                    branch(option=function() ifelse(get_attribute(env, "triage score")==2,1,0),continue=FALSE,
                                          trajectory("Non-ICU")%>%
                                             set_attribute("ED Boarding (start clock)",function()as.numeric(as.numeric(now(env))-45))%>%
                                             timeout(function()ifelse(get_attribute(env,"Heatwave patient")==1 & CoolDecanting==1 ,rpois(n=1,lambda=120)^1.1,0))%>%
                                             release_all("tenth ED doc")%>%
                                             release("quarter ED nurse",1)%>%
                                             timeout(function()sample(size=1,rgamma(n=1000,shape=3,scale=250)))%>%
                                             set_capacity("Non-ICU bed", value=function()ifelse(get_capacity(env,"Non-ICU bed")>nNonICUbed & get_seized(env, "Non-ICU bed")<(nNonICUbed-25),nNonICUbed,get_capacity(env,"Non-ICU bed")))%>%
                                             set_capacity("Non-ICU bed", value=function()ifelse(get_queue_count(env,"Non-ICU bed")>3 & get_capacity(env,"Non-ICU bed")==nNonICUbed,(nNonICUbed+20),nNonICUbed))%>%
                                             log_("Non-ICU admit",tag="NonICUAdmit")%>%
                                             seize("Non-ICU bed",1)%>%
                                             set_attribute("Non-ICU admit time",function()as.numeric(as.numeric(now(env))))%>%
                                             release_all("tenth ED doc")%>%
                                             release_all("quarter ED nurse")%>%
                                             release_all("ED bed")%>%
                                             set_attribute("Time in ED for highest severity",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ED admit time")))))%>%
                                             set_attribute("ED boarding time for Non-ICU",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ED Boarding (start clock)")))))%>%
                                             set_attribute("X_ED boarding time",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ED Boarding (start clock)")))))%>% 
                                             branch(option=function()
                                               ifelse(get_attribute(env, "Patient dies")>0 & get_attribute(env, "dieHosWard")==1,1,0), continue=FALSE,
                                               trajectory()%>%
                                                 timeout(function()rnorm(n=1,mean=120))%>%
                                                 set_attribute("Patient Non-ICU ward LOS",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"Non-ICU admit time"))))+3600)%>%
                                                 release_all("Non-ICU bed")%>%
                                                 simmer::join(CodeBlue2)) %>%
                                             renege_in(t=function()ifelse(rbinom(n=1,size=1,prob=0.05)==1,sample(size=1,replace=F,x=rgamma(n=1000,shape=1.8,scale=1000)),Inf),keep_seized = FALSE,
                                                      out=trajectory()%>%
                                                        set_attribute("Patients leave AMA",1)%>%
                                                        set_attribute("Patient Non-ICU ward LOS",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"Non-ICU admit time")))))%>%
                                                        release_all("Non-ICU bed"))%>%
                                             timeout(function()sample(size=1,rgamma(n=1000,shape=2.5,scale=2000),replace=F)) %>%
                                             set_attribute("ALC start",function()as.numeric(as.numeric(now(env))))%>%
                                             timeout(function()sample(size=1,rgamma(n=1000,shape=4,scale=250),replace=F))%>%
                                             release_all("Non-ICU bed")%>%
                                             set_attribute("Patient Non-ICU ward LOS",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"Non-ICU admit time"))+3500)))%>%
                                             set_attribute("ALC",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ALC start")))))
                                    ) %>%
                                    
                                    branch(option=function() ifelse(get_attribute(env, "triage score")==3,1,0), continue=FALSE,
                                             trajectory("ED discharge")%>%
                                             log_("discharging (triage score 3)")%>%  
                                               release_all("tenth ED doc")%>%
                                               release_all("quarter ED nurse")%>%
                                               release_all("ED bed")%>%
                                               set_attribute("Time in ED for middle severity",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ED admit time"))))))%>%
                                     
                                    
                                      log_("discharging (triage score 4 or 5)")%>%         
                                     release_all("tenth ED doc")%>%
                                     release_all("quarter ED nurse")%>%
                                     release_all("ED bed")%>%
                                     set_attribute("Time in ED for lowest severity",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ED admit time")))))
                                           
                                                
                                      
                        
                      #####
                        
                      
                      ### Hospital arrival
                      #####
                      
                          Hospital<-trajectory("Hospital") %>%
                            
                     ### Patient arrival
                                  log_("ED arrival")%>%
    
                          handle_unfinished(trajectory()%>%
                                              release_all("ED bed")%>%
                                              release_all("quarter ED nurse")%>%
                                              release_all("tenth ED doc")%>%
                                              release_all("Waiting room chair")%>%
                                              release_all("Ambulance")%>%
                                              release_all("ICU bed")%>%
                                              release_all("Non-ICU bed")%>%
                                              simmer::join(EDAcute))%>%
                          
                          
                                  set_attribute("Patients leave AMA",0)%>%
                                    #handle_unfinished(
                                      #trajectory() %>%
                                      #log_("resolving unfinished")%>%
                                      #timeout(1440))%>%
                                  set_attribute("ED arrival time",function()as.numeric(now(env)))%>%
                                  branch(option=function()ifelse(get_attribute(env, "mode of arrival")==0,1,0), continue=TRUE,
                                    trajectory()%>%
                                        log_("triaging patient",tag="TriageNurse")%>%
                                        seize("Triage nurse", 1)%>%
                                        timeout(function()rnorm(1,mean=11,sd=3))%>%
                                        release_all("Triage nurse"))%>%
                                    timeout(function()rnorm(1,mean=7,sd=1.5))%>%
                                    set_attribute(keys="Wait to triage patient",function()as.numeric(as.numeric(now(env)-(get_attribute(env,"ED arrival time")))))%>%
                                    log_("patient triaged")%>%
                    ### Test if die in waiting room
                                  branch(option=function() 
                                    ifelse(get_attribute(env, "Patient dies")>0 & get_attribute(env, "dieWaitingRoom")==1,1,0), continue=FALSE,
                                    trajectory()%>%
                                      log_("patient dies in waiting room")%>%
                                      release_all("Ambulance")%>%
                                      release_all("Waiting room chair")%>%
                                      set_attribute("Ambulance off-load time",function()ifelse(get_attribute(env,"mode of arrival")==1,as.numeric(as.numeric(now(env)-(get_attribute(env,"ED arrival time")))),0))%>%
                                      simmer::join(CodeBlue)) %>%
                                  ####
                                  log_("need chair", tag="WaitingRoom")%>%
                                  seize("Waiting room chair",1)%>%
                                  
                                  renege_in(t=function()ifelse(get_attribute(env,"mode of arrival")==0 & get_attribute(env, "triage score")>3.9, sample(size=1,replace=T,x=rgamma(n=1000,shape=1.8,scale=200)),Inf),keep_seized = FALSE,
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
                                  log_("ready for bed", tag="EDAdmit")%>%
                                  seize("ED bed",1)%>%
                                                ### Test if die in ED
                                  renege_abort()%>%
                                  log_("admitted to ED")%>%
                                  set_attribute("Patients leave AMA",0)%>%
                                  set_attribute("ED admit time",function()as.numeric(now(env)))%>%
                                  release_all("Waiting room chair")%>%
                                  set_attribute("Door to ED bed wait for highest severity",function()ifelse(get_attribute(env,"triage score")<2.1,as.numeric(as.numeric(now(env)-(get_attribute(env,"ED arrival time")))),NA))%>%
                                  set_attribute("Door to ED bed wait for lowest severity",function()ifelse(get_attribute(env,"triage score")>3.9,as.numeric(as.numeric(now(env)-(get_attribute(env,"ED arrival time")))),NA))%>%
                                  release_all("Ambulance")%>%
                                  set_attribute("Ambulance off-load time",function()ifelse(get_attribute(env,"mode of arrival")==1,as.numeric(as.numeric(now(env)-(get_attribute(env,"ED arrival time")))),NA))%>%
                          simmer::join(EDAcute)
                          
                      #####
                      
                      
                      ### Pre-Hospital trajectory
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
                                                 timeout(function()rnorm(n=1,mean=240))%>%
                                                 seize("Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==0,1,0))%>%
                                                 seize("Heatwave Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==1,1,0))%>%
                                                 set_attribute("Heatwave deaths",function()get_server_count(env,"Heatwave Morgue"))%>%
                                                 set_attribute("Total deaths",function()get_server_count(env,"Morgue")+get_server_count(env,"Heatwave Morgue"))%>%
                                                 release_all("coroner"))%>%
                                         log_("DOA")%>% 
                                         seize("police",1)%>%
                                         release_all("fire")%>%
                                         timeout(60)%>%
                                          log_("")%>%
                                         seize("coroner",1)%>%
                                         release_all("police")%>%
                                         timeout(function()rnorm(n=1,mean=240))%>%
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
                                            log_("")%>%
                                          seize("police",1)%>%
                                          timeout(45)%>%
                                          release_all("police")%>%
                                          seize("coroner",1)%>%
                                          timeout(function()rnorm(n=1,mean=120))%>%
                                           seize("Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==0,1,0))%>%
                                           seize("Heatwave Morgue",function()ifelse(get_attribute(env,"Heatwave patient")==1,1,0))%>%
                                           set_attribute("Heatwave deaths",function()get_server_count(env,"Heatwave Morgue"))%>%
                                           set_attribute("Total deaths",function()get_server_count(env,"Morgue")+get_server_count(env,"Heatwave Morgue"))%>%
                                          release_all("coroner"),
                                                 
                                  trajectory("Die prehos w/ provider and MCI")%>%
                                          simmer::select(resources=FirstResponder,policy="first-available",id=0)%>%
                                          log_("")%>%
                                          seize_selected(amount=1,id=0)%>%
                                          log_("ambulance")%>%
                                          seize("Ambulance",amount=function()ifelse(get_selected(env,id=0)=="Ambulance",0,1))%>%
                                          timeout(function()rnorm(n=1, mean=tAmbulanceResponse,sd=2))%>%
                                          set_attribute("Ambulance response time",function() as.numeric(as.numeric(now(env)-(get_attribute(env,"911 call time")))))%>%
                                          timeout(function()rnorm(n=1, mean=10,sd=3))%>%
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
                                          log_("ambulance")%>%
                                          seize("Ambulance",amount=function()ifelse(get_selected(env,id=0)=="Ambulance",0,1))%>%
                                          timeout(function()rnorm(n=1, mean=tAmbulanceResponse,sd=1))%>%
                                          set_attribute("Ambulance response time",function() as.numeric(as.numeric(now(env)-(get_attribute(env,"911 call time")))))%>%
                                          timeout(function()rnorm(n=1, mean=10,sd=3))%>%
                                          release_all("fire")%>%
                                          timeout(function()rnorm(n=1, mean=tTransportToHosSiren,sd=2))%>%
                                           set_prioritization(values=function(){
                                             TS<-get_attribute(env,"triage score")
                                             Prior<-(6-TS)
                                             Preempt<-(Prior+1)
                                             c(Prior,Preempt,FALSE)
                                           })%>%
                                          simmer::join(Hospital),
                                          
                                 trajectory("1B High acuity not dead")%>%
                                   #log_("not dead but sick")%>%
                                   simmer::select(FirstResponder,policy="first-available",id=1)%>%
                                    log_("")%>%
                                   seize_selected(amount=1,id=1)%>%
                                   seize("Ambulance",amount=function()ifelse(get_selected(env,id=1)=="Ambulance",0,1))%>%
                                  timeout(function()ifelse(get_attribute(env,"triage score")==1,rnorm(n=1, mean=4,sd=.5),rnorm(n=1,mean=tAmbulanceResponse,sd=1)))%>%
                                    log_("")%>%
                                   set_attribute("Ambulance response time",function() as.numeric(as.numeric(now(env)-(get_attribute(env,"911 call time")))))%>%
                                   release_all("fire")%>%
                                   timeout(function()rnorm(n=1, mean=tTransportToHosSiren,sd=2))%>%
                                   set_prioritization(values=function(){
                                     TS<-get_attribute(env,"triage score")
                                     Prior<-(6-TS)
                                     Preempt<-(Prior+1)
                                     c(Prior,Preempt,FALSE)
                                   })%>%
                                   simmer::join(Hospital))
                      
                      
                      
                      LowAcuity<-trajectory()%>%
                        #log_("not too sick")%>%
                        log_("ambulance")%>%
                        seize("Ambulance",amount=1)%>%
                        timeout(function()ifelse(get_attribute(env,"triage score")==5,rnorm(n=1, mean=12,sd=3),
                                                 ifelse(get_attribute(env,"triage score")==4,rnorm(n=1, mean=12,sd=2),
                                                        ifelse(get_attribute(env,"triage score")==3,rnorm(n=1, mean=8,sd=2),rnorm(n=1, mean=tAmbulanceResponse,sd=2))))) %>%
                        
                        set_attribute("Ambulance response time",function() as.numeric(as.numeric(now(env)-(get_attribute(env,"911 call time")))))%>%
                        release_all("fire")%>%
                        timeout(function()rnorm(n=1, mean=tTransportToHos,sd=2))%>%
                        set_prioritization(values=function(){
                          TS<-get_attribute(env,"triage score")
                          Prior<-(5-TS)
                          Preempt<-(Prior+1)
                          c(Prior,Preempt,FALSE)
                        })%>%
                        simmer::join(Hospital)
                      
                      
                      
                      PreHospital<-trajectory() %>%
                        ### Patient arrival
                        #log_("911 call")%>%
                        set_attribute("911 call time",function()as.numeric(now(env)))%>%
                        seize("911Dispatcher",1)%>%
                        timeout(function()ifelse(get_attribute(env,"triage score")>2.1,rnorm(n=1, mean=5,sd=.5),rnorm(n=1,mean=1.5,sd=0.1)))%>%
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
                        
                        # MCI protocol on (automatic) or off
                        set_attribute(keys="SALT?", values=function()ifelse(MCI==1 & (get_queue_count(env,"ED bed")/get_capacity(env,"ED bed"))>0.9,1,
                                                                            ifelse(MCI==1 & (get_queue_count(env,"Ambulance")/get_capacity(env,"Ambulance"))>1.5,1,0))) %>%
                        set_attribute(keys="Hour_of_day",values=function() floor((now(env) %% 1440)/60)) %>%

                        
                        #set non-heatwave attribute
                        set_attribute(keys="Heatwave patient", values=1)%>%
                        
                        #set arrival attribute (by walk in =0 or by ambulance =1)
                        set_attribute(keys="mode of arrival", values=0)%>%
                        
                        #set triage score (informed by arrival attribute)
                        set_attribute(keys="triage score",values=function() sample(size=1,x=triageX, prob=HWwalkinTriageProb,replace=F)) %>%
                        set_prioritization(values=function(){
                          TS<-get_attribute(env,"triage score")
                          Prior<-(5-TS)
                          Preempt<-(Prior+1)
                          c(Prior,Preempt,FALSE)
                        })%>%
                        
                        #set fragility attribute for FT (low co-morbidity/age =0 or high-risk =1)
                        set_attribute(keys="fragility",values=function()
                          rbinom(n=1,size=1,prob=0.35))%>%
                        
                        #set resource usage
                        set_attribute(keys="ED nurses siezed", values=function()
                          ifelse(get_attribute(env, "triage score")==1,4,
                                 ifelse(get_attribute(env, "triage score")==2,2,
                                        ifelse(get_attribute(env, "triage score")>2.1,1,1)))) %>%
                        
                        set_attribute(keys="ED doctors seized", values=function()
                          ifelse(get_attribute(env, "triage score")==1, 8,
                                 ifelse(get_attribute(env, "triage score")==2, 5,
                                    ifelse(get_attribute(env, "triage score")==3, 2,
                                        ifelse(get_attribute(env, "triage score")==4, 1,
                                               ifelse(get_attribute(env, "triage score")==5, 0,)))))) %>%
                        
                        #set dead or alive outcome
                        set_attribute(keys="Patient dies",0)%>%
                        #set_attribute(keys="Patient dies", values=function() rbinom(n=1,size=1,prob=0.35)) %>%

                        set_attribute(keys="Patient dies", values=function()ifelse(get_attribute(env, "triage score")==1,rbinom(n=1,size=1,prob=0.4),
                                                                                   ifelse(get_attribute(env, "triage score")==2,rbinom(n=1,size=1,prob=0.04),
                                                                                          ifelse(get_attribute(env, "triage score")==3,rbinom(n=1,size=1,prob=0.009),
                                                                                                 ifelse(get_attribute(env, "triage score")==4,rbinom(n=1,size=1,prob=0.005),0)))))%>%
                        set_attribute(keys="Patient dies",value=function()ifelse(get_attribute(env,"Patient dies")>0,1,0))%>%
                        
                        
                        #death distributions
                        set_attribute(keys="dieWaitingRoom", values=function()
                          ifelse(get_attribute(env,"Patient dies")>0,rbinom(n=1,size=1,prob=0.05),0)) %>%
                        set_attribute(keys="dieED", values=function()
                          ifelse(get_attribute(env,"Patient dies")>0,rbinom(n=1,size=1,prob=0.35),0)) %>%
                        set_attribute(keys="dieHosWard", values=function()
                          ifelse(get_attribute(env,"Patient dies")>0,rbinom(n=1,size=1,prob=1),0))%>%
                        
                        #log_(function() paste0("ED bed queue: ",as.character(get_queue_count(hospital,"ED bed"))))%>%
                        #log_(function() paste0("Triage score: ",as.character(get_attribute(hospital,"triage score"))))
                        

                        simmer::join(Hospital)
                      
                      
                      HW_EMSPatient<-trajectory() %>%
                        # MCI protocol on (automatic) or off
                        set_attribute(keys="SALT?", values=function()ifelse(MCI==1 & (get_queue_count(env,"ED bed")/get_capacity(env,"ED bed"))>0.9,1,
                                                                      ifelse(MCI==1 & (get_queue_count(env,"Ambulance")/get_capacity(env,"Ambulance"))>1.5,1,0))) %>%
                        
                        set_attribute(keys="Hour_of_day",values=function() floor((now(env) %% 1440)/60)) %>%
                        
                        #set non-heatwave attribute
                        set_attribute(keys="Heatwave patient", values=1)%>%
                        
                        #set arrival attribute (by walk in =0 or by ambulance =1)
                        set_attribute(keys="mode of arrival", values=1)%>%
                        
                        #set triage score (informed by arrival attribute)
                        set_attribute(keys="triage score",values=function() round(sample(size=1,x=triageX, prob=HWambulanceTriageProb,replace=F)))%>%
                        set_prioritization(values=function(){
                          TS<-get_attribute(env,"triage score")
                          Prior<-(5-TS)
                          Preempt<-(Prior+1)
                          c(Prior,Preempt,FALSE)
                        })%>%
                        
                        #set fragility attribute for FT (low co-morbidity/age =0 or high-risk =1)
                        set_attribute(keys="fragility",values=function()
                          rbinom(n=1,size=1,prob=0.65))%>%
                        
                      
                        

                        #set resource usage
                        set_attribute(keys="ED nurses siezed", values=function()
                          ifelse(get_attribute(env, "triage score")==1,4,
                                 ifelse(get_attribute(env, "triage score")==2,2,
                                        ifelse(get_attribute(env, "triage score")>2.1,1,1)))) %>%
                        
                        set_attribute(keys="ED doctors seized", values=function()
                          ifelse(get_attribute(env, "triage score")==1, 8,
                                 ifelse(get_attribute(env, "triage score")==2, 5,
                                        ifelse(get_attribute(env, "triage score")==3, 2,
                                               ifelse(get_attribute(env, "triage score")==4, 1,
                                                      ifelse(get_attribute(env, "triage score")==5, 0,)))))) %>%
                        
                        #set dead or alive outcome
                        set_attribute(keys="Patient dies",0)%>%
                        #set_attribute(keys="Patient dies", values=function()rbinom(n=1,size=1,prob=0.45)) %>%
                        
                        set_attribute(keys="Patient dies", values=function()ifelse(get_attribute(env, "triage score")==1,rbinom(n=1,size=1,prob=0.5),
                                                                                   ifelse(get_attribute(env, "triage score")==2,rbinom(n=1,size=1,prob=0.07),
                                                                                          ifelse(get_attribute(env, "triage score")==3,rbinom(n=1,size=1,prob=0.01),
                                                                                                 ifelse(get_attribute(env, "triage score")==3,rbinom(n=1,size=1,prob=0.005),0)))))%>%
                        set_attribute(keys="Patient dies",value=function()ifelse(get_attribute(env,"Patient dies")>0,1,0))%>%
                        
                        
                        
                        #death distributions
                        set_attribute(keys="DOA", values=function()
                          ifelse(get_attribute(env,"Patient dies")>0,rbinom(n=1,size=1,prob=0.5),0)) %>%
                        set_attribute(keys="diePreHos", values=function()
                          ifelse(get_attribute(env,"Patient dies")>0,rbinom(n=1,size=1,prob=1),0)) %>%
                        set_attribute(keys="dieWaitingRoom", 0) %>%
                        set_attribute(keys="dieED",1) %>%
                        set_attribute(keys="dieHosWard",0) %>%
                        
                        
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
                        
                        set_attribute(keys="SALT?", values=function()ifelse(MCI==1 & (get_queue_count(env,"ED bed")/get_capacity(env,"ED bed"))>0.9,1,
                                                                             ifelse(MCI==1 & (get_queue_count(env,"Ambulance")/get_capacity(env,"Ambulance"))>1.5,1,0))) %>%
                        set_attribute(keys="Hour_of_day",values=function() floor((now(env) %% 1440)/60)) %>%
                        
                        #set non-heatwave attribute
                        set_attribute(keys="Heatwave patient", values=0)%>%
                        
                        
                        #set arrival attribute (by walk in =0 or by ambulance =1)
                        set_attribute(keys="mode of arrival", values=0)%>%
                        
                        #set triage score (informed by arrival attribute)
            
                        set_attribute(keys="triage score",values=function() sample(size=1,x=triageX, prob=walkinTriageProb,replace=F))%>%
                        set_prioritization(values=function(){
                          TS<-get_attribute(env,"triage score")
                          Prior<-(5-TS)
                          Preempt<-(Prior+1)
                          c(Prior,Preempt,FALSE)
                        })%>%
                        
                        #set fragility attribute for FT (low co-morbidity/age =0 or high-risk =1)
                        set_attribute(keys="fragility",values=function()
                          rbinom(n=1,size=1,prob=0.55))%>%
                        
                        #set resource usage
                        #set resource usage
                        set_attribute(keys="ED nurses siezed", values=function()
                          ifelse(get_attribute(env, "triage score")==1,4,
                                 ifelse(get_attribute(env, "triage score")==2,2,
                                        ifelse(get_attribute(env, "triage score")>2.1,1,1)))) %>%
                        
                        set_attribute(keys="ED doctors seized", values=function()
                          ifelse(get_attribute(env, "triage score")==1, 8,
                                 ifelse(get_attribute(env, "triage score")==2, 5,
                                        ifelse(get_attribute(env, "triage score")==3, 2,
                                               ifelse(get_attribute(env, "triage score")==4, 1,
                                                      ifelse(get_attribute(env, "triage score")==5, 0,)))))) %>%
                        
                        #set dead or alive outcome
                        
                        set_attribute(keys="Patient dies",0)%>%
                        set_attribute(keys="Patient dies", values=function()ifelse(get_attribute(env, "triage score")==1,rbinom(n=1,size=1,prob=0.4),
                                                                                   ifelse(get_attribute(env, "triage score")==2,rbinom(n=1,size=1,prob=0.04),
                                                                                       ifelse(get_attribute(env, "triage score")==3,rbinom(n=1,size=1,prob=0.009),
                                                                                              ifelse(get_attribute(env, "triage score")==4,rbinom(n=1,size=1,prob=0.005),0)))))%>%
                        set_attribute(keys="Patient dies",value=function()ifelse(get_attribute(env,"Patient dies")>0,1,0))%>%
                        
                        #death distributions
                        set_attribute(keys="dieWaitingRoom", values=function()
                          ifelse(get_attribute(env,"Patient dies")>0,rbinom(n=1,size=1,prob=0.005),0))%>%
                        set_attribute(keys="dieED", values=function()
                          ifelse(get_attribute(env,"Patient dies")>0,rbinom(n=1,size=1,prob=0.27),0)) %>%
                        set_attribute(keys="dieHosWard", values=function()
                          ifelse(get_attribute(env,"Patient dies")>0,rbinom(n=1,size=1,prob=1),0)) %>%
                        
                        
                        
                        #Set surge capacity for staffing and night ambulance shift
                            set_capacity("Ambulance",value=function()ifelse(SurgeStaff==1 & now(env)>40320 & now(env)<54720, 14,
                                                                      ifelse(SurgeStaff==0 & (now(env)/1440-now(env)%/%1440)*24<7 & (now(env)/1440-now(env)%/%1440)*24<20,(nAmbulance-1),nAmbulance)))%>%
                      
                            set_capacity("fire",value=function()ifelse(SurgeStaff==1 & now(env)>40320 & now(env)<54720,3,nFire))%>%
                            set_capacity("quarter ED nurse",value=function()ifelse(SurgeStaff==1 & now(env)>40320 & now(env)<54720,32,nEDnurse))%>%
                            set_capacity("tenth ED doc",value=function()ifelse(SurgeStaff==1 & now(env)>40320 & now(env)<54720,40,nEDdoc))%>%
                            set_capacity("coroner",value=function()ifelse(SurgeStaff==1 & now(env)>40320 & now(env)<54720,2,nCoroner))%>%
                          
                        
                        
                        simmer::join(Hospital)
                      
                      
                      EMSPatient<-trajectory() %>%
                        # MCI protocol on (automatic) or off
                            #set_global(keys="MCI switch", values=MCI) %>%
                        
                        set_attribute(keys="SALT?", values=function()ifelse(MCI==1 & (get_queue_count(env,"ED bed")/get_capacity(env,"ED bed"))>0.9,1,
                                                                            ifelse(MCI==1 & (get_queue_count(env,"Ambulance")/get_capacity(env,"Ambulance"))>1.5,1,0))) %>%
                        set_attribute(keys="Hour_of_day",values=function() floor((now(env) %% 1440)/60)) %>%
                        
                        #set non-heatwave attribute
                        set_attribute(keys="Heatwave patient", values=0)%>%
                        
                        #set arrival attribute (by walk in =0 or by ambulance =1)
                        set_attribute(keys="mode of arrival", values=1)%>%
                        
                        #set triage score (informed by arrival attribute)
                        set_attribute(keys="triage score",values=function() round(sample(size=1,x=triageX, prob=ambulanceTriageProb,replace=F)))%>%
                        set_prioritization(values=function(){
                          TS<-get_attribute(env,"triage score")
                          Prior<-(5-TS)
                          Preempt<-(Prior+1)
                          c(Prior,Preempt,FALSE)
                        })%>%
                        
                        #set fragility attribute for FT (low co-morbidity/age =0 or high-risk =1)
                        set_attribute(keys="fragility",values=function()
                          rbinom(n=1,size=1,prob=0.65))%>%
                      
                        
                        #set dead or alive outcome
                        set_attribute(keys="Patient dies",0)%>%
                        set_attribute(keys="Patient dies", values=function()ifelse(get_attribute(env, "triage score")==1,rbinom(n=1,size=1,prob=0.4),
                                                                                   ifelse(get_attribute(env, "triage score")==2,rbinom(n=1,size=1,prob=0.051),
                                                                                          ifelse(get_attribute(env, "triage score")==3,rbinom(n=1,size=1,prob=0.00986),
                                                                                                 ifelse(get_attribute(env, "triage score")==3,rbinom(n=1,size=1,prob=0.00672),0)))))%>%
                        set_attribute(keys="Patient dies",value=function()ifelse(get_attribute(env,"Patient dies")>0,1,0))%>%
                        
                        
                        #set resource usage
                        #set resource usage
                        set_attribute(keys="ED nurses siezed", values=function()
                          ifelse(get_attribute(env, "triage score")==1,4,
                                 ifelse(get_attribute(env, "triage score")==2,2,
                                        ifelse(get_attribute(env, "triage score")>2.1,1,1)))) %>%
                        
                        set_attribute(keys="ED doctors seized", values=function()
                          ifelse(get_attribute(env, "triage score")==1, 8,
                                 ifelse(get_attribute(env, "triage score")==2, 5,
                                        ifelse(get_attribute(env, "triage score")==3, 2,
                                               ifelse(get_attribute(env, "triage score")==4, 1,
                                                      ifelse(get_attribute(env, "triage score")==5, 0,)))))) %>%
                        
                        
                        set_attribute(keys="Patient dies",value=function()ifelse(get_attribute(env,"Patient dies")>0,1,0))%>%
                        
                        #death distributions
                        set_attribute(keys="DOA", values=function()
                          ifelse(get_attribute(env,"Patient dies")>0,rbinom(n=1,size=1,prob=0.30),0)) %>%
                        set_attribute(keys="diePreHos", values=function()
                          ifelse(get_attribute(env,"Patient dies")>0,rbinom(n=1,size=1,prob=1),0)) %>%
                        set_attribute(keys="dieWaitingRoom",0) %>%
                        set_attribute(keys="dieED", 0) %>%
                        set_attribute(keys="dieHosWard",0)%>%
                        

                        #set if death needs coroner
                        set_attribute(keys="Coroner case",function() 
                          ifelse(get_attribute(env, "diePreHos")==1 && get_attribute(env,"DOA")==1, rbinom(n=1,size=1,prob=0.1), 0)) %>%


                        simmer::join(PreHospital)
                      
                      #####          
                      
                      
                      ### Model resources
                      #####

                      env%>%
                        add_resource(name="Ambulance", capacity=nAmbulance, queue_size=(nAmbulance*5),preemptive=F,preempt_order = "fifo") %>%
                        add_resource(name="fire", capacity=nFire, queue_size=(nFire*30),preemptive=F,preempt_order = "fifo") %>%
                        add_resource(name="police", capacity=nPolice, queue_size=(nPolice*5),preemptive=F,preempt_order = "fifo") %>%
                        add_resource(name="coroner", capacity=nCoroner, queue_size=(nCoroner*20),preemptive=F,preempt_order = "fifo") %>%
                        add_resource(name="family with body", capacity=Inf, queue_size=Inf,preemptive=F,preempt_order = "fifo") %>%
                        add_resource(name="911Dispatcher", capacity=nDispatcher, queue_size=(nDispatcher*5),preemptive=F,preempt_order = "fifo")
                        
                      
                      
                      env%>%
                        add_resource(name="Waiting room chair", capacity=nWaitingRoom, queue_size=(nWaitingRoom*1.5),preemptive=T,preempt_order = "fifo") %>%
                        add_resource(name="Morgue", capacity=Inf, queue_size=Inf,preemptive=F) %>%
                        add_resource(name="Heatwave Morgue", capacity=Inf, queue_size=Inf,preemptive=F) %>%
                        add_resource(name = "quarter ED nurse",capacity = nEDnurse, queue_size = Inf,preemptive = T,preempt_order = "fifo") %>%
                        add_resource(name = "Triage nurse",capacity = nTriageNurse, queue_size = Inf,preemptive = F,preempt_order = "fifo") %>%
                        add_resource(name = "ED bed",capacity = nEDbed, queue_size =Inf, preemptive =F,preempt_order = "fifo") %>%
                        add_resource(name = "tenth ED doc",capacity = nEDdoc, queue_size = Inf,preemptive = T,preempt_order = "fifo") %>%
                        add_resource(name="ICU bed", capacity=nICUbed, queue_size=Inf,preemptive=F,preempt_order = "fifo") %>%
                        add_resource(name="Non-ICU bed", capacity=nNonICUbed, queue_size=Inf,preemptive=F,preempt_order = "fifo")%>%
                        
                        add_resource(name="Fast Track bed", capacity=nFTbeds, queue_size=(nFTbeds*2),preemptive=F,preempt_order = "fifo") %>%
                        add_resource(name="fifth Fast Track nurse", capacity=nFTnurse, queue_size=Inf,preemptive=F,preempt_order = "fifo") %>%
                        add_resource(name="fifteenth Fast Track doc", capacity=nFTdoc, queue_size=Inf,preemptive=F,preempt_order = "fifo")
                      
                      
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
                                                    from_to(start_time=HWStartT[i],stop_time=HWStopT[i], dist=function()sample(HW_EMS[[i]],size=1,replace=F),arrive=F),
                                                    priority=0,preemptible=0,restart=F,mon=2)
                                 })  
                               } 
              
                      
                      #####
                      
                      ### Non-heatwave patient generators
                      #####
                      
                      
                            env%>%
                              add_generator("patient [0-3]",trajectory=WalkInPatient, 
                                            from_to(t_hour(0),t_hour(3),every = t_day(1), dist=function()sample(WI_0_3,size=1,replace=F),arrive=F),
                                            priority=0,preemptible=0,restart=F,mon=2) %>%
                              
                              add_generator("patient [3-6]",trajectory=WalkInPatient, 
                                            from_to(t_hour(3),t_hour(6),every = t_day(1), dist=function()sample(WI_3_6,size=1,replace=F),arrive=F),
                                            priority=0,preemptible=0,restart=F,mon=2) %>%
                              
                              add_generator("patient [6-9]",trajectory=WalkInPatient, 
                                            from_to(t_hour(6),t_hour(9),every = t_day(1), dist=function()sample(WI_6_9,size=1,replace=F),arrive=F),
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
                                            from_to(t_hour(0),t_hour(6),every = t_day(1), dist=function()sample(EMS_0_6,size=1,replace=F),arrive=F),
                                            priority=0,preemptible=0,restart=F,mon=2) %>%
                              
                              add_generator("EMS patient [6-9]",trajectory=EMSPatient, 
                                            from_to(t_hour(6),t_hour(9),every = t_day(1), dist=function()sample(EMS_6_9,size=1,replace=F),arrive=F),
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
                      
                     
                      
                      # END patient generators
                      #####
        
            
            sim_out<-env%>%
              simmer::run(nRunDays*1440)
                   
            wrap(sim_out)
            
            })
        })
  
  
############# Simulation result graphics and output tables
    #####
  
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

    # Model parameter graphics    
  output$TableParameters<-renderReactable({
      req(Hmodel)
      req(input$iterations)
      req(input$HA)
      HA<-input$HA
      nI<-input$iterations
      
      Log1<-get_mon_arrivals(Hmodel())
      Log2<-as.data.frame(get_mon_attributes(Hmodel()))
      Log2<-Log2[,2:5]
      colnames(Log2)<-c("name","key","value","replication")
      Log2$value<-as.numeric(Log2$value)
      Log2<-reshape2::dcast(Log2,name+replication~key,mean,fill=NaN)
      Log<-merge(Log1,Log2,by=c("name","replication"))
      
      HWStart<-(30)*1440
      HWStop<-HWStopTimes()
      HWStop<-HWStop[24]
      

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
      df<-df[,c(3,32,39,51,48,47,38,37,34,31)]
      
    ## target parameters
      df_target<-as.data.frame(c(NA,0,0.02,11.14,NA,NA,NA,NA,NA,NA,129.3,97.0,32.3,21.0,1.26,0,0,0.6,20.8,53.1,46.8,7.4))
      if(HA==0){
        df_target_HW<-as.data.frame(c(NA,0.32,0.15,10.47,NA,NA,NA,NA,NA,NA,137.53,103.29,33.79,24.80,2.71,8.23,1.45,1.7,25.5,57.7,51.2,8.6))
      }else if (HA==1){
        df_target_HW<-as.data.frame(c(NA,0.12,0.08,10.76,NA,NA,NA,NA,NA,NA,133.80,100.39,33.10,24.80,2.05,4.49,0.79,1.7,24.8,56.1,49.8,8.3))
      }else if(HA==2){
        df_target_HW<-as.data.frame(c(NA,0.68,0.24,10.24,NA,NA,NA,NA,NA,NA,140.57,105.61,34.35,24.80,3.25,11.26,1.99,1.8,26.0,58.9,52.4,8.8))
      }else if(HA==3){
        df_target_HW<-as.data.frame(c(NA,0.18,0.10,10.55,NA,NA,NA,NA,NA,NA,136.56,102.52,33.60,24.80,2.54,7.25,1.28,1.7,25.3,57.2,50.9,8.5))
      }else if(HA==4){
        df_target_HW<-as.data.frame(c(NA,0.05,0.05,10.68,NA,NA,NA,NA,NA,NA,134.80,101.11,33.15,24.80,2.21,5.49,0.95,1.7,25.0,56.5,50.2,8.4))
      }
      
    ## 14 day baseline period
      df_base<-df %>%
        filter(start_time < HWStart & start_time > 23040)
      df_base[df_base==""]<-NA
      df_base<-as.data.frame(df_base)
      
      t1<-as.numeric(sum(df_base[,5]==1,na.rm=T)/14/nI)
      t2<-as.numeric(sum(df_base[,5]==2, na.rm=T)/14/nI)
      t3<-as.numeric(sum(df_base[,5]==3, na.rm=T)/14/nI)
      t4<-as.numeric(sum(df_base[,5]==4, na.rm=T)/14/nI)
      t5<-as.numeric(sum(df_base[,5]==5, na.rm=T)/14/nI)
      totalpt<-as.numeric(nrow(df_base)/14/nI, na.rm=T)
      WIpt<-as.numeric(sum(df_base[,8]==0, na.rm=T)/14/nI)
      EMSpt<-as.numeric(sum(na.omit(df_base[,8]==1)/14/nI))
      HospAdmit<-as.numeric(sum(df_base[,7]>0 | df_base[,9]>0, na.rm=T)/14/nI)
      Deaths<-as.numeric(sum(df_base[,6]>0, na.rm=T)/14/nI)
      HWDeaths<-as.numeric(sum(df_base[,10]>0, na.rm=T)/14/nI)
      HWpt<-as.numeric(sum(df_base[,2]==1, na.rm=T)/14/nI)
      
      df_base<-as.data.frame(colMeans(df_base,na.rm = T))
      df_base[11,1]<-totalpt
      df_base[12,1]<-WIpt
      df_base[13,1]<-EMSpt
      df_base[14,1]<-HospAdmit
      df_base[15,1]<-Deaths
      df_base[16,1]<-HWpt
      df_base[17,1]<-HWDeaths
      df_base[18,1]<-t1
      df_base[19,1]<-t2
      df_base[20,1]<-t3
      df_base[21,1]<-t4
      df_base[22,1]<-t5
      
    
    ## 8 day heatwave period
      df_HW<-df %>%
        filter(start_time > HWStart & start_time < HWStop)
      t1<-as.numeric(sum(df_HW[,5]==1, na.rm=T)/8/nI)
      t2<-as.numeric(sum(df_HW[,5]==2, na.rm=T)/8/nI)
      t3<-as.numeric(sum(df_HW[,5]==3, na.rm=T)/8/nI)
      t4<-as.numeric(sum(df_HW[,5]==4, na.rm=T)/8/nI)
      t5<-as.numeric(sum(df_HW[,5]==5, na.rm=T)/8/nI)
      totalpt<-as.numeric(nrow(df_HW)/8/nI)
      WIpt<-as.numeric(sum(df_HW[,8]==0, na.rm=T)/8/nI)
      EMSpt<-as.numeric(sum(df_HW[,8]==1, na.rm=T)/8/nI)
      HospAdmit<-as.numeric(sum(df_HW[,7]>0 | df_HW[,9]>0, na.rm=T)/8/nI)
      Deaths<-as.numeric(sum(df_HW[,6]>0, na.rm=T)/8/nI)
      HWDeaths<-as.numeric(sum(df_HW[,10]>0, na.rm=T)/8/nI)
      HWpt<-as.numeric(sum(df_HW[,2]==1, na.rm=T)/8/nI)
      
      df_HW<-as.data.frame(colMeans(df_HW,na.rm = T))
      df_HW[11,1]<-totalpt
      df_HW[12,1]<-WIpt
      df_HW[13,1]<-EMSpt
      df_HW[14,1]<-HospAdmit
      df_HW[15,1]<-Deaths
      df_HW[16,1]<-HWpt
      df_HW[17,1]<-HWDeaths
      df_HW[18,1]<-t1
      df_HW[19,1]<-t2
      df_HW[20,1]<-t3
      df_HW[21,1]<-t4
      df_HW[22,1]<-t5
    
    ## 7 day post-heatwave period  
      df_PHW<-df %>% 
        filter(start_time > HWStop & start_time < HWStop+(1440*7))
      t1<-as.numeric(sum(df_PHW[,5]==1, na.rm=T)/7/nI)
      t2<-as.numeric(sum(df_PHW[,5]==2, na.rm=T)/7/nI)
      t3<-as.numeric(sum(df_PHW[,5]==3, na.rm=T)/7/nI)
      t4<-as.numeric(sum(df_PHW[,5]==4, na.rm=T)/7/nI)
      t5<-as.numeric(sum(df_PHW[,5]==5, na.rm=T)/7/nI)
      totalpt<-as.numeric(nrow(df_PHW)/7/nI)
      WIpt<-as.numeric(sum(df_PHW[,8]==0, na.rm=T)/7/nI)
      EMSpt<-as.numeric(sum(df_PHW[,8]==1, na.rm=T)/7/nI)
      HospAdmit<-as.numeric(sum(df_PHW[,7]>0 | df_PHW[,9]>0, na.rm=T)/7/nI)
      Deaths<-as.numeric(sum(df_PHW[,6]>0, na.rm=T)/7/nI)
      HWDeaths<-as.numeric(sum(df_PHW[,10]>0, na.rm=T)/7/nI)
      HWpt<-as.numeric(sum(df_PHW[,2]==1, na.rm=T)/7/nI)
      
      df_PHW<-as.data.frame(colMeans(df_PHW,na.rm = T))
      df_PHW[11,1]<-totalpt
      df_PHW[12,1]<-WIpt
      df_PHW[13,1]<-EMSpt
      df_PHW[14,1]<-HospAdmit
      df_PHW[15,1]<-Deaths
      df_PHW[16,1]<-HWpt
      df_PHW[17,1]<-HWDeaths
      df_PHW[18,1]<-t1
      df_PHW[19,1]<-t2
      df_PHW[20,1]<-t3
      df_PHW[21,1]<-t4
      df_PHW[22,1]<-t5
    
      
    ## aggregate
      DF<-cbind(df_target,df_base,df_target_HW,df_HW,df_PHW)
      
      DF<-DF%>%
        dplyr::mutate_if(is.numeric, round, digits=2)
      
      DF<-DF[c(-1,-5,-6,-7,-8,-9,-10),]
      colnames(DF)<-c("Target baseline","Model baseline mean","Target heatwave","Heatwave mean","Post heatwave mean")
      
      rownames(DF)<-c("Ratio of heatwave to non-heatwave pts","Ratio of pts who die","Mean time between patients (minutes)",
                      "Daily avg number of patients","Daily avg number of walk-ins","Daily avg number ambulance pts",
                      "Daily number hospital admissions","Daily number of deaths","Daily number of HW patients","Daily number of HW deaths",
                      "Daily avg CTAS 1","Daily avg CTAS 2","Daily avg CTAS 3","Daily avg CTAS 4","Daily avg CTAS 5")
      
      reactable(DF,
                theme=reactableTheme(style=list(fontFamily="Montserrat,sans-serif")),
                fullWidth = T,
                defaultColDef = colDef(align="left", headerStyle=list(background="#b4cbce"),minWidth = 120),
                bordered=F, striped = T, highlight=T,showSortable = T,
                minRows = 1, showPageSizeOptions = F,defaultPageSize=20
      )
      
    })
  
  output$downloadParametersTable<-downloadHandler(
    filename=function(){
      paste0("ValidationPrmetrs_",input$CatchmentPop,"_",input$HA,".csv")
    },
    content=function(file){
      HA<-input$HA
      nI<-input$iterations
      
      Log1<-get_mon_arrivals(Hmodel())
      Log2<-as.data.frame(get_mon_attributes(Hmodel()))
      Log2<-Log2[,2:5]
      colnames(Log2)<-c("name","key","value","replication")
      Log2$value<-as.numeric(Log2$value)
      Log2<-reshape2::dcast(Log2,name+replication~key,mean,fill=NaN)
      Log<-merge(Log1,Log2,by=c("name","replication"))
      
      HWStart<-HWStartTimes()
      HWStart<-HWStart[1]
      HWStop<-HWStopTimes()
      HWStop<-HWStop[24]
      
      
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
      df<-df[,c(3,32,39,51,48,47,38,37,34,31)]
      
      ## target parameters
      df_target<-as.data.frame(c(NA,0,0.02,11.14,NA,NA,NA,NA,NA,NA,129.3,97.0,32.3,21.0,1.26,0,0,0.6,20.8,53.1,46.8,7.4))
      if(HA==0){
        df_target_HW<-as.data.frame(c(NA,0.32,0.15,10.47,NA,NA,NA,NA,NA,NA,137.53,103.29,33.79,24.80,2.71,8.23,1.45,1.7,25.5,57.7,51.2,8.6))
      }else if (HA==1){
        df_target_HW<-as.data.frame(c(NA,0.12,0.08,10.76,NA,NA,NA,NA,NA,NA,133.80,100.39,33.10,24.80,2.05,4.49,0.79,1.7,24.8,56.1,49.8,8.3))
      }else if(HA==2){
        df_target_HW<-as.data.frame(c(NA,0.68,0.24,10.24,NA,NA,NA,NA,NA,NA,140.57,105.61,34.35,24.80,3.25,11.26,1.99,1.8,26.0,58.9,52.4,8.8))
      }else if(HA==3){
        df_target_HW<-as.data.frame(c(NA,0.18,0.10,10.55,NA,NA,NA,NA,NA,NA,136.56,102.52,33.60,24.80,2.54,7.25,1.28,1.7,25.3,57.2,50.9,8.5))
      }else if(HA==4){
        df_target_HW<-as.data.frame(c(NA,0.05,0.05,10.68,NA,NA,NA,NA,NA,NA,134.80,101.11,33.15,24.80,2.21,5.49,0.95,1.7,25.0,56.5,50.2,8.4))
      }
      
      ## 14 day baseline period
      df_base<-df %>%
        filter(start_time < HWStart & start_time > 23040)
      df_base[df_base==""]<-NA
      df_base<-as.data.frame(df_base)
      
      t1<-as.numeric(sum(df_base[,5]==1,na.rm=T)/14/nI)
      t2<-as.numeric(sum(df_base[,5]==2, na.rm=T)/14/nI)
      t3<-as.numeric(sum(df_base[,5]==3, na.rm=T)/14/nI)
      t4<-as.numeric(sum(df_base[,5]==4, na.rm=T)/14/nI)
      t5<-as.numeric(sum(df_base[,5]==5, na.rm=T)/14/nI)
      totalpt<-as.numeric(nrow(df_base)/14/nI, na.rm=T)
      WIpt<-as.numeric(sum(df_base[,8]==0, na.rm=T)/14/nI)
      EMSpt<-as.numeric(sum(na.omit(df_base[,8]==1)/14/nI))
      HospAdmit<-as.numeric(sum(df_base[,7]>0 | df_base[,9]>0, na.rm=T)/14/nI)
      Deaths<-as.numeric(sum(df_base[,6]>0, na.rm=T)/14/nI)
      HWDeaths<-as.numeric(sum(df_base[,10]>0, na.rm=T)/14/nI)
      HWpt<-as.numeric(sum(df_base[,2]==1, na.rm=T)/14/nI)
      
      df_base<-as.data.frame(colMeans(df_base,na.rm = T))
      df_base[11,1]<-totalpt
      df_base[12,1]<-WIpt
      df_base[13,1]<-EMSpt
      df_base[14,1]<-HospAdmit
      df_base[15,1]<-Deaths
      df_base[16,1]<-HWpt
      df_base[17,1]<-HWDeaths
      df_base[18,1]<-t1
      df_base[19,1]<-t2
      df_base[20,1]<-t3
      df_base[21,1]<-t4
      df_base[22,1]<-t5
      
      
      ## 8 day heatwave period
      df_HW<-df %>%
        filter(start_time > HWStart & start_time < HWStop)
      t1<-as.numeric(sum(df_HW[,5]==1, na.rm=T)/8/nI)
      t2<-as.numeric(sum(df_HW[,5]==2, na.rm=T)/8/nI)
      t3<-as.numeric(sum(df_HW[,5]==3, na.rm=T)/8/nI)
      t4<-as.numeric(sum(df_HW[,5]==4, na.rm=T)/8/nI)
      t5<-as.numeric(sum(df_HW[,5]==5, na.rm=T)/8/nI)
      totalpt<-as.numeric(nrow(df_HW)/8/nI)
      WIpt<-as.numeric(sum(df_HW[,8]==0, na.rm=T)/8/nI)
      EMSpt<-as.numeric(sum(df_HW[,8]==1, na.rm=T)/8/nI)
      HospAdmit<-as.numeric(sum(df_HW[,7]>0 | df_HW[,9]>0, na.rm=T)/8/nI)
      Deaths<-as.numeric(sum(df_HW[,6]>0, na.rm=T)/8/nI)
      HWDeaths<-as.numeric(sum(df_HW[,10]>0, na.rm=T)/8/nI)
      HWpt<-as.numeric(sum(df_HW[,2]==1, na.rm=T)/8/nI)
      
      df_HW<-as.data.frame(colMeans(df_HW,na.rm = T))
      df_HW[11,1]<-totalpt
      df_HW[12,1]<-WIpt
      df_HW[13,1]<-EMSpt
      df_HW[14,1]<-HospAdmit
      df_HW[15,1]<-Deaths
      df_HW[16,1]<-HWpt
      df_HW[17,1]<-HWDeaths
      df_HW[18,1]<-t1
      df_HW[19,1]<-t2
      df_HW[20,1]<-t3
      df_HW[21,1]<-t4
      df_HW[22,1]<-t5
      
      ## 7 day post-heatwave period  
      df_PHW<-df %>% 
        filter(start_time > HWStop & start_time < HWStop+(1440*7))
      t1<-as.numeric(sum(df_PHW[,5]==1, na.rm=T)/7/nI)
      t2<-as.numeric(sum(df_PHW[,5]==2, na.rm=T)/7/nI)
      t3<-as.numeric(sum(df_PHW[,5]==3, na.rm=T)/7/nI)
      t4<-as.numeric(sum(df_PHW[,5]==4, na.rm=T)/7/nI)
      t5<-as.numeric(sum(df_PHW[,5]==5, na.rm=T)/7/nI)
      totalpt<-as.numeric(nrow(df_PHW)/7/nI)
      WIpt<-as.numeric(sum(df_PHW[,8]==0, na.rm=T)/7/nI)
      EMSpt<-as.numeric(sum(df_PHW[,8]==1, na.rm=T)/7/nI)
      HospAdmit<-as.numeric(sum(df_PHW[,7]>0 | df_PHW[,9]>0, na.rm=T)/7/nI)
      Deaths<-as.numeric(sum(df_PHW[,6]>0, na.rm=T)/7/nI)
      HWDeaths<-as.numeric(sum(df_PHW[,10]>0, na.rm=T)/7/nI)
      HWpt<-as.numeric(sum(df_PHW[,2]==1, na.rm=T)/7/nI)
      
      df_PHW<-as.data.frame(colMeans(df_PHW,na.rm = T))
      df_PHW[11,1]<-totalpt
      df_PHW[12,1]<-WIpt
      df_PHW[13,1]<-EMSpt
      df_PHW[14,1]<-HospAdmit
      df_PHW[15,1]<-Deaths
      df_PHW[16,1]<-HWpt
      df_PHW[17,1]<-HWDeaths
      df_PHW[18,1]<-t1
      df_PHW[19,1]<-t2
      df_PHW[20,1]<-t3
      df_PHW[21,1]<-t4
      df_PHW[22,1]<-t5
      
      
      ## aggregate
      DF<-cbind(df_target,df_base,df_target_HW,df_HW,df_PHW)
      
      DF<-DF%>%
        dplyr::mutate_if(is.numeric, round, digits=2)
      
      DF<-DF[c(-1,-5,-6,-7,-8,-9,-10),]
      colnames(DF)<-c("Target baseline","Model baseline mean","Target heatwave","Heatwave mean","Post heatwave mean")
      
      rownames(DF)<-c("Ratio of heatwave to non-heatwave pts","Ratio of pts who die","Mean time between patients (minutes)",
                      "Daily avg number of patients","Daily avg number of walk-ins","Daily avg number ambulance pts",
                      "Daily number hospital admissions","Daily number of deaths","Daily number of HW patients","Daily number of HW deaths",
                      "Daily avg CTAS 1","Daily avg CTAS 2","Daily avg CTAS 3","Daily avg CTAS 4","Daily avg CTAS 5")
      
      
      write.csv(DF,file)
    })
    
    # Model KPI graphics  
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
      
      HWStart<-HWStartTimes()
      HWStart<-HWStart[1]
      HWStop<-HWStopTimes()
      HWStop<-HWStop[24]
      
  
      df<-Log%>%
        arrange(start_time)
      
      
      df<-df[,c(3,8,11,25,26,40,41,49,48,35,50)]
      
      df<-df %>% 
        mutate_if(is.character, as.numeric)
      
      colnames(df)<-c("start_time","ALC","EMS_RT","ED boarding time for ICU","ED boarding time for Non-ICU","Patient ICU LOS",
                      "Patient Non-ICU ward LOS","Wait to triage patient","triage_score","Time_to_IPA","boarding_time")
      
      df_target<-as.data.frame(c(NA,988,NA,NA,NA,4320.0,8236.8,15,NA,NA,810.0,7.8,12.9,240.0))
      df_target_HW<-as.data.frame(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,894.0,9.7,32.5,NA))
      
      ## Baseline data (day 16 to 30)
      df_base<-df %>%
        filter(start_time < HWStart & start_time>23040)
      
          CTAS1_EMS<-df_base%>%
            filter(triage_score==1)
          CTAS1_EMS<-mean(as.numeric(CTAS1_EMS$EMS_RT),na.rm=T)
          
          CTAS3_EMS<-df_base%>%
            filter(triage_score==3)
          CTAS3_EMS<-mean(as.numeric(CTAS3_EMS$EMS_RT),na.rm=T)
      
          IPA<-quantile(df_base$Time_to_IPA, probs = 0.9,na.rm=T)
      
      df_base<-as.data.frame(colMeans(df_base,na.rm = T))
      df_base[12,1]<-CTAS1_EMS
      df_base[13,1]<-CTAS3_EMS
      df_base[14,1]<-IPA
      
      #Heatwave data (8 days)
      df_HW<-df %>%
        filter(start_time > HWStart & start_time < HWStop)
          
          CTAS1_EMS<-df_HW%>%
            filter(triage_score==1)
          CTAS1_EMS<-mean(as.numeric(CTAS1_EMS$EMS_RT),na.rm=T)
          
          CTAS3_EMS<-df_HW%>%
            filter(triage_score==3)
          CTAS3_EMS<-mean(as.numeric(CTAS3_EMS$EMS_RT),na.rm=T)
          
          IPA<-quantile(df_HW$Time_to_IPA, probs = 0.9,na.rm=T)
          
      df_HW<-as.data.frame(colMeans(df_HW,na.rm = T))
      df_HW[12,1]<-CTAS1_EMS
      df_HW[13,1]<-CTAS3_EMS
      df_HW[14,1]<-IPA
      
      
      #post-heatwave (7 days)
      df_PHW<-df %>% 
        filter(start_time > HWStop & start_time < HWStop+(1440*7))
      
          CTAS1_EMS<-df_PHW%>%
            filter(triage_score==1)
          CTAS1_EMS<-mean(as.numeric(CTAS1_EMS$EMS_RT),na.rm=T)
          
          CTAS3_EMS<-df_PHW%>%
            filter(triage_score==3)
          CTAS3_EMS<-mean(as.numeric(CTAS3_EMS$EMS_RT),na.rm=T)
          
          IPA<-quantile(df_PHW$Time_to_IPA, probs = 0.9,na.rm=T)
      
      df_PHW<-as.data.frame(colMeans(df_PHW,na.rm = T))
      df_PHW[12,1]<-CTAS1_EMS
      df_PHW[13,1]<-CTAS3_EMS
      df_PHW[14,1]<-IPA
      
      
      DF<-cbind(df_target,df_base,df_target_HW,df_HW,df_PHW)
      
      DF<-DF%>%
        dplyr::mutate_if(is.numeric, round, digits=2)
      
      DF<-DF[c(-1,-3,-4,-5,-9),]
      colnames(DF)<-c("Target baseline","Model baseline mean","Target heatwave","Heatwave mean","Post heatwave mean")
      
      rownames(DF)<-c("Mean ALC time (minutes)","Mean ICU LOS (minutes)","Mean non-ICU ward LOS (minutes)",
                      "Mean triage wait time (minutes)","Mean time until physician assessment","Mean ED boarding time for admitted pts (minutes)",
                      "Mean ambulance response time for CTAS 1 (minutes)","Mean ambulance response time for CTAS 3 (minutes)","Time until physician assessment for 90% of pts")
  
  reactable(DF,
            theme=reactableTheme(style=list(fontFamily="Montserrat,sans-serif")),
            fullWidth = T,
            defaultColDef = colDef(align="left", headerStyle=list(background="#b4cbce"),minWidth = 120),
            bordered=F, striped = T, highlight=T,showSortable = T,
            minRows = 1, showPageSizeOptions = T,defaultPageSize=20
            )

  })
    
  output$downloadKPITable<-downloadHandler(
    filename=function(){
      paste0("KPIs_",input$CatchmentPop,"_",input$HA,".csv")
    },
    content=function(file){
      nI<-input$iterations
      
      Log1<-get_mon_arrivals(Hmodel())
      Log2<-as.data.frame(get_mon_attributes(Hmodel()))
      Log2<-Log2[,2:5]
      colnames(Log2)<-c("name","key","value","replication")
      Log2$value<-as.numeric(Log2$value)
      Log2<-reshape2::dcast(Log2,name+replication~key,mean,fill=NaN)
      Log<-merge(Log1,Log2,by=c("name","replication"))
      
      HWStart<-HWStartTimes()
      HWStart<-HWStart[1]
      HWStop<-HWStopTimes()
      HWStop<-HWStop[24]
      
      
      df<-Log%>%
        arrange(start_time)
      
      
      df<-df[,c(3,8,11,25,26,40,41,49,48,35,50)]
      
      df<-df %>% 
        mutate_if(is.character, as.numeric)
      
      colnames(df)<-c("start_time","ALC","EMS_RT","ED boarding time for ICU","ED boarding time for Non-ICU","Patient ICU LOS",
                      "Patient Non-ICU ward LOS","Wait to triage patient","triage_score","Time_to_IPA","boarding_time")
      
      df_target<-as.data.frame(c(NA,988,NA,NA,NA,4320.0,8236.8,15,NA,NA,810.0,7.8,12.9,240.0))
      df_target_HW<-as.data.frame(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,894.0,9.7,32.5,NA))
      
      ## Baseline data (day 16 to 30)
      df_base<-df %>%
        filter(start_time < HWStart & start_time>23040)
      
      CTAS1_EMS<-df_base%>%
        filter(triage_score==1)
      CTAS1_EMS<-mean(as.numeric(CTAS1_EMS$EMS_RT),na.rm=T)
      
      CTAS3_EMS<-df_base%>%
        filter(triage_score==3)
      CTAS3_EMS<-mean(as.numeric(CTAS3_EMS$EMS_RT),na.rm=T)
      
      IPA<-quantile(df_base$Time_to_IPA, probs = 0.9,na.rm=T)
      
      df_base<-as.data.frame(colMeans(df_base,na.rm = T))
      df_base[12,1]<-CTAS1_EMS
      df_base[13,1]<-CTAS3_EMS
      df_base[14,1]<-IPA
      
      #Heatwave data (8 days)
      df_HW<-df %>%
        filter(start_time > HWStart & start_time < HWStop)
      
      CTAS1_EMS<-df_HW%>%
        filter(triage_score==1)
      CTAS1_EMS<-mean(as.numeric(CTAS1_EMS$EMS_RT),na.rm=T)
      
      CTAS3_EMS<-df_HW%>%
        filter(triage_score==3)
      CTAS3_EMS<-mean(as.numeric(CTAS3_EMS$EMS_RT),na.rm=T)
      
      IPA<-quantile(df_HW$Time_to_IPA, probs = 0.9,na.rm=T)
      
      df_HW<-as.data.frame(colMeans(df_HW,na.rm = T))
      df_HW[12,1]<-CTAS1_EMS
      df_HW[13,1]<-CTAS3_EMS
      df_HW[14,1]<-IPA
      
      
      #post-heatwave (7 days)
      df_PHW<-df %>% 
        filter(start_time > HWStop & start_time < HWStop+(1440*7))
      
      CTAS1_EMS<-df_PHW%>%
        filter(triage_score==1)
      CTAS1_EMS<-mean(as.numeric(CTAS1_EMS$EMS_RT),na.rm=T)
      
      CTAS3_EMS<-df_PHW%>%
        filter(triage_score==3)
      CTAS3_EMS<-mean(as.numeric(CTAS3_EMS$EMS_RT),na.rm=T)
      
      IPA<-quantile(df_PHW$Time_to_IPA, probs = 0.9,na.rm=T)
      
      df_PHW<-as.data.frame(colMeans(df_PHW,na.rm = T))
      df_PHW[12,1]<-CTAS1_EMS
      df_PHW[13,1]<-CTAS3_EMS
      df_PHW[14,1]<-IPA
      
      
      DF<-cbind(df_target,df_base,df_target_HW,df_HW,df_PHW)
      
      DF<-DF%>%
        dplyr::mutate_if(is.numeric, round, digits=2)
      
      DF<-DF[c(-1,-3,-4,-5,-9),]
      colnames(DF)<-c("Target baseline","Model baseline mean","Target heatwave","Heatwave mean","Post heatwave mean")
      
      rownames(DF)<-c("Mean ALC time (minutes)","Mean ICU LOS (minutes)","Mean non-ICU ward LOS (minutes)",
                      "Mean triage wait time (minutes)","Mean time until physician assessment","Mean ED boarding time for admitted pts (minutes)",
                      "Mean ambulance response time for CTAS 1 (minutes)","Mean ambulance response time for CTAS 3 (minutes)","Time until physician assessment for 90% of pts")
      
      write.csv(DF,file)
    })
    
    #Model charts
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

    #Model print of all patients and Monte Carlo
  
  output$TableMonteCarlo<-renderReactable({
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
    
    HWStart<-HWStartTimes()
    HWStart<-HWStart[1]
    HWStop<-HWStopTimes()
    HWStop<-HWStop[24]
    
    
    df<-Log%>%
      arrange(start_time)
    
    
    df<-df[,c(2,3,8,11,25,26,40,41,49,48,35,50)]
    
    df<-df %>% 
      mutate_if(is.character, as.numeric)
    
    

    
    
    colnames(df)<-c("replication","start_time","ALC","EMS_RT","ED boarding time for ICU","ED boarding time for Non-ICU","Patient ICU LOS",
                    "Patient Non-ICU ward LOS","Wait to triage patient","triage_score","Time_to_IPA","boarding_time")

    df[,13]<-ifelse(df$triage_score==3 & df$Time_to_IPA >= (3*60) & df$Time_to_IPA <(6*60),1,0)
    df[,14]<-ifelse(df$triage_score==3 & df$Time_to_IPA >= (6*60),1,0)
    
    df[,15]<-ifelse(df$EMS_RT>7 & df$triage_score==1,1,0)
    df[,16]<-ifelse(df$EMS_RT<7 & df$EMS_RT>0 & df$triage_score==1,1,0)
    
    
    colnames(df)<-c("replication","start_time","ALC","EMS_RT","ED boarding time for ICU","ED boarding time for Non-ICU","Patient ICU LOS",
                    "Patient Non-ICU ward LOS","Wait to triage patient","triage_score","Time_to_IPA","boarding_time","Extended_ED_Wait_Flag3hr","Extended_ED_Wait_Flag6hr","EMS_CPR_Wait_Flag","EMS_CPR_Wait_OK")

    

    for(i in 1:(max(df$replication))){
      temp_df<-df%>%
        filter(replication==i)
      
          ## Baseline data (day 16 to 30)
              df_base<-temp_df %>%
                filter(start_time < HWStart & start_time>23040)
              
              CTAS1_EMS<-df_base%>%
                filter(triage_score==1)
              CTAS1_EMS<-mean(as.numeric(CTAS1_EMS$EMS_RT),na.rm=T)
              CTAS3_EMS<-df_base%>%
                filter(triage_score==3)
              CTAS3_EMS<-mean(as.numeric(CTAS3_EMS$EMS_RT),na.rm=T)
              IPA<-quantile(df_base$Time_to_IPA, probs = 0.9,na.rm=T)
              TotalPTs<-nrow(df_base)
              PTsEDWait3<-sum(df_base$Extended_ED_Wait_Flag3hr==1,na.rm=T)
              PTsEDWait6<-sum(df_base$Extended_ED_Wait_Flag6hr==1,na.rm=T)
              PTsCodeEMS<-sum(df_base$EMS_CPR_Wait_OK==1,na.rm=T)
              PTsCodeEMSWait7Plus<-sum(df_base$EMS_CPR_Wait_Flag==1,na.rm=T)
              
              df_base<-as.data.frame(colMeans(df_base,na.rm = T))
              df_base[17,1]<-CTAS1_EMS
              df_base[18,1]<-CTAS3_EMS
              df_base[19,1]<-IPA
              df_base[20,1]<-PTsEDWait3
              df_base[21,1]<-PTsEDWait6
              df_base[22,1]<-PTsCodeEMS
              df_base[23,1]<-PTsCodeEMSWait7Plus
              df_base[24,1]<-TotalPTs
              
              
          #Heatwave data (8 days)
              df_HW<-temp_df %>%
                filter(start_time > HWStart & start_time < HWStop)

              CTAS1_EMS<-df_HW%>%
                filter(triage_score==1)
              CTAS1_EMS<-mean(as.numeric(CTAS1_EMS$EMS_RT),na.rm=T)
              CTAS3_EMS<-df_HW%>%
                filter(triage_score==3)
              CTAS3_EMS<-mean(as.numeric(CTAS3_EMS$EMS_RT),na.rm=T)
              IPA<-quantile(df_HW$Time_to_IPA, probs = 0.9,na.rm=T)
              TotalPTs<-nrow(df_HW)
              PTsEDWait3<-sum(df_HW$Extended_ED_Wait_Flag3hr==1,na.rm=T)
              PTsEDWait6<-sum(df_HW$Extended_ED_Wait_Flag6hr==1,na.rm=T)
              PTsCodeEMS<-sum(df_HW$EMS_CPR_Wait_OK==1,na.rm=T)
              PTsCodeEMSWait7Plus<-sum(df_HW$EMS_CPR_Wait_Flag==1,na.rm=T)
              
              df_HW<-as.data.frame(colMeans(df_HW,na.rm = T))
              df_HW[17,1]<-CTAS1_EMS
              df_HW[18,1]<-CTAS3_EMS
              df_HW[19,1]<-IPA
              df_HW[20,1]<-PTsEDWait3
              df_HW[21,1]<-PTsEDWait6
              df_HW[22,1]<-PTsCodeEMS
              df_HW[23,1]<-PTsCodeEMSWait7Plus
              df_HW[24,1]<-TotalPTs
              
          #post-heatwave (7 days)
              df_PHW<-temp_df %>% 
                filter(start_time > HWStop & start_time < HWStop+(1440*7))
              
              CTAS1_EMS<-df_PHW%>%
                filter(triage_score==1)
              CTAS1_EMS<-mean(as.numeric(CTAS1_EMS$EMS_RT),na.rm=T)
              CTAS3_EMS<-df_PHW%>%
                filter(triage_score==3)
              CTAS3_EMS<-mean(as.numeric(CTAS3_EMS$EMS_RT),na.rm=T)
              IPA<-quantile(df_PHW$Time_to_IPA, probs = 0.9,na.rm=T)
              TotalPTs<-nrow(df_PHW)
              PTsEDWait3<-sum(df_PHW$Extended_ED_Wait_Flag3hr==1,na.rm=T)
              PTsEDWait6<-sum(df_PHW$Extended_ED_Wait_Flag6hr==1,na.rm=T)
              PTsCodeEMS<-sum(df_PHW$EMS_CPR_Wait_OK==1,na.rm=T)
              PTsCodeEMSWait7Plus<-sum(df_PHW$EMS_CPR_Wait_Flag==1,na.rm=T)
              
              df_PHW<-as.data.frame(colMeans(df_PHW,na.rm = T))
              df_PHW[17,1]<-CTAS1_EMS
              df_PHW[18,1]<-CTAS3_EMS
              df_PHW[19,1]<-IPA
              df_PHW[20,1]<-PTsEDWait3
              df_PHW[21,1]<-PTsEDWait6
              df_PHW[22,1]<-PTsCodeEMS
              df_PHW[23,1]<-PTsCodeEMSWait7Plus
              df_PHW[24,1]<-TotalPTs
              
              DF<-cbind(df_base,df_HW,df_PHW)
              
              DF<-DF%>%
                dplyr::mutate_if(is.numeric, round, digits=3)
              
              DF<-DF[c(-1,-2,-4,-5,-6,-10),]
              
              rn<-c("Mean ALC time (minutes)","Mean ICU LOS (minutes)","Mean non-ICU ward LOS (minutes)",
                    "Mean triage wait time (minutes)","Mean time until physician assessment","Mean ED boarding time for admitted pts (minutes)",
                    "Extended_ED_Wait_Flag3hr","Extended_ED_Wait_Flag6hr","EMS_CPR_Wait_Flag","EMS_CPR_Wait_OK",
                    "Mean ambulance response time for CTAS 1 (minutes)","Mean ambulance response time for CTAS 3 (minutes)","Time until physician assessment for 90% of pts",
                    "Number PTs Wait >3 and <6 hours for IPA","Number PTs Wait >6 hours for IPA","Prehospital codes with <7 response","Prehospital codes with >7 response","Total PTs")
              
              DF<-cbind(rn,DF,i)
              
              colnames(DF)<-c("variable","Model baseline mean","Heatwave mean","Post heatwave mean","replication")
              
              
              if(i!=1){DFOut<-rbind(DFOut,DF)}else if (i==1){DFOut<-DF}
          }
      
    
    
    reactable(as.data.frame(DFOut),
              theme=reactableTheme(style=list(fontFamily="Montserrat,sans-serif")),
              fullWidth = T,rownames = F,
              defaultColDef = colDef(align="left", headerStyle=list(background="#b4cbce"),minWidth = 120),
              bordered=F, striped = T, highlight=T,showSortable = T,
              minRows = 1, showPageSizeOptions = T,defaultPageSize=20
    )
    
  })
  
  output$downloadMonteCarloTable<-downloadHandler(
    filename=function(){
      paste0("MonteCarloRuns_",input$CatchmentPop,"_",input$HA,".csv")
    },
    content=function(file){
      nI<-input$iterations
      
      Log1<-get_mon_arrivals(Hmodel())
      Log2<-as.data.frame(get_mon_attributes(Hmodel()))
      Log2<-Log2[,2:5]
      colnames(Log2)<-c("name","key","value","replication")
      Log2$value<-as.numeric(Log2$value)
      Log2<-reshape2::dcast(Log2,name+replication~key,mean,fill=NaN)
      Log<-merge(Log1,Log2,by=c("name","replication"))
      
      HWStart<-HWStartTimes()
      HWStart<-HWStart[1]
      HWStop<-HWStopTimes()
      HWStop<-HWStop[24]
      
      
      df<-Log%>%
        arrange(start_time)
      
      
      df<-df[,c(2,3,8,11,25,26,40,41,49,48,35,50)]
      
      df<-df %>% 
        mutate_if(is.character, as.numeric)
      
      
      
      
      
      colnames(df)<-c("replication","start_time","ALC","EMS_RT","ED boarding time for ICU","ED boarding time for Non-ICU","Patient ICU LOS",
                      "Patient Non-ICU ward LOS","Wait to triage patient","triage_score","Time_to_IPA","boarding_time")
      
      df[,13]<-ifelse(df$triage_score==3 & df$Time_to_IPA >= (3*60) & df$Time_to_IPA <(6*60),1,0)
      df[,14]<-ifelse(df$triage_score==3 & df$Time_to_IPA >= (6*60),1,0)
      
      df[,15]<-ifelse(df$EMS_RT>7 & df$triage_score==1,1,0)
      df[,16]<-ifelse(df$EMS_RT<7 & df$EMS_RT>0 & df$triage_score==1,1,0)
      
      
      colnames(df)<-c("replication","start_time","ALC","EMS_RT","ED boarding time for ICU","ED boarding time for Non-ICU","Patient ICU LOS",
                      "Patient Non-ICU ward LOS","Wait to triage patient","triage_score","Time_to_IPA","boarding_time","Extended_ED_Wait_Flag3hr","Extended_ED_Wait_Flag6hr","EMS_CPR_Wait_Flag","EMS_CPR_Wait_OK")
      
      
      
      for(i in 1:(max(df$replication))){
        temp_df<-df%>%
          filter(replication==i)
        
        ## Baseline data (day 16 to 30)
        df_base<-temp_df %>%
          filter(start_time < HWStart & start_time>23040)
        
        CTAS1_EMS<-df_base%>%
          filter(triage_score==1)
        CTAS1_EMS<-mean(as.numeric(CTAS1_EMS$EMS_RT),na.rm=T)
        CTAS3_EMS<-df_base%>%
          filter(triage_score==3)
        CTAS3_EMS<-mean(as.numeric(CTAS3_EMS$EMS_RT),na.rm=T)
        IPA<-quantile(df_base$Time_to_IPA, probs = 0.9,na.rm=T)
        TotalPTs<-nrow(df_base)
        PTsEDWait3<-sum(df_base$Extended_ED_Wait_Flag3hr==1,na.rm=T)
        PTsEDWait6<-sum(df_base$Extended_ED_Wait_Flag6hr==1,na.rm=T)
        PTsCodeEMS<-sum(df_base$EMS_CPR_Wait_OK==1,na.rm=T)
        PTsCodeEMSWait7Plus<-sum(df_base$EMS_CPR_Wait_Flag==1,na.rm=T)
        
        df_base<-as.data.frame(colMeans(df_base,na.rm = T))
        df_base[17,1]<-CTAS1_EMS
        df_base[18,1]<-CTAS3_EMS
        df_base[19,1]<-IPA
        df_base[20,1]<-PTsEDWait3
        df_base[21,1]<-PTsEDWait6
        df_base[22,1]<-PTsCodeEMS
        df_base[23,1]<-PTsCodeEMSWait7Plus
        df_base[24,1]<-TotalPTs
        
        
        #Heatwave data (8 days)
        df_HW<-temp_df %>%
          filter(start_time > HWStart & start_time < HWStop)
        
        CTAS1_EMS<-df_HW%>%
          filter(triage_score==1)
        CTAS1_EMS<-mean(as.numeric(CTAS1_EMS$EMS_RT),na.rm=T)
        CTAS3_EMS<-df_HW%>%
          filter(triage_score==3)
        CTAS3_EMS<-mean(as.numeric(CTAS3_EMS$EMS_RT),na.rm=T)
        IPA<-quantile(df_HW$Time_to_IPA, probs = 0.9,na.rm=T)
        TotalPTs<-nrow(df_HW)
        PTsEDWait3<-sum(df_HW$Extended_ED_Wait_Flag3hr==1,na.rm=T)
        PTsEDWait6<-sum(df_HW$Extended_ED_Wait_Flag6hr==1,na.rm=T)
        PTsCodeEMS<-sum(df_HW$EMS_CPR_Wait_OK==1,na.rm=T)
        PTsCodeEMSWait7Plus<-sum(df_HW$EMS_CPR_Wait_Flag==1,na.rm=T)
        
        df_HW<-as.data.frame(colMeans(df_HW,na.rm = T))
        df_HW[17,1]<-CTAS1_EMS
        df_HW[18,1]<-CTAS3_EMS
        df_HW[19,1]<-IPA
        df_HW[20,1]<-PTsEDWait3
        df_HW[21,1]<-PTsEDWait6
        df_HW[22,1]<-PTsCodeEMS
        df_HW[23,1]<-PTsCodeEMSWait7Plus
        df_HW[24,1]<-TotalPTs
        
        #post-heatwave (7 days)
        df_PHW<-temp_df %>% 
          filter(start_time > HWStop & start_time < HWStop+(1440*7))
        
        CTAS1_EMS<-df_PHW%>%
          filter(triage_score==1)
        CTAS1_EMS<-mean(as.numeric(CTAS1_EMS$EMS_RT),na.rm=T)
        CTAS3_EMS<-df_PHW%>%
          filter(triage_score==3)
        CTAS3_EMS<-mean(as.numeric(CTAS3_EMS$EMS_RT),na.rm=T)
        IPA<-quantile(df_PHW$Time_to_IPA, probs = 0.9,na.rm=T)
        TotalPTs<-nrow(df_PHW)
        PTsEDWait3<-sum(df_PHW$Extended_ED_Wait_Flag3hr==1,na.rm=T)
        PTsEDWait6<-sum(df_PHW$Extended_ED_Wait_Flag6hr==1,na.rm=T)
        PTsCodeEMS<-sum(df_PHW$EMS_CPR_Wait_OK==1,na.rm=T)
        PTsCodeEMSWait7Plus<-sum(df_PHW$EMS_CPR_Wait_Flag==1,na.rm=T)
        
        df_PHW<-as.data.frame(colMeans(df_PHW,na.rm = T))
        df_PHW[17,1]<-CTAS1_EMS
        df_PHW[18,1]<-CTAS3_EMS
        df_PHW[19,1]<-IPA
        df_PHW[20,1]<-PTsEDWait3
        df_PHW[21,1]<-PTsEDWait6
        df_PHW[22,1]<-PTsCodeEMS
        df_PHW[23,1]<-PTsCodeEMSWait7Plus
        df_PHW[24,1]<-TotalPTs
        
        DF<-cbind(df_base,df_HW,df_PHW)
        
        DF<-DF%>%
          dplyr::mutate_if(is.numeric, round, digits=3)
        
        DF<-DF[c(-1,-2,-4,-5,-6,-10),]
        
        rn<-c("Mean ALC time (minutes)","Mean ICU LOS (minutes)","Mean non-ICU ward LOS (minutes)",
              "Mean triage wait time (minutes)","Mean time until physician assessment","Mean ED boarding time for admitted pts (minutes)",
              "Extended_ED_Wait_Flag3hr","Extended_ED_Wait_Flag6hr","EMS_CPR_Wait_Flag","EMS_CPR_Wait_OK",
              "Mean ambulance response time for CTAS 1 (minutes)","Mean ambulance response time for CTAS 3 (minutes)","Time until physician assessment for 90% of pts",
              "Number PTs Wait >3 and <6 hours for IPA","Number PTs Wait >6 hours for IPA","Prehospital codes with <7 response","Prehospital codes with >7 response","Total PTs")
        
        DF<-cbind(rn,DF,i)
        
        colnames(DF)<-c("variable","Model baseline mean","Heatwave mean","Post heatwave mean","replication")
        
        
        if(i!=1){DFOut<-rbind(DFOut,DF)}else if (i==1){DFOut<-DF}
      }
      
      write.csv(DFOut,file)
    })
  
  
  output$TableText<-renderReactable({
    req(DF_Dist)
    df<-DF_Dist()
    HWStart<-(30)*1440
    HWStop<-HWStopTimes()
    HWStop<-HWStop[24]
    
    
    df<-df%>%
      dplyr::mutate_if(is.numeric, round, digits=2)
    
    df[,51]<-ifelse(df$start_time < HWStart & df$start_time > 23040,"baseline",
                    ifelse(df$start_time > HWStart & df$start_time < HWStop,"HW",
                    ifelse(df$start_time > HWStop & df$start_time < HWStop+(1440*7),"post HW",0)))
      
    reactable(as.data.frame(df),
              theme=reactableTheme(style=list(fontFamily="Montserrat,sans-serif")),
              fullWidth = T,
              defaultColDef = colDef(align="left", headerStyle=list(background="#b4cbce"),minWidth = 200,filterable=T),
              
              bordered=F, striped = T, highlight=T,showSortable = T,
              minRows = 1, showPageSizeOptions = T, pageSizeOptions = c(10,20,50),defaultPageSize=10)
    
  })
  
  output$downloadLargeTable<-downloadHandler(
    filename=function(){
      paste0("AllPatients_",input$CatchmentPop,"_",input$HA,".csv")
    },
    content=function(file){
            Log1<-get_mon_arrivals(Hmodel())
            Log2<-as.data.frame(get_mon_attributes(Hmodel()))
            Log2<-Log2[,2:5]
            colnames(Log2)<-c("name","key","value","replication")
            Log2$value<-as.numeric(Log2$value)
            Log2<-reshape2::dcast(Log2,name+replication~key,mean,fill=NaN)
            Log<-merge(Log1,Log2,by=c("name","replication"))
            
            HWStart<-(30)*1440
            HWStop<-HWStopTimes()
            HWStop<-HWStop[24]
            
            df[,51]<-ifelse(df$start_time < HWStart & df$start_time > 23040,"baseline",
                            ifelse(df$start_time > HWStart & df$start_time < HWStop,"HW",
                                   ifelse(df$start_time > HWStop & df$start_time < HWStop+(1440*7),"post HW",0)))
            
      write.csv(Log,file)
    })
  
    #Distribution panels
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
      
      #Triage score 
          output$Dist1<-renderPlot({
            req(DF_Dist)
            df<-DF_Dist()
            HWStart<-HWStartTimes()
            HWStart<-HWStart[1]
            HWStop<-HWStopTimes()
            HWStop<-HWStop[24]
            
            #simulation period
            for(i in (1:nrow(df))){
              actTime<-df[i,4]
              if(actTime>HWStart & actTime<HWStop){v<-"heatwave"} else if (actTime<HWStart & actTime>(1440*16)){v<-"baseline"}else if (actTime>HWStop & actTime<(HWStop+(1440*7))) {v<-"post_heatwave"}else{v<-"ramping"}
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
              'post_heatwave'="Post heatwave",
              'ramping'='Model ramping up')
            
            ggplot(df, aes(x=triage_score, fill=simulation_period))+
              geom_bar()+
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
            HWStart<-HWStartTimes()
            HWStart<-HWStart[1]
            HWStop<-HWStopTimes()
            HWStop<-HWStop[24]
            
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
              actTime<-df[i,4]
              if(actTime<43200 & actTime>23040){v<-"baseline"}else if(actTime>HWStart & actTime<HWStop){v<-"heatwave"}else if(actTime>HWStop & actTime<HWStop+(1440*7)){v<-"post_heatwave"}else{v<-"ramping"}
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
              }else if(!is.na(df[i,38])){ws<-"Non-ICU ward"}else{ws<-"Not admitted to ward"}
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
              'post_heatwave'="Post heatwave",
              'ramping'="Model warmup")
            
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
        HWStart<-HWStartTimes()
        HWStart<-HWStart[1]
        HWStop<-HWStopTimes()
        HWStop<-HWStop[24]
        
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
          actTime<-df[i,4]
          if(actTime<HWStart & actTime>23040){v<-"baseline"}else if(actTime>HWStart & actTime<HWStop){v<-"heatwave"}else if(actTime>HWStop & actTime<HWStop+(1440*7)){v<-"post_heatwave"}else{v<-"ramping"}
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
          'post_heatwave'="Post heatwave",
          'ramping'="Model warmup")
        
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
      
      #Time in ICU and Non-ICU
          output$Dist4<-renderPlot({
            req(DF_Dist)
            df<-DF_Dist()
            HWStart<-HWStartTimes()
            HWStart<-HWStart[1]
            HWStop<-HWStopTimes()
            HWStop<-HWStop[24]
            
            ####
            # Edits to df ####
            df$ed_admit_time<-as.numeric(df$ed_admit_time)
            df$ed_arrival_time<-as.numeric(df$ed_arrival_time)
            df<-df %>%
              mutate(ed_door_to_bed_all=rowMeans(.[,c("door_to_ed_bed_wait_for_ft_bed","door_to_ed_bed_wait_for_highest_severity","door_to_ed_bed_wait_for_lowest_severity")],na.rm=T))
            
            df<-df %>%
              mutate(time_in_ward=rowSums(.[,c("patient_icu_los","patient_non_icu_ward_los","alc")],na.rm=T))
                     
            df<-df %>%
              mutate(time_in_ed_all=rowMeans(.[,c("time_in_ed_for_highest_severity","time_in_ed_for_lowest_severity","time_in_ed_for_middle_severity")],na.rm=T))
            
            #simulation period
            for(i in (1:nrow(df))){
              actTime<-df[i,4]
              if(actTime<HWStart & actTime>23040){v<-"baseline"}else if(actTime>HWStart & actTime<HWStop){v<-"heatwave"}else if(actTime>HWStop & actTime<HWStop+(1440*7)){v<-"post_heatwave"}else{v<-"ramping"}
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
              }else if(!is.na(df[i,38])){ws<-"Non-ICU ward"}else{ws<-"Not admitted to ward"}
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
              'post_heatwave'="Post heatwave",
              'ramping'="Model warmup")
            
            ggplot(df%>% filter(ward_status !="Not admitted to ward" & simulation_period != "post_heatwave"), aes(x=as.factor(ward_status),y=time_in_ward/1440))+
              geom_boxplot()+
              geom_jitter(aes())+
              facet_wrap(~factor(simulation_period),scales = "fixed",labeller=as_labeller(heatwave_non))+
              xlab("Simulation period") +
              ylab("Total inpatient LOS (days)")+
              labs(title="Distribution ICU and non-ICU ward LOS")+
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
      
      
   ##### 
  
  #END server
  
})