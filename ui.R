### BC health model
# (c) Dylan G. Clark
# 2022


library(shiny)
library(reactable)


shinyUI(
  bootstrapPage(title="BC Heatwave Dashboard",
                windowTitle=NA,
                tags$head(
                  tags$link(rel="stylesheet",type="text/css",href="www/CCI_style.css")
                ),
                
                
                tags$head(
                  tags$style(HTML("hr {border-top: 3px solid #B4B4B4;}"))
                ),
                
                ###############################################.
                ## Landing page ----
                ###############################################.
                
                tags$style('.container-fluid {
                    background-color: #E4EDEE}'),
                
                
                navbarPage(title=img(src="",height="40px",style='padding-top=0px'),
                           collapsible = T,
                           windowTitle = "BC Heatwave Dashboard",
                           position = "fixed-top",
                           header=tags$style(".navbar-right{float:right !important;}",
                                             "body {padding-top:0px;}",
                                             ".navbar-default .navbar-nav > .active > a",
                                             ".navbar-default .navbar-nav > .active > a:focus",
                                             ".navbar-default .navbar-nav > .active > a:hover {
                                 color:#E4EDEE; text-decoration:underline;}",
                                             ".navbar-default .navbar-nav > li > a:hover {
                                         color:#E17031; text-decoration:underline;}",
                                             ".navbar-header {float:left;}",
                                             ".navbar {
                                    font-family:'Montserrat'; 
                                    font-size: 30;}",
                                             ".navbar {
                                    min-height: 60px;
                                    margin-bottom: 23px;          
                                    border: 1px solid lightgrey;
                                         }",
                                             "@media (min-width: 768px) {
                                             .navbar {
                                                 border-radius: 3px;}
                                         }",
                                             "@media (min-width: 768px) {
                                             .navbar-header {
                                                 float: left;}
                                         }"
                           ),
    ###############################################.
    ## 'Climate' page ----
    ###############################################.                           

    tabPanel(h6("Climate Analyzer"), fluid=T,
        titlePanel("",title= tags$img(src='AnalystHeader.png', style='height:auto; max-width:100%; width:100%; padding-top:0px; padding-bottom:0px; padding-left:0px; padding-right:0px')),
                                    
    
    

    
    tags$style('.container-fluid {
                    background-color: #E1E4DA}'),

    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      fluid=T,
      position="left",
      sidebarPanel(
        width=4,
        br(),br(),
        h2("Climate Projection Options",align="center"),
        fluid=T,
        
        paste0("This is an open access portal. Original data is from PCIC."),
        br(),br(),
        
        
        br(),br(),
        shinycssloaders::withSpinner(
          uiOutput("inGCM",width="200px"),color="#545F66"),
        br(),
        paste0("Next, users should select a spatial resolution to explore. Note that it will take a minute to load the 'Population centre' database"),
        br(),br(),
        uiOutput("inGeoScale"),
        br(),
        actionButton("UpdateData","Load climate data"),
        br(),
        paste0("Once you select the GCM and spatial scale, load the data"),
        br(),

        hr(),
        br(),br(),
        paste0("On the right side of the page, users will see graphs to illustrate the temporal (annual and interannual) trends based on the variables selected below."),
        br(),br(),
        uiOutput("inPlaces"), 
        br(),
        br(),br(),
        paste0("Select the sector you would like to calculate impacts for (THIS IS NOT CURRENTLY IN USE)"),
        br(),br(),
        uiOutput("inSector"),
        br(),br(),br(),br(),br(),
        br(),br(),br(),br(),br(),
        p("", style = "margin-bottom: 300px;"),
        
        
        br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
        br(),br(),br(),br(),br(),
        p("", style = "margin-bottom: 1000px;"),
        p("", style = "margin-bottom: 1000px;"),
        
        br()
      ),
      
      
      
      mainPanel(
        
        h1("Climate Dashboard",align="center"),
        fluid=T,
        position="right",
        width=7, 
        style="margin-left:4%;margin-right:4%",
        br(),br(),
        
        
        shinycssloaders::withSpinner(
          plotOutput("AllPlot",height="1000px",width="100%"), color="#545F66"),
        p("This graph depicts the mean annual days that exceed B.C. heat warning criteria (2022) by decadal average. 
                   The values show annual average for the selected spatial scale. The chart is dynamic to the GCM in the left panel.", style="color:#b9c0c5"),
        br(),br(),br(),
        br(),br(),
        shinycssloaders::withSpinner(
          plotOutput("PlacePlot"), color="#545F66"),
        p("This graph depicts the daily minimum temperature (green) and daily maximum temperature (purple) for the place/region selected on the left. 
                   Dots represent daily data while the line reflects annual mean values. The chart is dynamic to the GCM and variable selected in the left panel.", style="color:#b9c0c5"),
        br(),br(),br(),
        
        shinycssloaders::withSpinner(
          plotOutput("VarHist"), color="#545F66"),
        br(),
        uiOutput("inVariable"),
        p("This graph shows the distribution (histogram) of a given temperature variable (minT or maxT selected below) for each decade. 
                   The chart is dynamic to the GCM selected in the left panel.", style="color:#b9c0c5"),
        
        br(),br(),br(),
        hr(),
        br(),br()
       
        
        #Close main panel
      )
      #Close sidebar panel
    )
    #Close developer tab
  ),

#####


# Health simmer page
#####
      tabPanel(h6("Health System Modeller"),
           titlePanel(title= tags$img(src='DeveloperHeader.png', style='height:auto; max-width:100%; width:100%; padding-top:0px; padding-bottom:0px; padding-left:0px; padding-right:0px')),
      
      sidebarLayout(
             fluid=T,
             position="left",
             sidebarPanel(
               width=3,fluid=T,
               br(),br(),
               h2("Simulation Options",align="center"),
               uiOutput("IHA",width="200px"),
               br(),
               uiOutput("IHWInput",width="200px"),
               uiOutput("IHWUpload",width="300px"),
               br(),
               numericInput(inputId = "iterations",label="Number of model runs (Monte Carlo)",value=5,width="200px"),
               numericInput(inputId = "RunTime", label="Number of days to run model for", value = 35, width="200px"),
               paste0("The default heatwave will start on day 14 of the model runs."),
               br(),br(),br(),
               shinyWidgets::materialSwitch(inputId="mci",label="Allow mass casualty protocols"),
               shinyWidgets::materialSwitch(inputId="SurgeStaff",label="Increase staff and resource capacity in disasters"),
               shinyWidgets::materialSwitch(inputId="dischargeCooling",label="Invest in safe cooling facility for discharged patients"),
               br(),
               actionButton(inputId = "RunHModel",label="Run model",width="200px",class = "btn-primary btn-lg"),
               br(),br(),br(),br(),br(),
               
               h2("Advanced Options",align="left"),
               br(),br(),
               sliderInput(inputId = "CatchmentPop",label="Total population of the hospital catchment ('000s)",value=100,width="200px",step=50, min=50,max=200),
               paste0("The default values for these advanced option are responsive to the population size selected above."),
               br(),br(),
               fluidRow(column(5,uiOutput("InEDBeds",width="200px")),
                        column(5,uiOutput("InEDNurse",width="200px"))),
               br(),
               fluidRow(column(5,uiOutput("Indoc",width="200px")),
                        column(5,uiOutput("InTriageNurse",width="200px"))),
               br(),
               fluidRow(column(5,uiOutput("InICUbed",width="200px")),
                        column(5,uiOutput("InNonICUbed",width="200px"))),
               br(),
               fluidRow(column(5,uiOutput("InFTBeds",width="200px")),
                        column(5,uiOutput("InFTdocs",width="200px"))),
               br(),
               fluidRow(column(5,uiOutput("InFTnurse",width="200px")),
                        column(5,uiOutput("InPolice",width="200px"))),
               br(),
               fluidRow(column(5,uiOutput("InFire",width="200px")),
                        column(5,uiOutput("InAmbulance",width="200px"))),
               br(),
               fluidRow(column(5,uiOutput("InDispatcher",width="200px")),
                        column(5,uiOutput("InCoroner",width="200px"))),
               br(),
               uiOutput("IAmbRespT",width="400px"),
               br(),
               uiOutput("IFireRespT",width="400px"),
               br(),
               uiOutput("ITransToHospSIREN",width="400px"),
               br(),
               uiOutput("ITransToHosp",width="400px"),
               br()
            ),
           
      mainPanel(
          h1("Modeller Dashboard",align="center"),
          fluid=T,
          position="right",
          width=8, 
          style="margin-left:2%;margin-right:2%",
          br(),br(),br(),
          shinycssloaders::withSpinner(
            plotOutput("Simmer1",width="100%"), color="#545F66"),
          br(),
          uiOutput("inResource"),
          br(),br(),
          shinycssloaders::withSpinner(
            plotOutput("Simmer2",width="100%"), color="#545F66"),
          br(),br(),
          shinycssloaders::withSpinner(
            plotOutput("Simmer3",width="100%"), color="#545F66"),
          br(),br(),
          shinycssloaders::withSpinner(
            plotOutput("Simmer4",height="1000px",width="100%"), color="#545F66"),
          br(),br(),
          shinycssloaders::withSpinner(
            plotOutput("Simmer4.5",height="1000px",width="100%"), color="#545F66"),
          br(),br(),
          shinycssloaders::withSpinner(
            plotOutput("Simmer5",height="1000px",width="100%"), color="#545F66"),
          br(),br(),
          reactableOutput("TableText"),
          br(),
          downloadButton("downloadTable","Download this data as a .csv",style="background-color:#F5FBFA;margin:25px 5px 15px 5px;"),
          br(),br(),br()
          
      ))
        #Close analyst tab       
      )
   #close navpage
   )
  #close bootstrap page
  )
#end
)