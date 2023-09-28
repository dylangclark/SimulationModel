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
          reactableOutput("TableKPI"),
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
          hr(),
          h3("Advanced Results",align="center"),
          hr(),
          br(),
          shinycssloaders::withSpinner(
            plotOutput("Dist1",height="400px",width="100%"), color="#545F66"),
          br(),
          shinycssloaders::withSpinner(
            plotOutput("Dist2",height="400px",width="100%"), color="#545F66"),
          br(),
          shinycssloaders::withSpinner(
            plotOutput("Dist3",height="400px",width="100%"), color="#545F66"),
          br(),
          shinycssloaders::withSpinner(
            plotOutput("Dist4",height="400px",width="100%"), color="#545F66"),
          br(),
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