library(DT)
library(leaflet)
library(ggplot2)



navbarPage("Toronto Child Care Study", id='nav', 
       

      tabPanel("Interactive map",           
          div(class="outer",
              
              tags$head(
                
                ####I included a custom CSS given in the shiny example in Rstudio, "superzip" by Joe Cheng.
                
                includeCSS("styles.css"),
                
                includeScript("gomap.js")
                
              ),
              
              leafletOutput("map", width="100%", height="100%"),
              
              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            
                            draggable = TRUE, top = "auto", left = "auto", right = 20, bottom =40 ,
                            
                            width = 330, height = "auto",
                        
                
              
                      checkboxGroupInput('regions', 'Districts', 
                              choices=c(
                                  'Etobicoke York',
                                  'North York',
                                  'Scarborough',
                                  'Toronto East York'
                                  ),
                              selected=c('Etobicoke York', 'North York', 'Scarborough', 'Toronto East York')
                              ),
                            
                      checkboxGroupInput('support', 'Subsidy', 
                              choices=c(
                                  'No',
                                  'Yes'
                                  ),
                              selected=c('No', 'Yes')
                            ),
                            
                      checkboxGroupInput('centretype', 'Type', 
                              choices=c(
                                  'City-Operated',
                                  'Commercial',
                                  'Non-Profit'
                                  ),
                              selected=c('City-Operated', 'Commercial', 'Non-Profit')
                            ),
                      actionButton('circle', 'Search')
                    )
              )
        ),
    
    tabPanel('Data Explorer',
             fluidRow(
               column(3,
                  selectInput('districts', 'Districts', c('All districts'='', 'Etobicoke York', 'North York', 'Scarborough', 'Toronto East York'), multiple=TRUE)
               ),
               column(3,
                  conditionalPanel('input.districts',
                      selectInput('types', 'Types', c('All types'=''),multiple = TRUE)
                                  )
                    ),
               column(3,
                  conditionalPanel('input.districts',
                      selectInput('fees','Subsidy', c('Both'=''),multiple=TRUE)
                                  )    
                    )
             ),
             hr(),
             DT::dataTableOutput('cctable')
            ),
    
    tabPanel('Key Insights',
             
             fluidRow(
               column(2,
                      helpText('The overview of the number child care centres is provided in the Google pie chart. The line chart compares the number of children on the wait list with the number of child spaces in the City of Toronto.'),
                      
                      wellPanel(
                        selectInput('by', 'Show by',
                                    choices=c(
                                      'subsidy',
                                      'type',
                                      'district'
                                    ),
                                    selected='subsidy',
                                    multiple = FALSE
                                    )
                                )
                      ),
               
               column(5,
                      htmlOutput('glineplot1')),
               
               
               column(5,
                      htmlOutput('glineplot2'))
             ),
             
             br(),
             br(),

             fluidRow(
               column(2,
                  wellPanel(
                    
                     selectInput('horizontal', 'Display horizontal axis by',
                                        choices=c(
                                          'district', 
                                          'type'),
                                        multiple = FALSE
                                  ),
                    
                      selectInput('childclass', 'Child Class',
                                  choices=c(
                                    'Total', 
                                    'Infant', 
                                    'Toddler', 
                                    'Preschooler', 
                                    'Kindergarten', 
                                    'Gradelevel'),
                                  multiple = FALSE
                                  ),
                      
                      checkboxGroupInput('sub','Fee Subsidy Contract',
                                  choices=c(
                                    'No',
                                    'Yes',
                                    'By Subsidy'='subsidy'
                                    ),
                                  selected=NULL
                                    ),
                      helpText('Click on By Subsidy to compare number of child care centres for fee subsidy choice.')
                          )
                    ),
               hr(),
               column(5,
                      plotOutput('barplot')
               ),
    
               column(5,
                      plotOutput('barplot2')
                      )
                )
             ),
            
    
    tabPanel('About This Application', 
             tags$div(
                  tags$p('Welcome to my Shiny application.'),
                  tags$p('This application has three purposes. In the first tab, the user can find the child care centres depending on his or her preferences given in the selection panel.'),
                  tags$p('Second tab allows the user to find the detail information about the child care centres.'),
                  tags$p('The last panel provides the key insights obtained from the dataset. This page includes barplots for various variables and Google chart to visualize the overview of the child care centres in the City of Toronto.'), br(),
                  tags$p(
                    tags$h4('Data Source')), 
                  'Datasets in this application is obtained from the City of Toronto', 
                  tags$a(href="https://www1.toronto.ca/wps/portal/contentonly?vgnextoid=1a66e03bb8d1e310VgnVCM10000071d60f89RCRD", "open data collection."),
                  tags$p('If you have any question, please feel free to contact me.'),
                  tags$p('Mustafa Koroglu'),
                  tags$p('July 18, 2017')
                  )
    )      
                  
)  

