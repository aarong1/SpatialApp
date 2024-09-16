#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(plyr)
library(shiny)

#load('all.Rdata')
source('requirements.R')

# Define UI for application that draws a histogram
ui <- fluidPage(includeCSS("www/styles.css"), style = 'margin-top:10px',
                theme = bs_theme(version = 5,primary='#36a165' ),
                 #includeCSS("style.css"),
                #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
              fluidRow(
                  column(1,img(src='img/network_icon.png',style='float:left;height:150px')),
                column(7,offset=2,
                       h1('Modeling geospatial Socio-Economic attributes',
                                     h5('In Northern Ireland'),
                                     style='float:centre;text-color:black;'),hr(),
                       ),
                column(1,img(src='img/ni.png',style='height:180px'))
                ),
                tabsetPanel( type = 'pills', header=hr(),
                  tabPanel(icon = icon('map'), 'Intro',br(),
                
                p('   ',shiny::tags$b('Postcode stubs'), ' are one of the medium scale statistical areas in Northern Ireland that
                allow for collection of commuity statistics. The shape files for Postcode Districts are, unlike those of the ', shiny::tags$b("Small Areas"),
                  ' not readily available in open source.  ',shiny::tags$i('Although,'),' the mapping of some long form postcodes are available
                  along with their latitude and longitude.  Using these we can model the Postcode District boundaries as voroni tesselations
                  and construct shape files.  This allows us to determine adjacency that we have show in the left hand panels. Contingency,
                  or adjacency, is an important feature in allowing for geospatial ',shiny::tags$b('aurocorrelation'),' modelling. In particular we run
                  ',shiny::tags$b('Morans I lag'), ' statistic to determine if there is sufficient correlation in distance of one postcodes districts dependent variable with others.
                  Here the definition of ,',shiny::tags$i('distance'), 'is explored on the topic of feature extraction.'),
      # hr(),
                #card('',style='height:150px;background: linear-gradient(#e66465, #9198e5);'),
        fluidRow(
      column(5,offset = 1,
    # Application title
    div(style='margin:10px;',#box-shadow:10px 10px; padding:20px;border:solid 1px grey;border-radius: 15px;padding:5px;background-color:rgba(50,50,50,0.05)',
        h3('The continguency graph showing adjacency statistical boundaries'),
           p(style='border:solid black;border-width: 0px 0px 0px 0px;','Network representation of PC districts that share a border'),
    visNetworkOutput('network'),br()

    )
    ,br(),br(),
    div(#class = 'grow',style='box-shadow:10px 10px; padding:20px;border:solid 1px grey;border-radius: 15px;padding:5px;background-color:rgba(50,50,50,0.05)',
        h3('Contingency Matrix'),
        p('Which districts are beside each other'),
    plotlyOutput('matrix'),br()
    )
    ),

    column(4,    offset=1,
               div(style='margin:10px;box-shadow:10px 10px rgba(50,50,50,0.2); padding:20px;border:solid 1px grey;border-radius: 15px;padding:5px;background-color:rgba(50,50,50,0.05)',

    h3('The map of Postcode stub districts'),
       p('Model of PC polygons using Voroni tesselation'),
    br(),
   leafletOutput('map')),br(),br(),
   # div(style='margin:10px;box-shadow:10px 10px rgba(50,50,50,0.2); padding:20px;border:solid 1px grey;border-radius: 15px;padding:5px;background-color:rgba(50,50,50,0.05)',
    h3('Histogram of global Case Counts'),
       p(' '),br(),br(),
   echarts4rOutput('hist')
   )
   # )
   ,
   column(1,textOutput('selected'))
        ),br()),
   tabPanel(icon = icon('table'),'Feature',br(),
         fluidRow(column(4,    
            div(style='align:centre;box-shadow:5px 5px rgba(50,50,50,0.2); padding:20px;border:solid 1px grey;border-radius: 15px;padding:5px;background-color:rgba(50,50,50,0.05)',
        h3('Visual Borders'),
        p('Shared borders and extracted lengths',style='border:solid black;border-width: 0px 0px 1px 0px;'),
    plotOutput('borders')
    )
    ),
   
      column(3,
     div(#style='box-shadow:10px 10px rgba(50,50,50,0.2); padding:20px;border:solid 1px grey;border-radius: 15px;padding:5px;background-color:rgba(50,50,50,0.05)',
        h3('BT1 Shared Borders'),
        p('For a qualitative description of connectedness for a quantative one, we measure the shared perimeters between the
          PC districts.  Showing the degree ofkissing ploygons.'),
        
    formattableOutput('shared_table',width = '95%')

     )),
    column(5,
           div(style='box-shadow:10px 10px rgba(50,50,50,0.2); padding:20px;border:solid 1px grey;border-radius: 15px;padding:5px;background-color:rgba(50,50,50,0.05)',
        h3('Degrees of Separation'),
        p('Separation by touching connectedness. Longer range connectedness implies weaker correlation'),
           plotOutput('dos_map')
           )
           )),
    br(),hr(),br(),
    fluidRow(
      column(6,
             h3('All individual Polygon PC districts'),
             p('Spatial Statistics to build a feature store for weighting contributions
             of these districts varaibles and attributes in any modelling application'),
    reactableOutput('attr_table',width = '100%'),br(),br()),
    column(6,
           br(),br(),div(style='box-shadow:10px 10px rgba(50,50,50,0.2); padding:20px;border:solid 1px grey;border-radius: 15px;padding:5px;background-color:rgba(50,50,50,0.05)',
        h3('K=10 Nearest Neighbours'),
        p('Separation by centroid distance (max 10)'),
    plotOutput('knn_map'))
    )
    )
    ),
   
   tabPanel('Model',icon=icon('compass'),br(),
            #div(style='box-shadow:10px 10px rgba(50,50,50,0.2); padding:20px;border:solid 1px grey;border-radius: 15px;padding:5px;background-color:rgba(50,50,50,0.05)',
            fluidRow(
              column(6,offset=1,
        h3('Chart of Positive Cases against the lagged Positive cases for k=10'),
        p('This compares the Case Count for BT1 against the lagged, or spatially proximal
          case count for close by districts, using our constructed metrics for closeness and similarity'),br(),
            echarts4rOutput('moranchart'),br()),
        column(3,offset=1,
               uiOutput('morans')
        )
            )
            #)ßß
  ),
   
   tabPanel('Submission',br()#,
            
            #uiOutput('frame')
  )
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
    showNotification(type = 'warning',duration = 10,"Focused on BT1")

    output$map <- renderLeaflet(mapshiny)
    
    output$network <- renderVisNetwork(networkshiny)
    
    output$matrix <- renderPlotly(matggplotly)
    
    output$hist <- renderEcharts4r(chart_hist)
    
    output$borders <- renderPlot(bt1contactarea)
    
    output$shared_table <- renderFormattable(
      data.frame(PC1 = SD_overlap$po1.postcode,
                 PC2 = opc$po1.postcode[unlist(map(SD_overlap$origins,~.x[2]))],
                 `len` = units::drop_units(signif(digits = 3,SD_overlap$length_intersection/1000)))%>%filter(PC1=='BT1')%>%
        formattable(list(
  area(col = c(len)) ~ normalize_bar("mediumseagreen", 0.2)
)
      )
)
    
    output$attr_table <- renderReactable(attr_table)
    
    output$frame <- renderUI({
      tags$iframe(src='https://www.opendatani.gov.uk', height=900, width=1200)
   })
    
  output$dos_map <- renderPlot(dos_map)
  
  output$knn_map <- renderPlot(bt1knn)
  
  output$moranchart <- renderEcharts4r(fit_graph)
  
  output$morans <- renderUI(
    {
    htmltools::HTML(paste(h2('Morans I statistic: '),
                            h4(
                              round(
                                M1$coefficients[[2]],3)))
      )
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
