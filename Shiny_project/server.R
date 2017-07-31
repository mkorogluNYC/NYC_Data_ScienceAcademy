library(RColorBrewer)
library(DT)
library(leaflet)
library(tidyr)
library(dplyr)
library(ggplot2)
library(googleVis)



function(input, output,session){

############################################################    
##Interactive Map###########################################
############################################################
  
######Define a color function that colors each type of child care centre.
  
  my_palette<-brewer.pal(3, "Set2")
    
   pal <- colorFactor(my_palette, domain=c('City-Operated', 'Commercial', 'Non-Profit'))   
    
   
   
#####eventReactive function is needed to have filtered dataset after each selection of the inputs to show on the map.
#####This function is used to update the map with chosen child care centres based on the selected inputs.    

  torontoMap <- eventReactive(input$circle, {
      
      toronto_cc %>% filter(district %in% input$regions &
                            subsidy %in% input$support &
                            type %in% input$centretype)  

    })
  
##### I create the map with this output function.  
######I specify the radius of the child care centre based on its contract type for fee subsidy.
  
  
  output$map <- renderLeaflet({
    
    leaflet(toronto_cc) %>%
      addProviderTiles("CartoDB.Positron") %>% 
      setView(lng =-79.38, lat = 43.70, zoom = 11) %>%
      addCircleMarkers(
        radius=10, 
        clusterOptions = markerClusterOptions(),
        color=~pal(type),
        fillOpacity=0.7, 
        stroke=FALSE,
        popup=~paste('<b><font color="Blue">',name,
                     '</font></b><br/>', 'Address:', str_no, street,
                     '</font></b><br/>','Phone:', phone, 
                     '<b><font><br/>', 'Total Space:', Total, 
                     '</font><br/>', 'Subsidy:', subsidy)) %>%
      addLegend(
        position='bottomleft',
        pal=pal,
        values=c('City-Operated', 'Commercial', 'Non-Profit'),
        opacity=0.5,
        title='Color by Type')
  }) 

 observe({
   leafletProxy('map', data=torontoMap())  %>% 
     clearMarkerClusters() %>%
    addCircleMarkers(
      radius=10, 
      clusterOptions = markerClusterOptions(),
      color=~pal(type),
      fillOpacity=0.7, 
      stroke=FALSE,
      popup=~paste('<b><font color="Blue">',name,
                   '</font></b><br/>', 'Address:', street,
                   '</font></b><br/>','Phone:', phone, 
                   '<b><font><br/>', 'Total Space:', Total, 
                   '</font><br/>', 'Subsidy:', subsidy))
     
})

##########################################################  
##Data explorer###########################################
##########################################################
  
  observe({
    types <- if(is.null(input$districts)) character(0) else{
      filter(toronto_cc, district %in% input$districts) %>%
        `$`('type')
    }
    stillSelected <- isolate(input$types[input$types %in% types])
    updateSelectInput(session, 'types', choices=types, selected = stillSelected)
  })
  
  observe({
    fees <- if(is.null(input$districts)) character(0) else{
      toronto_cc %>% 
        filter(district %in% input$districts,
               is.null(input$types) | type %in% input$types
               ) %>%
        `$` ('subsidy')
    }
    stillSelected <- isolate(input$fees[input$fees %in% fees])
    updateSelectInput(session, 'fees', choices=fees, selected = stillSelected)
  })

  output$cctable <- DT::renderDataTable({
    toronto_cc %>%
      filter(
        is.null(input$districts) | district %in% input$districts,
        is.null(input$types) | type %in% input$types,
        is.null(input$fees) | subsidy %in% input$fees
      )  
    
  }, options=list(autoWidth=TRUE, columnDefs=list(list(width='200px', targets=c(2)), list(width='120px', targets=c(10)), list(width='120px', targets=c(8))), scrollX=TRUE))

####################################################  
##Key Insights######################################
####################################################  
  
#####reactive function is used to update the dataset for the graphs after the selection of the available inputs. 
  
  
  dataUpdate <- reactive({
    
    df <- numSpace_toronto2 %>% select(district, type, input$childclass)
    
  })
  
  datadf <- reactive({
    
    if(input$sub=='subsidy'){
      df <- toronto_cc %>% select(name, type, district, input$sub)
      
    }else{
      df <- toronto_cc %>% filter(subsidy %in% input$sub)
    }
  }) 
  
  df_onlytoronto <- reactive({
    
          toronto_cc[, .N, by = eval(input$by)]
    
  })
  
  output$glineplot1 <- renderGvis({
    
    gvisColumnChart(df_onlytoronto(), 
                 options=list(width="600px", height="400px",
                              title='City of Toronto',
                              titleTextStyle="{fontSize: 14, italic: true}",
                              vAxis="{title: 'Number of Child Care Centre'}",
                              legend="{position: 'none'}"
                 ))
  })
  
  output$glineplot2 <- renderGvis({
    
    gvisLineChart(num_wait_child, 
                  xvar='Date', 
                  yvar=c('Child_Care_Spaces', 
                         'Wait_List_for_Fee_Subsidy'), 
                  options=my_options)
    
  })
  
  
  
  output$barplot <- renderPlot({
    
    if(input$horizontal=='district'){
    
        print(ggplot(dataUpdate(), aes_string(input$horizontal, input$childclass))+
            geom_bar(aes(fill=type), stat='identity', position = 'dodge')+
            labs(x='District', 
                 y='Total Number of Child Spaces', 
                 fill='Centre Type')+
            ggtitle('Child Spaces vs Centre Type')+
            theme(plot.title = element_text(hjust = 0.5, 
                                            face = 'bold.italic', 
                                            size = rel(1.5)), 
                  axis.title = element_text(size = rel(1.25), 
                                            face = 'italic'),
                  axis.text = element_text(size = rel(1.10)), 
                  legend.text = element_text(size = rel(1.10)),
                  axis.text.x = element_text(vjust = 1, hjust = 0, angle = -30),
                  legend.title = element_text(size = rel(1.10))))
    }else{
        print(ggplot(dataUpdate(), aes_string(input$horizontal, input$childclass))+
              geom_bar(aes(fill=district), stat='identity', position = 'dodge')+
              labs(x='Centre Type', 
                   y='Total Number of Child Spaces', 
                   fill='Districts')+
              ggtitle('Child Spaces vs District')+
              theme(plot.title = element_text(hjust = 0.5, 
                                              face = 'bold.italic', 
                                              size = rel(1.5)), 
                    axis.title = element_text(size = rel(1.25), 
                                              face = 'italic'),
                    axis.text = element_text(size = rel(1.10)), 
                    legend.text = element_text(size = rel(1.10)),
                    legend.title = element_text(size = rel(1.10))))
    }
  })
  
  
  output$barplot2 <- renderPlot({
    
    if(input$horizontal=='district'){
    
        if(is.null(input$sub)){
            print(ggplot(toronto_cc, aes_string(input$horizontal))+
                  geom_bar(aes(fill=type), position = 'dodge')+
                  labs(x='District', 
                      y='Total Number of Child Care Centre', 
                      fill='Centre Type')+ggtitle('Child Care Centre vs District')+
                  theme(plot.title = element_text(hjust = 0.5 , 
                                              face = 'bold.italic', 
                                              size = rel(1.5)), 
                    axis.title = element_text(size = rel(1.25), 
                                              face = 'italic'),
                    axis.text = element_text(size = rel(1.10)), 
                    legend.text = element_text(size = rel(1.10)),
                    axis.text.x = element_text(vjust = 1, hjust = 0, angle = -30),
                    legend.title = element_text(size = rel(1.10))))
      }else if(input$sub=='subsidy'){
          print(ggplot(datadf(), aes_string(input$horizontal))+
                  geom_bar(aes(fill=type), position = 'dodge')+
                  facet_grid(.~subsidy)+
                  labs(x='District', 
                       y='Total Number of Child Care Centre', 
                       fill='Centre Type')+
                  ggtitle('Child Care Centre vs District')+
                  theme(plot.title = element_text(hjust = 0.5 , 
                                              face = 'bold.italic', 
                                              size = rel(1.5)), 
                    axis.title = element_text(size = rel(1.25), 
                                              face = 'italic'),
                    axis.text = element_text(size = rel(1.10)), 
                    legend.text = element_text(size = rel(1.10)),
                    axis.text.x = element_text(vjust = 1, hjust = 0, angle = -30),
                    legend.title = element_text(size = rel(1.10)),
                    strip.text = element_text(size = rel(1.10))))
    }else{
      print(ggplot(datadf(), aes_string(input$horizontal))+
              geom_bar(aes(fill=type), position = 'dodge')+
              labs(x='District', 
                   y='Total Number of Child Care Centre', 
                   fill='Centre Type')+
              ggtitle('Child Care Centre vs District')+
              theme(plot.title = element_text(hjust = 0.5, 
                                              face = 'bold.italic', 
                                              size = rel(1.5)), 
                    axis.title = element_text(size = rel(1.25), 
                                              face = 'italic'),
                    axis.text = element_text(size = rel(1.10)), 
                    legend.text = element_text(size = rel(1.10)),
                    axis.text.x = element_text(vjust = 1, hjust = 0, angle = -30),
                    legend.title = element_text(size = rel(1.10))))
        }
    }else{
      if(is.null(input$sub)){
          print(ggplot(toronto_cc, aes_string(input$horizontal))+
                geom_bar(aes(fill=district), position = 'dodge')+
                labs(x='Centre Type', 
                     y='Total Number of Child Care Centre', 
                     fill='Districts')+ggtitle('Child Care Centre vs Centre Type')+
                theme(plot.title = element_text(hjust = 0.5 , 
                                                face = 'bold.italic', 
                                                size = rel(1.5)), 
                      axis.title = element_text(size = rel(1.25), 
                                                face = 'italic'),
                      axis.text = element_text(size = rel(1.10)), 
                      legend.text = element_text(size = rel(1.10)),
                      legend.title = element_text(size = rel(1.10))))
      }else if(input$sub=='subsidy'){
        print(ggplot(datadf(), aes_string(input$horizontal))+
                geom_bar(aes(fill=district), position = 'dodge')+
                facet_grid(.~subsidy)+
                labs(x='Centre Type', 
                     y='Total Number of Child Care Centre', 
                     fill='Districts')+
                ggtitle('Child Care Centre vs Centre Type')+
                theme(plot.title = element_text(hjust = 0.5 , 
                                                face = 'bold.italic', 
                                                size = rel(1.5)), 
                      axis.title = element_text(size = rel(1.25), 
                                                face = 'italic'),
                      axis.text = element_text(size = rel(1.10)), 
                      legend.text = element_text(size = rel(1.10)),
                      legend.title = element_text(size = rel(1.10)),
                      axis.text.x = element_text(vjust = 1, hjust = 0, angle = -30),
                      strip.text = element_text(size = rel(1.10))))
      }else{
          print(ggplot(datadf(), aes_string(input$horizontal))+
                geom_bar(aes(fill=district), position = 'dodge')+
                labs(x='Centre Type', 
                     y='Total Number of Child Care Centre', 
                     fill='Districts')+
                ggtitle('Child Care Centre vs Centre Type')+
                theme(plot.title = element_text(hjust = 0.5, 
                                                face = 'bold.italic', 
                                                size = rel(1.5)), 
                      axis.title = element_text(size = rel(1.25), 
                                                face = 'italic'),
                      axis.text = element_text(size = rel(1.10)), 
                      legend.text = element_text(size = rel(1.10)),
                      legend.title = element_text(size = rel(1.10))))
      }
    }  
  })
  
  
  
  
  
 
    
    
}    