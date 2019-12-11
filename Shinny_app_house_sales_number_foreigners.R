#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2movies)
library(ggplot2)
#


set.seed(61)

ui <- fluidPage(
    titlePanel("House Sales Numbers to Foreigners 2013-2019"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("Year", "Year:",c(unique(as.character( foreign_full$Year)))),
            selectInput("Province", "Province:",c(unique(as.character( foreign_full$Province))))
        ),
        
        mainPanel(
            plotOutput("distPlot"),
            plotOutput("distPlot2")
        )
    )
)

server <- function(input, output) {
    
    
    output$distPlot <- renderPlot({
        pl_df<-
            foreign_full%>%
            filter(Year==input$Year & Province==input$Province)
        
        pl_df_line<-
            foreign_full%>%
            filter(Province==input$Province)        
        
        id_and_cities <- data_frame(id = rownames(TURKEY@data),
                                    Province = TURKEY@data$NAME_1) %>% 
            left_join(foreign_full, by = "Province")%>% filter(Province == input$Province)
        
        final_map2 <- left_join(TUR_for, id_and_cities, by = "id")
        
        if(input$Province != "All") {
            pl_df<-pl_df %>% filter(Province==input$Province)
        }
        
        
        ggplot(final_map2) +
            geom_polygon( aes(x = long, y = lat, group = group, fill = Sales_Numbers),
                          color = "grey") +
            coord_map() +
            theme_void() + 
            labs(title = " House Sales Numbers to Foreigners",
                 subtitle = paste0("Total Sales Numbers of Houses: ",pl_df$Sales_Numbers),
                 caption = "Source: Turkiye Istatistik Kurumu") +
            scale_fill_distiller(name = "House Sales Numbers",
                                 palette = "Spectral", limits = c(0,20000), na.value = "black") +
            theme(plot.title = element_text(hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5))
    })
    
    output$distPlot2 <- renderPlot({
        pl_df_line<-
            foreign_full%>%
            filter(Province==input$Province) 
        
        ggplot(pl_df_line, aes(x=Year, y=Sales_Numbers))+  geom_bar(stat="identity")+  theme_minimal()+
            labs(x="Year",y="Sales Numbers",title=paste0("Total Sales Numbers in ", pl_df_line$Province),fill="Province")+  theme(axis.text.x = element_text(angle=30))
        
        
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)