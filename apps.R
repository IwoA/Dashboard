

library(shiny)
library(eurostat)
library(dplyr)
library(tidyr)
library(plotly)

## pobieranie danych 
df <- get_eurostat("mips_sa")
df_l <- get_eurostat("mips_sa", type = "label") #already with full labels
labels <- df_l$indic_ip
names(labels) <- "labels"
df1 <- bind_cols(df, tibble::enframe(labels, name = NULL)) %>% rename(labels = value) 
df2 <- df1 %>% filter(release == "SA21")
df2$labels <- droplevels(df2$labels)

# indicators_h <- df1 %>% select(indic, labels) %>% filter(indic == "IND_H")
# indicators_a <- df1 %>% select(indic, labels) %>% filter(indic == "IND_A")
# indicators <- c(unique(as.character(indicators_h$labels)), unique(as.character(indicators_a$labels)))
indicators_t <- df2 %>% select(indic, labels) %>% filter(indic == "IND_T")
indicators <- levels(indicators_t$labels)

# Define UI for application
ui <- fluidPage(theme = "bootstrap.css",
     
## Pierwsza czesc opisu    
      fluidPage(
           includeMarkdown("mip.Rmd") 
      ),
#######################

     fluidRow( column (1), column (11,
                                   selectInput("countries",
                                               "   Select country",
                                               choices = list("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany",
                                                              "Denmark", "Estonia", "Greece", "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland",
                                                              "Italy", "Lithuania", "Luksembourg", "Latvia", "Malta", "Netherlands", "Poland",
                                                              "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia"),
                                               selected = "Poland"),
                                   selectInput("indicator", "Select indicator",
                                               choices = as.list(indicators), selected = "Unemployment rate - %")
                                   
     )),
     
     # Show a plot of the generated distribution
     fluidRow(
          plotlyOutput("distPlot", width = "100%", height = "600px")
          #textOutput("text1"),
          #textOutput("text2")
     ),
## Druga czesc opisu
      fluidPage( 
           includeMarkdown("mip_cd.Rmd")
      )
#######################
)







# matching full country names from the menu with abbreviations used in script.
countries <- tibble(geo = c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", 
                         "EE", "EL", "ES", "FI", "FR", "HR", "HU", 
                         "IE", "IT", "LT", "LU", "LV", "MT", "NL",
                         "PL", "PT", "RO", "SE", "SI", "SK"),
                 geo1 = c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany",
                          "Denmark", "Estonia", "Greece", "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland",
                          "Italy", "Lithuania", "Luksembourg", "Latvia", "Malta", "Netherlands", "Poland",
                          "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia")) 

colnames(countries) <- c("geo", "geo1")





# Define server logic required to draw a graph
server <- function(input, output) {
     
        react<- reactive({countries %>%  filter(geo1==input$countries) %>% select(geo) })
        react.ind <- reactive({df2 %>%  mutate(indic_ip = as.character(indic_ip)) %>%  filter(labels==input$indicator) %>% select(indic_ip) %>% unique(.) })
        
        
     #output$text1 <- renderText(as.character(countries %>%  filter(geo1==input$countries) %>% select(geo)))
     #output$text2 <- renderText(as.character(df2 %>%  mutate(indic_ip = as.character(indic_ip)) %>%  filter(labels==input$indicator) %>% select(indic_ip) %>% unique(.)))
     
     output$distPlot <- renderPlotly({
             ind <- df2 %>%  mutate(indic_ip = as.character(indic_ip)) %>%  filter(labels==input$indicator) %>% select(indic_ip) %>% unique(.)
             tmp <- df2 %>% filter(indic_ip == as.character(ind))
             tmp <- tmp %>% select(time, values, geo, labels)
             country <- as.character(countries %>%  filter(geo1==input$countries) %>% select(geo))
             dfl <- tmp %>% mutate(selected = ifelse(geo==country, country,"EU"))
             sel   <- filter(dfl, geo ==country)  #trick to have
             unsel <- filter(dfl, geo !=country)  #lines drawn in correct order. Otherwise sometimes red line is behind grey lines.
             
             plot_ly() %>% 
                     add_trace(x=unsel$time, y=unsel$values, type = "scatter", mode = "lines", split = unsel$geo, color = unsel$selected, line = list(color = "lightgrey"),
                               text = paste("Country: ", unsel$geo,
                                            "<br>Value: ", round(unsel$values, digits = 2),
                                            "<br>Date: ", unsel$time),
                               hoverinfo = 'text') %>% 
                     add_trace(x = sel$time, y=sel$values, mode = "lines", line = list(color = "red", width = 4),
                               text = paste("Country: ", sel$geo,
                                            "<br>Value: ", round(sel$values, digits = 2),
                                            "<br>Date: ", sel$time),
                               hoverinfo = 'text') %>%
                     layout(showlegend = FALSE, title = "", xaxis = list(title = ""), yaxis = list(title = ""))
             
     })
}

# Run the application 
shinyApp(ui = ui, server = server)
