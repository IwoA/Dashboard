library(shiny) # dashboard
library(flexdashboard) # dashboard
library(shinydashboard) # dashboard
library(shinyWidgets) # dashboard
library(plotly) # visualisation
library(leaflet) # visualisation
library(tmap) # visualisation
library(DT) # visualisation
library(dygraphs) # visualisation

ui <- dashboardPage(
    dashboardHeader(title = "Financial Soundness Indicators", titleWidth = 320,
                    # all dropdown menus are copied from tutorial
                    dropdownMenu(type = "messages",  
                                 messageItem(
                                     from = "Sales Dept",
                                     message = "Sales are steady this month."
                                 ),
                                 messageItem(
                                     from = "New User",
                                     message = "How do I register?",
                                     icon = icon("question"),
                                     time = "13:45"
                                 ),
                                 messageItem(
                                     from = "Support",
                                     message = "The new server is ready.",
                                     icon = icon("life-ring"),
                                     time = "2014-12-01"
                                 )
                    ),
                    dropdownMenu(type = "notifications",
                                 notificationItem(
                                     text = "5 new users today",
                                     icon("users")
                                 ),
                                 notificationItem(
                                     text = "12 items delivered",
                                     icon("truck"),
                                     status = "success"
                                 ),
                                 notificationItem(
                                     text = "Server load at 86%",
                                     icon = icon("exclamation-triangle"),
                                     status = "warning"
                                 )
                    ),
                    dropdownMenu(type = "tasks", badgeStatus = "success",
                                 taskItem(value = 90, color = "green",
                                          "Documentation"
                                 ),
                                 taskItem(value = 17, color = "aqua",
                                          "Project X"
                                 ),
                                 taskItem(value = 75, color = "yellow",
                                          "Server deployment"
                                 ),
                                 taskItem(value = 80, color = "red",
                                          "Overall project"
                                 )
                    )
                    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            # menuItem("Maps", icon = icon("map-marker-alt"), tabName = "maps",   #not working now
            #          badgeLabel = "new", badgeColor = "green"),
            menuItem("Tables", icon = icon("table"), tabName = "tables"),
            menuItem("Charts & Graphs", icon = icon("chart-bar"), tabName = "graphs"),
            selectInput("countries",
                        "   Select country",
                        choices = list("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany",
                                       "Denmark", "Estonia", "Greece", "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland",
                                       "Italy", "Lithuania", "Luksembourg", "Latvia", "Malta", "Netherlands", "Poland",
                                       "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia"),
                        selected = "Poland"),
            selectInput("indicator", "Select indicator",
                        choices = list("Nonperforming loans to total gross loans",
                                       "Return on assets", "Return on equity",
                                       "Liquid assets to short-term liabilities",
                                       "Total debt to equity", "Household debt to GDP", selected = "Household debt to GDP")),
            materialSwitch(inputId = "id", label = "Quaterly or Annual data", status = "primary", right = TRUE)
        )
    ),
    
    
    
    dashboardBody(
    tabItems(
        tabItem(tabName = "dashboard",
                h2(
                  fluidRow(
                           box(collapsible = TRUE,
                               flexdashboard::gaugeOutput("gauge"), width=3,title="Average growth (CAGR)"
                           ),
                           box(collapsible = TRUE,
                               flexdashboard::gaugeOutput("gauge1"), width=3,title="Total growth"
                           ),

                          shinydashboard::valueBoxOutput("cagr"), 
                           infoBoxOutput("difference"),

                   fluidRow(
                       column(
                              width = 8,
                              plotlyOutput("lineplot"),
                              leafletOutput("map")
                              ),
                       column(
                              width = 4,
                              plotlyOutput("barplot", height = "800px")
                              )
                   )
                            )
                   )),
        # tabItem(tabName = "maps",
        #         h2(
        #            fluidRow(
        #              column(width = 6,
        #                     plotOutput("yd"))
        #            ))
        # ),
        tabItem(tabName = "tables",
                h2(
                   fluidRow(
                       column(width = 8,
                              DT::dataTableOutput("desc")
                              )

                   ))
        ),
        tabItem(tabName = "graphs",
                h2(
                    fluidRow(
                        dygraphOutput("dygraph")
                    )
                )
        )
                  )
     )
)


server <- function(input, output) {
    source("Data.R")
  
  # matching full country names from the menu with abbreviations used in script.
    countr <- tibble(geo = c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", 
                         "EE", "EL", "ES", "FI", "FR", "HR", "HU", 
                         "IE", "IT", "LT", "LU", "LV", "MT", "NL",
                         "PL", "PT", "RO", "SE", "SI", "SK"),
                       geo1 = c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany",
                         "Denmark", "Estonia", "Greece", "Spain", "Finland", "France", "Croatia", "Hungary", "Ireland",
                         "Italy", "Lithuania", "Luksembourg", "Latvia", "Malta", "Netherlands", "Poland",
                         "Portugal", "Romania", "Sweden", "Slovenia", "Slovakia")) 

    
    #### FSI Indicators used (most important and most self-explanatory):
    # 1. Nonperforming loans to total gross loans - indicator = "FSANL_PT"
    # 2. Return on assets - indicator = "FSERA_PT"
    # 3. Return on equity - "FSERE_PT"
    # 4. Liquid assets to short-term liabilities - "FSLS_PT"
    # Non-Financial corporations
    # 5. Total debt to equity - "FSTD_PT"
    # 6. Earnings to interest and principal expenses - "FSEI_PT" (only 5 countries)
    # Households
    # 7. Household debt to GDP - "FSHG_PT"
    # 8. Household debt service and principal payments to income - "FSHS_PT" (only 5 countries)
    ind <- tibble(ind = c("FSANL_PT", "FSERA_PT", "FSERE_PT", "FSLS_PT", "FSTD_PT", "FSHG_PT"),
                     name = c("Nonperforming loans to total gross loans",
                     "Return on assets", "Return on equity",
                     "Liquid assets to short-term liabilities",
                     "Total debt to equity", "Household debt to GDP"))
    
    react<- reactive({countr %>%  filter(geo1==input$countries) %>% select(geo) })
    react.ind <- reactive({ind %>%  filter(name==input$indicator) %>% select(ind) })
    react.freq <- reactive({ ifelse(input$id==TRUE, "A", "Q") })

    tmap_mode("view")
    
    output$lineplot <- renderPlotly({
        fig <- get.data(as.character(react.ind()), country = as.character(react()), title = "", freq = as.character(react.freq()))
        fig$lineplot
    })
    output$barplot <- renderPlotly({
        fig <- get.data(as.character(react.ind()), country = as.character(react()), title = "", freq = as.character(react.freq()))
        ggplotly(fig$barplot, tooltip = "value")
    })
    output$map <- renderLeaflet({
        map <- get.map(as.character(react.ind()), country = as.character(react()), title = "", freq = as.character(react.freq()))
        tmap_leaflet(map)
    })
    output$gauge <- flexdashboard::renderGauge({
        fig <- get.data(as.character(react.ind()), country = as.character(react()), title = "", freq = as.character(react.freq()))
            gauge(round(fig$cagr, 2), min = -10, max = 10, symbol = '%', gaugeSectors(
            success = c(-10, 0), warning = c(0.001, 5), danger = c(5.001, 100)
        ))
    })
    output$gauge1 <- flexdashboard::renderGauge({
        fig <- get.data(as.character(react.ind()), country = as.character(react()), title = "", freq = as.character(react.freq()))
            gauge(round(fig$difference, 2), min = -50, max = 50, symbol = '%', gaugeSectors(
            success = c(-50, 0), warning = c(0.001, 25), danger = c(25.001, 50)
        ))
    })

    output$cagr <- shinydashboard::renderValueBox({
        fig <- get.data(as.character(react.ind()), country = as.character(react()), title = "", freq = as.character(react.freq()))
        rate <- fig$cagr
        shinydashboard::valueBox(
            paste0(rate, "%"), "Average growth (CAGR)",
            icon = icon("area-chart")
        )
    })
    output$difference <- renderInfoBox({
        fig <- get.data(as.character(react.ind()), country = as.character(react()), title = "", freq = as.character(react.freq()))
        rate <- fig$difference
        shinydashboard::infoBox(
            "Total growth", tags$p(paste0(rate, "%"), style = "font-size: 200%;"), 
            icon = icon("chart-line")
        )
    })
    output$dygraph <- renderDygraph({
        fig <- get.data(as.character(react.ind()), country = as.character(react()), title = as.character(input$indicator), freq = as.character(react.freq()))
        fig$dyplot
    })
    output$desc <- DT::renderDataTable({
        dat <- get.data(as.character(react.ind()), country = as.character(react()), title = as.character(input$indicator), freq = as.character(react.freq()))
        dat$desc
    })
    # output$yd <- renderPlot({
    #   source("map.R")
    #   yd
    # })
}

shinyApp(ui, server)