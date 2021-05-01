#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readr)
library(leaflet)
library(raster)

global_pp_data <- read_csv("globalpowerplantdatabasev120/global_power_plant_database.csv")

rastercc <- raster::ccodes()[,c("NAME", "continent")]
g_p_d <- merge(global_pp_data, raster::ccodes()[,c("NAME", "continent")], by.x="country_long", by.y="NAME", all.x=T)
g_p_d$continent[g_p_d$country_long == "United States of America"] <- "North America"
global_pp_data$continent <- countrycode(sourcevar = global_pp_data%>% pull(country_long),origin = "country.name", destination = "continent")



fuel_types <- unique(global_pp_data$primary_fuel)
color <- c("darkolivegreen4","dodgerblue1","darkorange","darkorange4","firebrick1","darkorchid","darkorchid4","forestgreen","darkred","darkslategray","goldenrod1","darkslategray1","darkblue","brown2","darkcyan")
cpal <- colorFactor(color, domain = fuel_types)

#continents <- c("Africa", "Asia", "Europe", "North America", "South America", "Antarctica", "Australia")
continents<-unique(rastercc$continent)


# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "CS424 Spring 2021 ProjectX"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     sidebarMenu(
                         id="tabs",
                         menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         #menuItem("Split screen visualization", tabName = "split", icon = icon("dashboard")),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         #menuItem("The entire city of Chicago", tabName = "entire_Chicago", icon = icon("dashboard")),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         #menuItem("Plants added or idled", tabName = "Plants_variation", icon = icon("dashboard")),
                         
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("About page", tabName = "about", icon = icon("question"))
                     ),
                     hr()
    ),
    #==========    ==========    ==========    dashboard    ==========    ==========    ==========  
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                    column(12,
                           column(4,
                                  selectInput('con', 'Continent:', continents , selected = "North America"),
                           ),
                           column(4,
                                  pickerInput(
                                      inputId = "fuel_types_selected",
                                      label = "Select/deselect all + fuel types selected",
                                      choices = fuel_types,
                                      selected = fuel_types,
                                      options = list(
                                          `actions-box` = TRUE,
                                          size = 10,
                                          `selected-text-format` = "count > 3"
                                      ),
                                      multiple = TRUE
                                  )
                           ),
                           column(4,
                                  sliderInput("slider", "Capacity Range",
                                              min = 0, max = 22500, value = c(0, 22500)),
                                  textOutput("text"),
                                 
                           ),
                           # column(3,
                           #        selectInput('month1', 'Month:', month, selected = "Total"),
                           # ),
                           # column(2,
                           #        actionButton("reset1", "Reset"),
                           #        textOutput("text"),
                           # ),
                           # style="z-index:1002;"
                    ),
                    column(12,
                           # navbarPage("Near West Side",
                           #            tabPanel("Plot",
                                               box( width = NULL, status = "primary", solidHeader = TRUE, title= "Plot",

                                                    leafletOutput('NAplot', height = "500px"),
                                               ),
                           #                     
                           #            ),
                           #            tabPanel("Graph",
                           #                     box( width = NULL, status = "primary", solidHeader = TRUE, title= "Graph",
                           #                          column(6,
                           #                                 plotOutput("graph1", height = "350px"),
                           #                          ),
                           #                          column(6,
                           #                                 plotOutput("graph2", height = "350px")
                           #                          ),
                           #                     )
                           #                     
                           #                     # sidebarLayout(
                           #                     #     sidebarPanel(
                           #                     #         radioButtons("plotType", "Plot type",
                           #                     #                      c("Scatter"="p", "Line"="l")
                           #                     #         )
                           #                     #     ),
                           #                     #     mainPanel(
                           #                     #         box( width = NULL, status = "primary", solidHeader = TRUE, title= "Table",
                           #                     #              plotOutput("graph1",height = "300px"),
                           #                     #              plotOutput("graph2",height = "300px")
                           #                     #         )
                           #                     #     )
                           #                     # ),
                           #            ),
                           #            tabPanel("Table",
                           #                     box( width = NULL, status = "primary", solidHeader = TRUE, title= "Table",
                           #                          dataTableOutput("Table"),
                           #                          
                           #                     )
                           #            )
                           # ),
                    )
                    
                    
            ),
            # tabItem(tabName = "split",
            # ),
            # tabItem(tabName = "entire_Chicago",
            #         
            #         
            # ),
            
            
            tabItem(tabName = "about",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title= "About page",
                         
                         #helpText("Graphical representation of the reactive expressions called in the app. It is a minimal example with only the color and horizon setting as adjustable value. To build the graphic please use the mouse and drag the blue bar to the right."),
                        
                         h1("Data reference"),
                         h3("The data file is available from World Resources Institute (WRI)."),
                         uiOutput("tab1"),
                         h3("WRI is a global nonprofit organization that works with leaders in government, business and civil society to research, design, and carry out practical solutions that simultaneously improve people’s lives and ensure nature can thrive."),
                         

                         h1("App developer"),
                         h3("Ting-Shao, Lee"),
                         h3("This application is part of my CS424 project 3 at the University of Illinois at Chicago, Spring 2021."),
                         
                    )
            )
        )     
    )
    
    #setBackgroundColor("AliceBlue"),
    
    # Sidebar with a slider input for number of bins 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$text <- renderText({
        input$slider[1]
        #input$fuel_types_selected

    })
    
    output$NAplot <- renderLeaflet({
        data<- subset(g_p_d, continent==input$con)
        data<- subset(data, primary_fuel%in%input$fuel_types_selected)
        data<- subset(data, capacity_mw>=input$slider[1])
        data<- subset(data, capacity_mw<=input$slider[2])
        leaflet(data) %>% addTiles() %>%addCircleMarkers(radius = ~log(capacity_mw, base = 4),
                                                         color = ~cpal(primary_fuel),
                                                         stroke = FALSE, fillOpacity = 0.5, 
                                                         popup = paste("Country:", data$country_long, "<br>",
                                                                        "Plant name:",data$name, "<br>",
                                                                        "Capacity:", data$capacity_mw, " mw<br>",
                                                                        "Fuel type:", data$primary_fuel))%>%
            addLegend("bottomright", pal = cpal, values = ~primary_fuel,title = "Fuel type",opacity = 1)
    })
    output$tab1 <- renderUI({
        url <- a("Source", href="https://datasets.wri.org/dataset/globalpowerplantdatabase")
        tagList("URL link:", url)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
