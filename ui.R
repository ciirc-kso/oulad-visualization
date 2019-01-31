tab_parcoords_ui <- function() {
  fluidRow(column(width = 12,
                  fluidRow(
                    column(6,uiOutput("dataset_columns1")), 
                    column(6,uiOutput("dataset_selected_columns1"))
                  )
                  
           ),
           column(width = 12,
                  plotlyOutput("parcoorplot")),
           column(width=12,
                  uiOutput("ranges"))
    )
}

tab_splom_ui <- function() {
  fluidRow(column(width = 12,
                  fluidRow(
                    column(6,uiOutput("dataset_columns2")), 
                    column(6,uiOutput("dataset_selected_columns2"))
                  )
                  
           ), column(width = 12,
                  uiOutput("splom_matrix")),
           column(width = 12,
                  uiOutput("selected_data")))
}

tab_before_clicks <- function(){
  fluidRow(column(width = 12,
                  plotlyOutput("before_clicks")))
}
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Visualization"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Scatter plot matrix",
        tabName = "tab_splom",
        icon = icon("bar-chart")
      ),
      menuItem(
        "Parallel coordinates",
        tabName = "tab_parcoords",
        icon = icon("line-chart")
      ),
      menuItem(
        "Clicks summary",
        tabName = "tab_before_clicks",
        icon = icon("chart-area")
      ),
      selectInput("module", "Module:", filter_modules()),
      selectInput("presentation", "Presentation:", filter_presentations()),
      selectInput("colorPallete", "ColorPallete:", rownames(RColorBrewer::brewer.pal.info) %>% sort),
      sliderInput(
        "num_of_clusters",
        "Number of clusters:",
        min = 1,
        max = 15,
        value = 4
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    column(width = 12,
           tabItems(
             tabItem(tabName = "tab_splom", tab_splom_ui()),
             tabItem(tabName = "tab_parcoords", tab_parcoords_ui()),
             tabItem(tabName = "tab_before_clicks", tab_before_clicks())
             
           )
    )
    
  )
)