library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(rhandsontable)
library(r2d3)

our_starwars <- starwars %>% 
    select(name:gender, -skin_color) %>% 
    mutate_at(vars(hair_color, eye_color, gender), as_factor)

suppressWarnings(general_info_on_tbls <- readLines("handson_vs_DT.html"))
suppressWarnings(comparing_widgets_text <- readLines("specific_tabular_tests.html"))

# The ui to the app
ui <- dashboardPage(
    header = dashboardHeader(title = "Widget comparison"),
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem("Compare tables", tabName = "compare_tbls", icon = icon("table"))
        ),
        sidebarMenu(
            menuItem("Compare plotting", tabName = "compare_plots", icon = icon("line-chart"))
        )
    ),
    body = dashboardBody(
        tabItems(
            # The first tab used to compare tables ----
            tabItem(tabName = "compare_tbls",
                    fluidRow(box(width = 12, title = "DataTable versus handsontable", collapsible = T,
                                 HTML(general_info_on_tbls)
                    )),
                    fluidRow(box(width = 12, title = "Specific behaviour comparisons", collapsible = T, collapsed = T,
                                 HTML(comparing_widgets_text)
                    )),
                    fluidRow(
                        column(width = 5,
                               DTOutput("example_dt")),
                        column(width = 1, p("")),
                        column(width = 6,
                               rHandsontableOutput("example_handsontable"))
                    )
            ),
            # The second tab used to compare plots
            tabItem(tabName = "compare_plots")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Generate an example data table ------------------------------------------
    
    output$example_dt <- renderDT({
        datatable(our_starwars, editable = TRUE)
    })

    # Generate an example handsontable ----------------------------------------

    output$example_handsontable <- renderRHandsontable({
        rhandsontable(our_starwars, height = 700, search = TRUE)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
