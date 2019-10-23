library(shiny)
library(shinydashboard) # for the dashboard form
library(shinyWidgets) # for nice checkbox
library(tidyverse)
library(plotly) # for plotting plotly interactive chart
library(r2d3) # for charting a d3 visualization
library(rhandsontable) # for a handsontable
library(DT) # for the DataTable
library(sm) # for multiple density comparison in base-r plot

# Some data for the table comparison --------------------------------------

our_starwars <- starwars %>% 
    select(name:gender, -skin_color) %>% 
    mutate_at(vars(hair_color, eye_color, gender), as_factor)

suppressWarnings(general_info_on_tbls <- readLines("handson_vs_DT.html"))
suppressWarnings(comparing_widgets_text <- readLines("specific_tabular_tests.html"))
suppressWarnings(comparing_plots <- readLines("visualizations_comparisons.html"))


# Some data for the plot comparison ---------------------------------------
# Data sampled from tidytuesday
# https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv
ipf_lifts <- read_csv("ipf_lifts_sample.csv") %>% 
    select(starts_with("best3"), sex) %>% 
    pivot_longer(cols = -sex, names_to = "exercise", values_to = "weight") %>% 
    filter(weight > 0)


# The actual app ----------------------------------------------------------

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
            tabItem(tabName = "compare_plots",
                    fluidRow(box(width = 12, title = "Comparing plots", collapsible = T,
                                 HTML(comparing_plots),
                                 materialSwitch(inputId = "split_by_gender", value = FALSE,
                                             label = "Example: split by gender?", status = "success"))
                    ),
                    fluidRow(box(width = 4, title = "Base-r (package sm)",
                                    plotOutput("baser_location")
                                    ),
                             box(width = 4, title = "ggplot2",
                                    plotOutput("ggplot_location")
                                    ),
                             box(width = 4, title = "plotly using ggplotly() interface",
                                    plotlyOutput("plotly_location")
                                    ))
            )
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
    
    # Create a reactive expression for the ggplot
    
    ggplotready_reactive <- reactive({
        ggplotready <- ggplot(ipf_lifts, aes(x = weight, y = stat(density))) + 
            geom_density(aes(color = exercise), size = 1, bw = 10) + 
            theme_bw() + 
            theme(legend.position = "bottom")
        
        if (input$split_by_gender){
            ggplotready + facet_wrap(~sex)
        } else {
            ggplotready
        }
    })
    
    # Generate a ggplot2 ----
    output$baser_location <- renderPlot({
        if (input$split_by_gender){
            split_by_group <- factor(paste0(ipf_lifts$exercise, ipf_lifts$sex))
        } else {
            split_by_group <- factor(ipf_lifts$exercise)
        }
        sm.density.compare(x = ipf_lifts$weight, group = split_by_group, xlab="Weight")
    })
    
    # Generate a plotly based on ggplot2 ----
    output$plotly_location <- renderPlotly({
        ggplotly(ggplotready_reactive())
    })
    
    # Generate a base R density chart ----
    output$ggplot_location <- renderPlot({
        ggplotready_reactive()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
