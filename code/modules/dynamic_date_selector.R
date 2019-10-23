dynamicDateBoxUI <- function(id){
  ns <- NS(id)
  
  box(title = "Filter by date", status = "primary", collapsible = TRUE, collapsed = FALSE, width = 3,
      selectInput(ns("date_selector"), "Select dates", 
                  choices = c("Last 7 days",
                              "Last 30 days",
                              "Last year",
                              "Previous week",
                              "Previous month",
                              "Previous year",
                              "Custom range"),
                  selected = "Last 7 days"),
      dateRangeInput(ns("date_range_custom"), "Date range",
                     start = Sys.Date() - 7, end = Sys.Date(), separator = "to", autoclose = TRUE))
  
}

dynamicDateBoxServer <- function(input, output, session){
  
    observeEvent(input$date_range_custom, 
                 {   
                   start <- input$date_range_custom[1]
                   end <- input$date_range_custom[2]
                   
                   output$tmp_debug <- renderPrint(
                     {
                       paste0("type=", typeof(start), " value=", start, " check=", Sys.Date()-7 == start)
                     }
                   )
                   if (start == Sys.Date() - 7 & end == Sys.Date()){
                     updateSelectInput(session = session, inputId = "date_selector",
                                       selected = "Last 7 days")
                   } else if (start == Sys.Date() - 30 & end == Sys.Date()){
                     updateSelectInput(session = session, inputId = "date_selector",
                                       selected = "Last 30 days")
                   } else if (start == Sys.Date() - 365 & end == Sys.Date()){
                     updateSelectInput(session = session, inputId = "date_selector",
                                       selected = "Last year")
                   } else if (start == lubridate::floor_date(Sys.Date() - 7, unit = "week", week_start = 7) & 
                              end == lubridate::ceiling_date(Sys.Date() - 7, unit = "week", week_start = 7) - 1) {
                     updateSelectInput(session = session, inputId = "date_selector",
                                       selected = "Previous week")
                   } else if (start == lubridate::floor_date(Sys.Date() - 30, unit = "month", week_start = 7) & 
                              end == lubridate::ceiling_date(Sys.Date() - 30, unit = "month", week_start = 7) - 1) {
                     updateSelectInput(session = session, inputId = "date_selector",
                                       selected = "Previous month")
                   } else if (start == lubridate::floor_date(Sys.Date() - 365, unit = "year", week_start = 7) & 
                              end == lubridate::ceiling_date(Sys.Date() - 365, unit = "year", week_start = 7) - 1) {
                     updateSelectInput(session = session, inputId = "date_selector",
                                       selected = "Previous year")
                   } else {
                     updateSelectInput(session = session, inputId = "date_selector",
                                       selected = "Custom range")
                   }
                   
                 })
    
    observeEvent(input$date_selector,
                 {
                   
                   start <- input$date_range_custom[1]
                   end <- input$date_range_custom[2]
                   
                   # find proper range
                   if (input$date_selector == "Last 7 days"){
                     start <- Sys.Date() - 7
                     end <- Sys.Date()
                   } else if (input$date_selector == "Last 30 days"){
                     start <- Sys.Date() - 30
                     end <- Sys.Date()
                   } else if (input$date_selector == "Last year"){
                     start <- Sys.Date() - 365
                     end <- Sys.Date()
                   } else if (input$date_selector == "Previous week"){
                     start <- lubridate::floor_date(Sys.Date() - 7, unit = "week", week_start = 7)
                     end <- lubridate::ceiling_date(Sys.Date() - 7, unit = "week", week_start = 7) - 1
                   } else if (input$date_selector == "Previous month"){
                     start <- lubridate::floor_date(Sys.Date() - 30, unit = "month", week_start = 7)
                     end <- lubridate::ceiling_date(Sys.Date() - 30, unit = "month", week_start = 7) - 1
                   } else if (input$date_selector == "Previous year"){
                     start <- lubridate::floor_date(Sys.Date() - 365, unit = "year", week_start = 7)
                     end <- lubridate::ceiling_date(Sys.Date() - 365, unit = "year", week_start = 7) - 1
                   }
                   
                   updateDateRangeInput(session, inputId = "date_range_custom",
                                        start = start, end = end)
                   
                 })
    
    # return the final date range:
    reactive({
      start <- input$date_range_custom[1]
      end <- input$date_range_custom[2]
      
      c(start, end)
      
    })
    
    
}