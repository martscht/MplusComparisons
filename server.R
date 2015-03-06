library(shiny)

# Load Chi2 comparison syntax
source('SBChi.R')

shinyServer(function(input, output) {
  
  # Perform Model comparison (using SBchi)
  modelCompare <- reactive({
    suppressWarnings(SBchi(input$h0file$datapath,input$h1file$datapath))
  })
  
  tabFilter <- reactive({
    # Trim Table
    tabs <- modelCompare()    
    filter <- rep(TRUE,13)
    if (!('chi' %in% input$stats)) {
      filter[4:6] <- FALSE
    }    
    filter[7:10] <- is.element(names(tabs)[7:10],input$stats)
    filter[11:13] <- is.element('IC',input$stats)
    tabs[,filter]
  })
  
  # Return output
  output$tabs <- renderTable({
    if (is.null(input$h0file) | is.null(input$h1file)) return(NULL)
    tabs <- tabFilter()
    tabs}
  )
  
  output$negs <- renderText({
    if (is.null(input$h0file) | is.null(input$h1file)) return(NULL)
    
    if ('chi' %in% input$stats) {
      tabs <- tabFilter()
      if (tabs[3,'df'] < 0) {
        return('The degrees of freedom of the model comparison are negative. Consider switching the order of the Mplus-Output files you provided.')
      }
      else return(NULL)
    }
    else return(NULL)
  })
  
})
