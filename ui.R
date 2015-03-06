library(shiny)

shinyUI(
  fluidPage(
  
    titlePanel('Mplus Model Comparisons'),
  
    sidebarLayout(
      
      sidebarPanel(
        # H0 Model choose-file
#        h5(strong(HTML('H<sub>0</sub>'),'Output')),
        helpText('Output file of the', strong('more'), 'restrictive model.'),
        fileInput('h0file', label=HTML('H<sub>0</sub> Output'), 
          accept=c('text/plain','.out')),
        
        # H1 Model choose-file
#        h5(strong(HTML('H<sub>1</sub>'),'Output')),
        helpText('Output file of the', strong('less'), 'restrictive model.'),
        fileInput('h1file', label=HTML('H<sub>1</sub> Output'),
          accept=c('text/plain','.out')),
        
        # Select Fit-Statistics with checkboxes
        checkboxGroupInput('stats',
          label='Fit-Statistics',
          choices=list('Chi-Square'='chi',
            'RMSEA'='RMSEA',
            'SRMR'='SRMR',
            'CFI'='CFI',
            'TLI'='TLI',
            'Information Criteria'='IC'),
          selected=c('chi','IC')
        ),
        
        # Show note regarding SB-Chi computation on side panel
        conditionalPanel(
          condition = 'input.stats.indexOf("chi") >= 0',
          p(em('Note: The comparison of models estimated using MLR is based on the Satorra-Bentler Scaled', 
            HTML('&chi;<sup>2</sup>'), 
            '(Satorra & Bentler, 2000) as shown on the', 
            a('Mplus-Website.', href='http://www.statmodel.com/chidiff.shtml'),
            'These are not the results of the strictly-positive version (Satorra & Bentler, 2010) as of yet.',style='font-size:8pt'))
        )
      ),

      mainPanel(h1('Usage'),
      
        p('This app is a simple tool to compare the fit of two nested models estimated in Mplus. To do so, select the Mplus-Output files of the models you wish to compare and choose the fit-statistics which are of interest to you.'),  
        p('Currently this app can be used to compare models estimated ML and MLR. If you want to compare models using the MLM, MLMV, WLSM, or WLSMV estimators please use the', strong('difftest'), 'capabilities of Mplus.'),
        
        h1('Output'),
        tableOutput('tabs'),
        em(textOutput('negs'))
      )
    )
  )
)
