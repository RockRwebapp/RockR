tabsetPanel(
  
  # Info tab
  tabPanel("Ternary Background",
           includeHTML("www/Info/TernInfo.html")
  ),
  
  # Interactive Ternary Example Tab
  tabPanel( "Interactive Ternary Example",
            h3("Interactive Ternary Diagram"),
            h5("Change the values below to see how a point shifts on the ternary diagram."),
            fluidPage(
              column(6,
                     fluidRow(
                       column(2, 
                              numericInput("ternIntroA", " Value 'A'", value = 1, min = 0)
                       ),
                       column(10, 
                              textOutput("ternIntroMathA"),
                              tags$style(type='text/css', "#ternIntroMathA {
                                         margin-top: 25px; color:black; font-size:18px
                                         }")
                      )
                              ),
                     fluidRow(
                       column(2,
                              numericInput("ternIntroB", " Value 'B'", value = 1, min = 0)
                       ),
                       column(10,
                              textOutput("ternIntroMathB"),
                              tags$style(type='text/css', "#ternIntroMathB {
                                     margin-top: 25px; color:#d55e00; font-size:18px
                                     }")
                       )
                     ),
                     fluidRow(
                       column(2, 
                              numericInput("ternIntroC", " Value 'C'", value = 1, min = 0)
                       ),
                       column(10, 
                              textOutput("ternIntroMathC"),
                              tags$style(type='text/css', "#ternIntroMathC {
                                     margin-top: 25px; color:#0072b2; font-size:18px; 
                                     }")
                       )
                     ),
                     textOutput("ternIntroSum"),
                     tags$style(type='text/css', "#ternIntroSum {
                                     color:black; font-size:18px
                                     }")
                       ),
              
              column(6,
                     plotOutput("ternIntroPlot", width = '100%', height = 600)
              )
            ),
            hr()
  ),
  
  # Data Tab       
  tabPanel("Step 1: Input Data",
           
           # Data Sidebar Panel
           sidebarPanel( width = 3,
                         
                         h4( "Load Your Raw Data" ), 
                         
                         # input selector for choosing data file
                         fileInput( "ternFile",
                                    "Input .xlsx, .xls, or .csv file:",
                                    accept = c(".xls, .xlsx, .csv")),
                         
                         h4("Select Ternary Data"),
                         h5("Select input data columns to plot. Multiple inputs for each axis are allowed and are summed automatically."),
                         
                         # input selector for A variable
                         uiOutput( "ternChoose_A" ),
                         
                         # input selector for B variable
                         uiOutput( "ternChoose_B" ),
                         
                         # input selector for C variable
                         uiOutput( "ternChoose_C" ),
                         
                         # input selector for G (group data)
                         uiOutput( "ternChoose_G" ),
                         h5("With no grouping variable the data will default to a single group.")
                         
                         
           ),
           
           # Data Tab Main Panel
           mainPanel(
             tabsetPanel(
               tabPanel("Input Data",
                        dataTableOutput("ternFullData")
               ),
               tabPanel("Plot Data",
                        dataTableOutput("ternPlotData")
               )
             )
           )
           
  ),
  
  # Plot Tab       
  tabPanel("Step 2: Create Plot",
           fluidPage(
             
             # Plot Left Panel
             column(width = 3,
                    
                    # Panel for geom controls
                    h4("Control geometries for each group below"),
                    fluidRow(
                      column(6, actionButton("ternShowAll", "Show All", width = "100%")),
                      column(6, actionButton("ternHideAll", "Hide All", width = "100%"))
                    ),
                    bsCollapse( 
                      bsCollapsePanel("Point shape key:", style = "primary",
                                      img(src = "Figs/pch.png", width = "100%")
                      )
                    ),
                    wellPanel(style = "overflow-y:scroll; max-height: 500px",
                      uiOutput("ternPtControls"),
                      br(),
                      br()
                    )
             ),
             
             # Plot Center Panel
             column( width = 7,
                     
                     div(
                       style = "position:relative",
                       withSpinner(
                         plotOutput("ternPlot", width = '100%', height = "auto"), #, height = 580
                         type = 6, color = "#990000")
                     ),
                     
                     hr(),
                     
                     wellPanel (
                       h4("If you have chosen a discrimination plot, controls for polygon color will show below:"),
                       
                       conditionalPanel(condition = 'input.ternDiscrim != ""',
                                        
                                        h5("Click on the boxes below to change polygon colors. You can copy a HEX code for a color from one polygon to another."),
                                        
                                        uiOutput("ternPlyPanel")
                       )
                     ),
                     fluidRow(
                       h4("General plot controls:"),
                       column(4, 
                              h5("Add/Remove plot components"),
                              selectInput("ternGrid", "Grid", c("On", "Off")),
                              selectInput("ternAxisArrows", "Axis Arrows", c("On", "Off")),
                              selectInput("ternClockwise", "Axis Rotation", c("Clockwise", "Counter Clockwise"))
                              
                              ),
                       column(4, 
                              h5("Add/Remove group components")
                              ),
                       column(4, 
                              h5("Add/Remove surfaces")
                              )
                     ),
                     hr(), # Adds space to bottom of app for drop down selectors
                     br(),
                     br()
             ),
             
             # Plot right panel
             column( width = 2,
                     
                     h4("Download Plot:"),
                     h5("Recognizes the extensions eps/ps, tex (pictex), pdf, jpeg, tiff, png, bmp, svg and wmf (windows only)."),
                     textInput("ternDownloadTxt", label = NA, value = "myplot.pdf"),
                     downloadButton('ternDownloadPlot', "Download"),
                     
                     h4("Select Discrimination Plot:"),
                     selectInput("ternDiscrim", "Select Plot Type:",
                                             c("","QFL", "QFL2", "QAPi", "QAPe", "NFsp", "MFsp65", "NFsp65", "USDAsoils", "Shep", "AFM", "FspClass", "PeridClass", "PyroxClass", "Folk"),
                                             selected = NULL),
                     
                     wellPanel(
                       
                       # Title of Plot
                       textInput("ternTitle", "Title of plot:", value = "My Title"),
                       # Axis label for A axis 
                       textInput("ternPlotLabA", "A axis label:", value = "A"),
                       # Axis label for A arrow 
                       textInput("ternPlotArrowA", "A arrow label:", value = "A (%)"),
                       # Axis label for B axis 
                       textInput("ternPlotLabB", "B axis label:", value = "B"),
                       # Axis label for B arrow 
                       textInput("ternPlotArrowB", "B arrow label:", value = "B (%)"),
                       # Axis label for C axis
                       textInput("ternPlotLabC", "C axis label:", value = "C"),
                       # Axis label for C arrow 
                       textInput("ternPlotArrowC", "C arrow label:", value = "C (%)")
                     )
             )         
           )
  )
)