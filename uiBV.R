tabsetPanel(
  
  # Info tab
  tabPanel("Bivariate Background",
           
           includeHTML("www/Info/BVInfo.html")
           
  ),
  
  # Data Tab       
  tabPanel("Step 1: Input Data",
           
           # Data Sidebar Panel
           sidebarPanel( width = 3,
                         
                         h4( "Load Your Raw Data" ), 
                         
                         # input selector for choosing data file
                         fileInput( "bvFile",
                                    "Input .xlsx, .xls, or .csv file:",
                                    accept = c(".xls, .xlsx, .csv")),
                         
                         h4("Select Bivariate Data"),
                         h5("Select input data columns to plot. Multiple inputs for each axis are allowed and are summed automatically."),
                         
                         # input selector for X variable
                         uiOutput( "bvChoose_X" ),
                         
                         # input selector for Y variable
                         uiOutput( "bvChoose_Y" ),
                         
                         # input selector for G (group data)
                         uiOutput( "bvChoose_G" ),
                         h5("With no grouping variable the data will default to a single group.")
                         
                         
           ),
           
           # Data Tab Main Panel
           mainPanel(
             tabsetPanel(
               tabPanel("Input Data",
                        dataTableOutput("bvFullData")
               ),
               tabPanel("Plot Data",
                        dataTableOutput("bvPlotData")
               )
             )
           )
           
  ),# Close Data Tab
  
  # Plot Tab       
  tabPanel("Step 2: Create Plot",
           fluidPage(
             
             # Plot Left Panel
             column(width = 3,
                    
                    # Panel for geom controls
                    h4("Control geometries for each group below"),
                    fluidRow(
                      column(6, actionButton("bvShowAll", "Show All", width = "100%")),
                      column(6, actionButton("bvHideAll", "Hide All", width = "100%"))
                    ),
                    bsCollapse(
                      bsCollapsePanel("Point shape key:", style = "primary",
                                      img(src = "Figs/pch.png", width = "100%")
                      )
                    ),
                    wellPanel(style = "overflow-y:scroll; max-height: 500px",
                      uiOutput("bvPtControls"),
                      br(),
                      br()
                    )
             ),
             
             # Plot Center Panel
             column( width = 7,
                     
                     div(
                       style = "position:relative",
                       withSpinner(
                         plotOutput("bvPlot", width = '100%', height = 580,
                                  dblclick = "bvPlotDblClick",
                                  brush = brushOpts(id = "bvPlotBrush", resetOnNew = TRUE),
                                  hover = hoverOpts("bvPlotHover", delay = 100, delayType = "debounce")),
                         type = 6, color = "#990000"),
                       uiOutput("bvHoverInfo")
                     ),
                     
                     hr(),
                     
                     bsCollapse(open = "General Plot Controls:",
                       bsCollapsePanel("General Plot Controls:", style = "primary",
                                       checkboxInput("bvGrid", "Show grid:", value = FALSE)
                                       )
                     ),
                     bsCollapse(open = "Axes Controls:",
                                bsCollapsePanel("Axes Controls:", style = "primary",
                                                fluidRow(
                                                  column(6,
                                                         h4("X-axis controls"),
                                                         numericInput("bvXmin", "X-min", value = 0),
                                                         numericInput("bvXmax", "X-max", value = 100),
                                                         numericInput("bvXinc", "X-increment", 10, min = 0, max = 100, step = .1),
                                                         selectInput("bvXTrans", "X-axis Transformation", c("identity","sqrt", "log", "log10"))
                                                  ),
                                                  column(6, 
                                                         h4("Y-axis controls"),
                                                         numericInput("bvYmin", "Y-min", value = 0),
                                                         numericInput("bvYmax", "Y-max", value = 100),
                                                         numericInput("bvYinc", "Y-increment", 5, min = 0, max = 100, step = .1),
                                                         selectInput("bvYTrans", "Y-axis Transformation", c("identity","sqrt", "log", "log10"))
                                                  )
                                                )
                                )
                     ),
                     uiOutput("bvPlyPanel"),
                     
                     br(), # Adds space to bottom of app for drop down selectors
                     br(),
                     br()
             ),
             
             # Plot right panel
             column( width = 2,
                     
                     h4("Download Plot:"),
                     h5("Recognizes the extensions eps/ps, tex (pictex), pdf, jpeg, tiff, png, bmp, svg and wmf (windows only)."),
                     textInput("bvDownloadTxt", label = NA, value = "myplot.pdf"),
                     downloadButton('bvDownloadPlot', "Download"),
                     
                     h4("Select Discrimination Plot:"),
                     selectInput("bvDiscrim", "Select Plot Type:",
                                             c("", "TAS_classic", "TAS_ext", "TAS_int", "TAS_cox", "TAS_wilson"),
                                             selected = NULL),
                     wellPanel(
                       
                       # Title of Plot
                       textInput("bvPlotTitle", "Title of plot:", value = "My Title"),
                       # Axis label for A axis 
                       textInput("bvPlotLabX", "A axis label:", value = "X"),
                       # Axis label for B axis 
                       textInput("bvPlotLabY", "B axis label:", value = "Y")
                     ),
                     
                     wellPanel(
                       
                       # Alkaline/Subalkaline line checkbox
                       checkboxInput("tasAlkSubalk", "Alkaline vs Subalkaline", FALSE),
                       # Rock Names checkbox
                       checkboxInput("rxNames", "Rock Field Names", TRUE)
                     )

             )         
           )
  )
)