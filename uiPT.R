tabsetPanel(
  
  # Info tab
  tabPanel("Metamorphic Facies Background",
           
           includeHTML("www/Info/PTInfo.html")
           
  ), 
  
  # Data Tab       
  tabPanel("Step 1: Input Data",
           
           # Data Sidebar Panel
           sidebarPanel( width = 3,
                         
                         h3( "Load Your Data" ), 
                         
                         # input selector for choosing data file
                         fileInput( "ptFile",
                                    "Input .xlsx, .xls, or .csv file:",
                                    accept = c(".xls, .xlsx, .csv")),
                         
                         # input selector for X variable
                         uiOutput( "ptChoose_X" ),
                         
                         # input selector for Y variable
                         uiOutput( "ptChoose_Y" ),
                         
                         # input selector for G (group data)
                         uiOutput( "ptChoose_G" )
                         
           ),
           
           # Data Tab Main Panel
           mainPanel(
             tabsetPanel(
               tabPanel("Input Data",
                        dataTableOutput("ptFullData")
               ),
               tabPanel("Plot Data",
                        dataTableOutput("ptPlotData")
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
                      column(6, actionButton("ptShowAll", "Show All", width = "100%")),
                      column(6, actionButton("ptHideAll", "Hide All", width = "100%"))
                    ),
                    bsCollapse(
                      bsCollapsePanel("Point shape key:", style = "primary",
                                      img(src = "Figs/pch.png", width = "100%")
                      )
                    ),
                    wellPanel(style = "overflow-y:scroll; max-height: 500px",
                              uiOutput("ptPtControls"),
                              br(),
                              br()
                    )
             ),
             
             # Plot Center Panel
             column( width = 7,
                     
                     div(
                       style = "position:relative",
                       withSpinner(
                         plotOutput("ptPlot", width = '100%', height = 580,
                                dblclick = "ptPlotDblClick",
                                brush = brushOpts(id = "ptPlotBrush", resetOnNew = TRUE),
                                hover = hoverOpts("ptPlotHover", delay = 100, delayType = "debounce")
                                ),
                         type = 6, color = "#990000")
                     ),
                         
                     hr(),
                     
                     uiOutput("ptPlyPanel"),
                     
                     hr(), # Adds space to bottom of app for drop down selectors
                     br(),
                     br()
                     
             ),
             
             # Plot right panel
             column( width = 2,
                     
                     h4("Download Plot:"),
                     h5("Recognizes the extensions eps/ps, tex (pictex), pdf, jpeg, tiff, png, bmp, svg and wmf (windows only)."),
                     textInput("ptDownloadTxt", label = NA, value = "myplot.pdf"),
                     downloadButton('ptDownloadPlot', "Download"),

                     h4("Select Discrimination Plot:"),
                     selectInput("ptPlys", "Facies Plot Choice:",
                                 c("PT", "PT_peacock"),
                                 selected = "PT"),
                     
                     wellPanel(
                       textInput("ptPlotTitle", "Title of plot:", value = "PT Plot")
                     ),
                     
                     wellPanel(style = "overflow-y:scroll; max-height: 500px",
                               
                       h4("Additional Curves:"),
                       # checkboxInput("ptFrb", "Not Found in Nature", TRUE),
                       checkboxInput("ptKAS", "Aluminosilicates", FALSE),
                       checkboxInput("ptIntru", "Intrusion Model", FALSE),
                       checkboxInput("ptColl", "Collision Model", FALSE),
                       checkboxInput("ptPlotInv", "Invert Y axis", FALSE),
                       checkboxInput("ptGTherm10", "10 degree/km Geothermal Gradient", FALSE),
                       checkboxInput("ptGTherm20", "20 degree/km Geothermal Gradient", FALSE),
                       checkboxInput("ptGTherm30", "30 degree/km Geothermal Gradient", FALSE),
                       checkboxInput("ptGSol", "Wet Granite Solidus", FALSE),
                       # checkboxInput("ptSatGr", "H2O Saturated Minimum Granite Melting Temperature", FALSE),
                       checkboxInput("ptAnQz", "Annite + Quartz + 1/2 O2 and K-Spar + Magnetite + H2O", FALSE),
                       checkboxInput("ptPhlQz", "Phlogopite + Quartz and Melt + Orthopyroxene", FALSE),
                       checkboxInput("ptAnhy", "Anhydrous Minimum Melting Temperature", FALSE),
                       checkboxInput("ptBtSil", "Biotite + Sillimanite and Cordierite + K-Spar", FALSE),
                       checkboxInput("ptAgCa", "Aragonite and Calcite", FALSE),
                       checkboxInput("ptMuQz", "Muscovite + Quartz and Al2SiO5 + KAlSi3O8 + H2O", FALSE)
                                              
                     )
             )        
           )
  )
)