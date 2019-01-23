# Start Data Tab Code #####################################################################################################

################ Code For Importing Data ################

# This block of code recognizes when the user selects an input file
ptFullData <- reactive({
  
  inFile <- input$ptFile
  
  if(is.null(inFile))
    return(NULL)
  
  ptFoo <- strsplit(inFile$name, "[.]")[[1]][2]
  
  if(ptFoo == "xlsx" | ptFoo == "xls"){
    foo2 <- read_excel(inFile$datapath, 1, na = "NA")
  }
  
  if(ptFoo == "csv"){
    foo2 <- read_csv(inFile$datapath, na = "NA")
  }
  
  # ga_collect_event(event_category = "App Uploads", event_action = "Data Upload", event_label = "User uploaded PT data")
  
  foo2  
})

# This block of code builds a reactiveValues object to store data
ptMain <- reactiveValues()

# This block of code stores the raw input data into a reactiveValues object as $Raw
observeEvent(input$ptFile, {
  
  ptMain$inputData <- ptFullData()
  
})

################ Code For Selecting and Building Plot Data ################

# This block of code pre-renders two user input boxes that
# allow the user to select which columns contain their Pressure and Temperature and grouping variables.
output$ptChoose_X <- renderUI({
  
  selectizeInput("ptX", "Select Temperature variable/s:", choices = as.list(c("", colnames(ptMain$inputData))), selected = NULL, multiple = TRUE)
  
})

output$ptChoose_Y <- renderUI({
  
  selectizeInput("ptY", "Select Pressure variable/s:", choices = as.list(c("", colnames(ptMain$inputData))), selected = NULL, multiple = TRUE)
  
})

output$ptChoose_G <- renderUI({
  
  selectizeInput("ptG", "Select grouping variable:", choices = as.list(c("", colnames(ptMain$inputData))), selected = NULL, multiple = FALSE)
  
})

# This block of code builds the plot data set
observeEvent( c(input$ptX, input$ptY, input$ptG), {
  
  if(!is.null(input$ptX) & !is.null(input$ptY)) {
    names <- colnames(ptMain$inputData)
    data <- ptMain$inputData
    
    X <- apply(data[,which(names %in% input$ptX)], 1, sum, na.rm = TRUE)
    Y <- apply(data[,which(names %in% input$ptY)], 1, sum, na.rm = TRUE)
    
    if(input$ptG != "") {
      Group <- apply(data[,which(names == input$ptG)], 1, factor)
    } else {
      Group <- as.factor(rep(1, length(X)))
    }
    
    ptMain$plotData <- data.frame(X = X, Y = Y, Group = Group)
    
  }
  
})

################ Code For Rendering Data Tables ################

# This line of code renders the raw data file and sends it the UI
output$ptFullData <- renderDataTable({ 
  ptMain$inputData
})

# This line of code renders the plot data and sends it the UI
output$ptPlotData <- renderDataTable({
  ptMain$plotData
})

# End Data Tab Code ##########################################################################################################

# Start Plot Tab Code #############################################################################################################

################ Code For Server Rendered Plot UI Controls ################

# This block of code pre-renders the controls for each group
output$ptPtControls <- renderUI({
  
  if(is.null(input$ptX) & is.null(input$ptY)) {return(NULL)}
  
  lev <- unique(as.character(ptMain$plotData$Group))
  cols <- gg_fill_hue(length(lev))
  
  lapply(seq_along(lev), function(i) { # start lapply
    
    bsCollapse(
      bsCollapsePanel(title = paste0("Group: ", lev[i]), style = "primary",
                      bsCollapse(open = "Point Controls:",
                                 bsCollapsePanel("Point Controls:", style = "primary",
                                                 checkboxInput(
                                                   inputId = paste0("ptPntShw", lev[i]),
                                                   label = "Show Points", 
                                                   value = TRUE
                                                 ),
                                                 fluidRow(
                                                   column(6,
                                                          colourInput(inputId = paste0("ptPntCol", lev[i]),
                                                                      label = "Color", 
                                                                      value = cols[i],
                                                                      allowTransparent = TRUE),
                                                          colourInput(inputId = paste0("ptPntFil", lev[i]),
                                                                      label = "Fill Color", 
                                                                      value = "gray50",
                                                                      allowTransparent = TRUE)
                                                   ),
                                                   column(6,
                                                          selectInput(inputId = paste0("ptPntSiz", lev[i]),
                                                                      label = "Pt. Size",
                                                                      c(1:7), selected = 4),
                                                          selectInput(inputId = paste0("ptPntShp", lev[i]),
                                                                      label = "Pt. Shape",
                                                                      c(0:25), selected = 19)
                                                   )
                                                 )
                                 ),
                                 bsCollapsePanel("Boundary controls:", style = "primary",
                                                 checkboxInput(inputId = paste0("ptBndShw", lev[i]),
                                                               label = "Add boundary:",
                                                               value = FALSE
                                                 ),
                                                 fluidRow(
                                                   column(6,
                                                          colourInput(inputId = paste0("ptBndCol", lev[i]),
                                                                      label = "Color", 
                                                                      value = cols[i],
                                                                      allowTransparent = TRUE),
                                                          colourInput(inputId = paste0("ptBndFil", lev[i]),
                                                                      label = "Fill Color", 
                                                                      value = "gray50",
                                                                      allowTransparent = TRUE),
                                                          numericInput(inputId = paste0("ptBndAngl", lev[i]),
                                                                       label = "Angularity",
                                                                       value = 0.5,
                                                                       step = 0.1
                                                                       )
                                                   ),
                                                   column(6,
                                                          selectInput(inputId = paste0("ptBndLty", lev[i]),
                                                                      label = "LineType",
                                                                      c("solid", "dashed", "dotdash", "dotted", "longdash", "twodash"), selected = "solid"),
                                                          selectInput(inputId = paste0("ptBndSiz", lev[i]),
                                                                      label = "LineWidth",
                                                                      c(1:6), selected = 3),
                                                          numericInput(inputId = paste0("ptBndExp", lev[i]),
                                                                       label = "Expansion",
                                                                       value = 0.01,
                                                                       step = 0.01
                                                                       )
                                                   )
                                                 )
                                 ),
                                 bsCollapsePanel("Path controls:", style = "primary",
                                                 checkboxInput(inputId = paste0("ptPthShw", lev[i]),
                                                               label = "Add path:",
                                                               value = FALSE
                                                 ),
                                                 fluidRow(
                                                   column(6,
                                                          colourInput(inputId = paste0("ptPthCol", lev[i]),
                                                                      label = "Color", 
                                                                      value = cols[i],
                                                                      allowTransparent = TRUE)
                                                   ),
                                                   column(6,
                                                          selectInput(inputId = paste0("ptPthLty", lev[i]),
                                                                      label = "LineType",
                                                                      c("solid", "dashed", "dotdash", "dotted", "longdash", "twodash"), selected = "solid"),
                                                          selectInput(inputId = paste0("ptPthSiz", lev[i]),
                                                                      label = "LineWidth",
                                                                      c(1:6), selected = 3)
                                                   )
                                                 )
                                 )
                      )
      )
    )
  }) #close lapply
  
})

# This block of code turns off all groups on the main plot
observeEvent(input$ptHideAll, {
  
  grpPts <- paste0("ptPntShw", unique(as.character(ptMain$plotData$Group)))
  
  for (i in 1:length(grpPts)) {
    updateCheckboxInput(session, grpPts[i], value = FALSE)
  }
  
})

# This block of code turns on all groups on the main plot
observeEvent(input$ptShowAll, {
  
  grpPts <- paste0("ptPntShw", unique(as.character(ptMain$plotData$Group)))
  
  for (i in 1:length(grpPts)) {
    updateCheckboxInput(session, grpPts[i], value = TRUE)
  }
  
})

# This block of code pre-renders the color controls for discrimination polygons
output$ptPlyPanel <- renderUI({
  
  lbls <- unique(ptMain$disc$plys$Label)
  
  bsCollapse(
    bsCollapsePanel("Discrimination plot polygon controls:", style = "primary",
                    conditionalPanel(condition = 'input.ptPlys != ""',
                                     
                                     h5("Click on the boxes below to change polygon colors."),
                                     lapply(seq_along(lbls), function(i) {
                                       div(style="display: inline-block; vertical-align:middle; width: 23%;",
                                           colourInput(paste0("ptPly", i), lbls[i], "#FFFFFF80", allowTransparent = TRUE)
                                       )
                                     })
                    )
    )
  )
  
})

################ Code For Discrimination Plot Components And Controls ################

# This block of code updates the axis labels and plot title when a discrimination plot is loaded
observeEvent(c(input$ptPlys, input$tabs == "pt"), {
  
  File <- paste0("www/plys/PT/",isolate(input$ptPlys), ".xlsx")
  plys <- read_excel(File, 1)
  lbls <- read_excel(File, 2)
  txt <- read_excel(File, 3)
  
  temp <- list(plys = plys, lbls = lbls, txt = txt)
  
  # ga_collect_event(event_category = "App Inputs", event_action = "Discrim Selection", event_label = paste0("PT: ", input$ptPlys))
  
  ptMain$disc <- temp
  ptMain$zoom <- list(xmin = txt$Xmin, xmax = txt$Xmax, ymin = txt$Ymin, ymax = txt$Ymax)
  
}, ignoreInit = T)

################ Code For Plot Interactivity ################

# This block of code resets the plot xmin, xmax, ymin, ymax on a brush event
observeEvent(input$ptPlotBrush, {

  brush <- input$ptPlotBrush
  if (!is.null(brush)) {
    ptMain$zoom <- list(xmin = brush$xmin, xmax = brush$xmax, ymin = brush$ymin, ymax = brush$ymax)
  }

})

# This block of code resets the zoom to null on a plot dblclick
observeEvent(input$ptPlotDblClick, {

  txt <- ptMain$disc$txt
  ptMain$zoom <- list(xmin = txt$Xmin, xmax = txt$Xmax, ymin = txt$Ymin, ymax = txt$Ymax)
    
})

# # This block of code returns plot hover info as a custom well panel popup near a hovered plot point
# output$ptHoverInfo <- renderUI({
#   req(input$ptPlotHover)
#   hover <- isolate(input$ptPlotHover)
#   point <- nearPoints(ptMain$plotData, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
#   if (nrow(point) == 0) return(NULL)
#   
#   # calculate point position INSIDE the image as percent of total dimensions
#   # from left (horizontal) and from top (vertical)
#   left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
#   top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
#   
#   # calculate distance from left and bottom side of the picture in pixels
#   left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
#   top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
#   
#   # create style property for tooltip
#   # background color is set so tooltip is a bit transparent
#   # z-index is set so we are sure our tooltip will be on top
#   style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
#                   "left:", left_px + 2, "px; top:", top_px + 2, "px;")
#   
#   # actual tooltip created as wellPanel
#   wellPanel(
#     style = style,
#     p(HTML(paste0("<b> Group: </b>", point$Group, "<br/>",
#                   "<b> X: </b>", point$X, "<br/>",
#                   "<b> Y: </b>", point$Y, "<br/>")))
#   )
# })

# This block of code creates a reactive object for PT plot polygons and labels
observeEvent( input$ptPlys, {
  if (input$ptPlys != "") {
    File <- paste0("www/plys/PT/",input$ptPlys, ".xlsx")
    plys <- read_excel(File, 1)
    lbls <- read_excel(File, 2)
    txt <- read_excel(File, 3)
    
    ptMain$disc <- list(plys = plys, lbls = lbls, txt = txt)
  }
})

# This block of code pre-renders the output of color controls for polygons
output$ptPlyPanel <- renderUI({ 
  
  lapply(seq_along(ptMain$disc$lbls$Label), function(i) {
    div(style="display: inline-block; vertical-align:moddle; width: 23%;",
        colourInput(paste0("ptPly", i),
                    ptMain$disc$lbls$Label[i],
                    ptMain$disc$lbls$Color[i],
                    allowTransparent = TRUE)
    )
  })
})

observe({
  
  if(!is.null(ptMain$plotData) & 
     !is.null(eval(parse(text = paste0("c(", paste0("input$ptPntShw", unique(as.character(ptMain$plotData$Group))[1], collapse = ", "), ")"))))
  ) {
    
    lev <- unique(as.character(ptMain$plotData$Group))
    
    pntOn <- paste0("c(", paste0("input$ptPntShw", lev, collapse = ", "), ")")
    pntOn <- eval(parse(text = pntOn))
    pnt.On <- data.frame(Group=lev, pntOn=pntOn) # Parse Point inputs
    ptMain$plotVars$pntOn <- join(ptMain$plotData, pnt.On)[,"pntOn"]
    
    pntCols <- paste0("c(", paste0("input$ptPntCol", lev, collapse = ", "), ")")
    pntCols <- eval(parse(text = pntCols))
    pnt.colors <- data.frame(Group=lev, pntCols=pntCols)
    ptMain$plotVars$pntCols <- join(ptMain$plotData, pnt.colors)[,"pntCols"]
    
    pntFils <- paste0("c(", paste0("input$ptPntFil", lev, collapse = ", "), ")")
    pntFils <- eval(parse(text = pntFils))
    pnt.fills <- data.frame(Group=lev, pntFils=pntFils)
    ptMain$plotVars$pntFils <- join(ptMain$plotData, pnt.fills)[,"pntFils"]
    
    pntSizs <- paste0("c(", paste0("input$ptPntSiz", lev, collapse = ", "), ")")
    pntSizs <- as.numeric(eval(parse(text = pntSizs)))
    pnt.sizes <- data.frame(Group=lev, pntSizs=pntSizs)
    ptMain$plotVars$pntSizs <- join(ptMain$plotData, pnt.sizes)[,"pntSizs"]
    
    pntPchs <- paste0("c(", paste0("input$ptPntShp", lev, collapse = ", "), ")")
    pntPchs <- as.integer(eval(parse(text = pntPchs)))
    pnt.characters <- data.frame(Group=lev, pntPchs=pntPchs)
    ptMain$plotVars$pntPchs <- join(ptMain$plotData, pnt.characters)[,"pntPchs"]
    
    # Parse Boundary Inputs
    bndOn <- paste0("c(", paste0("input$ptBndShw", lev, collapse = ", "), ")")
    bndOn <- eval(parse(text = bndOn))
    bnd.On <- data.frame(Group=lev, bndOn=bndOn)
    ptMain$plotVars$bndOn <- join(ptMain$plotData, bnd.On)[,"bndOn"]
    
    bndCols <- paste0("c(", paste0("input$ptBndCol", lev, collapse = ", "), ")")
    bndCols <- eval(parse(text = bndCols))
    bnd.colors <- data.frame(Group=lev, bndCols=bndCols)
    ptMain$plotVars$bndCols <- join(ptMain$plotData, bnd.colors)[,"bndCols"]
    
    bndFils <- paste0("c(", paste0("input$ptBndFil", lev, collapse = ", "), ")")
    bndFils <- eval(parse(text = bndFils))
    bnd.fills <- data.frame(Group=lev, bndFils=bndFils)
    ptMain$plotVars$bndFils <- join(ptMain$plotData, bnd.fills)[,"bndFils"]
    
    bndLtys <- paste0("c(", paste0("input$ptBndLty", lev, collapse = ", "), ")")
    bndLtys <- eval(parse(text = bndLtys))
    bnd.linetypes <- data.frame(Group=lev, bndLtys=bndLtys)
    ptMain$plotVars$bndLtys <- join(ptMain$plotData, bnd.linetypes)[,"bndLtys"]
    
    bndSizs <- paste0("c(", paste0("input$ptBndSiz", lev, collapse = ", "), ")")
    bndSizs <- as.numeric(eval(parse(text = bndSizs)))
    bnd.sizes <- data.frame(Group=lev, bndSizs=bndSizs)
    ptMain$plotVars$bndSizs <- join(ptMain$plotData, bnd.sizes)[,"bndSizs"]
    
    bndAngls <- paste0("c(", paste0("input$ptBndAngl", lev, collapse = ", "), ")")
    bndAngls <- as.numeric(eval(parse(text = bndAngls)))
    bnd.angularities <- data.frame(Group=lev, bndAngls=bndAngls)
    ptMain$plotVars$bndAngls <- join(ptMain$plotData, bnd.angularities)[,"bndAngls"]
    
    bndExps <- paste0("c(", paste0("input$ptBndExp", lev, collapse = ", "), ")")
    bndExps <- as.numeric(eval(parse(text = bndExps)))
    bnd.expansions <- data.frame(Group=lev, bndExps=bndExps)
    ptMain$plotVars$bndExps <- join(ptMain$plotData, bnd.expansions)[,"bndExps"]
    
    # Parse Path Inputs
    pthOn <- paste0("c(", paste0("input$ptPthShw", lev, collapse = ", "), ")")
    pthOn <- eval(parse(text = pthOn))
    pth.On <- data.frame(Group=lev, pthOn=pthOn)
    ptMain$plotVars$pthOn <- join(ptMain$plotData, pth.On)[,"pthOn"]
    
    pthCols <- paste0("c(", paste0("input$ptPthCol", lev, collapse = ", "), ")")
    pthCols <- eval(parse(text = pthCols))
    pth.colors <- data.frame(Group=lev, pthCols=pthCols)
    ptMain$plotVars$pthCols <- join(ptMain$plotData, pth.colors)[,"pthCols"]
    
    pthLtys <- paste0("c(", paste0("input$ptPthLty", lev, collapse = ", "), ")")
    pthLtys <- eval(parse(text = pthLtys))
    pth.linetypes <- data.frame(Group=lev, pthLtys=pthLtys)
    ptMain$plotVars$pthLtys <- join(ptMain$plotData, pth.linetypes)[,"pthLtys"]
    
    pthSizs <- paste0("c(", paste0("input$ptPthSiz", lev, collapse = ", "), ")")
    pthSizs <- as.numeric(eval(parse(text = pthSizs)))
    pth.sizes <- data.frame(Group=lev, pthSizs=pthSizs)
    ptMain$plotVars$pthSizs <- join(ptMain$plotData, pth.sizes)[,"pthSizs"]
    
  }
  
})

# This block of code builds the plot
ptPlot <- reactive({
  
  req(input$ptPlotTitle, input$ptPly1)
  
  # Builds Aluminosilicate polygons and labels
  AS <- read_excel("www/plys/PT/PT.xlsx", 4)
  AS.lbl <- read_excel("www/plys/PT/PT.xlsx", 5)
  
  # Builds Model Path lines
  IntruPath <- read_excel("www/plys/PT/PT_pathways.xlsx", 1)
  CollPath <- read_excel("www/plys/PT/PT_pathways.xlsx", 2)
  
  # Load Polygon data, create label variable and parse polygon color inputs
  plys <- ptMain$disc$plys
  plyLbls <- ptMain$disc$lbls$Label
  plyCols <- paste0("c(", paste0("input$ptPly", c(1:length(plyLbls)), collapse = ", "), ")")
  plyCols <- eval(parse(text = plyCols))

  #### Base Plot ####
  
  #browser()
  
  PT <- ggplot()
  
  #### Add discrimination polygons ####
  
  for (i in 1 : max(plys$ID)) {
    PT <- PT +
      geom_polygon(data = plys[which(plys$ID == i),],
                   aes(X, Y),
                   fill = plyCols[i],
                   col = "black",
                   lwd = 1)
  }
  
  #### Add Points and other Group Components ####
  
  if(!is.null(ptMain$plotData)) {
    data <- cbind(ptMain$plotData, do.call(cbind.data.frame, ptMain$plotVars))
    
    # Add Boundaries
    if (any(data$bndOn) == TRUE) {
      
      bndData <- subset(data, bndOn == TRUE)
      names(bndData)[3] <- "bndGrps"
      
      PT <- PT +
        geom_encircle(data = bndData,
                      aes(X, Y, fill = bndGrps),
                      col = as.character(bndData$bndCols),
                      linetype = as.character(bndData$bndLtys),
                      size = bndData$bndSizs,
                      s_shape = bndData$bndAngl,
                      expand = bndData$bndExp
        ) +
        scale_fill_manual(name = "Boundaries",
                          values = as.character(unique(bndData[,c("bndFils","bndGrps")])[,1])
        ) +
        guides(fill = guide_legend(order = 3,
                                   keywidth = 3,
                                   override.aes = list(color = as.character(unique(bndData[,c("bndCols","bndGrps")])[,1]),
                                                       linetype = as.character(unique(bndData[,c("bndLtys","bndGrps")])[,1]),
                                                       size = unique(bndData[,c("bndSizs","bndGrps")])[,1],
                                                       alpha = rep(1, length(unique(as.character(bndData$bndGrps))))
                                   )
        )
        )
      
      
    }
    
    # Add Paths
    if (any(data$pthOn) == TRUE) {
      
      pthData <- subset(data, pthOn == TRUE)
      names(pthData)[3] <- "pthGrps"
      
      PT <- PT +
        geom_path(data = pthData,
                  aes(X, Y, linetype = pthGrps),
                  col = as.character(pthData$pthCols),
                  size = pthData$pthSizs
        ) +
        scale_linetype_manual(name = "Paths",
                              values = as.character(unique(pthData[,c("pthLtys","pthGrps")])[,1])
        ) +
        guides(linetype = guide_legend(order = 2,
                                       keywidth = 3,
                                       override.aes = list(color = as.character(unique(pthData[,c("pthCols","pthGrps")])[,1]),
                                                           size = unique(pthData[,c("pthSizs","pthGrps")])[,1],
                                                           alpha = rep(1, length(unique(as.character(pthData$pthGrps))))
                                       )
        )
        )
      
    }
    
    # Add points
    if (any(data$pntOn) == TRUE){
      
      pntData <- subset(data, pntOn == TRUE)
      names(pntData)[3] <- "pntGrps"
      
      PT <- PT +
        geom_point(data = pntData,
                   aes(X, Y, col = pntGrps),
                   fill = as.character(pntData$pntFils),
                   size = pntData$pntSizs,
                   shape = pntData$pntPchs
        ) +
        scale_colour_manual(name = "Points",
                            values = as.character(unique(pntData[,c("pntCols","pntGrps")])[,1])
        ) +
        guides(colour = guide_legend(order = 1,
                                     keywidth = 3,
                                     override.aes = list(fill = as.character(unique(pntData[,c("pntFils","pntGrps")])[,1]),
                                                         shape = unique(pntData[,c("pntPchs","pntGrps")])[,1],
                                                         size = unique(pntData[,c("pntSizs","pntGrps")])[,1],
                                                         alpha = rep(1, length(unique(as.character(pntData$pntGrps))))
                                     )
        )
        )
      
    }
    
  }
  
  # Add various lines and polygons

  # Add aluminosilicate lines
  if (input$ptKAS == TRUE) {
    for (i in 1: max(AS$ID)) {
      PT <- PT +
        geom_polygon(data = AS[which(AS$ID == i),], aes(x = x, y = y), fill = "#FFFFFF2C",
                     col = "black", lwd = 1)
    }
  }
  
  # Add other lines here
  if (input$ptIntru == TRUE) {
      PT <- PT +
        geom_path(data = IntruPath, aes (x = X, y = Y), arrow = arrow(angle = 25, 
        type = "closed", length = unit(0.12, "inches")), lineend = "round", linejoin = "round", color = IntruPath$Color, lwd = 1)
  }
  
  if (input$ptColl == TRUE) {
      PT <- PT +
        geom_path(data = CollPath, aes (x = X, y = Y), arrow = arrow(angle = 25, 
        type = "closed", length = unit(0.12, "inches")), lineend = "round", linejoin = "round", color = CollPath$Color, lwd = 1)
  }
  
  if (input$ptGTherm10 == TRUE) {
    PT <- PT +
      geom_line(data = data.frame(x = c(0,702), y = c(0, 20)), aes(x, y), col = "black", lty =2, lwd = 2)
  }

  if (input$ptGTherm20 == TRUE) {
    PT <- PT +
      geom_line(data = data.frame(x = c(0,1200), y = c(0, 17.1)), aes(x, y), col = "black", lty =2, lwd = 2)
  }

  if (input$ptGTherm30 == TRUE) {
    PT <- PT +
      geom_line(data = data.frame(x = c(0,1200), y = c(0, 11.4)), aes(x, y), col = "black", lty =2, lwd = 2)
  }
  
  if (input$ptGSol == TRUE) {
      gsol <- data.frame(
        x = c(600, 640, 650, 700, 725, 800, 870),
        y = c(14, 4, 3.5, 2.5, 2, 1, 0))
      
      PT <- PT +
        geom_line(data = gsol, aes(x = x, y = y), col = "black", lwd = 2) 
  }
  
  if (input$ptMuQz == TRUE) {
    MuQz <- data.frame(
      x = c(460, 543, 608, 660, 727, 800),
      y = c(0, 1.1, 2.42, 3.79, 5.84, 8.8))
    
    PT <- PT +
      geom_line(data = MuQz, aes(x = x, y = y), col = "black", lwd = 2) 
  }
  
  if (input$ptAnQz == TRUE) {
    AnQz <- data.frame(
      x = c(688, 825, 883, 947, 1000),
      y = c(0.88, 2.62, 4.04, 6.11, 8.80))
    
    PT <- PT +
      geom_line(data = AnQz, aes(x = x, y = y), col = "black", lwd = 2) 
  }
  
  if (input$ptPhlQz == TRUE) {
    PhlQz <- data.frame(
      x = c(970, 1023, 1074, 1119, 1166, 1185),
      y = c(0.19, 0.59, 1.53, 3.1, 5.61, 6.8))
    
    PT <- PT +
      geom_line(data = PhlQz, aes(x = x, y = y), col = "black", lwd = 2) 
  }
  
  if (input$ptAnhy == TRUE) {
    Anhy <- data.frame(
      x = c(969, 1128),
      y = c(0, 14.9))
    
    PT <- PT +
      geom_line(data = Anhy, aes(x = x, y = y), col = "black", lwd = 2) 
  }
  
  if (input$ptBtSil == TRUE) {
    BtSil <- data.frame(
      x = c(312, 469, 646, 833, 1036),
      y = c(3.33, 4.19, 5.01, 5.75, 6.26))
    
    PT <- PT +
      geom_line(data = BtSil, aes(x = x, y = y), col = "black", lwd = 2) 
  }
  
  if (input$ptAgCa == TRUE) {
    AgCa <- data.frame(
      x = c(0, 1200),
      y = c(3, 20))
    
    PT <- PT +
      geom_line(data = AgCa, aes(x = x, y = y), col = "black", lwd = 2) 
  }
  
  ##### Add Text and Labels #####
  
  # Add discrimination polygon labels
  if (input$ptPlys != "") {
    
    lbls <- ptMain$disc$lbls
    
    for (i in 1:nrow(lbls)) {
      PT <- PT +
        geom_text(data = lbls[i,],
                  aes(X, Y),
                  label = as.character(lbls$Label[i]),
                  angle = lbls$Angle[i],
                  hjust = lbls$Hjust[i],
                  vjust = lbls$Vjust[i],
                  color = "black",
                  size = 3)
    }
    
  }  
  
  if (input$ptKAS == TRUE) {
    for (i in 1: max(AS$ID)) {
      PT <- PT +
        geom_text(data = AS.lbl, aes(x, y, label = as.character(Label)), color = 'black',
                  size = 5, fontface = "italic")
      
    }
  }
  
  # Inverting the Y-axis
  if (input$ptPlotInv == TRUE) {
      PT <- PT +
        scale_y_reverse(expand = FALSE) +
        scale_y_reverse( sec.axis = sec_axis(~.*(200/57), name = "Depth (Km)"), expand = FALSE)
  } else {
    PT <- PT +
      scale_y_continuous( sec.axis = sec_axis(~.*(200/57), name = "Depth (Km)"), expand = FALSE)
  }

  if (input$ptPlotInv == TRUE) {
    fooAngle <- -1
  } else {
    fooAngle <- 1
  }
  
 # The following if statements will control the text labels of clickable curves (e.g. gethermal gradient, mineral boundaries, etc.)  
 # These must be after the invert plot control, so the text angle can change accordingly
      
  if (input$ptIntru == TRUE) {
    PT <- PT +
      geom_text(data = data.frame(x = 600, y = 5), aes(x, y), label = "Intrusion", color = "black", size = 7)
  }
  
  if (input$ptColl == TRUE) {
    PT <- PT +
      geom_text(data = data.frame(x = 400, y = 10), aes(x, y), label = "Collision", color = "black", size = 7)
  }
  
  if (input$ptGTherm10 == TRUE) {
        PT <- PT +
          geom_text(data = data.frame(x = (ptMain$disc$txt$Ymax/0.0285) + 50, y = ptMain$disc$txt$Ymax - 1), aes(x, y), label = "Geothermal Gradient\n(10 degrees/km)", color = "black", size = 3)
  }

  if (input$ptGTherm20 == TRUE) {
      PT <- PT +
        geom_text(data = data.frame(x = ptMain$disc$txt$Xmax - ptMain$disc$txt$Xmax/10.5, y = ptMain$disc$txt$Xmax*0.01425), aes(x, y), label = "Geothermal Gradient\n(20 degrees/km)", color = "black", size = 3)
  }
  
  if (input$ptGTherm30 == TRUE) {
      PT <- PT +
        geom_text(data = data.frame(x = ptMain$disc$txt$Xmax - ptMain$disc$txt$Xmax/10.5, y = ptMain$disc$txt$Xmax*0.0095), aes(x, y), label = "Geothermal Gradient\n(30 degrees/km)", color = "black", size = 3)
  }
 
  if (input$ptGSol == TRUE) {
      PT <- PT +
        geom_text(data = data.frame(x = 640, y = 9), aes(x, y), label = "Wet Granite Solidus", color = "black", size = 3, angle = -85*fooAngle)
  }
  
  if (input$ptMuQz == TRUE) {
      PT <- PT +
        geom_text(data = data.frame(x = 620, y = 3.5), aes(x, y), label = "Muscovite + Quartz", color = "black", size = 3, angle = 53*fooAngle) +
        geom_text(data = data.frame(x = 650, y = 2.9), aes(x, y), label = "Al2SiO5 + K-Spar + H2O", color = "black", size = 3, angle = 53*fooAngle)
  }
  
  if (input$ptAnQz == TRUE) {
      PT <- PT +
        geom_text(data = data.frame(x = 790, y = 2.85), aes(x, y), label = "Annite + Quartz + 1 / 2 O2", color = "black", size = 3, angle = 40*fooAngle) +
        geom_text(data = data.frame(x = 825, y = 2.25), aes(x, y), label = "K-Spar + Magnetite + H2O", color = "black", size = 3, angle = 40*fooAngle)
  }
  
  if (input$ptPhlQz == TRUE) {
      PT <- PT +
        geom_text(data = data.frame(x = 1065, y = 2.9), aes(x, y), label = "Phlogopite + Quartz", color = "black", size = 3, angle = 58*fooAngle) +
        geom_text(data = data.frame(x = 1110, y = 1.65), aes(x, y), label = "Melt + Orthopyroxene", color = "black", size = 3, angle = 58*fooAngle)
  }
  
  if (input$ptAnhy == TRUE) {
      PT <- PT +
        geom_text(data = data.frame(x = 1040, y = 4.8), aes(x, y), label = "Anhydrous Minimum Melting", color = "black", size = 3, angle = 78*fooAngle)
  }
 
  if (input$ptBtSil == TRUE) {
      PT <- PT +
        geom_text(data = data.frame(x = 800, y = 6), aes(x, y), label = "Biotite + Sillimanite", color = "black", size = 3, angle = 12*fooAngle) +
        geom_text(data = data.frame(x = 800, y = 5.1), aes(x, y), label = "Cordierite + K-Spar", color = "black", size = 3, angle = 12*fooAngle)
  }
  
  if (input$ptAgCa == TRUE) {
      PT <- PT +
        geom_text(data = data.frame(x = 600, y = 12), aes(x, y), label = "Aragonite", color = "black", size = 3, angle = 35*fooAngle) +
        geom_text(data = data.frame(x = 620, y = 11.4), aes(x, y), label = "Calcite", color = "black", size = 3, angle = 35*fooAngle)
  }
  
  border <- data.frame(x = c(ptMain$zoom$xmin, ptMain$zoom$xmin, ptMain$zoom$xmax, ptMain$zoom$xmax),
                       y = c(ptMain$zoom$ymin, ptMain$zoom$ymax, ptMain$zoom$ymax, ptMain$zoom$ymin))
  
  # Make final changes and output final plot
  PT +
    labs(title = input$ptPlotTitle) +
    xlab( expression(paste("Temperature [",degree,"C]")) )+
    ylab("Pressure (Kbar)") +
    geom_polygon(data = border, aes(x = x, y = y), fill = NA, col = "black", lwd = 2) +
    coord_cartesian(expand = FALSE, xlim = c(ptMain$zoom$xmin, ptMain$zoom$xmax), ylim = c(ptMain$zoom$ymin, ptMain$zoom$ymax)) +
    # Add secondary y-axis
    theme_classic() +
    theme(plot.title = element_text(size = 20, hjust = 0.5))
  
  
})

# This block of code renders the plot and sends it to the UI as an output
output$ptPlot <- renderPlot({ ptPlot() })

# This block of code creates a download handler and downloads a plot
output$ptDownloadPlot <- downloadHandler(
  
  filename =  function() {
    if(any(c("ps", "eps", "png", "pdf", "jpeg", "tiff", "bmp", "svg", "wmf")%in%strsplit(input$ptDownloadTxt, "[.]")[[1]][2])) {
      input$ptDownloadTxt
    } else {
      "myPlot.pdf"
    }
  },
  
  content = function(file) {
    # ga_collect_event(event_category = "App Outputs", event_action = "Plot Download", event_label = "User downloaded PT Plot")
    ggsave(file, plot = ptPlot(), width = 11, height = 8.5, units = 'in')
  }
  
  # filename =  function() {
  #   paste("MyPT", input$ptDownloadType, sep=".")
  # },
  # 
  # content = function(file) {
  #   ggsave(file, plot = ptPlot(), width = 10, height = 8.14, device = input$ptDownloadType)
  # }
)