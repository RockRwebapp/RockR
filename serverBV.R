# Start Data Tab Code #####################################################################################################

################ Code For Importing Data ################

# This block of code recognizes when the user selects an input file
bvFullData <- reactive({
  
  inFile <- input$bvFile
  
  if(is.null(inFile))
    return(NULL)
  
  bvFoo <- strsplit(inFile$name, "[.]")[[1]][2]
  
  if(bvFoo == "xlsx" | bvFoo == "xls"){
    foo2 <- read_excel(inFile$datapath, 1, na = "NA")
  }
  
  if(bvFoo == "csv"){
    foo2 <- read_csv(inFile$datapath, na = "NA")
  }
  
  # ga_collect_event(event_category = "App Uploads", event_action = "Data Upload", event_label = "User uploaded BV data")
  
  foo2  
})

# This block of code builds a reactiveValues object to store data
bvMain <- reactiveValues(plotData = data.frame(X = 0:100, Y = 0:100, Group = rep(1, 101)))

# This block of code stores the raw input data into a reactiveValues object as $Raw
observeEvent(input$bvFile, {
  
  bvMain$inputData <- bvFullData()

})

################ Code For Selecting and Building Plot Data ################

# This block of code pre-renders four user input boxes that
# allow the user to select which column is their A, B, C, and grouping variables.
output$bvChoose_X <- renderUI({
  
  selectizeInput("bvX", "Select X variable/s:", choices = as.list(c("", colnames(bvMain$inputData))), selected = NULL, multiple = TRUE)

})

output$bvChoose_Y <- renderUI({
  
  selectizeInput("bvY", "Select Y variable/s:", choices = as.list(c("", colnames(bvMain$inputData))), selected = NULL, multiple = TRUE)

})

output$bvChoose_G <- renderUI({
  
  selectizeInput("bvG", "Select grouping variable:", choices = as.list(c("", colnames(bvMain$inputData))), selected = NULL, multiple = FALSE)

})

# This block of code builds the plot data set
observeEvent( c(input$bvX, input$bvY, input$bvG), {
  
  if(!is.null(input$bvX) & !is.null(input$bvY)) {
    names <- colnames(bvMain$inputData)
    data <- bvMain$inputData
    
    X <- apply(data[,which(names %in% input$bvX)], 1, sum, na.rm = TRUE)
    Y <- apply(data[,which(names %in% input$bvY)], 1, sum, na.rm = TRUE)
    
    #browser()
    
    if(input$bvG != "") {
      Group <- apply(data[,which(names == input$bvG)], 1, factor)
    } else {
      Group <- as.factor(rep(1, length(X)))
    }

    bvMain$plotData <- data.frame(X = X, Y = Y, Group = Group)
    
    if(input$bvDiscrim == "") {
      updateNumericInput(session, "bvXmin", value = min(bvMain$plotData$X))
      updateNumericInput(session, "bvXmax", value = max(bvMain$plotData$X))
      updateNumericInput(session, "bvYmin", value = min(bvMain$plotData$Y))
      updateNumericInput(session, "bvYmax", value = max(bvMain$plotData$Y))
    } else {}
  }
  
})

################ Code For Rendering Data Tables ################

# This line of code renders the raw data file and sends it the UI
output$bvFullData <- renderDataTable({ 
  bvMain$inputData
})

# This line of code renders the plot data and sends it the UI
output$bvPlotData <- renderDataTable({
  bvMain$plotData
})

# End Data Tab Code ##########################################################################################################


# Start Plot Tab Code #############################################################################################################

################ Code For Server Rendered Plot UI Controls ################

# This block of code pre-renders the controls for each group
output$bvPtControls <- renderUI({
  
  if(is.null(input$bvX) & is.null(input$bvY)) {return(NULL)}
  
  lev <- unique(as.character(bvMain$plotData$Group))
  cols <- gg_fill_hue(length(lev))
  
  lapply(seq_along(lev), function(i) { # start lapply
    
    bsCollapse(
          bsCollapsePanel(title = paste0("Group: ", lev[i]), style = "primary",
            bsCollapse(open = "Point Controls:",
              bsCollapsePanel("Point Controls:", style = "primary",
                checkboxInput(
                  inputId = paste0("bvPntShw", lev[i]),
                  label = "Show Points", 
                  value = TRUE
                ),
                fluidRow(
                  column(6,
                         colourInput(inputId = paste0("bvPntCol", lev[i]),
                            label = "Color", 
                            value = cols[i],
                            allowTransparent = TRUE),
                         colourInput(inputId = paste0("bvPntFil", lev[i]),
                                     label = "Fill Color", 
                                     value = "gray50",
                                     allowTransparent = TRUE)
                  ),
                  column(6,
                         selectInput(inputId = paste0("bvPntSiz", lev[i]),
                                     label = "Pt. Size",
                                     c(1:7), selected = 4),
                         selectInput(inputId = paste0("bvPntShp", lev[i]),
                                     label = "Pt. Shape",
                                     c(0:25), selected = 19)
                  )
                )
              ),
              bsCollapsePanel("Boundary controls:", style = "primary",
                checkboxInput(inputId = paste0("bvBndShw", lev[i]),
                                label = "Add boundary:",
                                value = FALSE
                ),
                fluidRow(
                  column(6,
                         colourInput(inputId = paste0("bvBndCol", lev[i]),
                                     label = "Color", 
                                     value = cols[i],
                                     allowTransparent = TRUE),
                         colourInput(inputId = paste0("bvBndFil", lev[i]),
                                     label = "Fill Color", 
                                     value = "gray50",
                                     allowTransparent = TRUE),
                         numericInput(inputId = paste0("bvBndAngl", lev[i]),
                                      label = "Angularity",
                                      value = 0.5,
                                      step = 0.1
                                      )
                  ),
                  column(6,
                         selectInput(inputId = paste0("bvBndLty", lev[i]),
                                     label = "LineType",
                                     c("solid", "dashed", "dotdash", "dotted", "longdash", "twodash"), selected = "solid"),
                         selectInput(inputId = paste0("bvBndSiz", lev[i]),
                                     label = "LineWidth",
                                     c(1:6), selected = 3),
                         numericInput(inputId = paste0("bvBndExp", lev[i]),
                                      label = "Expansion",
                                      value = 0.01,
                                      step = 0.01
                                      )
                  )
                )
              ),
              bsCollapsePanel("Path controls:", style = "primary",
                              checkboxInput(inputId = paste0("bvPthShw", lev[i]),
                                            label = "Add path:",
                                            value = FALSE
                              ),
                              fluidRow(
                                column(6,
                                       colourInput(inputId = paste0("bvPthCol", lev[i]),
                                                   label = "Color", 
                                                   value = cols[i],
                                                   allowTransparent = TRUE)
                                ),
                                column(6,
                                       selectInput(inputId = paste0("bvPthLty", lev[i]),
                                                   label = "LineType",
                                                   c("solid", "dashed", "dotdash", "dotted", "longdash", "twodash"), selected = "solid"),
                                       selectInput(inputId = paste0("bvPthSiz", lev[i]),
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
observeEvent(input$bvHideAll, {
  
  grpPts <- paste0("bvPntShw", unique(as.character(bvMain$plotData$Group)))
  
  for (i in 1:length(grpPts)) {
    updateCheckboxInput(session, grpPts[i], value = FALSE)
  }
  
})

# This block of code turns on all groups on the main plot
observeEvent(input$bvShowAll, {
  
  grpPts <- paste0("bvPntShw", unique(as.character(bvMain$plotData$Group)))
  
  for (i in 1:length(grpPts)) {
    updateCheckboxInput(session, grpPts[i], value = TRUE)
  }
  
})

# This block of code pre-renders the color controls for discrimination polygons
output$bvPlyPanel <- renderUI({
  
  lbls <- unique(bvMain$disc$plys$Label)

  bsCollapse(
    bsCollapsePanel("Discrimination plot polygon controls:", style = "primary",
                    conditionalPanel(condition = 'input.bvDiscrim != ""',
                                     
                                     h5("Click on the boxes below to change polygon colors."),
                                     lapply(seq_along(lbls), function(i) {
                                       div(style="display: inline-block; vertical-align:middle; width: 23%;",
                                           colourInput(paste0("bvPly", i), lbls[i], "#FFFFFF80", allowTransparent = TRUE)
                                       )
                                     })
                    )
    )
  )
  
})

################ Code For Discrimination Plot Components And Controls ################

# This block of code updates the axis labels and plot title when a discrimination plot is loaded
observeEvent(input$bvDiscrim, {
  
  if (isolate(input$bvDiscrim) == "") {
    
    #update reactive values
    updateNumericInput(session, "bvXmin", value = min(bvMain$plotData$X))
    updateNumericInput(session, "bvXmax", value = max(bvMain$plotData$X))
    updateNumericInput(session, "bvYmin", value = min(bvMain$plotData$Y))
    updateNumericInput(session, "bvYmax", value = max(bvMain$plotData$Y))

    #update input boxes without reactivity
    updateTextInput(session, "bvPlotTitle", value = "My Title")
    updateTextInput(session, "bvPlotLabX", value = "X")
    updateTextInput(session, "bvPlotLabY", value = "Y")
    
    updateSelectInput(session, "bvXTrans", selected = "identity")
    updateSelectInput(session, "bvYTrans", selected = "identity")
    
  }
  
  if (isolate(input$bvDiscrim) != "") {
    
    File <- paste0("www/plys/BV/",isolate(input$bvDiscrim), ".xlsx")
    plys <- read_excel(File, 1)
    lbls <- read_excel(File, 2)
    txt <- read_excel(File, 3)
    
    temp <- list(plys = plys, lbls = lbls, txt = txt)
    data <- temp$txt
    
    updateNumericInput(session, "bvXmin", value = data$Xmin)
    updateNumericInput(session, "bvXmax", value = data$Xmax)
    updateNumericInput(session, "bvYmin", value = data$Ymin)
    updateNumericInput(session, "bvYmax", value = data$Ymax)
    
    updateTextInput(session, "bvPlotTitle", value = as.character(data$Title))
    updateTextInput(session, "bvPlotLabX", value = as.character(data$X))
    updateTextInput(session, "bvPlotLabY", value = as.character(data$Y))
    
    updateSelectInput(session, "bvXTrans", selected = as.character(data$Xtrans))
    updateSelectInput(session, "bvYTrans", selected = as.character(data$Ytrans))
    
    # ga_collect_event(event_category = "App Inputs", event_action = "Discrim Selection", event_label = paste0("BV: ", input$bvDiscrim))
    bvMain$disc <- temp
    
  }
  
})

################ Code For Plot Interactivity ################

# This block of code resets the plot xmin, xmax, ymin, ymax on a brush event
observeEvent(input$bvPlotBrush, {
  
  brush <- input$bvPlotBrush
  if (!is.null(brush)) {
    # reset inputs on brush event
    updateNumericInput(session, "bvXmin", value = brush$xmin)
    updateNumericInput(session, "bvXmax", value = brush$xmax)
    updateNumericInput(session, "bvYmin", value = brush$ymin)
    updateNumericInput(session, "bvYmax", value = brush$ymax)
  }
  
})

# This block of code resets the zoom to null on a plot dblclick
observeEvent(input$bvPlotDblClick, {
    
    if (input$bvDiscrim != "") {
      data <- bvMain$disc$txt
      updateNumericInput(session, "bvXmin", value = data$Xmin)
      updateNumericInput(session, "bvXmax", value = data$Xmax)
      updateNumericInput(session, "bvYmin", value = data$Ymin)
      updateNumericInput(session, "bvYmax", value = data$Ymax)
    } else {
      updateNumericInput(session, "bvXmin", value = min(bvMain$plotData$X))
      updateNumericInput(session, "bvXmax", value = max(bvMain$plotData$X))
      updateNumericInput(session, "bvYmin", value = min(bvMain$plotData$Y))
      updateNumericInput(session, "bvYmax", value = max(bvMain$plotData$Y))
    }
    
})

# This block of code returns plot hover info as a custom well panel popup near a hovered plot point
output$bvHoverInfo <- renderUI({
  req(input$bvPlotHover)
  hover <- isolate(input$bvPlotHover)
  point <- nearPoints(bvMain$plotData, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
  if (nrow(point) == 0) return(NULL)
  
  # calculate point position INSIDE the image as percent of total dimensions
  # from left (horizontal) and from top (vertical)
  left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
  
  # calculate distance from left and bottom side of the picture in pixels
  left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  
  # create style property for tooltip
  # background color is set so tooltip is a bit transparent
  # z-index is set so we are sure our tooltip will be on top
  style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                  "left:", left_px + 2, "px; top:", top_px + 2, "px;")
  
  # actual tooltip created as wellPanel
  wellPanel(
    style = style,
    p(HTML(paste0("<b> Group: </b>", point$Group, "<br/>",
                  "<b> X: </b>", point$X, "<br/>",
                  "<b> Y: </b>", point$Y, "<br/>")))
  )
})

observe({
  
  if(!is.null(bvMain$plotData) & 
     !is.null(eval(parse(text = paste0("c(", paste0("input$bvPntShw", unique(as.character(bvMain$plotData$Group))[1], collapse = ", "), ")"))))
  ) {
    
    lev <- unique(as.character(bvMain$plotData$Group))
    
    pntOn <- paste0("c(", paste0("input$bvPntShw", lev, collapse = ", "), ")")
    pntOn <- eval(parse(text = pntOn))
    pnt.On <- data.frame(Group=lev, pntOn=pntOn) # Parse Point inputs
    bvMain$plotVars$pntOn <- join(bvMain$plotData, pnt.On)[,"pntOn"]
    
    pntCols <- paste0("c(", paste0("input$bvPntCol", lev, collapse = ", "), ")")
    pntCols <- eval(parse(text = pntCols))
    pnt.colors <- data.frame(Group=lev, pntCols=pntCols)
    bvMain$plotVars$pntCols <- join(bvMain$plotData, pnt.colors)[,"pntCols"]
    
    pntFils <- paste0("c(", paste0("input$bvPntFil", lev, collapse = ", "), ")")
    pntFils <- eval(parse(text = pntFils))
    pnt.fills <- data.frame(Group=lev, pntFils=pntFils)
    bvMain$plotVars$pntFils <- join(bvMain$plotData, pnt.fills)[,"pntFils"]
    
    pntSizs <- paste0("c(", paste0("input$bvPntSiz", lev, collapse = ", "), ")")
    pntSizs <- as.numeric(eval(parse(text = pntSizs)))
    pnt.sizes <- data.frame(Group=lev, pntSizs=pntSizs)
    bvMain$plotVars$pntSizs <- join(bvMain$plotData, pnt.sizes)[,"pntSizs"]
    
    pntPchs <- paste0("c(", paste0("input$bvPntShp", lev, collapse = ", "), ")")
    pntPchs <- as.integer(eval(parse(text = pntPchs)))
    pnt.characters <- data.frame(Group=lev, pntPchs=pntPchs)
    bvMain$plotVars$pntPchs <- join(bvMain$plotData, pnt.characters)[,"pntPchs"]
    
    # Parse Boundary Inputs
    bndOn <- paste0("c(", paste0("input$bvBndShw", lev, collapse = ", "), ")")
    bndOn <- eval(parse(text = bndOn))
    bnd.On <- data.frame(Group=lev, bndOn=bndOn)
    bvMain$plotVars$bndOn <- join(bvMain$plotData, bnd.On)[,"bndOn"]
    
    bndCols <- paste0("c(", paste0("input$bvBndCol", lev, collapse = ", "), ")")
    bndCols <- eval(parse(text = bndCols))
    bnd.colors <- data.frame(Group=lev, bndCols=bndCols)
    bvMain$plotVars$bndCols <- join(bvMain$plotData, bnd.colors)[,"bndCols"]
    
    bndFils <- paste0("c(", paste0("input$bvBndFil", lev, collapse = ", "), ")")
    bndFils <- eval(parse(text = bndFils))
    bnd.fills <- data.frame(Group=lev, bndFils=bndFils)
    bvMain$plotVars$bndFils <- join(bvMain$plotData, bnd.fills)[,"bndFils"]
    
    bndLtys <- paste0("c(", paste0("input$bvBndLty", lev, collapse = ", "), ")")
    bndLtys <- eval(parse(text = bndLtys))
    bnd.linetypes <- data.frame(Group=lev, bndLtys=bndLtys)
    bvMain$plotVars$bndLtys <- join(bvMain$plotData, bnd.linetypes)[,"bndLtys"]
    
    bndSizs <- paste0("c(", paste0("input$bvBndSiz", lev, collapse = ", "), ")")
    bndSizs <- as.numeric(eval(parse(text = bndSizs)))
    bnd.sizes <- data.frame(Group=lev, bndSizs=bndSizs)
    bvMain$plotVars$bndSizs <- join(bvMain$plotData, bnd.sizes)[,"bndSizs"]
    
    bndAngls <- paste0("c(", paste0("input$bvBndAngl", lev, collapse = ", "), ")")
    bndAngls <- as.numeric(eval(parse(text = bndAngls)))
    bnd.angularities <- data.frame(Group=lev, bndAngls=bndAngls)
    bvMain$plotVars$bndAngls <- join(bvMain$plotData, bnd.angularities)[,"bndAngls"]
    
    bndExps <- paste0("c(", paste0("input$bvBndExp", lev, collapse = ", "), ")")
    bndExps <- as.numeric(eval(parse(text = bndExps)))
    bnd.expansions <- data.frame(Group=lev, bndExps=bndExps)
    bvMain$plotVars$bndExps <- join(bvMain$plotData, bnd.expansions)[,"bndExps"]
    
    # Parse Path Inputs
    pthOn <- paste0("c(", paste0("input$bvPthShw", lev, collapse = ", "), ")")
    pthOn <- eval(parse(text = pthOn))
    pth.On <- data.frame(Group=lev, pthOn=pthOn)
    bvMain$plotVars$pthOn <- join(bvMain$plotData, pth.On)[,"pthOn"]
    
    pthCols <- paste0("c(", paste0("input$bvPthCol", lev, collapse = ", "), ")")
    pthCols <- eval(parse(text = pthCols))
    pth.colors <- data.frame(Group=lev, pthCols=pthCols)
    bvMain$plotVars$pthCols <- join(bvMain$plotData, pth.colors)[,"pthCols"]
    
    pthLtys <- paste0("c(", paste0("input$bvPthLty", lev, collapse = ", "), ")")
    pthLtys <- eval(parse(text = pthLtys))
    pth.linetypes <- data.frame(Group=lev, pthLtys=pthLtys)
    bvMain$plotVars$pthLtys <- join(bvMain$plotData, pth.linetypes)[,"pthLtys"]
    
    pthSizs <- paste0("c(", paste0("input$bvPthSiz", lev, collapse = ", "), ")")
    pthSizs <- as.numeric(eval(parse(text = pthSizs)))
    pth.sizes <- data.frame(Group=lev, pthSizs=pthSizs)
    bvMain$plotVars$pthSizs <- join(bvMain$plotData, pth.sizes)[,"pthSizs"]
    
  }
  
})

################ Code For Building The Plot ################
  
# This block of code builds the plot
bvPlot <- reactive({
  
  req(input$bvPlotTitle)
  
  # browser()
  
  # Load Plot data
  dataPlot <- bvMain$plotData

  # Load Polygon data, create label variable and parse polygon color inputs
  plys <- bvMain$disc$plys
  plylbls <- unique(plys$Label)
  plyCols <- paste0("c(", paste0("input$bvPly", c(1:length(plylbls)), collapse = ", "), ")")
  plyCols <- eval(parse(text = plyCols))
  
  #### Base Plot ####
  
  BV <- ggplot(data = dataPlot, aes(X,Y))
  
  # set theme for base plot
  if (input$bvGrid == FALSE) {
    BV <- BV +
      theme_classic()
  } else {
    BV <- BV +
      theme_bw()
  }
  
  # Return blank plot if no data or polygons are loaded
  if (isolate(input$bvDiscrim) == "" & is.null(input$bvX) & is.null(input$bvY)) {
    return(
      BV +
        scale_y_continuous(labels = scaleFUN, breaks = seq(input$bvYmin, input$bvYmax, input$bvYinc)) +
        scale_x_continuous(labels = scaleFUN, breaks = seq(input$bvXmin, input$bvXmax, input$bvXinc)) +
        labs(title = input$bvPlotTitle) +
        xlab(input$bvPlotLabX) +
        ylab(input$bvPlotLabY) +
        theme(plot.title = element_text(size = 20, hjust = 0.5)) +
        #coord_cartesian(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE)
        coord_cartesian(xlim = c(input$bvXmin, input$bvXmax), ylim = c(input$bvYmin, input$bvYmax), expand = FALSE)
      )
  }
  
  #### Add discrimination polygons ####
  
  if (isolate(input$bvDiscrim) != "") {
    
    req(input$bvPly1)
    
    for (i in 1 : max(plys$ID)) {
      BV <- BV +
        geom_polygon(data = plys[which(plys$ID == i),],
                     aes(X, Y),
                     fill = plyCols[i],
                     col = "black",
                     lwd = 1)
    }
    
  }
  
  #### Add Alkaline/Subalkaline line to plot (toggle option) ####
  
  if (input$tasAlkSubalk == TRUE) {
    BV <- BV +
      geom_line(data = data.frame(x = c(39.2, 40, 43.2, 45, 48, 50, 53.7, 55, 60, 65, 77), 
                                  y = c(0, 0.4, 2, 2.8, 4, 4.75, 6, 6.4, 8, 8.8, 10)), 
                aes(x, y), col = "black", lty =1, lwd = 2)
  }
  
  #### Add Points and other Group Components ####
  
  if(!is.null(bvMain$plotData) & !is.null(input$bvX) & !is.null(input$bvY)) {
    data <- cbind(bvMain$plotData, do.call(cbind.data.frame, bvMain$plotVars))
    
    # Add Boundaries
    if (any(data$bndOn) == TRUE) {
      
      bndData <- subset(data, bndOn == TRUE)
      names(bndData)[3] <- "bndGrps"
      
      BV <- BV +
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
      
      BV <- BV +
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
      
      BV <- BV +
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
  
  ##### Add Text and Labels #####
  
  # Add discrimination polygon labels
  if (input$bvDiscrim != "" && input$rxNames == TRUE) {
    
    lbls <- bvMain$disc$lbls
    
    for (i in 1:nrow(lbls)) {
      BV <- BV +
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
  
  # Control label for Alkaline/Subalkaline toggle
  if (input$tasAlkSubalk == TRUE) {
    BV <- BV +
      geom_text(data = data.frame(x = 40, y = 12.5), aes(x, y), label = "Alkaline", color = "red", size = 7) +
      geom_text(data = data.frame(x = 82, y = 5), aes(x, y), label = "Subalkaline", color = "red", size = 7)
  }
  
  # Make final changes and output final plot
  BV +
    scale_y_continuous(trans = input$bvYTrans, labels = scaleFUN, breaks = seq(input$bvYmin, input$bvYmax, input$bvYinc)) +
    scale_x_continuous(trans = input$bvXTrans, labels = scaleFUN, breaks = seq(input$bvXmin, input$bvXmax, input$bvXinc)) +
    labs(title = input$bvPlotTitle) +
    xlab(input$bvPlotLabX) +
    ylab(input$bvPlotLabY) +
    theme(plot.title = element_text(size = 20, hjust = 0.5)) +
    coord_cartesian(xlim = c(input$bvXmin, input$bvXmax), ylim = c(input$bvYmin, input$bvYmax), expand = FALSE)
  
})

################ Code For Rendering The Plot ################

# This block of code renders the main plot to the UI
output$bvPlot <- renderPlot({
  
  bvPlot()
  
})

################ Code For Downloading Plots ################

# This block of code creates a download handler and downloads a plot
output$bvDownloadPlot <- downloadHandler(
  
  filename =  function() {
    if(any(c("ps", "eps", "png", "pdf", "jpeg", "tiff", "bmp", "svg", "wmf")%in%strsplit(input$bvDownloadTxt, "[.]")[[1]][2])) {
      input$bvDownloadTxt
    } else {
      "myPlot.pdf"
    }
  },

  content = function(file) {
    # ga_collect_event(event_category = "App Outputs", event_action = "Plot Download", event_label = "User downloaded BV Plot")
    ggsave(file, plot = bvPlot(), width = 11, height = 8.5, units = 'in')
  }
  
  # filename =  function() {
  #   paste0("MyBivariate.", input$bvDownloadType)
  # },
  # 
  # content = function(file) {
  #   if(input$bvDownloadType == "rds") {
  #     saveRDS(bvPlot(), file)
  #   } else{
  #     ggsave(file, plot = bvPlot(), width = 11, height = 8.5, device = input$bvDownloadType)
  #   }
  # }

)

