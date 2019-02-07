# This block of code recognizes when the user selects an input file
ternFullData <- reactive({
  inFile <- input$ternFile
  
  if(is.null(inFile))
    return(NULL)
  
  ternFoo <- strsplit(inFile$name, "[.]")[[1]][2]
  
  if(ternFoo == "xlsx" | ternFoo == "xls"){
    foo2 <- read_excel(inFile$datapath, 1, na = "NA")
  }
  
  if(ternFoo == "csv"){
    foo2 <- read_csv(inFile$datapath, na = "NA")
  }
  
  foo2

})

# This block of code builds a reactiveValues object to store data
ternMain <- reactiveValues()

# This block of code stores the raw input data into a reactiveValues object as $Raw
observeEvent(input$ternFile, {
  
  ternMain$inputData <- ternFullData()
  
})

# This block of code pre-renders four user input boxes that
# allow the user to select which column is their A, B, C, and grouping variables.
output$ternChoose_A <- renderUI({
  selectizeInput("ternA", "Select A variable/s:", choices = as.list(c("", colnames(ternFullData()))), selected = NULL, multiple = TRUE)
})

output$ternChoose_B <- renderUI({
  selectizeInput("ternB", "Select B variable/s:", choices = as.list(c("", colnames(ternFullData()))), selected = NULL, multiple = TRUE)
})

output$ternChoose_C <- renderUI({
  selectizeInput("ternC", "Select C variable/s:", choices = as.list(c("", colnames(ternFullData()))), selected = NULL, multiple = TRUE)
})

output$ternChoose_G <- renderUI({
  selectizeInput("ternG", "Select grouping variable:", choices = as.list(c("", colnames(ternFullData()))), selected = NULL, multiple = FALSE)
})

# This block of code builds the Ternary data set
observeEvent( c(input$ternA, input$ternB, input$ternC, input$ternG), {
  
  if(!is.null(input$ternA) & !is.null(input$ternB) & !is.null(input$ternC)) {
    names <- colnames(ternMain$inputData)
    data <- ternMain$inputData
    
    A <- apply(data[,which(names %in% input$ternA)], 1, sum, na.rm = TRUE)
    B <- apply(data[,which(names %in% input$ternB)], 1, sum, na.rm = TRUE)
    C <- apply(data[,which(names %in% input$ternC)], 1, sum, na.rm = TRUE)
    
    if(input$ternG != "") {
      Group <- apply(data[,which(names == input$ternG)], 1, factor)
    } else {
      Group <- as.factor(rep(1, length(A)))
    }
    
    data2 <- data.frame(A = A, B = B, C = C, Group = Group)
    data2[which(apply(data2[,1:3],1,function(x){all(x == 0)})),1:3] <- NA
    ternMain$plotData <- data2
  }

})

# This line of code renders the raw data file and sends it the UI as output
output$ternFullData <- renderDataTable({ ternMain$inputData })
output$ternPlotData <- renderDataTable({ ternMain$plotData })

# This block of code pre-renders the controls for each group
output$ternPtControls <- renderUI({
  
  lev <- unique(as.character(ternMain$plotData$Group))
  cols <- gg_fill_hue(length(lev))
  
  lapply(seq_along(lev), function(i) {
    
    bsCollapse(
          bsCollapsePanel(title = paste0("Group: ", lev[i]), style = "primary",
            bsCollapse(open = "Point Controls:",
              bsCollapsePanel("Point Controls:", style = "primary",
                checkboxInput(
                  inputId = paste0("ternPntShw", lev[i]),
                  label = "Show Points", 
                  value = TRUE
                ),
                fluidRow(
                  column(6,
                         colourInput(inputId = paste0("ternPntCol", lev[i]),
                            label = "Color", 
                            value = cols[i],
                            allowTransparent = TRUE),
                         colourInput(inputId = paste0("ternPntFil", lev[i]),
                                     label = "Fill Color", 
                                     value = "gray50",
                                     allowTransparent = TRUE)
                  ),
                  column(6,
                         selectInput(inputId = paste0("ternPntSiz", lev[i]),
                                     label = "Pt. Size",
                                     c(1:7), selected = 4),
                         selectInput(inputId = paste0("ternPntShp", lev[i]),
                                     label = "Pt. Shape",
                                     c(0:25), selected = 19)
                  )
                )
              ),
              bsCollapsePanel("Boundary controls:", style = "primary",
                checkboxInput(inputId = paste0("ternBndShw", lev[i]),
                                label = "Add boundary:",
                                value = FALSE
                ),
                fluidRow(
                  column(6,
                         colourInput(inputId = paste0("ternBndCol", lev[i]),
                                     label = "Color", 
                                     value = cols[i],
                                     allowTransparent = TRUE),
                         colourInput(inputId = paste0("ternBndFil", lev[i]),
                                     label = "Fill Color", 
                                     value = "gray50",
                                     allowTransparent = TRUE),
                         numericInput(inputId = paste0("ternBndAngl", lev[i]),
                                      label = "Angularity",
                                      value = 0.5,
                                      step = 0.1
                                      )
                  ),
                  column(6,
                         selectInput(inputId = paste0("ternBndLty", lev[i]),
                                     label = "Line Type",
                                     c("solid", "dashed", "dotdash", "dotted", "longdash", "twodash"), selected = "solid"),
                         selectInput(inputId = paste0("ternBndSiz", lev[i]),
                                     label = "Line Width",
                                     c(1:6), selected = 3),
                         numericInput(inputId = paste0("ternBndExp", lev[i]),
                                      label = "Expansion",
                                      value = 0.01,
                                      step = 0.01
                                      )
                  )
                )
              ),
              bsCollapsePanel("Path controls:", style = "primary",
                              checkboxInput(inputId = paste0("ternPthShw", lev[i]),
                                            label = "Add path:",
                                            value = FALSE
                              ),
                              fluidRow(
                                column(6,
                                       colourInput(inputId = paste0("ternPthCol", lev[i]),
                                                   label = "Color", 
                                                   value = cols[i],
                                                   allowTransparent = TRUE)
                                ),
                                column(6,
                                       selectInput(inputId = paste0("ternPthLty", lev[i]),
                                                   label = "LineType",
                                                   c("solid", "dashed", "dotdash", "dotted", "longdash", "twodash"), selected = "solid"),
                                       selectInput(inputId = paste0("ternPthSiz", lev[i]),
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

# This block of code pre-renders the color controls for polygons
output$ternPlyPanel <- renderUI({
  File <- paste0("www/plys/Tern/", input$ternDiscrim, ".xlsx")
  lbls <- data.frame(read_excel(File, 2))[,1]
  
  lapply(seq_along(lbls), function(i) {
    div(style="display: inline-block; width: 23%;",
        colourInput(paste0("ternPly", i), lbls[i], "#FFFFFF80", allowTransparent = TRUE)
    )
  })
})

# This block of code creates a reactive object for Ternary plot polygons and labels
observeEvent( input$ternDiscrim, {
  if (input$ternDiscrim != "") {
    File <- paste0("www/plys/Tern/",input$ternDiscrim, ".xlsx")
    plys <- read_excel(File, 1)
    lbls <- read_excel(File, 2)
    txt <- read_excel(File, 3)
    
    ternMain$disc <- list(plys = plys, lbls = lbls, txt = txt)
  }
})

observe({
  
  if(!is.null(ternMain$plotData) & 
     !is.null(eval(parse(text = paste0("c(", paste0("input$ternPntShw", unique(as.character(ternMain$plotData$Group))[1], collapse = ", "), ")"))))
     ) {
    
    lev <- unique(as.character(ternMain$plotData$Group))
    
    pntOn <- paste0("c(", paste0("input$ternPntShw", lev, collapse = ", "), ")")
    pntOn <- eval(parse(text = pntOn))
    pnt.On <- data.frame(Group=lev, pntOn=pntOn) # Parse Point inputs
    ternMain$plotVars$pntOn <- join(ternMain$plotData, pnt.On)[,"pntOn"]
    
    pntCols <- paste0("c(", paste0("input$ternPntCol", lev, collapse = ", "), ")")
    pntCols <- eval(parse(text = pntCols))
    pnt.colors <- data.frame(Group=lev, pntCols=pntCols)
    ternMain$plotVars$pntCols <- join(ternMain$plotData, pnt.colors)[,"pntCols"]
    
    pntFils <- paste0("c(", paste0("input$ternPntFil", lev, collapse = ", "), ")")
    pntFils <- eval(parse(text = pntFils))
    pnt.fills <- data.frame(Group=lev, pntFils=pntFils)
    ternMain$plotVars$pntFils <- join(ternMain$plotData, pnt.fills)[,"pntFils"]
    
    pntSizs <- paste0("c(", paste0("input$ternPntSiz", lev, collapse = ", "), ")")
    pntSizs <- as.numeric(eval(parse(text = pntSizs)))
    pnt.sizes <- data.frame(Group=lev, pntSizs=pntSizs)
    ternMain$plotVars$pntSizs <- join(ternMain$plotData, pnt.sizes)[,"pntSizs"]
    
    pntPchs <- paste0("c(", paste0("input$ternPntShp", lev, collapse = ", "), ")")
    pntPchs <- as.integer(eval(parse(text = pntPchs)))
    pnt.characters <- data.frame(Group=lev, pntPchs=pntPchs)
    ternMain$plotVars$pntPchs <- join(ternMain$plotData, pnt.characters)[,"pntPchs"]

    # Parse Boundary Inputs
    bndOn <- paste0("c(", paste0("input$ternBndShw", lev, collapse = ", "), ")")
    bndOn <- eval(parse(text = bndOn))
    bnd.On <- data.frame(Group=lev, bndOn=bndOn)
    ternMain$plotVars$bndOn <- join(ternMain$plotData, bnd.On)[,"bndOn"]
    
    bndCols <- paste0("c(", paste0("input$ternBndCol", lev, collapse = ", "), ")")
    bndCols <- eval(parse(text = bndCols))
    bnd.colors <- data.frame(Group=lev, bndCols=bndCols)
    ternMain$plotVars$bndCols <- join(ternMain$plotData, bnd.colors)[,"bndCols"]
    
    bndFils <- paste0("c(", paste0("input$ternBndFil", lev, collapse = ", "), ")")
    bndFils <- eval(parse(text = bndFils))
    bnd.fills <- data.frame(Group=lev, bndFils=bndFils)
    ternMain$plotVars$bndFils <- join(ternMain$plotData, bnd.fills)[,"bndFils"]
    
    bndLtys <- paste0("c(", paste0("input$ternBndLty", lev, collapse = ", "), ")")
    bndLtys <- eval(parse(text = bndLtys))
    bnd.linetypes <- data.frame(Group=lev, bndLtys=bndLtys)
    ternMain$plotVars$bndLtys <- join(ternMain$plotData, bnd.linetypes)[,"bndLtys"]
    
    bndSizs <- paste0("c(", paste0("input$ternBndSiz", lev, collapse = ", "), ")")
    bndSizs <- as.numeric(eval(parse(text = bndSizs)))
    bnd.sizes <- data.frame(Group=lev, bndSizs=bndSizs)
    ternMain$plotVars$bndSizs <- join(ternMain$plotData, bnd.sizes)[,"bndSizs"]
    
    bndAngls <- paste0("c(", paste0("input$ternBndAngl", lev, collapse = ", "), ")")
    bndAngls <- as.numeric(eval(parse(text = bndAngls)))
    bnd.angularities <- data.frame(Group=lev, bndAngls=bndAngls)
    ternMain$plotVars$bndAngls <- join(ternMain$plotData, bnd.angularities)[,"bndAngls"]
    
    bndExps <- paste0("c(", paste0("input$ternBndExp", lev, collapse = ", "), ")")
    bndExps <- as.numeric(eval(parse(text = bndExps)))
    bnd.expansions <- data.frame(Group=lev, bndExps=bndExps)
    ternMain$plotVars$bndExps <- join(ternMain$plotData, bnd.expansions)[,"bndExps"]
    
    # Parse Path Inputs
    pthOn <- paste0("c(", paste0("input$ternPthShw", lev, collapse = ", "), ")")
    pthOn <- eval(parse(text = pthOn))
    pth.On <- data.frame(Group=lev, pthOn=pthOn)
    ternMain$plotVars$pthOn <- join(ternMain$plotData, pth.On)[,"pthOn"]
    
    pthCols <- paste0("c(", paste0("input$ternPthCol", lev, collapse = ", "), ")")
    pthCols <- eval(parse(text = pthCols))
    pth.colors <- data.frame(Group=lev, pthCols=pthCols)
    ternMain$plotVars$pthCols <- join(ternMain$plotData, pth.colors)[,"pthCols"]
    
    pthLtys <- paste0("c(", paste0("input$ternPthLty", lev, collapse = ", "), ")")
    pthLtys <- eval(parse(text = pthLtys))
    pth.linetypes <- data.frame(Group=lev, pthLtys=pthLtys)
    ternMain$plotVars$pthLtys <- join(ternMain$plotData, pth.linetypes)[,"pthLtys"]
    
    pthSizs <- paste0("c(", paste0("input$ternPthSiz", lev, collapse = ", "), ")")
    pthSizs <- as.numeric(eval(parse(text = pthSizs)))
    pth.sizes <- data.frame(Group=lev, pthSizs=pthSizs)
    ternMain$plotVars$pthSizs <- join(ternMain$plotData, pth.sizes)[,"pthSizs"]
    
  }
    
})

# This block of code builds the plot
ternPlot <- reactive({
  
  #browser()
  
  #### Add base plot ####
  Tern <- ggtern(A=1, B=1, C=1) +
    theme_bw() +
    theme_nomask()
  
  # Rotate baseplot clockwise or counter clockwise
  if (input$ternClockwise == "Clockwise") {
    Tern <- Tern +
      theme_clockwise()
  }
  
  # Add or remove background grid
  if (input$ternGrid == "Off") {
    Tern <- Tern +
      theme_nogrid()
  }
  
  # Add or remove axis arrows
  if (input$ternAxisArrows == "On") {
    Tern <- Tern +
      theme_showarrows() +
      theme(tern.axis.arrow = element_line(color = "black"))
  }
  
  #### Add discrimination polygons ####
  
  if (input$ternDiscrim != "") {
    plys <- ternMain$disc$plys
    
    req(input$ternPly1)
    
    plycols <- paste0("c(", paste0("input$ternPly", c(1:nrow(ternMain$disc$lbls)), collapse = ", "), ")")
    plycols <- eval(parse(text = plycols))
    
    for (i in 1: max(plys$ID)) {
      Tern <- Tern +
        geom_polygon(data = plys[which(plys$ID == i),],
                     aes(B, A, C),
                     fill = plycols[i],
                     col = "black",
                     lwd = 1)
    }
    
  }

  #### Add Points and other Group Components ####
  
  if(!is.null(ternMain$plotData)) {
    data <- cbind(ternMain$plotData, do.call(cbind.data.frame, ternMain$plotVars))
    
    # Add Boundaries
    if (any(data$bndOn) == TRUE) {
      
      bndData <- subset(data, bndOn == TRUE)
      names(bndData)[4] <- "bndGrps"

        Tern <- Tern +
          geom_encircle(data = bndData,
                        aes(B, A, C, fill = bndGrps),
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
      names(pthData)[4] <- "pthGrps"
      
      Tern <- Tern +
        geom_path(data = pthData,
                  aes(B, A, C, linetype = pthGrps),
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
      names(pntData)[4] <- "pntGrps"
      
      Tern <- Tern +
        geom_point(data = pntData,
                   aes(B, A, C, col = pntGrps),
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
  if (input$ternDiscrim != "") {
    lbls <- ternMain$disc$lbls  
    
    Tern <- Tern +
      #geom_mask() +
      geom_text(data = lbls,
                aes(B, A, C, label = as.character(Label), angle = Angle, hjust = Hjust, vjust = Vjust, size = Size),
                color = 'black', size = 3)
  }
  
  Tern +
    labs(title = input$ternTitle, 
         x = input$ternPlotLabB,
         y = input$ternPlotLabA,
         z = input$ternPlotLabC) +
    Tarrowlab(input$ternPlotArrowA) +
    Larrowlab(input$ternPlotArrowB) +
    Rarrowlab(input$ternPlotArrowC) +
    theme(plot.title = element_text(size = 20, hjust = 0.5))
})

# This block of code turns off all groups on the main plot
observeEvent(input$ternHideAll, {
  
  grpPts <- paste0("ternPntShw", unique(as.character(ternMain$plotData$Group)))

  for (i in 1:length(grpPts)) {

    updateCheckboxInput(session, grpPts[i], value = FALSE)
    
  }
  
})

# This block of code turns on all groups on the main plot
observeEvent(input$ternShowAll, {
  
  grpPts <- paste0("ternPntShw", unique(as.character(ternMain$plotData$Group)))
  
  for (i in 1:length(grpPts)) {
    
    updatePrettyCheckbox(session, grpPts[i], value = TRUE)
    
  }
  
})

# This block of code builds the intro plot and plots the intro inputs
ternIntroPlot <- reactive({
  data <- data.frame(A = input$ternIntroA, B = input$ternIntroB, C = input$ternIntroC)
  dataNorm <- as.matrix(data/sum(data))
  
  lineA <- data.frame(A = c(0,(dataNorm[1]/sum(dataNorm[-2])), (dataNorm[1]/sum(dataNorm[-3]))), 
                      B = c((dataNorm[2]/sum(dataNorm[-1])), 0, (dataNorm[2]/sum(dataNorm[-3]))), 
                      C = c((dataNorm[3]/sum(dataNorm[-1])), (dataNorm[3]/sum(dataNorm[-2])), 0),
                      xend = dataNorm[1],
                      yend = dataNorm[2], 
                      zend = dataNorm[3]
  )
  
  ggtern(data = data, aes(B, A, C)) +
    theme_bvbw() +
    theme_nogrid_minor() +
    theme_clockwise() +
    theme_showarrows() +
    theme(tern.axis.arrow = element_line(color = "black")) +
    geom_polygon(data = data.frame(x = c(1,0,0), y = c(0,1,0), z = c(0,0,1)), 
                 aes(x, y, z), fill = "white", alpha = 0.70) +
    
    geom_Tmark(lwd = 1.5, arrow = arrow()) +
    #geom_segment(data = lineA[2,], aes(A, B, C, xend = xend, yend = yend, zend = zend),
    #             color = 'black', size = 1.5, lty = 3) +
    
    geom_Lmark(lwd = 1.5, col = "#d55e00", arrow = arrow()) +
    #geom_segment(data = lineA[1,], aes(A, B, C, xend = xend, yend = yend, zend = zend),
    #             color = '#d55e00', size = 1.5, lty = 3) +
    
    
    geom_Rmark(lwd = 1.5, col = "#0072b2", arrow = arrow()) +
    #geom_segment(data = lineA[3,], aes(A, B, C, xend = xend, yend = yend, zend = zend),
    #             color = '#0072b2', size = 1.5, lty = 3) +
    
    theme_nomask() +
    geom_point(size = 6, pch = 21, fill = "gray70")

})

# This block of code normalizes the intro input data
ternIntroData <- reactive({ 
  data.frame(A = input$ternIntroA, B = input$ternIntroB, C = input$ternIntroC)
  })

# This block of code outputs the sum of the intro data
output$ternIntroSum <- renderText({ paste("Sum =  ", sum(ternIntroData()))})

# This block of code outputs the math for the ternary intro
output$ternIntroMathA <- renderText({
  paste("Normalizes as --> 100*[A / Sum(A,B,C)] --> 100*[", 
        ternIntroData()$A, "/",sum(ternIntroData()), "] = ",
        100 * round(ternIntroData()$A / sum(ternIntroData()), 2 ), "%")
})
output$ternIntroMathB <- renderText({
  paste("Normalizes as --> 100*[B / Sum(A,B,C)] --> 100*[", 
        ternIntroData()$B, " / ",sum( ternIntroData() ), "] = ",
        100 * round(ternIntroData()$B / sum(ternIntroData()), 2 ), "%")
})
output$ternIntroMathC <- renderText({
  paste("Normalizes as --> 100*[C / Sum(A,B,C)] --> 100*[", 
        ternIntroData()$C, " / ",sum( ternIntroData() ), "] = ",
        100 * round(ternIntroData()$C / sum(ternIntroData()), 2 ), "%")
})

# This block of code renders the main plot to the UI
# output$ternPlot <- renderPlot({ ternPlot() })
output$ternPlot <- renderPlot({ 
  print(ternPlot())
  NULL
  },
  height = function() {session$clientData$output_ternPlot_width*.77}
  )

# This block of code renders the intro plot to the UI
output$ternIntroPlot <- renderPlot({ print(ternIntroPlot()) })

# This block of code updates the axis labels and plot title when a discrimination plot is loaded
observeEvent(input$ternDiscrim, {
  
  if (input$ternDiscrim == "") {

    # This will change the value of input$inText, based on x
    updateTextInput(session, "ternTitle", value = "")
    updateTextInput(session, "ternPlotLabA", value = "A")
    updateTextInput(session, "ternPlotLabB", value = "B")
    updateTextInput(session, "ternPlotLabC", value = "C")
    updateTextInput(session, "ternPlotArrowA", value = "A (%)")
    updateTextInput(session, "ternPlotArrowB", value = "B (%)")
    updateTextInput(session, "ternPlotArrowC", value = "C (%)")
  }
  
  if (input$ternDiscrim != "") {

    data <- ternMain$disc$txt
  
    # This will change the value of input$inText, based on x
    updateTextInput(session, "ternTitle", value = as.character(data[1,7]))
    updateTextInput(session, "ternPlotLabA", value = as.character(data[1,1]))
    updateTextInput(session, "ternPlotLabB", value = as.character(data[1,2]))
    updateTextInput(session, "ternPlotLabC", value = as.character(data[1,3]))
    updateTextInput(session, "ternPlotArrowA", value = as.character(data[1,4]))
    updateTextInput(session, "ternPlotArrowB", value = as.character(data[1,5]))
    updateTextInput(session, "ternPlotArrowC", value = as.character(data[1,6]))
  }

})

# This block of code creates a download handler and downloads a plot
output$ternDownloadPlot <- downloadHandler(
  
  filename =  function() {
    if(any(c("ps", "eps", "png", "pdf", "jpeg", "tiff", "bmp", "svg", "wmf")%in%strsplit(input$ternDownloadTxt, "[.]")[[1]][2])) {
      input$ternDownloadTxt
    } else {
      "myPlot.pdf"
    }
  },
  
  content = function(file) {
    # ga_collect_event(event_category = "App Outputs", event_action = "Plot Download", event_label = "User downloaded Tern Plot")
    ggsave(file, plot = ternPlot(), width = 11, height = 8.5, units = 'in')
  }
  
  # filename =  function() {
  #   paste("MyTernary", input$ternDownloadType, sep=".")
  # },
  # 
  # content = function(file) {
  #   ggsave(file, plot = ternPlot(), width = 10, height = 10, device = input$ternDownloadType)
  # }
)