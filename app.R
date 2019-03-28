# Required packages
# Shiny packages
require(shiny)
require(shinyWidgets)
require(shinyBS)
require(shinycssloaders)
require(shinyjs)

#Data manipulation
require(plyr)
require(glue)

#Latex support
require(latex2exp)

#Data loading
require(readxl)
require(readr)

#Plotting
require(ggplot2)
require(ggtern)
require(ggalt)

# Load this package last!
require(colourpicker)

# Load other functions
source("functions.R", local = TRUE)

#### User Interface (UI) ####
ui <- fluidPage(
  
  useShinyjs(),  # Set up shinyjs
  
  # Custom IUPUI CSS theme
  theme = "IUPUI.css",

  tags$head(includeHTML(("google-analytics.html"))),
  
  tags$head(tags$script(src = "scripts2.js")),
  
  # This code is used to build what shows in the users browser tab when the app is loaded
  list(tags$head(HTML('<link rel="icon", href="Figs/trident_large.png",
                      type="image/png" />'))),
  div(style="padding: 0px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="RockR! v.2.45" # Change name and version here!
      )
  ),

  tags$head(
    #tags$meta(name = "description", content = "..."),
    tags$meta(name = "url", content = "http://apps.earthsciences.iupui.edu:3838/RockR"),
    tags$meta(name = "keywords", content = "rockr, ternary, discrimination diagram, bivariate, metamorphic facies, plotting tool, geoscience, petrology, geology"),
    tags$meta(name = "title", content = "RockR!"),
    tags$meta(name = "robots", content = "index, follow"),
    tags$meta(name = "image", content = "Figs/RockR.png")
  ),
  
  div(class = "icon-bar",
      #github icon
      a(href = "https://github.com/RockRwebapp/RockR",
        class = "fa fa-github",
        target = "_blank",
        style = "display: inline-block; vertical-align: middle",
        onclick = "gtag('event', 'github_icon', {
        'event_category': 'link click',
        'event_label': 'user clicked github icon link'
        })"
      ),
      # Create url with the 'facebook-share-button' class
      a(href = "https://www.facebook.com/RockRwebapp/?eid=ARAR3piAw1ZiSVQWLZzVOEJa47Zn11XKpC8WY8s0Izli2wTlSi7mFzaOVdEIAg1y8UJYtcJ_XKD2jIs",
        class = "fa fa-facebook",
        target = "_blank",
        style = "display: inline-block; vertical-align: middle",
        onclick = "gtag('event', 'facebook_icon', {
          'event_category': 'link click',
          'event_label': 'user clicked facebook icon link'
          })"
      ),
      # Copy the script from https://dev.twitter.com/web/javascript/loading into your app
      # You can source it from the URL below. It must go after you've created your link
      a(href = "https://twitter.com/RRwebapp",
        class = "fa fa-twitter",
        target = "_blank",
        style = "display: inline-block; vertical-align: middle",
        onclick = "gtag('event', 'twitter_icon', {
          'event_category': 'link click',
          'event_label': 'user clicked twitter icon link'
          })"
      ),
      a(href="https://twitter.com/intent/tweet?text=Visit%20RockR!&url=http://apps.earthsciences.iupui.edu:3838/RockR/&via=RRwebapp",
        "Tweet",
        class="twitter-share-button",
        style = "display: inline-block; vertical-align: middle"
      ),

      includeScript("widgets.js")

  ),
  
  navbarPage( id = "tabs",

    # The title section below is where you can alter the navbar title and logo
    title = 
      a(id = "earthSciTitle",
        fluidRow(
          column(2,
               img(src="Figs/trident_large.png", height = 40, width = 40)
          ),
          column(10,
              p("IUPUI Earth Sciences Apps")
          )
        ),
      id = "linkIUPUI",
      target = "_blank",
      href = "https://earthsciences.iupui.edu/",
      style = "color: white; font-size: 20px; font-weight: bold",
      onclick = "gtag('event', 'IUPUIEarthSciences', {
          'event_category': 'link click',
          'event_label': 'user clicked IUPUI Earth Sciences link'
          })"
      ),

    # UI code for App Info tab
    tabPanel (id = "home", "RockR! Home", fluid = TRUE,# Change app title here!
               
               # this block of code supresses all error output in the final app
               # during development comment this out!!!!
               tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"
               ),
              
              uiOutput("appLanding")
    ),
    
    # UI code for Bivariate tab
    tabPanel(id = "bv", "Create Bivariate", fluid = TRUE,
               
               # this block of code supresses all error output in the final app
               # during development comment this out!!!!
               tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"
               ),
               
               source("uiBV.R", local = TRUE)$value
               
    ),
    
    # UI code for Ternary tab
    tabPanel(id = "tern", "Create Ternary", fluid = TRUE,
               
               # this block of code supresses all error output in the final app
               # during development comment this out!!!!
               tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"
               ),
               
               source("uiTernary.R", local = TRUE)$value
               
    ),
    
    # UI code for Mm Facies tab
    tabPanel(id = "pt", "Create Mm Facies", fluid = TRUE,
               
               # this block of code supresses all error output in the final app
               # during development comment this out!!!!
               tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"
               ),
               
               source("uiPT.R", local = TRUE)$value
               
    ),
    
    tabPanel(id = "resources", "Resources", fluid = TRUE,
             
             hr(style="border-color: black;"),
             h2("Geologic/Geochemical Data Portals", align = "center"),
             hr(style="border-color: black;"),
             
             fluidRow(
               column(2,
                      div(height = 300, width = 300)
               ),
               column(4,
                      div(title = "EarthChem",
                        a(
                          img(src = "Figs/EarthChem.png", height = '300px', width = '300px'),
                          target = "_blank",
                          href = "https://www.earthchem.org/data/access"
                          )
                      )
               ),
               column(4,
                      div(title = "USGS Data Catalog",
                        a(
                          img(src = "Figs/USGS.png", height = '300px', width = '300px'),
                          target = "_blank",
                          href = "https://data.usgs.gov/datacatalog/"
                          )
                        )
               ),
               column(2,
                      div(height = 300, width = 300)
               ),
               align = "center"
             ),
             
             hr(style="border-color: black;"),
             h2("Mineralogy and Petrology Databases", align = "center"),
             hr(style="border-color: black;"),
             
             fluidRow(
               column(2,
                      div(height = 300, width = 300)
               ),
               column(4,
                      div(title = "Mineral Data",
                        a(img(src = "Figs/MinDat.png", height = '300px', width = '300px'),
                          target = "_blank",
                          href = "https://www.mindat.org/"
                        )
                      )
               ),
               column(4,
                      div(title = "Web Mineral",
                        a(img(src = "Figs/WebMineral.png", height = '300px', width = '300px'),
                          target = "_blank",
                          href = "http://www.webmineral.com/"
                        )
                      )
               ),
               column(2,
                      div(height = 300, width = 300)
               ),
               align = "center"
             ),
             
             hr(style="border-color: black;"),
             h2("Geologic/Geochemical Societies", align = "center"),
             hr(style="border-color: black;"),
             
             fluidRow(
               column(4,
                      div(title = "Geological Society of America",
                        a(img(src = "Figs/gsa.png", height = '300px', width = '300px'),
                          target = "_blank",
                          href = "https://www.geosociety.org/"
                        )
                      )
               ),
               column(4,
                      div(title = "American Geoscientific Institute",
                          a(img(src = "Figs/agi.png", height = '300px', width = '300px'),
                            target = "_blank",
                            href = "https://www.americangeosciences.org/"
                          )
                      )
               ),
               column(4,
                      div(title = "American Geophysical Union",
                        a(img(src = "Figs/agu.png", height = '300px', width = '300px'),
                          target = "_blank",
                          href = "https://sites.agu.org/"
                        )
                      )
               ),
               align = "center"
             ),
             br(),
             br(),
             fluidRow(
               column(2,
                      div(height = 300, width = 300)
               ),
               column(4,
                      div(title = "American Institute of Professional Geologists",
                        a(img(src = "Figs/aipg.png", height = '300px', width = '300px'),
                          target = "_blank",
                          href = "http://www.aipg.org/"
                        )
                      )
               ),
               column(4,
                      div(title = "Mineral Society of America",
                        a(img(src = "Figs/minsocam.png", height = '300px', width = '300px'),
                          target = "_blank",
                          href = "http://www.minsocam.org/"
                        )
                      )
               ),
               column(2,
                      div(height = 300, width = 300)
               ),
               align = "center"
             ),
             
             hr(style="border-color: black;"),
             h2("Useful Resources", align = "center"),
             hr(style="border-color: black;"),
             
             fluidRow(
               column(2,
                      div(height = 300, width = 300)
               ),
               column(4,
                      div(title = "Interactive Periodic Table",
                        a(img(src = "Figs/PTable.png", height = '300px', width = '300px'),
                          target = "_blank",
                          href = "https://www.ptable.com/#"
                        )
                      )
               ),
               column(4,
                      div(title = "Geo Plotters",
                        a(img(src = "Figs/GeoPlotters.png", height = '300px', width = '300px'),
                          target = "_blank",
                          href = "https://www.geoplotters.com/"
                        )
                      )
               ),
               column(2,
                      div(height = 300, width = 300)
               ),
               align = "center"
             ),

             hr(style="border-color: black;"),
             h2("Open Access Publications", align = "center"),
             p("These individual publication links are all available via the ", 
               a(href = "http://www.minsocam.org/msa/openaccess_publications/", 
                 "Mineral Society of America Open Access Publications List.",
                 target = "_blank"), 
               align = "center"),
             hr(style="border-color: black;"),
             
             fluidRow(
               column(3,
                      div(title = "Guide to Thin Section Microscopy",
                          a(img(src = "Figs/ThinGuide.png", width = '300px'),
                            target = "_blank",
                            href = "Info/Thin_Sctn_Mcrscpy_2_rdcd_eng.pdf"
                          )
                      )
               ),
               column(3,
                      div(title = "Quartz: A Bullseye on Optical Activity",
                          a(img(src = "Figs/Quartz_Bullseye_on_Optical_Activity.png", width = '300px'),
                            target = "_blank",
                            href = "Info/Quartz_Bullseye_on_Optical_Activity.pdf"
                          )
                      )
               ),
               column(3,
                      div(title = "Double Trouble: Navigating Birefringence",
                          a(img(src = "Figs/Double_Trouble_Navigating_Birefringence.png", width = '300px'),
                            target = "_blank",
                            href = "Info/Double_Trouble_Navigating_Birefringence.pdf"
                          )
                      )
               ),
               column(3,
                      div(title = "Metasomatism and Metasomatic Rocks",
                          a(img(src = "Figs/Metasomatism.png", width = '300px'),
                            target = "_blank",
                            href = "Info/Metasomatism.pdf"
                          )
                      )
               ),
               align = "center"
             ),
             br(),
             br()
             
    ),
    
    # UI code for Help tab
    tabPanel(id = "help", "Help", fluid = TRUE,# Change app title here!
           
           # this block of code supresses all error output in the final app
           # during development comment this out!!!!
           tags$style(type="text/css",
                      ".shiny-output-error { visibility: hidden; }",
                      ".shiny-output-error:before { visibility: hidden; }"
           ),
      
           tabsetPanel(
             tabPanel("Inputting Data",
                      h4("Download Sample Data Sets"),
                      fluidRow(
                        column(4,
                               h5("Bivariate Sample Data: Andes Oxide Data"),
                               downloadButton('baseDownloadBVData', "Get Bivariate Data")
                        ),
                        column(4,
                               h5("Ternary Sample Data: QFL Data"),
                               downloadButton('baseDownloadTernData', "Get Ternary Data")
                        ),
                        column(4,
                               h5("Mm Facies Sample Data: "),
                               downloadButton('baseDownloadPTData', "Get Mm Facies Data")
                        )
                      ),
                      includeHTML("www/Info/dataInput.html")
                      ),
             tabPanel("Building Plots",
                      includeHTML("www/Info/buildPlot.html")
                      ),
             tabPanel("RockR! offline",
                      
                      h3("Want to use RockR! offline? Want to contribute to RockR!?"),
                      
                      h4("RockR! was designed as a web app. However, because it is truly free
                         and open source, you can download the full program below and run it locally.
                          To run RockR! locally, unzip the RockR! directory, and open the app.R file in Rstudio. 
                         Within Rstudio we recommend you click the BLACK DOWN ARROW next to 'Run App' at
                         the top of the app.R script page and set it to 'Run External', which will run RockR!
                         locally within your default web browser of choice. Next, make sure you have installed
                         all of the required packages to run RockR! (you can find them listed at the top of app.R). 
                         Last just click the 'Run App' to run RockR! locally. When running RockR! locally, the app
                         will never timeout, as it does when run on a server as a web app, and you can run it
                         anywhere at anytime, even in the field! Have fun and rock on!"),
                      
                      h4("Because RockR is an ongoing project. We update the app frequently as we discover bugs that need
                         addressing or when we wish to add functionality. However, you can download current version of RockR using the Github button below. It's open source so feel free to modify/share the program as you see fit following the ",
                         a("GNU GPL V3.0 license", href = "https://github.com/RockRwebapp/RockR/blob/master/LICENSE", target = "_blank"), "."),
                      
                      br(),
                      #downloadButton('baseDownloadRockR', "Get RockR!")
                      #github icon
                      a(p("@RockRwebapp"),
                        href = "https://github.com/RockRwebapp/RockR",
                        class = "fa fa-github",
                        target = "_blank",
                        style = "display: inline-block; vertical-align: middle; font-size: 48px",
                        onclick = "gtag('event', 'github_icon', {
                        'event_category': 'link click',
                        'event_label': 'user clicked github icon link'
                        })")
                      
                      )
           )
          ),
    tabPanel(title = ""),
    tabPanel(title = ""),
    tabPanel(title = ""),
    tabPanel(title = ""),
    tabPanel(title = ""),
    tabPanel(title = ""),
    tabPanel(title = "")
  )
)

#### End User Interface (UI) ####

##### Server ####
server <- function(input, output, session){
  
  onclick(
    id = "tabs",
    shinyjs::runjs("window.scrollTo(0,0)")
  )
  
  output$appLanding <- renderUI({
    div(
      div(class="slideshow-container", style= glue('background-image: url("Figs/LandingImages/RockRLanding{round(runif(1,1,6),0)}.png");
                     background-size: 100% 100%; vertical-align:top; text-align:center; min-height: calc(100vh - 100px);
                     padding: 0px 0px; opacity: 1;'),
          div(
            img(src = "Figs/RockRHexBig.png", width = "30%")
          ),
          br(),
          h1("Welcome! Let's make a ", style = 'font-weight: bold; display:table; background-color: #ffffff;
                        opacity: .9; 0px solid black; padding: 15px; margin: 0 auto;',
             span(id = "verb-and-cursor",
                  span(id = "verb", style = 'color: #990000; font-size: 40px; font-weight: bold;'),
                  span(id = "cursor", "|", style = 'color: #01426A; font-size: 40px; font-weight: bold;')
             ),
             "plot."
          ),
          br(),
          span(style = 'background-color: #ffffff; border-radius: 25px; opacity: .75; padding: 5px;',
               "Scroll to learn more")
      ),
      
      br(),
      
      tabsetPanel(
        tabPanel("Introduction",
                 includeHTML("www/Info/RockRInfo.html")
        ),
        tabPanel("Available Plots",
                 includeHTML("www/Info/allTables.html")
        ),
        tabPanel("Credits",
                 includeHTML("www/Info/RockRCredits.html")
        )
      )
  )
  })

  # # load Ternary section server code
  source("serverBV.R", local = TRUE)$value
  
  # load Ternary section server code
  source("serverTernary.R", local = TRUE)$value
  
  # load PT section server code
  source("serverPT.R", local = TRUE)$value
  
  # output$baseDownloadBuild <- downloadHandler(
  #   filename <- function() {
  #     "BuildDiscrim.html"
  #   },
  # 
  #   content <- function(file) {
  #     file.copy("Info/Downloads/BuildDiscrim.ppt", file)
  #   },
  #   contentType = "application/zip"
  # )
  
  output$baseDownloadRockR <- downloadHandler(
    filename <- function() {
      "RockR.zip"
    },
    
    content <- function(file) {
      file.copy("www/Downloads/RockR.zip", file)
    },
    contentType = "application/zip"
  )
  
  output$baseDownloadBVData <- downloadHandler(
    filename <- function() {
      "Andes_Example.xlsx"
    },
    
    content <- function(file) {
      file.copy("www/Downloads/ExampleData/Andes.xlsx", file)
    }
  )
  
  output$baseDownloadTernData <- downloadHandler(
    filename <- function() {
      "QFL_Example.xlsx"
    },
    
    content <- function(file) {
      file.copy("www/Downloads/ExampleData/qfl1.xlsx", file)
    }
  )
  
  output$baseDownloadPTData <- downloadHandler(
    filename <- function() {
      "PT_Example.xlsx"
    },
    
    content <- function(file) {
      file.copy("www/Downloads/ExampleData/PTsample.xlsx", file)
    }
  )
  
} 
#### End Server ####

# Initiate app by calling this function
shinyApp(ui=ui, server=server)
