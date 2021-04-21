## Attach packages
library(shiny)
library(caladaptr)
library(ggplot2)
library(shinyhelper)
library(leaflet)      ## used a lot in app.R
library(dplyr)
library(shinyjs)      ## must use library; requirenamespace won't cut it

## The following is an attempt to 'trick' the Shiny app publisher to 
## make sure the following packages are installed on the server
## They are not actually needed right here
## This works I should add scales and DT here also.

#######################################################################
## Check for required namespaces

## Instead of using library(), the following packages are verified as available
## but not attached. I do this as general best practice to minimize name clashes, 
## in three cases:

## 1) I am only using one or two functions from the package and will invoke them
## explicitly (e.g., scales::percent).

## 2) In the case of DT, I choose to explicitly call those functions
## because there are identical functions in shiny which I explicitly don't want.

## 3) The functions are not needed here but needed in report.Rmd

requireNamespace("DT")
requireNamespace("lubridate")    ## only 1-2 functions used in app.R
requireNamespace("htmltools")    ## basically all of the functions needed in app.R are re-exported from Shiny. Need for Rmd
requireNamespace("conflicted")   ## one function used in app.R (also Rmd)
requireNamespace("units")        ## only one function used in app.R
requireNamespace("stringr")      ## use 2 functions in app.R (str_wrap and str_split)
requireNamespace("rmarkdown")    ## used to launch report.Rmd
requireNamespace("kableExtra")   ## used in report.Rmd
requireNamespace("tidyr")        ## pivot_* functions used here and report.Rmd

## requireNamespace("scales")   No longer needed - I format columns as percent using DT or manually with paste0()
## requireNamespace("chillR")   chillR is problematic to install on ShinyApps.io 
##                              Specifically, the dependent package RMAWGEN has 3 dependencies that
##                              aren't picked up by rsconnect.


## The following three packages are *not* needed, but are dependencies of
## RMAWGEN. For some reason however the script which deploys this app
## to ShinyApps.io doesn't automatically install these dependencies,
## so to get the app to deploy I have to pretend like this script needs them.
# MEMORY HIT: THESE THREE NAMESPACES CHEW 10 MB
# UPDATE: NO LONGER NEEDED B/C I NO LONGER LOAD CHILLR DIRECTLY
# requireNamespace("date")
# requireNamespace("vars")
# requireNamespace("chron")


## Set up conflict resolution
conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("count", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("select", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("show", "shinyjs", quiet = TRUE)

## Utility function to add an additional class to the outer-most div of a shiny-tag
shinytag_add_class <- function(x, additional_class) {
  if (!inherits(x, "shiny.tag")) stop("x should be of class shiny.tag")
  x$attribs$class <- paste(x$attribs$class, additional_class)
  x
}

## Utility function to modify a Shiny tag with 2 children, wrapping the 2nd child in a DIV with a style attribute.
## If style = "display:inline-block;", this will have the effect of making the label inline.
shinytag_wrapchild2div <- function(x, style) {
  if (length(x$children) != 2) stop("This function is designed to modify a shiny.tag with two children")
  x$children[[2]] <- div(x$children[[2]], style = style)
  x
}

# define js function for opening urls in new tab/window
# js_code <- "shinyjs.browseMe = function(url) {
#   window.open(url,'_blank');
# }"

## Utility function to print memory usage to the console
# requireNamespace("pryr")
# report_memory <- function(x) {
#   cat("\n", x, "\n", sep = "")
#   cat(" - total memory used: ", round(as.numeric(pryr::mem_used()) / 1000000), " MB\n", sep = "")
#   cat(" - objects in memory: ", paste(ls(), collapse = ", "), "\n", sep = "")
#   cat(" - memory used by objects is: ", as.numeric(pryr::object_size(ls())) / 1000000, " MB\n", sep = "")
# }

## Load a custom version of chillr::make_hourly_temps
source('ca_make_hourly_temps.R')
source('chillr_daylength.R')
source('chillr_dynamic_model.R')

gtag_fn <- "gtag_chill2.js"

## report_memory("Memory usages after packages are loaded")

ui <- function(request) {

  fluidPage(
    ## Only load the Google Analytics tracking tag if the app is running on ShinyApps.io
    if (file.exists(gtag_fn) && Sys.getenv('SHINY_PORT') != "") {
      tags$head(includeHTML(gtag_fn))
    },
    
    # set up shiny js to be able to call our browseMe function
    useShinyjs(),
    #extendShinyjs(text = js_code, functions = 'browseMe'),

    tags$head(tags$style(type="text/css", 
        "p.step {color:black; font-weight:bold; font-size:120%; padding-top:5px;}
        p.topborder {border-top:3px solid lightgray;}
        p.desc {font-style:italic; font-size:90%;}
        h1.report {font-weight:bold; font-size:16px; padding-top:10px; border-top:1px solid lightgray;}
        h2 {font-weight:bold;}
        h3 {font-weight:bold;}
        .leaflet-container {cursor: pointer !important;}
        .iblock {display: inline-block;}   /* inline block for side-by-side UI elements  */
        div.error-msg {color:red; margin:1em 0;}
        div.happy-msg {color:green; margin:1em 0;}
        div#shiny-notification-panel {right:0; left:0; margin:0 auto;}
        .space_above_below {margin-top:0.5em; margin-bottom:0.5em;}
        .indented2 {margin-left:2em;}")
    ),

    ## We put this tag in the body of the HTML document, rather than the head, so it doesn't
    ## get over-written by shinyhelper.css. This is needed to move the shinyhelp info buttons
    ## closer to the label rather than the far right edge of the column
    tags$style(type="text/css",
               ".shinyhelper-container {display: inline-block; position: relative; margin-left: 1em;}
               div.dt-buttons {margin-top:5px; float:right;}"),
    
    titlePanel(title = "Projected Chill Portions Under Climate Change Calculator 2.0",
      windowTitle = "Chill Portions Calculator"),
  
    fluidRow(
      column(12,
             tags$p(),
             tags$div(tags$a(
               href = "https://cran.r-project.org/package=chillR", 
               tags$img(src="chillr_hex_75x65.png", height = "75px"),
               target = "_blank",
               rel = "noopener"), 
               style = "margin-top:10px; float:right; padding-left:10px; width:65px;"),
             
             tags$div(
               tags$a(href = "https://ucanr-igis.github.io/caladaptr/",
                 tags$img(src="caladaptr_hex_173x200.gif", 
                     height = "75px"),
                 target = "_blank",
                 rel = "noopener"), 
               style = "margin-top:10px; float:right; width:65px;"),
             
             tags$p("Introduction", class = "step topborder"),
             HTML("<p>This calculator can be used to compute end-of-season <a href='http://fruitsandnuts.ucdavis.edu/Weather_Services/chilling_accumulation_models/about_chilling_units/' target='_blank' rel='noopener'>chill portions</a> under projected climate scenarios. The calculator draws upon downscale projected climate data from the California 4th Climate Change assessment hosted on <a href='https://cal-adapt.org/' target='_blank' rel='noopener'>Cal-Adapt.org</a>.</p>"),
             HTML("<details>
                  <summary style='color:#337ab7; cursor:pointer; outline:none;'>Click here for more info.</summary>
                  <ul>
                  
                  <li>This is a pilot app for demonstration purposes only. A <a href='https://youtu.be/5TYs3dbUU7A' rel='noopener' target='_blank'>YouTube demo</a> is available.</li>

                  <li>This calculator uses downscaled climate data from Cal-Adapt, which is available for California, Nevada, and a <a href='https://ucanr-igis.github.io/caladaptr-res/workshops/caladaptr_intro_dec20/slides_files/figure-slidy/unnamed-chunk-6-1.png' target='_blank'>little bit of neighboring states</a> in the western USA.</li>
                  
                  <li>This calculator uses chill <i>portions</i> rather than chill <i>hours</i>, because chill portions do a better job at predicting tree phenology. <a href='http://fruitsandnuts.ucdavis.edu/Weather_Services/chilling_accumulation_models/about_chilling_units/' target='_blank' rel='noopener'>More info</a>.</li>
                  
                  <li>RStudio users can run this and other demo Shiny apps directly from RStudio 
                  using the <a href=\"https://github.com/ucanr-igis/caladaptr.apps\" target=\"_blank\" rel=\"noopener\">caladaptr.apps</a> package.</li> 
                  
                  <li>To compute projected chill portions using a script, see this <a href='https://ucanr-igis.github.io/caladaptr-res/notebooks/chill.nb.html' target='_blank' rel='noopener'>
                  R Notebook</a>.</li>
                  
                  <li>If the calculator unexpectedly disconnects while processing, the most likely reason is an out-of-memory error on the server. Refresh the page, reduce the number of GCMs or years, and try again. Or you can run the app locally from RStudio (recommended).</li>
                  <li>Questions or suggestions? Please email for <a href='mailto:caladaptr@gmail.com?subject=Chill Portions Shiny App'>feedback and support</a>.</li>
                  </ul>
                  </details>")
      )
    ),
    
    fluidRow(
      column(12,
             tags$br(),
             tags$p("1. Select location(s)", class = "step topborder"))
    ),
    
    fluidRow(
      column(8,
             tabsetPanel(
               tabPanel("From Map", 
                        tags$p("Select a location on the map, give it a name, and click 'Add to location list'. Repeat as needed.") %>% 
                          shinytag_add_class("space_above_below"),
                        div(leafletOutput("mymap"),
                            style = "max-width:600px; margin-bottom:1em;"),
                        textInput("txtin_ptname_map", "Location name (optional): ", width = "680px", placeholder = "My Farm") %>% 
                          shinytag_wrapchild2div("display:inline-block;") %>% 
                          shinytag_add_class("space_above_below"),
                        actionButton("cmd_addloc_map", "Add to location list") %>% 
                          shinytag_add_class("space_above_below") %>% 
                          shinytag_add_class("indented2"),
                        htmlOutput("htmlout_addmap2lst_msg") %>% 
                          shinytag_add_class("error-msg")
                        ),

               tabPanel("Enter Coordinates", 
                        tags$p("Enter the longitude and latitude coordinates in decimal degrees separated by a comma, then click 'Add to locations'. Repeat if needed.") %>% 
                          shinytag_add_class("space_above_below"),
                        textInput("txtin_coords", label = "Coordinates: ", placeholder = "-120.425, 37.365") %>% 
                          shinytag_wrapchild2div("display:inline-block;") %>% 
                          shinytag_add_class("space_above_below") %>% 
                          shinytag_add_class("iblock"),
                        textInput("txtin_ptname_coords", "Location name (optional): ", width = "680px", placeholder = "My Farm") %>% 
                          shinytag_wrapchild2div("display:inline-block;") %>% 
                          shinytag_add_class("space_above_below"),
                        actionButton("cmd_addloc_coords", "Add to location list") %>% 
                          shinytag_add_class("space_above_below"),
                        htmlOutput("htmlout_coords_not_dd_msg") %>% 
                          shinytag_add_class("error-msg")
                        ),
               
               tabPanel("Upload CSV", 
                        tags$p("Upload a CSV file below. Or you can load some ",
                               actionLink("cmd_sample_data", label = "sample data.")) %>% 
                          shinytag_add_class("space_above_below"),
                        fileInput("infile_csv", "CSV File", multiple = FALSE) %>%
                          shinytag_add_class("iblock") %>%
                          helper(type = "markdown",
                                 icon = "info-circle",
                                 content = "csv_upload",
                                 buttonLabel = "OK",
                                 size = "m")
                        )
             )),
      column(4)
    ),

    fluidRow(
      column(12, 
             tags$p("2. Location List", class = "step topborder")
      )
    ),
    
    fluidRow(
      column(4, 
             DT::dataTableOutput("outtbl_locations"),
             tags$p(actionLink("cmd_clear_locs", "Clear all"),
                    style="text-align:right;"),
             tags$p()
             ),
      column(8)
    ),

    fluidRow(
      column(12,
             tags$br(),
             tags$p("3. Select the climate data to use as the ", tags$u("historic baseline"), class = "step topborder"),
             sliderInput("base_year", "Year range:", min = 1950, max = 2005, value = c(1975, 2005), sep = "", step = 1) %>% 
               shinytag_add_class("iblock") %>% 
               helper(type = "markdown",
                      icon = "info-circle",
                      content = "years_select",
                      buttonLabel = "OK",
                      size = "m"),
             htmlOutput("htmlout_basetime_msg") %>% shinytag_add_class("error-msg"),
             selectInput("base_gcm", label = "GCMs:", choices = gcms[1:10], selected = gcms[1:4], multiple = TRUE) %>% 
               shinytag_add_class("iblock") %>% 
               helper(type = "markdown",
                      icon = "info-circle",
                      content = "base_gcm",
                      buttonLabel = "OK",
                      size = "l"),
             selectInput("base_scenario", label = "Emissions scenario:", choices = scenarios[3], selected = scenarios[3], multiple = FALSE)
      )
    ),
    fluidRow(
      column(12,
             tags$br(),
             tags$p("4. Select the climate data to use for the ", tags$u("projected future"), class = "step topborder"),
             sliderInput("prj_year", "Year range:", min = 2006, max = 2099, value = c(2035, 2065), sep = "", step = 1) %>% 
               shinytag_add_class("iblock") %>% 
               helper(type = "markdown",
                      icon = "info-circle",
                      content = "year_select",
                      buttonLabel = "OK",
                      size = "m"),
             htmlOutput("htmlout_prjtime_msg") %>% shinytag_add_class("error-msg"),
             selectInput("prj_gcm", label = "GCMs:", choices = gcms[1:10], selected = gcms[1:4], multiple = TRUE) %>% 
               shinytag_add_class("iblock") %>% 
               helper(type = "markdown",
                      icon = "info-circle",
                      content = "prj_gcm",
                      buttonLabel = "OK",
                      size = "m"),
             selectInput("prj_scenario", label = "Emissions scenarios:", choices = scenarios[1:2], selected = scenarios[1:2], multiple = TRUE) %>% 
               shinytag_add_class("iblock") %>% 
               helper(type = "markdown",
                      icon = "info-circle",
                      content = "prj_scenario",
                      buttonLabel = "OK",
                      size = "m")
             
      )
    ),
    
    fluidRow(
      column(12,
             tags$br(),
             tags$p("5. Chill analysis options", class = "step topborder"),
             tags$p(tags$strong("Start counting chill portions on the first day of:")),
             div(
               div(selectInput("chill_month", label = NULL, choices = month.abb, selected = "Sep", 
                               multiple = FALSE, width = "90px"),
                   style = "display:inline-block; width:100px; vertical-align:top;")
               ),
             tags$head(tags$style(type="text/css", "#safe_chill, #req_chill {width: 60px}")),
             textInput("safe_chill", label = "Safe chill certainty level (%):", value = 90) %>% 
               shinytag_add_class("iblock") %>% 
               helper(type = "markdown",
                      icon = "info-circle",
                      content = "safe_chill",
                      buttonLabel = "OK",
                      size = "m"),
             textInput("req_chill", label = "Required chill portions:", value = 48) %>% 
               shinytag_add_class("iblock") %>% 
               helper(type = "markdown",
                      icon = "info-circle",
                      content = "req_chill",
                      buttonLabel = "OK",
                      size = "m")
      )
    ),
    
    fluidRow(column(12,
                    tags$p("6. Compute chill", class = "step topborder"),
                    tags$div(
                      checkboxGroupInput("in_chkgrp_rptopts", "Report Options",
                                         choices = c("Location Map" = "loc_map",
                                                     "Chill Distribution Histograms" = "chill_dist_hist",
                                                     "Safe Chill Table" = "safe_chill_tbl",
                                                     "Chill Portions by Month" = "chill_month_plot",
                                                     "Likelihood of Getting Required Chill" = "req_chill_tbl",
                                                     "Site comparison table" = "comp_table"),
                                         selected = c("loc_map", "chill_dist_hist", "safe_chill_tbl", 
                                                      "chill_month_plot", "req_chill_tbl", "comp_table")),
                      style = "margin-left:2em;"
                      
                    ),
                      
                    actionButton("cmd_fetch2", "Fetch Data and Generate Report") %>% 
                      shinytag_add_class("space_above_below"),
                    
                    textOutput("txtout_loc_lst_empty") %>% shinytag_add_class("error-msg"),
                    
                    textOutput("txtout_rpt_coming") %>% shinytag_add_class("happy-msg"),
                    htmlOutput("htmlout_rpt_link"),
                    textOutput("txt_status"),
                    tags$p())
    ),

    includeHTML("igis_footer.html")

  )
}

server <- function(input, output, session) {
  
  ###############################################
  ## Set some bookmarking options
  
  ## Create a reactive val object to store the bookmark URL
  bookmark_url <- reactiveVal()
  
  ## Assign a callback function to update the bookmark_url reactiveVal whenever bookmarking occurs
  onBookmarked(function(url) {
    bookmark_url(url)
  })
  
  ## We don't want to record the state of command buttons (which if they were clicked would automatically
  ## run the observeEvent code on restore)
  setBookmarkExclude(c("cmd_fetch2", "cmd_addloc_map", "cmd_addloc_coords"))
  
  # Manually add a pipe-delimited copy of the locs_tbl() data frame to the bookmark state, so it can be
  # transmitted via the bookmark URL
  onBookmark(function(state) {
    state$values$locations_csv <- paste(apply(locs_tbl(), 1, paste, collapse="|"), 
                                        collapse = "|")
  })
  
  # Read values from state$values when we restore
  onRestore(function(state) {
    
    rebuilt_df <- strsplit(state$values$locations_csv, "\\|")[[1]] %>% 
       matrix(ncol = 3, byrow = TRUE) %>% 
       as.data.frame() %>% 
       dplyr::rename(site = V1, lon = V2, lat = V3) %>% 
       dplyr::mutate(lon = as.numeric(lon), lat = as.numeric(lat))
    
    locs_tbl(rebuilt_df)
  })

  ## Add an observer to watch out for clicks on the shinyhelper buttons.
  ## The md files will be located in the default 'helpfiles' subdir
  observe_helpers()
  
  ## Create a reactiveVal object for all the locations (to be filled with a data frame)
  locs_tbl <- reactiveVal()
  
  # ## Create a (hidden) reactive output that is the condition expression for a conditional panel
  # rpt_coming_show  <- reactiveVal(FALSE)
  # output$rpt_coming_vis <- reactive({
  #   rpt_coming_show()  
  # })
  # outputOptions(output, "rpt_coming_vis", suspendWhenHidden=FALSE)
  
  ## THIS DOESN'T WORK, I THINK BECAUSE IT DOESN'T HAVE AN OBSERVER
  ## outputOptions(output, "txtout_rpt_coming", priority = 10)  

  ## Set the initial map extent (called once but never called again after that)
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      setView(-120.2, 36.4, zoom=6)
  })

  ## The following will run whenever input$mymap_click changes
  observeEvent(input$mymap_click, {
    
    ## Get the coordinates
    click_coords_lst <- input$mymap_click

    ## Clear existing markers and add a marker at the new location
    leafletProxy('mymap') %>% 
      clearMarkers() %>%
      addMarkers(lng = click_coords_lst$lng, 
                 lat = click_coords_lst$lat)

  })
  
  ## The following will run whenever cmd_addloc_map is clicked
  observeEvent(input$cmd_addloc_map, {
    
    ## Get the coordinates
    click_coords_lst <- input$mymap_click
    
    if (is.null(click_coords_lst)) {
      output$htmlout_addmap2lst_msg <- renderUI(HTML("Error: Click a point on the map first"))  
    
    } else {
      output$htmlout_addmap2lst_msg <- renderUI(NULL) 
      
      ## Generate a location name if there isn't one
      if (input$txtin_ptname_map == "") {
        this_site <- paste0("Pt ", ifelse(is.null(locs_tbl()), 0, nrow(locs_tbl())) + 1)
      } else {
        this_site <- input$txtin_ptname_map
      }

      locs_tbl(locs_tbl() %>% 
                 bind_rows(tibble(site = this_site,
                                  lon = round(click_coords_lst$lng, 5),
                                  lat = round(click_coords_lst$lat, 5)))
      )
      
    }

  })
  
  ## The following will run whenever cmd_addloc_coords is clicked
  observeEvent(input$cmd_addloc_coords, {
    
    ## Parse out the coordinates
    mycoords <- stringr::str_split(input$txtin_coords, ",")[[1]] %>%
      trimws() %>%
      as.numeric()
    
    if (NA %in% mycoords) {
      output$htmlout_coords_not_dd_msg <- renderUI(HTML("Error: Please enter coordinates in decmial degrees separated by a comma.<br/>Example:  -120.226, 36.450"))
    
    } else {
      
      ## Clear error messages 
      output$htmlout_coords_not_dd_msg <- renderUI(NULL)
      
      ## TODO: Check if these coordinates are new
      
      #if (!isTRUE(all.equal(pt_coords(), c(mycoords[1], mycoords[2]), tolerance = 0.0001))) {
      
      if (TRUE) {
        
        ## Generate a location name if there isn't one
        if (input$txtin_ptname_coords == "") {
          this_site <- paste0("Pt ", ifelse(is.null(locs_tbl()), 0, nrow(locs_tbl())) + 1)
        } else {
          this_site <- input$txtin_ptname_coords
        }
        
        locs_tbl(locs_tbl() %>% 
                   bind_rows(tibble(site = this_site,
                                    lon = mycoords[1],
                                    lat = mycoords[2])))

      }

    }

    
  })

  ## The following will run whenever infile_csv changes
  observeEvent(input$infile_csv, {
    
    # input$infile_csv will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    ## Update the locs_tbl() reactiveVal object
    locs_tbl(read.csv(input$infile_csv$datapath, header = TRUE))

  })
  
  ## The following will run whenever cmd_sample_data link is clicked
  observeEvent(input$cmd_sample_data, {
    locs_tbl(read.csv('farms.csv', header = TRUE))
  })
  
  ## The following will run whenever cmd_sample_data link is clicked
  observeEvent(input$cmd_clear_locs, {
    locs_tbl(NULL)
  })
  
  ## If location changes or cmd_clear_locs is clicked, delete any messages
  ## below the 'fetch' button
  observeEvent({
    locs_tbl() 
    input$cmd_clear_locs
    1
    }, {
    
    output$txtout_loc_lst_empty <- renderText(NULL)

    ## txtout_rpt_coming and htmlout_rpt_link are managed with shinyjs,
    ## because the render functions don't flush in time
    shinyjs::html(id = "txtout_rpt_coming", html = "")
    shinyjs::html(id = "htmlout_rpt_link", html = "")
    
  })

  ## Update the outtbl_locations whenever locs_tbl() is updated
  output$outtbl_locations <- DT::renderDataTable({
    req(locs_tbl())
    DT::datatable(locs_tbl(),
                  rownames= FALSE, 
                  extensions = 'Buttons',
                  options = list(
                    paging = FALSE,
                    searching = FALSE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    info = FALSE,
                    dom = 'tB',
                    buttons = c('copy', 'csv')
                  ),
                  class = "display")
    
  })

  ## Render an error messages for time periods that are too short - historic period
  output$htmlout_basetime_msg <- renderUI({
    req(input$base_year)
    if (diff(input$base_year) < 10) {
      HTML("Caution: this time period may be too short to be representative of climate. See info button for details.")
    } else {
      NULL
    }
  })
  
  ## Render an error messages for time periods that are too short - projected future
  output$htmlout_prjtime_msg <- renderUI({
    req(input$prj_year)
    if (diff(input$prj_year) < 10) {
      HTML("Caution: this time period may be too short to be representative of climate. See info button for details.")
    } else {
      NULL
    }
  })

  
  ## Fetch data and start the analysis
  observeEvent(input$cmd_fetch2, {
    
    ## Check if a location has been selected
    if (is.null(locs_tbl())) {
      output$txtout_loc_lst_empty <- renderText("Location list empty!")
      
      ## output$txtout_rpt_coming <- renderText(NULL)
      ## output$htmlout_rpt_link <- renderUI(NULL)
      
      shinyjs::html(id = "txtout_rpt_coming", html = "")
      shinyjs::html(id = "htmlout_rpt_link", html = "")
      
      NULL

    } else {
      ## Erase any message that might be in txtout_loc_lst_empty
      output$txtout_loc_lst_empty <- renderText(NULL)
      
      # output$txtout_rpt_coming <- renderText(
      #   "Please wait. When the report is generated, the link will appear below. To keep a copy, be sure to save the HTML file to your computer, or print it."
      # ) 
      
      ## Use shinyjs instead of renderXX() in order for the output to be refreshed immediately
      shinyjs::html(id = "txtout_rpt_coming", 
                    html = "Please wait, the report is being generated. When finished, the link will appear below.<br/>To keep a copy, be sure to save the HTML file to your computer, or print it.")
      
      shinyjs::html(id = "htmlout_rpt_link", html = "")

      ## Record the report options as a character vector
      rpt_opts <- isolate(input$in_chkgrp_rptopts)

      ## Trigger a bookmark update. 
      ## In turn this will trigger the onBookmarked function which saves the
      ## current bookmark URL into the bookmark_url() reactiveVal (which we pass to report.Rmd)
      session$doBookmark()

      ## Prepare some lists to hold objects created in the loop
      site_info_lst <- list()
      progress_lst <- list()
            
      for (i in 1:nrow(locs_tbl())) {
        
        site_name <- locs_tbl()[i, 1, drop = TRUE]
        x_coord <- as.numeric(locs_tbl()[i, 2])
        y_coord <- as.numeric(locs_tbl()[i, 3])
        
        ## Create an API request object for projected climate
        cur_prj_cap2 <- ca_loc_pt(coords = c(x_coord, y_coord)) %>% 
          ca_gcm(input$prj_gcm) %>% 
          ca_scenario(input$prj_scenario) %>% 
          ca_period("day") %>% 
          ca_cvar(c("tasmax", "tasmin")) %>% 
          ca_dates(start = paste(input$prj_year[1] - 1, which(input$chill_month == month.abb), "01", sep = "-"), 
                   end = paste(input$prj_year[2], which(input$chill_month == month.abb) - 1, "30", sep = "-"))
        
        ###########################################
        ## FETCH AND PROCESS PROJECTED DATA
        ###########################################
        
        ## Create a progress object that we can pass to ca_getvals_tbl()
        ## Make sure it closes when we exit even if there's an error
        progress_lst[[i]] <- Progress$new()
        on.exit(progress_lst[[i]]$close())
        
        progress_lst[[i]]$set(value = 0, message = paste0(site_name, ". Fetching projected data: "), 
                    detail = "Daily min and max temp")
        
        ## Get values
        pt_prj_dtemp_tbl2 <- ca_getvals_tbl(cur_prj_cap2, 
                                           quiet = TRUE, 
                                           shiny_progress = progress_lst[[i]]) 

        ## Add Year, Month and Day, temp_c, growing season; pivot tasmin
        progress_lst[[i]]$set(message = paste0(site_name, ". Processing:"), 
                     detail = "Formatting projected data columns")
        
        pt_prj_ymd_gs_wide_tbl2 <- pt_prj_dtemp_tbl2 %>%
          mutate(Year = as.integer(substr(dt, 1, 4)), 
                 Month = as.integer(substr(dt, 6, 7)),
                 Day = as.integer(substr(dt, 9, 10)),
                 temp_c = as.numeric(units::set_units(val, degC))) %>% 
          mutate(gs = case_when(Month < which(input$chill_month == month.abb) ~ Year,
                                Month >= which(input$chill_month == month.abb) ~ as.integer(Year + 1))) %>% 
          filter(!is.na(gs)) %>% 
          select(cvar, temp_c, Year, Month, Day, gcm, scenario, gs) %>%
          tidyr::pivot_wider(names_from = cvar, values_from = temp_c) %>%
          rename(Tmax = tasmax, Tmin = tasmin)
        
        ## recover some memory
        # rm(pt_prj_dtemp_tbl2)
        
        ## Interpolate Hourly Temperatures in 'long' format
        ## This uses 'ca_make_hourly_temps', a modified version of chillr::make_hourly_temps
        progress_lst[[i]]$set(message = paste0(site_name, ". Processing:"), 
                     detail = "Interpolating projected hourly temps")
        
        pt_prj_hrtmp_long2 <- pt_prj_ymd_gs_wide_tbl2 %>% 
          ca_make_hourly_temps(latitude = y_coord) %>% 
          mutate(date_hour = lubridate::make_datetime(Year, Month, Day, Hour, tz = "America/Los_Angeles")) %>% 
          arrange(date_hour)
        
        ## recover some memory
        # rm(pt_prj_ymd_gs_wide_tbl2) 
        
        ## Compute hourly chill portions
        progress_lst[[i]]$set(message = paste0(site_name, ". Processing:"), 
                     detail = "Computing projected hourly chill portions")
        
        ## Save a tibble with chill portions
        chill_portions_prj_tbl2 <- pt_prj_hrtmp_long2 %>% 
          group_by(gs, gcm, scenario) %>% 
          mutate(accum_chill_prtn = Dynamic_Model(Temp))
        
        ## Compute the end-of-season chill for PROJECTED climate for each 
        ## growing season, emissions scenario, and GCM, 
        pt_prj_eos_chill_tbl2 <- chill_portions_prj_tbl2 %>% 
          group_by(scenario, gs, gcm) %>% 
          summarise(max_chill = max(accum_chill_prtn), .groups = 'drop') %>% 
          mutate(time_period = paste0(isolate(input$prj_year[1]), "-", isolate(input$prj_year[2])))
        
        ###########################################
        ## FETCH AND PROCESS HISTORICAL DATA
        ###########################################
        
        cur_base_cap2 <- ca_loc_pt(coords = c(x_coord, y_coord)) %>% 
          ca_gcm(input$base_gcm) %>% 
          ca_scenario(input$base_scenario) %>% 
          ca_period("day") %>% 
          ca_cvar(c("tasmax", "tasmin")) %>% 
          ca_dates(start = paste(input$base_year[1] - 1, which(input$chill_month == month.abb), "01", sep = "-"), 
                   end = paste(input$base_year[2], which(input$chill_month == month.abb) - 1, "30", sep = "-"))
        
        progress_lst[[i]]$set(message = paste0(site_name, ". Fetching historic data: "), 
                     detail = "Daily min and max temp", value = 0)
        
        pt_base_dtemp_tbl2 <- ca_getvals_tbl(cur_base_cap2, 
                                             quiet = TRUE, 
                                             shiny_progress = progress_lst[[i]]) 
        
        progress_lst[[i]]$set(message = paste0(site_name, ". Processing:"), 
                     detail = "Formatting historic data columns")
        
        ## Add Year, Month, Day growing season column and temp_c
        pt_base_ymd_gs_wide_tbl2 <- pt_base_dtemp_tbl2 %>%
          mutate(Year = as.integer(substr(dt, 1, 4)), 
                 Month = as.integer(substr(dt, 6, 7)),
                 Day = as.integer(substr(dt, 9, 10)),
                 temp_c = as.numeric(units::set_units(val, degC))) %>% 
          mutate(gs = case_when(Month < which(input$chill_month == month.abb) ~ Year,
                                Month >= which(input$chill_month == month.abb) ~ as.integer(Year + 1))) %>% 
          filter(!is.na(gs)) %>% 
          select(cvar, temp_c, Year, Month, Day, gcm, scenario, gs) %>%
          tidyr::pivot_wider(names_from = cvar, values_from = temp_c) %>%
          rename(Tmax = tasmax, Tmin = tasmin)

        ## Compute hourly temperatures
        progress_lst[[i]]$set(message = paste0(site_name, ". Processing:"), 
                     detail = "Interpolating historic hourly temps")
        
        pt_base_hrtmp_long2 <- pt_base_ymd_gs_wide_tbl2 %>% 
          ca_make_hourly_temps(latitude = y_coord) %>% 
          mutate(date_hour = lubridate::make_datetime(Year, Month, Day, Hour, tz = "America/Los_Angeles")) %>% 
          arrange(date_hour)
        
        ## Compute hourly chill portions
        progress_lst[[i]]$set(message = paste0(site_name, ". Processing:"), 
                     detail = "Computing historic hourly chill portions")
        
        ## Return the tibble with hourly chill portions column
        chill_portions_base_tbl2 <- pt_base_hrtmp_long2 %>% 
          group_by(gs, gcm, scenario) %>% 
          mutate(accum_chill_prtn = Dynamic_Model(Temp))
        
        ## Compute the end-of-season chill for BASELINE climate for each 
        ## growing season, emissions scenario, and GCM, 
        pt_base_eos_chill_tbl2 <- chill_portions_base_tbl2 %>% 
          group_by(scenario, gs, gcm) %>% 
          summarise(max_chill = max(accum_chill_prtn), .groups = 'drop') %>% 
          mutate(time_period = paste0(isolate(input$base_year[1]), "-", isolate(input$base_year[2])))
        
        ## Compute safe winter chill for each RCP
        comb_safe_chill_tbl2 <- pt_base_eos_chill_tbl2 %>% 
          bind_rows(pt_prj_eos_chill_tbl2) %>%
          group_by(time_period, scenario) %>% 
          summarize(safe_chill = quantile(max_chill, probs = 1 - as.numeric(input$safe_chill) / 100), .groups = "drop")

        ################################################################
        ## Compute the Histograms of EOS Chill
        ################################################################
        
        if ("chill_dist_hist" %in% rpt_opts) {
          
          ## Compute the range of EOS chill for a uniform set of x-axis values for histograms
          comb_eos_chill_range2 <- range(
            range(pt_prj_eos_chill_tbl2 %>% pull(max_chill)),
            range(pt_base_eos_chill_tbl2 %>% pull(max_chill)))
          
          ## Create the histogram of the EOS chill - baseline
          gg_base_eos_chill <- ggplot(data = pt_base_eos_chill_tbl2, aes(x=max_chill)) + 
            geom_histogram() +
            xlim(comb_eos_chill_range2) +
            labs(title = "Historical: End-of-Season Chill Portion", 
                 subtitle = paste0("Modelled historic data. ", isolate(input$base_year[1]), " - ", isolate(input$base_year[2])), 
                 caption = paste0(
                   "Data source: Cal-Adapt.org\n",
                   stringr::str_wrap(paste0("GCMs: ", paste(isolate(input$base_gcm), collapse = ", ")),
                            width = 150)),
                 x = "end-of-season chill portion", 
                 y = "count") +
            geom_vline(data = comb_safe_chill_tbl2 %>% 
                         filter(scenario == "historical"), 
                       color = "red", aes(xintercept = safe_chill), size = 1) +
            theme(plot.caption = element_text(hjust = 0),
                  plot.margin = margin(20, 20, 20, 20),
                  plot.background = element_rect(colour = "gray50", size = 3))
          
          ## Create the RCP45 histogram
          eos_chill_45_tbl <- pt_prj_eos_chill_tbl2 %>% 
            filter(scenario == "rcp45")
          
          if (nrow(eos_chill_45_tbl) == 0) {
            gg_rcp45_eos_chill <- NA
          } else {
            gg_rcp45_eos_chill <- ggplot(data = eos_chill_45_tbl, aes(x=max_chill)) + 
              geom_histogram() +
              xlim(comb_eos_chill_range2) +
              labs(title = "RCP 4.5: End-of-Season Chill Portion", 
                   subtitle = paste0(isolate(input$prj_year[1]), " - ", isolate(input$prj_year[2])), 
                   caption = paste0(
                     "Data source: Cal-Adapt.org\n",
                     stringr::str_wrap(paste0("GCMs: ", paste(isolate(input$prj_gcm), collapse = ", ")),
                              width = 120)),
                   x = "end-of-season chill portion", 
                   y = "count") +
              geom_vline(data = comb_safe_chill_tbl2 %>% 
                           filter(scenario == "rcp45"), 
                         color = "red", aes(xintercept = safe_chill), size = 1) +
              theme(plot.caption = element_text(hjust = 0),
                    plot.margin = margin(20, 20, 20, 20),
                    plot.background = element_rect(colour = "gray50", size = 3))
          }
          
          ## Create the RCP85 histogram
          eos_chill_85_tbl <- pt_prj_eos_chill_tbl2 %>% 
            filter(scenario == "rcp85")
          
          if (nrow(eos_chill_85_tbl) == 0) {
            gg_rcp85_eos_chill <- NA
          } else {
            gg_rcp85_eos_chill <- ggplot(data = eos_chill_85_tbl, aes(x=max_chill)) + 
              geom_histogram() +
              xlim(comb_eos_chill_range2) +
              labs(title = "RCP 8.5: End-of-Season Chill Portion", 
                   subtitle = paste0(isolate(input$prj_year[1]), " - ", isolate(input$prj_year[2])), 
                   caption = paste0(
                     "Data source: Cal-Adapt.org\n",
                     stringr::str_wrap(paste0("GCMs: ", paste(isolate(input$prj_gcm), collapse = ", ")),
                              width = 150)),
                   x = "end-of-season chill portion", 
                   y = "count") +
              geom_vline(data = comb_safe_chill_tbl2 %>% 
                           filter(scenario == "rcp85"), 
                         color = "red", aes(xintercept = safe_chill), size = 1) +
              theme(plot.caption = element_text(hjust = 0),
                    plot.margin = margin(20, 20, 20, 20),
                    plot.background = element_rect(colour = "gray50", size = 3))
          }

        } else {
          gg_base_eos_chill <- NA
          gg_rcp45_eos_chill <- NA
          gg_rcp85_eos_chill <- NA
          
        }

        ################################################################
        ## Compute the Percentage of Runs that Meet the Required Chill
        ################################################################
        
        if ("req_chill_tbl" %in% rpt_opts || "comp_table" %in% rpt_opts) {
          ## For each baseline scenario, compute the percent of runs that hit the required chill
          base_req_chill_pct_tbl2 <- pt_base_eos_chill_tbl2 %>%
            group_by(gs, gcm, scenario) %>%
            summarise(reached_thresh = max(max_chill) >= as.numeric(input$req_chill), .groups = 'drop') %>%
            group_by(scenario) %>%
            summarise(percent_reached_thresh = sum(reached_thresh) / n(), .groups = 'drop') %>%
            mutate(time_period = paste0(isolate(input$base_year[1]), "-", isolate(input$base_year[2])), 
                   percent_reached_thresh = percent_reached_thresh) %>%
            relocate(time_period, scenario, percent_reached_thresh)
          
          ## For each projected scenario, compute the percent of runs that hit the required chill
          prj_req_chill_pct_tbl2 <- pt_prj_eos_chill_tbl2 %>%
            group_by(gs, gcm, scenario) %>%
            summarise(reached_thresh = max(max_chill) >= as.numeric(input$req_chill), .groups = 'drop') %>%
            group_by(scenario) %>%
            summarise(percent_reached_thresh = sum(reached_thresh) / n(), .groups = 'drop') %>%
            mutate(time_period = paste0(isolate(input$prj_year[1]), "-", isolate(input$prj_year[2])), 
                   percent_reached_thresh = percent_reached_thresh) %>%
            relocate(time_period, scenario, percent_reached_thresh)
          
          ## Combine the baseline and projected tables of proportion of runs that reach required chill
          comb_req_chill_pct_tbl2 <- base_req_chill_pct_tbl2 %>% 
            bind_rows(prj_req_chill_pct_tbl2)

        } else {
          comb_req_chill_pct_tbl2 <- NA
        }
        

        ################################################################
        ## Generate the Time Series of Chill Portions Line Plots
        ################################################################
        
        if ("chill_month_plot" %in% rpt_opts) {
          
          ## Render the chill portion time series for the historical period
          ## Convert the month to an integer
          chill_month_int <- which(month.abb == isolate(input$chill_month))
          
          ## Generate the time series plot for the baseline
          gg_chillts_base <- ggplot(data = chill_portions_base_tbl2 %>%
                                      filter(Hour == 0) %>% 
                                      mutate(year_plot = if_else(Month >= chill_month_int, 1970, 1971)) %>%
                                      mutate(date_plot = lubridate::make_date(year_plot, Month, Day),
                                             gs_gcm = paste(gs, gcm, sep = "_")) %>%
                                      select(scenario, gs, gcm, gs_gcm, date_plot, accum_chill_prtn),
                                    aes(x = date_plot, y = accum_chill_prtn)) +
            geom_line(aes(color=gs_gcm), show.legend = FALSE) +
            geom_hline(color = "black", aes(yintercept = as.numeric(input$req_chill)), size = 1) +
            scale_x_date(date_breaks = "1 month", date_labels = "%b") +
            labs(title = "Historical: Chill Portion Accumulation",
                 subtitle = paste0(isolate(input$base_year[1]), " - ", isolate(input$base_year[2])), 
                 caption = paste0(
                   "Data source: Cal-Adapt.org\n",
                   stringr::str_wrap(paste0("GCMs: ", paste(isolate(input$base_gcm), collapse = ", ")),
                            width = 80)),
                 x = NULL, y = "Chill Portion") +
            theme(plot.caption = element_text(hjust = 0),
                  plot.margin = margin(20, 20, 20, 20),
                  plot.background = element_rect(colour = "gray50", size = 3))
          
          ## Generate the time series plot for the rcp45
          gg_chillts_rcp45 <- ggplot(data = chill_portions_prj_tbl2 %>%
                                       filter(Hour == 0, scenario == "rcp45") %>% 
                                       mutate(year_plot = if_else(Month >= chill_month_int, 1970, 1971)) %>%
                                       mutate(date_plot = lubridate::make_date(year_plot, Month, Day),
                                              gs_gcm = paste(gs, gcm, sep = "_")) %>%
                                       select(scenario, gs, gcm, gs_gcm, date_plot, accum_chill_prtn),
                                     aes(x = date_plot, y = accum_chill_prtn)) +
            geom_line(aes(color=gs_gcm), show.legend = FALSE) +
            geom_hline(color = "black", aes(yintercept = as.numeric(input$req_chill)), size = 1) +
            scale_x_date(date_breaks = "1 month", date_labels = "%b") +
            labs(title = "RCP 4.5: Chill Portion Accumulation",
                 subtitle = paste0(isolate(input$prj_year[1]), " - ", isolate(input$prj_year[2])), 
                 caption = paste0(
                   "Data source: Cal-Adapt.org\n",
                   stringr::str_wrap(paste0("GCMs: ", paste(isolate(input$prj_gcm), collapse = ", ")),
                            width = 80)),
                 x = NULL, y = "Chill Portion") +
            theme(plot.caption = element_text(hjust = 0),
                  plot.margin = margin(20, 20, 20, 20),
                  plot.background = element_rect(colour = "gray50", size = 3))
          
          ## Generate the time series plot for the rcp85
          gg_chillts_rcp85 <- ggplot(data = chill_portions_prj_tbl2 %>%
                                       filter(Hour == 0, scenario == "rcp85") %>% 
                                       mutate(year_plot = if_else(Month >= chill_month_int, 1970, 1971)) %>%
                                       mutate(date_plot = lubridate::make_date(year_plot, Month, Day),
                                              gs_gcm = paste(gs, gcm, sep = "_")) %>%
                                       select(scenario, gs, gcm, gs_gcm, date_plot, accum_chill_prtn),
                                     aes(x = date_plot, y = accum_chill_prtn)) +
            geom_line(aes(color=gs_gcm), show.legend = FALSE) +
            geom_hline(color = "black", aes(yintercept = as.numeric(input$req_chill)), size = 1) +
            scale_x_date(date_breaks = "1 month", date_labels = "%b") +
            labs(title = "RCP 8.5: Chill Portion Accumulation",
                 subtitle = paste0(isolate(input$prj_year[1]), " - ", isolate(input$prj_year[2])), 
                 caption = paste0(
                   "Data source: Cal-Adapt.org\n",
                   stringr::str_wrap(paste0("GCMs: ", paste(isolate(input$prj_gcm), collapse = ", ")),
                            width = 80)),
                 x = NULL, y = "Chill Portion") +
            theme(plot.caption = element_text(hjust = 0),
                  plot.margin = margin(20, 20, 20, 20),
                  plot.background = element_rect(colour = "gray50", size = 3))        
          
        } else {
          gg_chillts_base <- NA
          gg_chillts_rcp45 <- NA
          gg_chillts_rcp85 <- NA
        }

        #####################################################
        ## DONE CREATING THE OUTPUTS FOR THIS LOCATION
        #####################################################
        
        progress_lst[[i]]$set(message = paste0(site_name, "."), detail = "Done")
        
        ## progress_lst[[i]]$close()
        
        site_info_lst[[i]] <- list(site_name = site_name,
                                   site_coords = c(x_coord, y_coord),
                                   safe_chill_tbl = comb_safe_chill_tbl2,
                                   gg_base_eos_chill = gg_base_eos_chill,
                                   gg_rcp45_eos_chill = gg_rcp45_eos_chill,
                                   gg_rcp85_eos_chill = gg_rcp85_eos_chill,
                                   gg_chillts = list(gg_chillts_base = gg_chillts_base,
                                                           gg_chillts_rcp45 = gg_chillts_rcp45,
                                                           gg_chillts_rcp85 = gg_chillts_rcp85),
                                   comb_req_chill_pct_tbl = comb_req_chill_pct_tbl2)

      } ## for i in 1:nrow(locs_tbl())
      
      progress_rpt <- Progress$new()
      progress_rpt$set(message = "Generating report.", detail = "Please wait, this can take a minute")
      
      ##########################################
      ## Generate the report
      ##########################################

      output_fn <- "report.html"
      index_html <- rmarkdown::render("report.Rmd",
                                      output_file = output_fn,
                                      output_dir = "www",
                                      params = list(bookmark_url = bookmark_url(),
                                                    rpt_opts = rpt_opts,
                                                    site_info_lst = site_info_lst,
                                                    model_settings_lst = list(
                                                      base_year = isolate(input$base_year),
                                                      base_gcm = isolate(input$base_gcm),
                                                      base_scenario = isolate(input$base_scenario),
                                                      prj_year = isolate(input$prj_year),
                                                      prj_gcm = isolate(input$prj_gcm),
                                                      prj_scenario = isolate(input$prj_scenario),
                                                      chill_month = isolate(input$chill_month),
                                                      safe_chill = isolate(input$safe_chill),
                                                      req_chill = isolate(input$req_chill))
                                                    )
                                      )

      ## Close progress objects
      suppressWarnings({
        progress_rpt$close()
        for (i in 1:length(progress_lst)) {
          progress_lst[[i]]$close()
        }
      })
      
      ## Remove the content from txtout_rpt_coming
      ## output$txtout_rpt_coming <- renderText(NULL)  ## doesn't always work
      
      ## shinyjs::html(id = "txtout_rpt_coming", html = "")

      shinyjs::html(id = "htmlout_rpt_link", 
                    html = "<p><a href=\"report.html\" target=\"_blank\" class=\"btn btn-default\" style=\"background-color:#e5f5ca; color:green; font-weight:bold; margin-left:2em;\" rel=\"noopener\">Open Report</a></p>")
      
      ## Open the report
      ## browseURL(output_fn)    ## doesn't work on shinyapps.io
      ## js$browseMe(output_fn)  ## creates a pop-up windows :-(

    }  ## if not is.null locs_tbl() 

  })
  

}

shinyApp(ui, server, enableBookmarking = "url")

