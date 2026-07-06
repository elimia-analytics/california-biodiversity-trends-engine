#' ---
#' title: California Biodiversity Trends Engine
#' ---
#'
#' # Server setup
#' ## Load libraries
library(shiny)
library(shinydashboard)
library(shinybusy)
library(shinyBS)
library(shinyglide)
library(leaflet)
library(leaflet.extras)
library(purrr)
library(shinyjs)
library(sf)
library(shinycssloaders)
library(dygraphs)
library(plotly)
library(readr)
library(DT)
library(shinyWidgets)
library(bslib)
library(leaflet.minicharts)
library(leafgl)
library(sortable)
library(flexdashboard)
library(dygraphs)
library(bslib)
library(natserv)
library(duckdbfs)
library(picante)
library(ecoCopula)
library(mvabund)
library(units)
library(memoise)
library(glue)
#'
#' ## Load data
#' ### Areas of interest polygons
aoi_polygons <- readRDS("data/aoi_polygons.rds") %>% 
  dplyr::filter(aoi_name %in% gsub("_data_full.rds", "", list.files("data/outputs"))) %>% 
  dplyr::arrange(aoi_name)
#' ## Add custom Javascript
scr <- tags$script(HTML(
  "
Shiny.addCustomMessageHandler(
  'removeleaflet',
  function(x){
    console.log('deleting',x)
    // get leaflet map
    var map = HTMLWidgets.find('#' + x.elid).getMap();
    // remove
    map.removeLayer(map._layers[x.layerid])
  })
"
))
tooltip_js <- "
$(function () {
  $('[data-toggle=tooltip]').tooltip()
})
"
#'
#'
#'
#' # User Interface
navbarPage(title = HTML("<span style='float: left; display: inline-block; padding-left: 0px;'><img src = 'California_academy_logo.png', height = '45'></span><span style='display: inline-block; padding: 12px 5px 5px 15px;'><p style = 'font-size: 22px; font-family: Archivo !important; color: #585032 !important;'><strong>California Biodiversity Trends Engine</strong></p></span>"), 
           windowTitle = "California Biodiversity Trends Engine", 
           id="nav", theme = "style.css", collapsible = TRUE,
           
           tags$head(
             HTML("<link href='https://fonts.googleapis.com/css2?family=Roboto&display=swap' rel='stylesheet'>"),
             HTML("<link href='https://fonts.googleapis.com/css2?family=Archivo&display=swap' rel='stylesheet'>"),
             HTML("<meta name='viewport' content='width=device-width, initial-scale=1'>"),
             tags$style(HTML("#pages li:nth-child(5) a:first-of-type { display: none; }")) 
           ),

           div(class="outer",
               
               useShinyjs(),     ## Call to use shinyJS
               
               scr,
               
                        layout_sidebar(
                          sidebar = sidebar(
                            id = "place_sidebar",
                            width = "25em",
                            padding = "1em", 
                            list(
                            div(strong(h3("Select conservation place of interest", style = "margin-top: 0px; padding-top: 0px; color: #585032; float: left; font-weight: 700;")), style = "margin-top: 0px; text-align: center;"),
                            div(h4("Choose from dropdown menu or from the map", style = "color: #585032; float: left;"), style = "text-align: center;"),
                            selectizeInput(inputId = "select_map_aoi",
                                           label = "",
                                           choices = c("", unique(aoi_polygons$aoi_name)), 
                                           multiple = FALSE,
                                           width = "100%",
                            ), 
                            br(),
                            br(),
                            actionButton(inputId = "aoi_go", label = "Explore place", icon = icon("chart-simple"), block = TRUE, class = "btn-primary btn-lg", width = "100%", style = "font-size: 14px !important;")
                            )
                          ),
                          tabsetPanel(id = "pages", type = "tabs", 
                                      
                                      tabPanel("DATA", 
                                               
                                               useShinyjs(),     ## Call to use shinyJS
                                               
                                               fluidRow(style = "padding: 0 5px 0 15px;",
                                                                        leafletOutput("main_map", height = "50vh"),
                                                        # Optional global busy spinner
                                                        add_busy_spinner(
                                                          spin = "scaling-squares", 
                                                          margins = c("38vh", "45vw"), 
                                                          color = "#585032",
                                                          height = "75px",
                                                          width = "75px"
                                                        ),                                               ),
                                               shinyjs::hidden(
                                               absolutePanel(id = "time_plot_panel",
                                                             class = "panel panel-default",
                                                             top = "12vh", right = "3vw", left = "auto", bottom = "auto",
                                                             width = "30vw",
                                                             height = "43vh",
                                                             style = "padding: 10px 10px 30px 10px; border: none; box-shadow: none !important; border-bottom: none; border-color: transparent; background-color: rgba(255, 255, 255, 0.7); z-index: 1000 !important; overflow-y: hidden !important; overflow-x: hidden;",
                                                             dygraphOutput("time_plot", height = "40vh")
                                               )
                                               ),
                                               
                                               shinyjs::hidden(
                                               div(id = "data_output",
                                               fluidRow(style = "padding: 0.5em 0 0 2em;",
                                                                        column(width = 4, style = "padding: 0 5px 10px 0;",
                                                                               htmlOutput("metric_total"),
                                                                               tabsetPanel(id = "metric_switch", type = "pills",
                                                                                           tabPanel("Records", height = "100%"),
                                                                                           tabPanel("Species", height = "100%"),
                                                                                           tabPanel("Habitats", height = "100%"),
                                                                                           tabPanel("Observers", height = "100%"),
                                                                                           tabPanel("Locations", height = "100%"),
                                                                                           tabPanel("Visits", height = "100%")
                                                                               )
                                                                        ),
                                                                        column(width = 8, 
                                                                               div(style = "position: relative; float: right; padding-top: 28px;",
                                                                                   materialSwitch(inputId = "redo_search", label = "Limit output \n to map bounds", width = "100%", right = TRUE, value = FALSE)
                                                                               ),
                                                                               div(style = "width: 11vw; position: relative; float: right; padding-top: 15px;",
                                                                                   checkboxInput("deselect_all", label = "Deselect all", value = FALSE)
                                                                               ),
                                                                               div(style = "width: 5vw; position: relative; float: right; padding-top: 15px;",
                                                                                      checkboxInput("select_all", label = "Select all", value = FALSE)
                                                                               )
                                                                        
                                                        )
                                               ),
                                               fluidRow(style = "padding: 0 10px 0 0; overflow-x: scroll; scrollbar-color: #C7C7C7 rgba(255, 255, 255, 1) !important; min-height: 80vh;",
                                                        layout_sidebar(
                                                          sidebar = sidebar(
                                                            position = "right",
                                                            width = "17vw",
                                                            height = "80vh",
                                                            padding = "0.5em",
                                                            list(
                                                            br(),
                                                            br(),
                                                            selectizeInput(inputId = "select_species", label = "", choices = NULL, multiple = TRUE, options = list(placeholder = "Select species")),
                                                            h5("Select taxon", style = "padding-left: 12px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; font-size: 14px; color: #777; font-weight: 400;"),
                                                            plotlyOutput("taxa_donut", width = "110%", height = "250px")
                                                            )
                                                          ), 
                                                          div(style = "min-height: 80vh;",
                                                            DT::dataTableOutput("records_table", height = "80vh")
                                                            # uiOutput("records_table"),
                                                          )
                                                        )
                                               )
                                               
                                                 )
                                               )
                                      ),
                                      
                                      tabPanel("TRENDS", height = "100%",
                                               
                                               # Optional global busy spinner
                                               add_busy_spinner(
                                                 spin = "scaling-squares", 
                                                 margins = c("38vh", "45vw"), 
                                                 color = "#585032",
                                                 height = "75px",
                                                 width = "75px"
                                               ),
                                               
                                               fluidRow(style = "padding: 0 10px 0 30px;",
                                                        column(width = 4, style = "width: 30vw; padding-right: 30px;",
                                                               fluidRow(style = "overflow-x: scroll; scrollbar-color: #C7C7C7 rgba(255, 255, 255, 1) !important; ",
                                                                        fluidRow(
                                                                        column(width = 12,
                                                                        tabsetPanel(id = "species_trends_tabs", type = "pills",
                                                                                    tabPanel("At a Glance", height = "100%"),
                                                                                    tabPanel("Decreasing Species", height = "100%"),
                                                                                    tabPanel("Increasing Species", height = "100%"),
                                                                                    tabPanel("All Species", height = "100%")
                                                                        )
                                                                        )
                                                                        ),
                                                                        DT::dataTableOutput("trends_table", width = "100%")
                                                               )
                                                        ),
                                                        column(width = 8, style = "width: 60vw; padding-top: 0;",
                                                               fluidRow(style = "padding: 0 4px 10px 20px; margin-top: 0;",
                                                                        tabsetPanel(id = "species_trends_outputs", type = "pills",
                                                                                    tabPanel("Temporal Trend ", height = "100%",
                                                                                             div(style = "padding-top: 10px;",
                                                                                             plotOutput("species_trends_output", height = "75vh")
                                                                                             )
                                                                                             ),
                                                                                    tabPanel("Spatiotemporal Change", height = "100%",
                                                                                             div(style = "padding-top: 10px;",
                                                                                             leafletOutput("trends_map", height = "75vh")
                                                                                             )
                                                                                             ),
                                                                                    tabPanel("Reference Taxa", height = "100%",
                                                                                             div(style = "padding-top: 20px;",
                                                                                             h4(em("Taxa most frequently observed with focal species"), style = "padding-top: 0; margin-top: 0;"),
                                                                                             DT::dataTableOutput("association_table", height = "75vh")
                                                                                             )
                                                                                    )
                                                                        )
                                                               ),
                                                               absolutePanel(id = "trend_species_selection_panel",
                                                                             class = "panel panel-default",
                                                                             top = 0, right = 100, left = "auto", bottom = "auto",
                                                                             width = "30vw",
                                                                             height = "5em",
                                                                             style = "padding: 0 15px 0 0; border: none; box-shadow: none !important; border-bottom: none; border-color: transparent; background-color: transparent; z-index: 1000 !important; overflow-y: hidden !important; overflow-x: hidden;",
                                                                             fluidRow(
                                                                               span(selectizeInput(inputId = "select_species_trend", label = "", choices = NULL, multiple = FALSE, options = list(placeholder = "Search for species")), style = "margin-top: -5px; float: right;")
                                                                             ),
                                                               )

                                                        )
                                               )
                                      ),
                                      tabPanel("INSIGHTS", height = "100%", 
                                               div(style = "height: 100vh; width: 100vw;",

                                               add_busy_spinner(
                                                 spin = "scaling-squares", 
                                                 margins = c("38vh", "45vw"), 
                                                 color = "#585032",
                                                 height = "75px",
                                                 width = "75px"
                                               ),
                                               uiOutput("insights_report_iframe") 
                                                 )
                                      ),
                                      tabPanel("METHODOLOGY",
                                               
                                               div(style = "padding: 0 35px 35px 35px; font-family: 'Roboto', sans-serif; line-height: 1.5;",
                                                   
                                                   h3("Overview", 
                                                      style = "
                                                        font-family: 'Archivo';
                                                        font-size: 18px;
                                                        color: #1F417D;
                                                        margin-top: 20px;
                                                        margin-bottom: 10px;
                                                        border-bottom: 3px solid #4275D0;
                                                      "),
                                                   
                                                   p("The California Biodiversity Trends Engine mobilizes records from digitized museum biocollections, community science projects, and additional sources integrated via the Global Biodiversty Information Facility (GBIF) to detect long-term and short-term trends in the observation and occurrence of species and habitats."),
                                                   
                                                   p("Trends are estimated across conservation places of interest via comparisons with historical and regional baselines, all the while accounting for biases in the frequency and intensity of observation. A focus on quantifying changes in biodiversity detection potentially indicative of regeneration opportunities, successful management actions, and early warning signals enables this work to provide actionable biodiversity insights in support of monitoring and management across the areas of focus."),
                                                   
                                                   p("The California Biodiversity Trends Engine is a collaboration between the Center for Biodiversity and Community Science at the California Academy of Sciences and Elimia."),

                                                   h4("Key Steps", 
                                                      style = "
                                                        font-family: 'Roboto';
                                                        font-size: 14px;
                                                        font-style: italic;
                                                        color: #1F417D;
                                                        margin-top: 20px;
                                                        margin-bottom: 10px;
                                                      "),
                                                   p("❏ For each conservation place of interest, the California Biodiversity Trends Engine:"),
                                                   p("❏ Identifies patterns in biodiversity recording over space and time."),
                                                   p("❏ Integrates records based on taxonomy, habitat, location, and observer."),
                                                   p("❏ Quantifies species-habitat relationships based on the frequency of recording for each species in each habitat."),
                                                   p("❏ Combines related sets of records into sampling visits - the units of trends analyses."),
                                                   p("❏ Determines which species are frequently recorded together to identify sets of 'reference' taxa of relevance for each focal species."),
                                                   p("❏ Mobilizes long-term and regional data on sampling visits to establish baseline expectations of the frequency of recording for each species in each year and location."),
                                                   p("❏ Calculates trends in over- or under-reporting by comparing the observed frequency of recording in each year and location to the expected baseline."),
                                                   p("❏ Synthesizes patterns and trends emerging from the data into guidelines for monitoring and management of the conservation place of interest."),
                                                   
                                                   h4("Get Started", 
                                                      style = "
                                                        font-family: 'Roboto';
                                                        font-size: 14px;
                                                        font-style: italic;
                                                        color: #1F417D;
                                                        margin-top: 20px;
                                                        margin-bottom: 10px;
                                                      "),
                                                   
                                                   p("1. Choose a conservation place of interest from dropdown menu in left sidebar panel or by clicking on the map."),
                                                   p("2. Click the 'Explore place' button to view outputs."),
                                                   p("3. Use the DATA tab to explore spatial and temporal variation in recording intensity, the species, locations, and habitats observed, as well as the observers and observation visits producing producing the data."),
                                                   p("4. Use the TRENDS tab to explore yearly trends in the observation and over- or under-reporting of species across the area of focus."),
                                                   p("5. Use the INSIGHTS tab to generate a synthesis of the spatial and temporal trends quantified in support of monitoring and management of the conservation place of interest."),
                                                   
                                                   br(),
                                                   
                                                   h3("Data Sources", 
                                                      style = "
                                                        font-family: 'Archivo';
                                                        font-size: 18px;
                                                        color: #1F417D;
                                                        margin-top: 20px;
                                                        margin-bottom: 10px;
                                                        border-bottom: 3px solid #4275D0;
                                                      "),
                                                   
                                                   p("For each conservation place of interest, analyses rely on two primary data sources:"),
                                                   p("❏ Global Biodiversity Information Facility (",
                                                     a(
                                                       paste0("GBIF ", substr(Sys.Date(), 1, 4)),
                                                       href = "https://www.gbif.org/",
                                                       target = "_blank",
                                                       style = "font-size: 13px; font-weight: 400;"
                                                     ), 
                                                     ")"
                                                   ),
                                                   p("❏ California Wildlife Habitat Relationships Vegetation Types (",
                                                     a(
                                                       paste0("California Department of Forestry and Fire Protection ", substr(Sys.Date(), 1, 4)),
                                                       href = "https://data.ca.gov/dataset/california-vegetation-whrtype",
                                                       target = "_blank",
                                                       style = "font-size: 13px; font-weight: 400;"
                                                     ), 
                                                     ")"
                                                   ),
                                                   
                                                   br(),
                                                   
                                                   h3("Areas of Conservation Interest", 
                                                      style = "
                                                        font-family: 'Archivo';
                                                        font-size: 18px;
                                                        color: #1F417D;
                                                        margin-top: 20px;
                                                        margin-bottom: 10px;
                                                        border-bottom: 3px solid #4275D0;
                                                      "),
                                                   
                                                   HTML(
                                                     "<p>The California Biodiversity Trends Engine is a place-based tool that centers around places of conservation interest (e.g. Preserves, Land Trusts, Public Parks) across California.</p>",
                                                     "<p>It is important to note that most analyses and outputs are centered around the focal place but extend further out to a 'baseline' area of interest which includes the full extent of all watersheds that overlap the boundary of the focal place of interest to any degree. This approach allows engine analyses to leverage the full biogeographic context of the place of interest and make use of all possible relevant data for detecting patterns and trends.</p>",
                                                   ),
                                                   
                                                   h3("Data Processing", 
                                                      style = "
                                                        font-family: 'Archivo';
                                                        font-size: 18px;
                                                        color: #1F417D;
                                                        margin-top: 20px;
                                                        margin-bottom: 10px;
                                                        border-bottom: 3px solid #4275D0;
                                                      "),
                                                   
                                                   HTML(
                                                     "<p>The California Biodiversity Trends Engine extracts Global Biodiversity Information Facility (GBIF) data for each place of conservation interest and the surrounding baseline area from <a href = 'https://source.coop/cboettig/gbif/2025-06/hex' target = '_blank'>H3-Indexed cloud-optimized snapshots of GBIF </a> created by the Boettiger lab at UC Berkeley. These snapshots include all GBIF data available from January 1900 up to and including June 2025. </p>",
                                                     "<p>All GBIF data sources are processed and included in the Engine, with the exception of eBird for no other reason than the sheer magnitude of the dataset would overwhelm the analyses and outputs. GBIF records with a coordinate uncertainty of exceeding 1400m (the side of the primary hexagon size used as spatial units in downstream analyses) or unknown are exlcuded from the tool.</p>",
                                                     "<p>This biodiversity dataset forms the basis of all analyses, outputs, summaries, and visualizations included in the web application.</p>"
                                                   ),

                                                   h3("Species-habitat Relationships", 
                                                      style = "
                                                        font-family: 'Archivo';
                                                        font-size: 18px;
                                                        color: #1F417D;
                                                        margin-top: 20px;
                                                        margin-bottom: 10px;
                                                        border-bottom: 3px solid #4275D0;
                                                      "),
                                                   
                                                   HTML(
                                                     "<p>Each GBIF record is attributed a habitat/vegetation class by extracting the CWHR Vegetation type corresponding to the 30m cell overlapped by the record coordinates.</p>",
                                                     "<p>The number of records in each habitat type are counted for each species. A species is identified as significantly associated with a given habitat if at least 15% of its records were collected in that habitat type.</p>",
                                                   ), 
                                                   
                                                   h3("Sampling Visits", 
                                                      style = "
                                                        font-family: 'Archivo';
                                                        font-size: 18px;
                                                        color: #1F417D;
                                                        margin-top: 20px;
                                                        margin-bottom: 10px;
                                                        border-bottom: 3px solid #4275D0;
                                                      "),
                                                   
                                                   HTML(
                                                     "<p>Defining sampling visits is a key step to correct for sampling bias in unstructured opportunistic biodiversity records (Rapacciuolo et al. 2021).</p>",
                                                     "<p>Conceptually, a sampling visit (hereafter simply 'visit') represents a set of records collected by the same observer over a bounded amount of time and space. In standardized ecological surveys, this information allows us to make inferences from the data based on precise estimates of sampling effort (i.e. how much was observed given how hard observers were searching).</p>",
                                                     "<p>In practice, understanding which observations were part of the same sampling visit in a big unstructured dataset extracted from GBIF can be tricky.</p>",
                                                     "<p>For the purpose of the analyses presented in this tool, sampling visits are defined as the set of GBIF records collected on the same date by the same observer over an area of approximately 5 square kilometers, and served as part of the same dataset. Any combination of this information is used in instances where some of these metadata are missing (e.g. observer ID). </p>",
                                                     "<p>Defining sampling visits in this way allows the California Biodiversity Trends Engine to make inferences about sampling effort when making inferences about given areas, years, taxa, or habitats.</p>",
                                                   ), 
                                                   
                                                   h3("Data Summaries", 
                                                      style = "
                                                        font-family: 'Archivo';
                                                        font-size: 18px;
                                                        color: #1F417D;
                                                        margin-top: 20px;
                                                        margin-bottom: 10px;
                                                        border-bottom: 3px solid #4275D0;
                                                      "),
                                                   
                                                   HTML(
                                                     "<p>In the 'DATA' tab, the density and information content of biodiversity data is summarized spatially via the map, temporally via the barplot, and in detail in tabular format. Summaries are provided for 6 indicators: Records, Species, Habitats, Observers, Locations, and Visits.</p>"
                                                   ),
                                                   
                                                   h3("Reference Taxa", 
                                                      style = "
                                                        font-family: 'Archivo';
                                                        font-size: 18px;
                                                        color: #1F417D;
                                                        margin-top: 20px;
                                                        margin-bottom: 10px;
                                                        border-bottom: 3px solid #4275D0;
                                                      "),
                                                   
                                                   HTML(
                                                     "<p>Borrowing strength across taxa - that is, leveraging information on taxa associated with the target taxon - can improve inferences of species changes from unstructured opportunistic biodiversity records (Rapacciuolo et al. 2021).</p>",
                                                     "<p>While standardized ecological surveys explicitly specify the list of taxa searched, this information is unavailable from unstructured opportunistic biodiversity datasets. In these cases, the list of taxa associated with the focal taxon (or 'reference' taxa) needs to be inferred from the data.</p>",
                                                     "<p>Sampling visits enable identifying reference taxa, as they hold information on which taxa tend to be observed by the same people at the same locations and times.</p>",
                                                     "<p>In California Biodiversity Trends Engine analyses, reference taxa are defined as the set of species that is most frequently observed during the same visits wherein focal taxa are also observed. Information on the list of reference taxa identified for each focal taxon can be found in the 'TRENDS' tab, under 'Reference taxa'.</p>"
                                                   ),
                                                   
                                                   h3("Species Trends", 
                                                      style = "
                                                        font-family: 'Archivo';
                                                        font-size: 18px;
                                                        color: #1F417D;
                                                        margin-top: 20px;
                                                        margin-bottom: 10px;
                                                        border-bottom: 3px solid #4275D0;
                                                      "),
                                                   
                                                   HTML(
                                                     "<p>Reverse-engineering survey structure by identifying sampling visits, together with borrowing strength across taxa by identifying the list of reference taxa are the key steps underlying species trends estimates in the California Biodiversity Trends Engine. Trends for each species across the baseline place of interest from 1900 to 2025 are estimated as follows:</p>",
                                                     "<p>1) All visits resulting in an observation of the focal taxon or any of its reference taxa are identified and included in the analysis set.</p>",
                                                     "<p>2) For each visit in the analysis set, detection or non-detection of the focal taxon is quantified.</p>",
                                                     "<p>3) The yearly reporting rate is quantified as the proportion of analysis visits in a year that generated at least one observation of the focal taxon. Because the reporting rate is expressed as a proportion of all relevant visits, it is assumed that this effectively controls for differences in overall observation effort across years.</p>",
                                                     "<p>4) To understand if yearly changes in reporting rates constitute significant long- or medium-term differences in reporting rate, a reporting rate anomaly is calculated. The reporting rate anomaly is the difference in reporting rate compared to an expected baseline reporting rate for the focal taxon based on the particular locations and times it was or was not detected. This baseline reporting rate is estimated by randomizing the observed matrix of species detections/non-detections across visits using the quasiswap algorithm. 
                                                     The quasiswap algorithm creates randomized matrices that maintain two strict biological constraints simultaneously: fixed row sums (i.e. the number of species at each site stays the same); fixed column sums: the number of visits each species is detected in stays the same (species prevalence across space and time stays the same). Because of this, it is assumed that reporting rate anomalies are able to disentangle the observed pattern of over- or under-reporting while accounting for differences in sampling effort across locations and dates.
                                                     </p>",
                                                     "<p>Yearly values of reporting rate anomaly are the species trends reported by the California Biodiversity Trends Engine. These trends can be interpreted as increases or decreases in the rate of reporting for a focal species year-on-year across the baseline place of interest. These increases or decreases could indicate a true ecological signal of expansion or decline across the area or a drastic change in the targeted observation of particular species.</p>",
                                                   ),
                                                   
                                                   h3("Insights Report", 
                                                      style = "
                                                        font-family: 'Archivo';
                                                        font-size: 18px;
                                                        color: #1F417D;
                                                        margin-top: 20px;
                                                        margin-bottom: 10px;
                                                        border-bottom: 3px solid #4275D0;
                                                      "),
                                                   
                                                   HTML(
                                                     "<p>For each conservation place of interest, biodiversity data and analyses are synthesized in a series of insights:</p>",
                                                     "<p>1) Most underrepresented habitats: the set of habitat types with disproportionately low numbers of records given the acreage they occupy across the conservation place of interest.</p>",
                                                     "<p>2) Sampling coldspots: a map indicating the areas throughout the conservation place of interest with a particularly low density of records in underrepresented habitats.</p>",
                                                     "<p>3) Under-represented/over-represented taxa: a plot estimating the degree of over- or under-representation of major taxa in the dataset. Representation refers to the degree of available information for a taxon (i.e. the proportion of all records that are records of the taxon) given how frequently the taxon occurs across the conservation area of interest (i.e. the proportion of all species ever recorded that belong to the taxon).</p>",
                                                     "<p>4) Average trends across taxa and habitats: yearly trends in reporting rate anomaly averaged across all species in each major taxon or associated with each habitat.</p>",
                                                     "<p>5) Predictors of trends: results from a predictive machine learning (i.e. random forest) model exploring the strongest correlates of increasing versus decreasing yearly trend in reporting rate anomaly.</p>",
                                                     "<p>These insights may be used to futher understand biodiversity sampling and trends across the conservation place interest in support of more efficient and targeted monitoring and management</p>"
                                                   ),
                                               )      
                                      ),
                                      tabPanel(
                                        title = tags$a(
                                          href = "https://github.com/elimia-analytics/california-biodiversity-trends-engine",
                                          target = "_blank",
                                          onclick = "event.stopPropagation();", # Prevents Shiny from switching to this tab blankly
                                          "CODE"
                                        )
                                      )
                        )
               ),
               absolutePanel(id = "elimia",
                             class = "panel panel-default",
                             top = 10, right = 0, left = "auto", bottom = "auto",
                             width = "3em",
                             height = "5em",
                             style = "padding: 0; border: none; box-shadow: none !important; border-bottom: none; border-color: transparent; background-color: transparent; z-index: 1000 !important; overflow-y: hidden !important; overflow-x: hidden;",
                             HTML("
                         <a href='https://elimia.io' target='_blank'><img src = 'elimia-logo-snail.png', height = '45%'></a>
                              ")
               )
           )
)
                          