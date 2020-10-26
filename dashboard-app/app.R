#### Build a Shiny dashboard ####
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)
library(tidyverse)
library(RColorBrewer)
library(leaflet)
library(ggplot2)
library(tigris)
library(sp)
library(sf)
library(scales)
library(plotly)
set.seed(5000)

#### Import dataset ####
allstate_import <- read.csv("https://raw.githubusercontent.com/texastipi/broadband_entrepreneurship/master/Broadband-Entrepreneurship-TXKSME.csv")
# Tigris county polygons
txksme_counties <- counties(state = c("TX","ME","KS"))
# FCC county population estimates
fcc_staff_est_county2018 <- read.csv("https://www.fcc.gov/file/17821/download", header = T)
fcc_staff_est_county2018 <- fcc_staff_est_county2018 %>% 
  select(Id2, Geography, ends_with("2018")) %>% 
  rename(hu2018 = `Housing.Unit.Estimate..as.of.July.1....2018`,
         pop2018 = `Population.Estimate..as.of.July.1....2018`)
allstate_import <- allstate_import %>% 
  mutate(pct_fixed_acs_2018 = pct_fixed_acs_2018/100,
         county_FIPS = as.character(county_FIPS))

allstate_import <- left_join(allstate_import, fcc_staff_est_county2018, by = c("county_FIPS" = "Id2"))
allstate_import_geo <- sp::merge(txksme_counties, allstate_import, by.x = "GEOID", by.y = "county_FIPS")
allstate_import_geo_sf <- sf::st_as_sf(allstate_import_geo)

# Census data import
cbp_2012_2018 <- read.csv("https://raw.githubusercontent.com/texastipi/broadband_entrepreneurship/master/cbp-2014-2018-v2.csv")
bea_prop <- read.csv("https://raw.githubusercontent.com/texastipi/broadband_entrepreneurship/master/bea-2014-2018-v2.csv")


## Build an app showing broadband histogram and map ##
## UI part ##
ui <- dashboardPage(
  dashboardHeader(title = paste("Broadband & Entrepreneurship"), titleWidth = "350"),
  dashboardSidebar(
    width = "160",
    sidebarMenu(
      menuItem("Broadband", tabName = "broadband", icon = icon("broadcast-tower")),
      menuItem("Entrepreneurship", tabName = "entrepreneurship", icon = icon("briefcase")),
      selectInput(inputId = "state", label = "Choose a state",
                  choices = c("Texas" = "TX",
                              "Maine" = "ME",
                              "Kansas" = "KS"), multiple = F)
    )
  ),
  dashboardBody(
    shinyDashboardThemes("grey_light"),
    tabItems(
    tabItem("broadband",
            fluidPage(
              fluidRow(
                column(5,
                       box(title = "About", width = NULL,
                           tags$div(
                             tags$p("Several different measures of broadband are currently available.",
                                    "The most widely used measure is", strong("FCC"),"'s broadband availability measures.",
                                    "The measure has been deemed reliable, however it has also been criticized of overestimating broadband accessibility",
                                    "especially in the rural areas of America.","Few other alternative measures have highlighted potential inaccuracy of FCC's broadband numbers.",
                                    "Also, there are other suplemental measures of broadband subscription and usage that help us better understand the reality of braodband access in America",
                                    "Here you can explore how different broadband measures paint different pictures of broadband in America"),
                             tags$p(
                               h3("Definitions"),
                               tags$ul(
                                 tags$li(tags$strong("FCC Broadband Availability"),tags$sup("1"),":","% of people per county with access to broadband at download speed of 25Mbps and upload speed of 3Mbps."),
                                 tags$li(tags$strong("Microsoft Broadband Usage"),tags$sup("2"),":","% of people per county that actually experienced broadband at download speed of 25Mbps and upload speed of 3Mbps",
                                         "according to their usage of various Microsoft services."),
                                 tags$li(tags$strong("ACS Broadband Subscription"),tags$sup("3"),":","% of people per county that has fixed broadband service subscription."),
                                 tags$li(tags$strong("M-Lab Broadband Usage"),tags$sup("4"),":","Average % of people who reported actual usage of broadband at download speed of 25Mbps and upload speed of 3Mbps",
                                         "in M-Lab speed tests.")
                               ))
                           ),
                           footer = tags$footer(tags$i(tags$sup("1"),"The data published as of June 2019 is used"),br(),
                                                tags$i(tags$sup("2"),"The data is gathered in November 2019"),br(),
                                                tags$i(tags$sup("3"),"The data is from the 2018 American Community Survey 5-year estimates. It does not include mobile broadband subscription"),br(),
                                                tags$i(tags$sup("4"),"M-Lab collects broadband speed test data from voluntary particiapnts. The data represents average percentages from September 2019 to December 2019"),
                                                style = "font-size: 80%"),
                           tags$hr(),
                           h3("Select Measure"),
                           fluidRow(
                             column(12, radioButtons(inputId = "bbtype", label = "Choose a Broadband Measure",
                                                    choices = c("FCC" = "pct_bb_fcc_2019",
                                                                "Microsoft" = "pct_broadband_MS",
                                                                "ACS" = "pct_fixed_acs_2018",
                                                                "M-Lab" = "pct_broadband_mlab")))
                                    )
                           )
                       ),
                column(7, tabsetPanel(
                  tabPanel("Explore Broadband Measures",
                           fluidPage(fluidRow(
                                       box(title = "Histogram of Broadband Measures", width = NULL,
                                           plotlyOutput("hist")
                                           )
                                       ),
                                     tags$style("#poptext {font-size:15px;
                                                font-style:boldfaced;
                                                align:center"),
                                     tags$style("#county_50 {font-size:25px;
                                                font-style:boldfaced;
                                                color:#D15136;
                                                align:center"),
                                     tags$style(".small-box.bg-yellow {
                                                background-color: #BC5721 !important;
                                                color: #000000 !important; }"),
                                     tags$style(".small-box.bg-blue {
                                                background-color: #A4A3A3 !important;
                                                color: #000000 !important; }"),
                                     fluidRow(
                                       valueBoxOutput("vbox_county", width = 6),
                                       valueBoxOutput("vbox_county_pct", width = 6)
                                     )
                                     )
                           ),
                  tabPanel("Broadband Availability Map",
                           leafletOutput(outputId = "map", height = 800)
                           )
                  )
                  )
                )
              
            )
      ),
    tabItem("entrepreneurship",
            fluidPage(
              fluidRow(
                column(5,
                       box(title = "About", width = NULL,
                           tags$div(
                             tags$p("Entrepreneurship and entrepreneurial business play a critical role in economic development.",
                                    "Entrepreneurial activity enhances the vitality of the economy, creates jobs, and sometimes even creates new industries with groundbreaking innovations.",
                                    "Traditionally, entrepreneurship has been measured using various statistics including the number and share of small businesses, ",
                                    "and ", tags$a(href="https://www.bls.gov/bdm/entrepreneurship/entrepreneurship.htm", "the number of new businesses over time."),
                                    "In the US, several measures have been available for measuring small businesses.",
                                    "You can find more details and differences between the measures ", tags$a(href="https://www.sba.gov/sites/default/files/12-13_Nonemployer_v%20Sole_Prop.pdf", "here."),
                                    tags$br(), "Nevertheless, each statistic offers strengths and weaknesses in understanding new business activity in the US.",
                                    "Moreover, understanding entrepreneurial activity in regions, counties, or small and rural communities presents challenges to researchers.",
                                    "Some new research efforts incorporate metrics that complement traditional measures of entrepreneurship. One of these efforts focuses on small business activities online.",
                                    tags$a(href="https://techdatasociety.asu.edu/content/new-measure-digital-participation-and-economic-opportunity-communities-data-20-million", "Mossberger, Tolbert, & LaCombe (2020)"),
                                    "introduced a venture density index that reflects actual online activities by entrepreneurial businesses using data provided by",
                                    tags$a(href="https://www.godaddy.com/", "GoDaddy"), ", the world’s largest registrar of domain names.",
                                    "Below, we provide definitions of several entrepreneurship measures that you can explore in this dashboard."
                                    ),
                             tags$p(
                               h3("Definitions"),
                               tags$ul(
                                 tags$li(tags$strong("% of Business Establishments with less than 10 employees"),tags$sup("1"),":","% of business establishment with less than 10 employees among all business establishments."),
                                 tags$li(tags$strong("% of Nonfarm Proprietors"),tags$sup("2"),":","% of nonfarm proprietors among total employments."),
                                 tags$li(tags$strong("Venture Density"),tags$sup("3"),":","Density of online venture activities represented by GoDaddy domain websites.")
                               ))
                           ),
                           footer = tags$footer(tags$i(tags$sup("1"),"Source: County Business Dynamics (2018)"),br(),
                                                tags$i(tags$sup("2"),"Source: Bureau of Economic Analysis (2018)"),br(),
                                                tags$i(tags$sup("3"),"Source: GoDaddy; Density measures used are originally calculated by Mossberger, Tolber, & LaCombe (2020); Number represented here is an average of data points available throughout 2018 and 2019"),
                                                style = "font-size: 80%"),
                           tags$hr(),
                           h3("Select Measure"),
                           fluidRow(
                             column(12, radioButtons(inputId = "entmeasure", label = "Choose a Entrepreneurship Measure",
                                                    choices = c("% of Business Establishments (less than 10 employees)" = "pct_10_est_cbp_2018",
                                                                "% of Nonfarm Proprietors" = "pct_nonfarm_bea_2018",
                                                                "Venture Density" = "venturedensity_mean")))
                           ))
              ),
              column(7, tabsetPanel(
                tabPanel(
                  "Explore Entrepreneurship Measure",
                  fluidPage(
                    fluidRow(
                    box(title = "Histogram of Entrepreneurship Measures", width = NULL,
                        plotlyOutput("hist_entrepreneurship")
                        )
                    ),
                    fluidRow(
                      tabBox(title = "Trend between 2014 & 2018", width = NULL,
                             tabPanel("% of Business Establishments (<10 employees)",
                                      plotOutput("line_est10")),
                             tabPanel("% of Propreitors",
                                      plotOutput("line_nonfarm")))
                    )
                    )
                  ),
                tabPanel("Entrepreneurship Map",
                         leafletOutput(outputId = "map2", height = 800))
                )
                )
              )
            ))
    )
  )
)

## Server part ##
server <- function(input, output) {
  # Set up reactive dataset
  st_reactive <- reactive({
    allstate_import_geo_sf %>% filter(ST == req(input$state))
  })
  
  cbp_reactive <- reactive({
    cbp_2012_2018 %>% filter(ST == req(input$state)) %>% 
      group_by(year) %>% 
      summarise(emp = sum(emp_cbp),
                est = sum(est_cbp),
                est_50 = sum(est_50_cbp),
                est_10 = sum(est_10_cbp)) %>% 
      mutate(pct_50 = est_50 / est,
             pct_10 = est_10 / est) %>% gather(., key = "var_fact", value = "value", pct_10)
  })
  
  bea_reactive <- reactive({
    bea_prop %>% filter(ST == req(input$state)) %>% 
      group_by(year) %>% 
      summarise(totalemp = sum(totalemp),
                proprietors = sum(proprietors),
                nonfarm_proprietors = sum(nonfarm_proprietors)) %>% 
      mutate(pct_prop_emp = proprietors / totalemp,
             pct_nonfarm_prop_emp = nonfarm_proprietors / totalemp) %>% gather(., key = "var_fact", value = "value", pct_prop_emp:pct_nonfarm_prop_emp)
  })
  
  pal <- colorNumeric(palette = "YlOrRd", domain = c(0,1))
  pal2 <- colorNumeric(palette = "YlOrRd", domain = c(0,12))
  
  # Reactive label for BB
  currentbb_label <- reactive({
    if (req(input$bbtype) == "pct_bb_fcc_2019"){"FCC Broadband (June 2019)"}
    else if (req(input$bbtype) == "pct_broadband_MS"){"Microsoft Broadband (2019)"}
    else if (req(input$bbtype) == "pct_fixed_acs_2018"){"ACS Broadband Subscription (2018)"}
    else if (req(input$bbtype) == "pct_broadband_mlab"){"M-Lab Broadband (Sep-Dec 2019)"}
  })
  # Reactive label for Entrepreneurship
  currentent_label <- reactive({
    if (req(input$entmeasure) == "pct_10_est_cbp_2018"){"% of Establishments (<10 employee, 2018)"}
    else if (req(input$entmeasure) == "pct_nonfarm_bea_2018"){"% of Nonfarm Proprietors (2018)"}
    else if (req(input$entmeasure) == "venturedensity_mean"){"Average Venture Density (2018-2019)"}
  })
  
  # Reactive variables for text outputs
  pop_vals <- reactiveValues()
  observe({
    df <- allstate_import %>% filter(ST == req(input$state))
    if (input$bbtype == "pct_bb_fcc_2019") {
      df_50 <- df %>% filter(pct_bb_fcc_2019 <= 0.5)
      pop_vals$sum_50 <- df_50 %>% dplyr::summarise(sum_50 = sum(pop2018)) %>% pull(sum_50)
      pop_vals$sum_50_county <- df_50 %>% dplyr::summarise(sum_50_county = n()) %>% pull(sum_50_county)
      pop_vals$sum_pop <- df %>% dplyr::summarise(sum_pop = sum(pop2018)) %>% pull(sum_pop)
      pop_vals$sum_county <- df %>% dplyr::summarise(sum_county = n()) %>% pull(sum_county)
    } else if (input$bbtype == "pct_broadband_MS") {
      df_50 <- df %>% filter(pct_broadband_MS <= 0.5)
      pop_vals$sum_50 <- df_50 %>% dplyr::summarise(sum_50 = sum(pop2018)) %>% pull(sum_50)
      pop_vals$sum_50_county <- df_50 %>% dplyr::summarise(sum_50_county = n()) %>% pull(sum_50_county)
      pop_vals$sum_pop <- df %>% dplyr::summarise(sum_pop = sum(pop2018)) %>% pull(sum_pop)
      pop_vals$sum_county <- df %>% dplyr::summarise(sum_county = n()) %>% pull(sum_county)
    } else if (input$bbtype == "pct_fixed_acs_2018") {
      df_50 <- df %>% filter(pct_fixed_acs_2018 <= 0.5)
      pop_vals$sum_50 <- df_50 %>% dplyr::summarise(sum_50 = sum(pop2018)) %>% pull(sum_50)
      pop_vals$sum_50_county <- df_50 %>% dplyr::summarise(sum_50_county = n()) %>% pull(sum_50_county)
      pop_vals$sum_pop <- df %>% dplyr::summarise(sum_pop = sum(pop2018)) %>% pull(sum_pop)
      pop_vals$sum_county <- df %>% dplyr::summarise(sum_county = n()) %>% pull(sum_county)
    } else if (input$bbtype == "pct_broadband_mlab") {
      df_50 <- df %>% filter(pct_broadband_mlab <= 0.5)
      pop_vals$sum_50 <- df_50 %>% dplyr::summarise(sum_50 = sum(pop2018)) %>% pull(sum_50)
      pop_vals$sum_50_county <- df_50 %>% dplyr::summarise(sum_50_county = n()) %>% pull(sum_50_county)
      pop_vals$sum_pop <- df %>% dplyr::summarise(sum_pop = sum(pop2018)) %>% pull(sum_pop)
      pop_vals$sum_county <- df %>% dplyr::summarise(sum_county = n()) %>% pull(sum_county)
    }
  })
  
  # Value boxes
  output$vbox_county <- renderValueBox({
    subtitle_text <- if (input$bbtype == "pct_bb_fcc_2019") {
      paste0("Counties", " out of ", pop_vals$sum_county, " had", " less than 50% broadband availability")
    } else if (input$bbtype == "pct_broadband_MS") {
      paste0("Counties", " out of ", pop_vals$sum_county, " had", " less than 50% of population use broadband","at 25/3Mbps")
    } else if (input$bbtype == "pct_fixed_acs_2018") {
      paste0("Counties", " out of ", pop_vals$sum_county, " had", " less than 50% fixed broadband subsciprion rate")
    } else if (input$bbtype == "pct_broadband_mlab") {
      paste0("Counties", " out of ", pop_vals$sum_county, " had", " less than 50% of population test for","broadband at 25/3Mbps")
    }
    valueBox(pop_vals$sum_50_county, subtitle = subtitle_text, color = "yellow")
  })
  
  output$vbox_county_pct <- renderValueBox({
    subtitle_state <- if (input$state == "TX") {
      "Texas"
    } else if (input$state == "ME") {
      "Maine"
    } else if (input$state == "KS") {
      "Kansas"
    } 
    subtitle_bb <- if (input$bbtype == "pct_bb_fcc_2019") {" population live in counties with less than 50% broadband availability"} 
    else if (input$bbtype == "pct_broadband_MS") {" population live in counties where less than 50% of its population use broadband at 25/3Mbps"}
    else if (input$bbtype == "pct_fixed_acs_2018") {" population live in counties with less than 50% fixed broadband subscription rate"}
    else if (input$bbtype == "pct_broadband_mlab") {" population live in counties where less than 50% of its population tested for broadband at 25/3Mbps"}
    subtitle_text <- paste0("of ", subtitle_state, subtitle_bb)
    valueBox(paste0(round((pop_vals$sum_50/pop_vals$sum_pop)*100, 0),"%"), subtitle = subtitle_text, color = "blue")
  })
  
  ## Keeping a subtitle draft here for later use if necessary
  # if (input$bbtype == "pct_bb_fcc_2019") {
  #   paste0("of ", subtitle_state, " population live in counties with less than 50% broadband availability")
  # } else if (input$bbtype == "pct_broadband_MS") {
  #   paste0("of ", subtitle_state, " population live in counties where less than half of its population used broadband at 25/3Mbps")
  # } else if (input$bbtype == "pct_fixed_acs_2018") {
  #   paste0("of ", subtitle_state, " population live in counties with less than 50% fixed broadband subsciprion rate")
  # } else if (input$bbtype == "pct_broadband_mlab") {
  #   paste0("of ", subtitle_state, " population live in counties where less than 50% its population reported broadband at 25/3Mbps")
  # }
  
  # Frequency histograms
  output$hist <- renderPlotly({
    ggplot(st_reactive(), aes_string(x = req(input$bbtype))) + 
      geom_histogram(fill = "orangered3") + theme_minimal() + 
      theme(axis.text = element_text(face = "bold"),
            axis.title = element_text(face = "bold")) + 
      xlab(currentbb_label()) + ylab("Number of Counties") +
      scale_y_continuous(breaks = scales::breaks_pretty()) +
      scale_x_continuous(labels = scales::percent, breaks = scales::breaks_pretty(),
                         limits = c(0,1.1))
    })
  
  output$hist_entrepreneurship <- renderPlotly({
    if (input$entmeasure != "venturedensity_mean") {
      ggplotly(ggplot(st_reactive(), aes_string(x = req(input$entmeasure))) + 
        geom_histogram(fill = "orangered3") + theme_minimal() + 
        theme(axis.text = element_text(face = "bold"),
              axis.title = element_text(face = "bold")) + 
        xlab(currentent_label()) + ylab("Number of Counties") +
        scale_y_continuous(breaks = scales::breaks_pretty()) +
        scale_x_continuous(labels = scales::percent, breaks = scales::breaks_pretty(), limits = c(0,1.1)))
    } else if (input$entmeasure == "venturedensity_mean") {
      ggplotly(ggplot(st_reactive(), aes_string(x = req(input$entmeasure))) + 
        geom_histogram(fill = "orangered3") + theme_minimal() + 
        theme(axis.text = element_text(face = "bold"),
              axis.title = element_text(face = "bold")) + 
        xlab(currentent_label()) + ylab("Number of Counties") +
        scale_y_continuous(breaks = scales::breaks_pretty()) +
        scale_x_continuous(breaks = scales::breaks_pretty()))
    }
  })
  
  # Line graphs for entrepreneurship
  output$line_est10 <- renderPlot({
    print(cbp_reactive())
    ggplot(cbp_reactive(), aes(year, value, group = 1, color = var_fact)) + geom_line(size = 2.5) + 
      geom_point(size = 6) + geom_point(size = 4, color = "white") + 
      theme_minimal() + theme(plot.title = element_text(face = "bold", size = 14),
                              plot.subtitle = element_text(size = 9, face = "italic"),
                              legend.position = "none",
                              axis.text = element_text(size = 11),
                              axis.title = element_text(size = 12, face = "bold")) +
      labs(title = "% of Establishments with <10 Employees (2014-2018)", subtitle = "Source: County Business Patterns (Census Bureau)",
           x = "Year", y = "%") + 
      scale_y_continuous(labels = scales::percent,
                         breaks = scales::breaks_pretty()) +
      scale_color_manual(values = "royalblue4")
  })
  
  output$line_nonfarm <- renderPlot({
    print(bea_reactive())
    ggplot(bea_reactive(), aes(year, value, group = var_fact, color = var_fact)) + geom_line(size = 2.5) + 
      geom_point(size = 6) + geom_point(size = 4, color = "white") + 
      theme_minimal() + theme(plot.title = element_text(face = "bold", size = 14),
                              plot.subtitle = element_text(size = 9, face = "italic"),
                              axis.text = element_text(size = 11),
                              axis.title = element_text(size = 12, face = "bold")) +
      labs(title = "Share of Proprietors/Non-farm Proprietors (2014-2018)",
           subtitle = "Source: Bureau of Economic Analysis",
           x = "Year", y = "%", color = "Colors") + 
      scale_y_continuous(limits = c(min(bea_reactive()[,"value"]),max(bea_reactive()[,"value"])),
                         labels = scales::percent,
                         breaks = scales::breaks_pretty()) +
      scale_color_manual(labels = c("% of Proprietors", "% of Nonfarm Proprietors"),
                         values = c("red4","royalblue4"))
  })
    
    
  # Leaflet maps
  output$map <- renderLeaflet({
    leaflet(st_reactive()) %>% addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -99, lat = 45, zoom = 10)
  })
  
  output$map2 <- renderLeaflet({
    leaflet(st_reactive()) %>% addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = -99, lat = 45, zoom = 10)
  })
  
  # Dynamic lng & lat
  lonlat <- reactive({
    allstate_import_geo_sf %>% filter(ST == req(input$state)) %>% select(ST, INTPTLAT, INTPTLON) %>% 
      group_by(ST) %>% summarise(lng = median(as.numeric(INTPTLON), na.rm = T),
                                 lat = median(as.numeric(INTPTLAT), na.rm = T))
  })
  
  # Broadband Observe
  observe({
    proxy <- leafletProxy("map", data = st_reactive())
    if (input$bbtype == "pct_bb_fcc_2019") {
      proxy %>% clearControls() %>% 
        addPolygons(stroke = F, smoothFactor = 0.2, fillOpacity = 0.9,
                          color = ~pal(pct_bb_fcc_2019)) %>% 
        setView(lng = lonlat()[,"lng"], lat = lonlat()[,"lat"], zoom = 6) %>% 
        addLegend("bottomright", pal = pal, values = ~pct_bb_fcc_2019,
                  title = "FCC Broadband (%)",
                  labFormat = labelFormat(suffix = "%", transform = function(x) 100*x), na.label = "N/A", opacity = 1)
    }
    else if (input$bbtype == "pct_broadband_MS") {
      proxy %>% clearControls() %>% 
        addPolygons(stroke = F, smoothFactor = 0.2, fillOpacity = 0.9,
                            color = ~pal(pct_broadband_MS)) %>% 
        setView(lng = lonlat()[,"lng"], lat = lonlat()[,"lat"], zoom = 6) %>% 
        addLegend("bottomright", pal = pal, values = ~pct_broadband_MS,
                  title = "MS Broadband (%)",
                  labFormat = labelFormat(suffix = "%", transform = function(x) 100*x), na.label = "N/A", opacity = 1)
      }
    else if (input$bbtype == "pct_fixed_acs_2018") {
      proxy %>% clearControls() %>% 
        addPolygons(stroke = F, smoothFactor = 0.2, fillOpacity = 0.9,
                            color = ~pal(pct_fixed_acs_2018)) %>% 
        setView(lng = lonlat()[,"lng"], lat = lonlat()[,"lat"], zoom = 6) %>% 
        addLegend("bottomright", pal = pal, values = ~pct_fixed_acs_2018,
                  title = "ACS Broadband (%)",
                  labFormat = labelFormat(suffix = "%", transform = function(x) 100*x), na.label = "N/A", opacity = 1)
    }
    else if (input$bbtype == "pct_broadband_mlab") {
      proxy %>% clearControls() %>% 
        addPolygons(stroke = F, smoothFactor = 0.2, fillOpacity = 0.9,
                            color = ~pal(pct_broadband_mlab)) %>% 
        setView(lng = lonlat()[,"lng"], lat = lonlat()[,"lat"], zoom = 6) %>% 
        addLegend("bottomright", pal = pal, values = ~pct_broadband_mlab,
                  title = "M-Lab Broadband (%)",
                  labFormat = labelFormat(suffix = "%", transform = function(x) 100*x), na.label = "N/A", opacity = 1)
    }
  })
  
  # Entrepreneurship Observe
  observe({
    proxy <- leafletProxy("map2", data = st_reactive())
    if (input$entmeasure == "pct_10_est_cbp_2018") {
      proxy %>% clearControls() %>% 
        addPolygons(stroke = F, smoothFactor = 0.2, fillOpacity = 0.9,
                    color = ~pal(pct_10_est_cbp_2018)) %>% 
        setView(lng = lonlat()[,"lng"], lat = lonlat()[,"lat"], zoom = 6) %>% 
        addLegend("bottomright", pal = pal, values = ~pct_10_est_cbp_2018,
                  title = "% of Establishments<br>(<10 Employees)",
                  labFormat = labelFormat(suffix = "%", transform = function(x) 100*x), na.label = "N/A", opacity = 1)
    }
    else if (input$entmeasure == "pct_nonfarm_bea_2018") {
      proxy %>% clearControls() %>% 
        addPolygons(stroke = F, smoothFactor = 0.2, fillOpacity = 0.9,
                    color = ~pal(pct_nonfarm_bea_2018)) %>% 
        setView(lng = lonlat()[,"lng"], lat = lonlat()[,"lat"], zoom = 6) %>% 
        addLegend("bottomright", pal = pal, values = ~pct_nonfarm_bea_2018,
                  title = "% of Nonfarm<br>Proprietors (2018)",
                  labFormat = labelFormat(suffix = "%", transform = function(x) 100*x), na.label = "N/A", opacity = 1)
    }
    else if (input$entmeasure == "venturedensity_mean") {
      proxy %>% clearControls() %>% 
        addPolygons(stroke = F, smoothFactor = 0.2, fillOpacity = 0.9,
                    color = ~pal2(venturedensity_mean)) %>% 
        setView(lng = lonlat()[,"lng"], lat = lonlat()[,"lat"], zoom = 6) %>% 
        addLegend("bottomright", pal = pal2, values = ~venturedensity_mean,
                  title = "Venture Density<br>(2018-2019)", opacity = 1)
    }
  })
  
}


shinyApp(ui = ui, server = server)