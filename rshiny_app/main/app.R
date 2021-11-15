#### File Comments ----
# List of sections, just in case code collapsing isn't possible
# Please add to the list if more sections are introduced
# 
# Use the following keywords to find the section you're looking for:
# - Initial_Setup
# - Load_Data
# - Home_Page
# - Cases_Page
# - Variants_Page
# - Sequencing_Page
# - UI_Compilation
# - Custom_Server_Functions
# - Server_Logic
# - App_Start

####  Initial Setup ----
# Set working directory ( change this based on where your project lives )
# setwd('~/Codebases/r/tba3103/project_2/')

# Load required libraries
library(ggplot2)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(shiny)
library(shinydashboard)


#### Load_Data ----
# our assumption is that the rShiny server & datasource is being hosted in the same URL.
# swap the data_source_url if that is not the case
data_source_url <- 'http://localhost/'
# data_source_url <- 'http://159.65.134.12/'

df_cases <- read.csv(paste0(data_source_url, 'cases'))
df_variants <- read.csv(paste0(data_source_url, 'variants'))
df_sequences <- read.csv(paste0(data_source_url, 'sequences'))

# set a new column as a date object for all 3 dataframes
df_cases$date <- as.Date(df_cases$date_monday)
df_variants$date <- as.Date(df_variants$date_monday)
df_sequences$date <- as.Date(df_sequences$date_monday)

# df <- read.csv("eu_covid_variants_cleaned.csv")
# df_for_bp <- read.csv("eu_covid_variants_with_date.csv")
# df$date <- as.Date(df$date_monday)
# df_for_bp$date <- as.Date(df_for_bp$date_monday)
# year_weeks <- factor(unique(df$year_week), ordered = TRUE)
# year_week_min <- min(year_weeks) # min year week ordered factor 
# year_week_max <- max(year_weeks) # max year week ordered factor 

# Load the constants that we'd need for the filter
min_date <- min(df_cases$date)
max_date <- max(df_cases$date)
regions <- unique(df_cases$region)
countries <- unique(df_cases$country) 


#### Constants ----
colour_palette <- c(
  '#0a4d2d', # dark green
  '#089050', # green
  '#70d038', # light green
  '#b4e448', # lime
  '#70e5ff', # bright(?) blue
  '#6c94f0', # light blue
  '#3952c4', # blue
  '#402c99', # dark blue
  '#3b1c70', # very dark(?) blue
  '#6a24a8', # purple
  '#9c45e3', # light purple
  '#c380ff', # lavender
  '#ea9efc', # pink
  '#cc4f8f', # dark pink
  '#f46fa6', # hot pink
  '#ff4050', # light red
  '#d4151f', # red
  '#a30f11', # dark red
  '#751e21', # very dark(?) red
  '#7a3b32', # very dark(?) brown
  '#995943', # dark brown
  '#b87754', # brown
  '#dead59', # light brown(?)
  '#ffc96b', # light orange
  '#f28135', # dark orange
  '#ffb12d', # orange
  '#ffea70', # yellow
  '#fff7c4', # light yellow
  '#c7b48b', # clay(?)
  '#786654', # dark clay(?)
  '#403631', # coffee(?)
  '#46516e', # navy(?)
  '#858fab', # light navy(?)
  '#b8c0d9', # very light navy(?)
  '#d7d9ff', # very light purple(?)
  '#cccccc', # light gray
  '#999999', # gray
  '#4d4d4d'  # dark gray
)


###### Shared_UI_Elements ----
common_map_themes <- theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 22)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 0.9, size = 10)) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.title = element_text(size = 16)) +
  theme(plot.margin = unit(c(3,3,6,3), "mm")) +
  theme(legend.position = "none")

get_filter_widgets <- function(page){
  return(
    tabBox(
      width = 3,
      title = "",
      # tabPanel(
      #   "Legend",
      #   tableOutput(paste0(page, "_legend"))
      # ),
      tabPanel(
        "Filters",
        dateRangeInput(
          paste0(page, "_filter_date_range"), 
          "Date range",
          min = min_date,
          max = max_date,
          start = min_date,
          end = max_date
        ),
        radioButtons(
          paste0(page, "_filter_by"), 
          "Filter By", 
          c("Region" = "region", "Country" = "country"),
          selected = "region"
        ),
        conditionalPanel(
          condition = paste0("input.", page, "_filter_by == 'region'"),
          checkboxGroupInput(paste0(page, "_regions_checkbox"), "EU Regions selection", regions, selected = regions)
        ),
        conditionalPanel(
          condition = paste0("input.", page, "_filter_by == 'country'"),
          strong("EU Countries selection"),
          fluidRow(
            column(4,actionButton(paste0(page, "_select_all"), label = "Select All")),
            column(4,actionButton(paste0(page, "_select_none"), label = "Select None"))
          ),
          checkboxGroupInput(paste0(page, "_countries_checkbox"), NULL, countries, selected = countries)
        )
      )
    )
  )
}


######  Home_Page ----
home_page <- tabItem(
  tabName = "home",
  uiOutput("homepage")
)


######  Cases_Page ----
cases_page <- tabItem(
  tabName = "cases",
  fluidPage(
    h2("Cases"),
    fluidRow(
      valueBoxOutput("cases_total_new_cases")
    ),
    fluidRow(
      column(
        9,
        fluidRow(
          box(
            width = 12,
            title = "Bubble map of total cases",
            leafletOutput("cases_bubble_map")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Line chart of total cases",
            plotlyOutput("cases_line_chart")
          )
        )
      ),
      get_filter_widgets('cases')
    )
  )
)


######  Variants_Page ----
variants_page <- tabItem(
  tabName = "variants",
  fluidPage(
    h2("Variants"),
    fluidRow(
      valueBoxOutput("variants_total_sequenced_cases"),
      valueBoxOutput("variants_most_sequenced_variant")
    ),
    fluidRow(
      column(
        9,
        fluidRow(
          box(
            width = 6,
            title = "Variants detected",
            status = "warning",
            plotlyOutput("variants_stacked_bar")
          ),
          box(
            width = 6,
            status = "danger",
            title = "% of variants detected",
            plotlyOutput("variants_stacked_bar_percentage")
          ),
          uiOutput("variants_individual_filtered_by_boxes")
        )
      ),
      get_filter_widgets("variants")
    )
  )
)


######  Sequencing_Page ----
sequencing_page <- tabItem(
  tabName = "sequencing",
  fluidPage(
    h2("Sequences"),
    fluidRow(
      valueBoxOutput("sequences_total_new_cases"),
      valueBoxOutput("sequences_total_sequenced_cases"),
      valueBoxOutput("sequences_unknown_sequenced_cases")
    ),
    fluidRow(
      column(
        9,
        fluidRow(
          box(
            width = 12,
            title = "% of cases sequenced",
            plotlyOutput("sequences_boxplot")
          ),
          uiOutput("sequences_individual_filtered_by_boxes")
        )
      ),
      get_filter_widgets("sequences")
    )
  )
)

########   UI_Compilation ----
header <- dashboardHeader(title = "Euro Variant Tracker")

sidebar <- dashboardSidebar(
  sidebarMenuOutput("sidebar")
)

body <- dashboardBody(
  tabItems(
    home_page,
    cases_page,
    variants_page,
    sequencing_page
  )
)

ui <- dashboardPage(
  header, 
  sidebar, 
  body,
  skin = "blue"
)


########   Custom_Server_Functions ----
custom_plot_stacked_bar_chart <- function(df, x_axis_label, y_axis_label, 
                                          geom_bar_stat = geom_bar(stat="identity"),
                                          y_scale = scales::comma){
  return(
    ggplot(df, aes(x = date, y = number_detections_variant,fill = variant)) +
      geom_bar_stat +
      labs(x = x_axis_label, y = y_axis_label) +
      scale_y_continuous(expand = c(0,0), labels = y_scale) + 
      common_map_themes
  )
}

custom_plot_bar_chart <- function(df, x_axis_label, y_axis_label,
                                  geom_bar_stat = geom_bar(stat="identity"),
                                  y_scale = scales::comma){
  return(
    ggplot(df, aes(x = date, y = number_sequenced, fill = "orange")) +
    geom_bar_stat +
      labs(x = x_axis_label, y = y_axis_label) +
      scale_y_continuous(expand = c(0,0), labels = y_scale) + 
      common_map_themes
  )
}


########   Server_Logic ----
server <- function(input, output, session){
  #### Reactive value for user information ----
  user_info <- reactiveValues(
    showLoginScreen = FALSE,
    loggedInUser = FALSE
  )
  
  #### Observables for login pages ----
  observe({
    if(!is.null(input$show_login_page_button) && input$show_login_page_button > 0 ){
      user_info$showLoginScreen <- TRUE
    }
  })
  
  observeEvent(input$login_button, {
    if(input$login_username_input == "researcher" && input$login_password_input == "covid"){
      user_info$loggedInUser <- TRUE
      user_info$showLoginScreen <- FALSE
      }
   })
  
  observeEvent(input$logout_button, {
    user_info$loggedInUser <- FALSE
    user_info$showLoginScreen <- FALSE
  })
  
  #### Observables for select all/none countries ----
  observe({
    if(input$cases_select_all > 0){
      updateCheckboxGroupInput(session = session,
                               inputId = "cases_countries_checkbox",
                               choices = countries,
                               selected = countries)
    }
  })

  observe({
    if(input$cases_select_none > 0){
      updateCheckboxGroupInput(session = session,
                               inputId = "cases_countries_checkbox",
                               choices = countries,
                               selected = NULL)
    }
  })
   
  observe({
    if(input$variants_select_all > 0){
      updateCheckboxGroupInput(session = session,
                               inputId = "variants_countries_checkbox",
                               choices = countries,
                               selected = countries)
    }
  })
  
  observe({
    if(input$variants_select_none > 0){
      updateCheckboxGroupInput(session = session,
                               inputId = "variants_countries_checkbox",
                               choices = countries,
                               selected = NULL)
    }
  })
  
  observe({
    if(input$sequences_select_all > 0){
      updateCheckboxGroupInput(session = session,
                               inputId = "sequences_countries_checkbox",
                               choices = countries,
                               selected = countries)
    }
  })
  
  observe({
    if(input$sequences_select_none > 0){
      updateCheckboxGroupInput(session = session,
                               inputId = "sequences_countries_checkbox",
                               choices = countries,
                               selected = NULL)
    }
  })
  
  #### UI processing for sidebar ----
  output$sidebar <- renderMenu({
    if(user_info$loggedInUser == TRUE){
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Cases", tabName = "cases", icon = icon("ambulance")),
        menuItem("Variants", tabName = "variants", icon = icon("disease")),
        menuItem("Sequencing", tabName = "sequencing", icon = icon("microscope"))
      )
    } else {
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Cases", tabName = "cases", icon = icon("ambulance"))
      )
    }
  })
  
  
  #### UI processing for home page ----
  output$homepage <- renderUI({
    if(user_info$showLoginScreen == TRUE){
      fluidPage(
        h2("Login"),
        textInput(
          "login_username_input",
          h3("Username")
        ),
        passwordInput(
          "login_password_input",
          h3("Password")
        ),
        actionButton(
          "login_button",
          class = "btn-primary",
          label = "Sign In",
          style = "color: #fff"
        )
      )
    } else {
      if(user_info$loggedInUser == TRUE){
        fluidPage(
          h2("Homepage"),
          actionButton(
            "logout_button",
            class = "btn-danger",
            icon = icon("sign-out-alt"),
            label = "Logout",
            style = "color: #fff"
          )
        )
      } else {
        fluidPage(
          h2("Homepage"),
          actionButton(
            "show_login_page_button",
            class = "btn-info",
            label = "Sign In",
            style = "color: #fff"
          )
        )
      }
    }
  })
  #### Data processing for cases page ----
  # Calculate a dataframe based on the filters
  filtered_cases_df <- reactive({
    filtered_df <- df_cases[df_cases$date >= as.Date(input$cases_filter_date_range[1]) &
                              df_cases$date <= as.Date(input$cases_filter_date_range[2]),]
    
    if(input$cases_filter_by == 'region'){
      filtered_df <- filtered_df[filtered_df$region %in% input$cases_regions_checkbox,]
      filtered_df$location <- filtered_df$region
      filtered_df$lat <- filtered_df$region_lat
      filtered_df$lng <- filtered_df$region_lng
    } else{
      filtered_df <- filtered_df[filtered_df$country %in% input$cases_countries_checkbox,]
      filtered_df$location <- filtered_df$country
      filtered_df$lat <- filtered_df$capital_lat
      filtered_df$lng <- filtered_df$capital_lng
    }
    
    filtered_df
  })
  
  legend_cases_named_list <- reactive({
    locations <- unique(filtered_cases_df()$location)
    colours <- colour_palette[1:length(locations)]
    names(colours <- locations)
    
    colours
  })
  
  # Render total cases box
  output$cases_total_new_cases <- renderValueBox({
    total_cases <- sum(filtered_cases_df()$new_cases)
    valueBox(
      formatC(total_cases, format="d", big.mark=","),
      "Total cases",
      icon = icon("notes-medical"),
      color = "purple"
    )
  })

  # Render cases' bubble map
  output$cases_bubble_map <- renderLeaflet({
    local_df <- filtered_cases_df() %>%
      group_by(location, lat, lng) %>%
      summarise(total_cases = sum(new_cases))
    max_total_cases <- max(local_df$total_cases)
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers( lng = as.numeric(local_df$lng),
                        lat = as.numeric(local_df$lat),
                        label = paste0(
                          "(", 
                          as.character(local_df$location), 
                          ") Total Cases: ", 
                          formatC(local_df$total_cases, format="d", big.mark=",")
                        ),
                        labelOptions = labelOptions(textsize = "20px", direction = "top"),
                        radius = 20 * local_df$total_cases / max_total_cases,
                        fillOpacity = 0.1) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  # Render cases' line chart
  output$cases_line_chart <- renderPlotly({
    local_df <- filtered_cases_df() %>%
      group_by(location, date) %>%
      summarise(total_cases = sum(new_cases))
    
    ggplot(local_df, aes(x = date, y = total_cases, group = location)) +
      geom_line(aes(color = location)) +
      labs(
        title = '', 
        x = 'Date', 
        y = 'New cases'
      ) +
      # scale_color_manual(values = legend_cases_named_list()) +
      scale_x_date(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), labels = scales::comma) +
      common_map_themes
  })
  
  
  #### Data processing for variants page ----
  # Calculate a dataframe based on the filters
  filtered_variants_df <- reactive({
    filtered_df <- df_variants[df_variants$date >= as.Date(input$variants_filter_date_range[1]) &
                              df_variants$date <= as.Date(input$variants_filter_date_range[2]),]
    
    if(input$variants_filter_by == 'region'){
      filtered_df <- filtered_df[filtered_df$region %in% input$variants_regions_checkbox,]
      filtered_df$location <- filtered_df$region
    } else{
      filtered_df <- filtered_df[filtered_df$country %in% input$variants_countries_checkbox,]
      filtered_df$location <- filtered_df$country
    }
    
    filtered_df
  })

  legend_variants_named_list <- reactive({
    variants <- unique(filtered_variants_df()$variant)
    colours <- colour_palette[1:length(variants)]
    names(colours <- variants)
    
    colours
  })
  
  # Render total cases box
  output$variants_total_sequenced_cases <- renderValueBox({
    total_cases <- sum(filtered_variants_df()$number_detections_variant)
    valueBox(
      formatC(total_cases, format="d", big.mark=","),
      "Sequenced cases",
      icon = icon("dna"),
      color = "blue"
    )
  })
  
  # Render most sequenced variant box
  output$variants_most_sequenced_variant <- renderValueBox({
    top_variant <- filtered_variants_df() %>% 
      group_by(variant) %>%
      summarise(total_count = sum(number_detections_variant)) %>%
      arrange(desc(total_count))
    
    valueBox(
      top_variant$variant[1],
      "Most frequent variant",
      icon = icon("search"),
      color = "yellow"
    )
  })

  # render variants graphs
  output$variants_stacked_bar <- renderPlotly({
    local_df <- filtered_variants_df() %>%
      group_by(variant, date) %>%
      summarise(number_detections_variant = sum(number_detections_variant))

    custom_plot_stacked_bar_chart(
      local_df,
      'Date',
      'Number of variants detected'
    )
  })

  output$variants_stacked_bar_percentage <- renderPlotly({
    local_df <- filtered_variants_df() %>%
      group_by(variant, date) %>%
      summarise(number_detections_variant = sum(number_detections_variant))
    
    custom_plot_stacked_bar_chart(
      local_df,
      'Date',
      '% of variants detected',
      geom_bar(position="fill", stat="identity"),
      scales::percent
    )
  })

  output$variants_individual_filtered_by_boxes <- renderUI({
    if(input$variants_filter_by == 'region'){
      local_df <- filtered_variants_df() %>%
        group_by(region, variant, date) %>%
        summarise(number_detections_variant = sum(number_detections_variant))
      
      # render variant's individual region boxes
      lapply(input$variants_regions_checkbox, function(selected_region){
        box(
          width = 6,
          title = selected_region,
          status = "primary",
          renderPlotly(
            custom_plot_stacked_bar_chart(
              local_df[local_df$region == selected_region,],
              'Date',
              'Number of variants detected'
            )
          )
        )
      })
    } else if (input$variants_filter_by == 'country'){
      # render variant's individual country boxes
      lapply(input$variants_countries_checkbox, function(selected_country){
        local_df <- filtered_variants_df()
        box(
          width = 6,
          title = selected_country,
          status = "primary",
          renderPlotly(
            custom_plot_stacked_bar_chart(
              local_df[local_df$country == selected_country,],
              'Date',
              'Number of variants detected'
            )
          )
        )
      })
    }
  })

  
  #### Data processing for sequences page ----
  # Calculate a dataframe based on the filters
  filtered_sequences_df <- reactive({
    filtered_df <- df_sequences[df_sequences$date >= as.Date(input$sequences_filter_date_range[1]) &
                                  df_sequences$date <= as.Date(input$sequences_filter_date_range[2]),]
    
    if(input$sequences_filter_by == 'region'){
      filtered_df <- filtered_df[filtered_df$region %in% input$sequences_regions_checkbox,]
      filtered_df$location <- filtered_df$region
    } else{
      filtered_df <- filtered_df[filtered_df$country %in% input$sequences_countries_checkbox,]
      filtered_df$location <- filtered_df$country
    }
    
    filtered_df
  })
  
  # Render total cases box
  output$sequences_total_new_cases <- renderValueBox({
    total_cases <- sum(filtered_sequences_df()$new_cases)
    valueBox(
      formatC(total_cases, format="d", big.mark=","),
      "Total cases",
      icon = icon("notes-medical"),
      color = "purple"
    )
  })
  output$sequences_total_sequenced_cases <- renderValueBox({
    sequenced_cases <- sum(filtered_sequences_df()$number_sequenced)
    valueBox(
      formatC(sequenced_cases, format="d", big.mark=","),
      "Sequenced cases",
      icon = icon("dna"),
      color = "blue"
    )
  })
  output$sequences_unknown_sequenced_cases <- renderValueBox({
    unknown_variant_cases <- sum(filtered_sequences_df()$unknown_variants)
    valueBox(
      formatC(unknown_variant_cases, format="d", big.mark=","),
      "Cases with unknown variants",
      icon = icon("question"),
      color = "red"
    )
  })
  
  # Render boxplot
  output$sequences_boxplot <- renderPlotly({
    local_df <- filtered_sequences_df() %>%
      group_by(date, location) %>%
      summarise(percentage_sequenced = sum(number_sequenced))

    max_sequenced <- max(local_df$percentage_sequenced)
    local_df$percentage_sequenced <- local_df$percentage_sequenced / max_sequenced
    
    
    ggplot(local_df, aes(date, percentage_sequenced)) + 
      geom_boxplot(fill = 'orange', outlier.colour = 'red')+
      labs(
        x = 'Date',
        y = 'Percentage of cases sequenced'
      ) +
      scale_y_continuous(expand = c(0,0), labels = scales::percent) + 
      common_map_themes
  })
  
  # Render individual regions/countries
  output$sequences_individual_filtered_by_boxes <- renderUI({
    if(input$sequences_filter_by == 'region'){
      local_df <- filtered_sequences_df() %>%
        group_by(region, date) %>%
        summarise(number_sequenced = sum(number_sequenced))
      
      # render variant's individual region boxes
      lapply(input$sequences_regions_checkbox, function(selected_region){
        box(
          width = 6,
          title = selected_region,
          status = "primary",
          renderPlotly(
            custom_plot_bar_chart(
              local_df[local_df$region == selected_region,],
              'Date',
              'Number of sequenced cases'
            )
          )
        )
      })
    } else if (input$sequences_filter_by == 'country'){
      # render variant's individual country boxes
      lapply(input$sequences_countries_checkbox, function(selected_country){
        local_df <- filtered_sequences_df()
        box(
          width = 6,
          title = selected_country,
          status = "primary",
          renderPlotly(
            custom_plot_bar_chart(
              local_df[local_df$country == selected_country,],
              'Date',
              'Number of sequenced cases'
            )
          )
        )
      })
    }
  })
}



########   App_Start ----
shinyApp(ui = ui, server = server)
