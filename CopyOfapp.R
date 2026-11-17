# ============================================================
# FORMULA 1 SHINY APP
# Season selector and race winners only
# ============================================================
#
# This deliberately small app demonstrates the complete Shiny link:
#
#   selectInput()
#        -> input$overview_year
#        -> reactive data
#        -> renderUI()
#        -> uiOutput()
#
# Expected folder structure:
#
#   f1_data/
#   |-- race_winners_app.R
#   |-- results.csv
#   |-- races.csv
#   |-- drivers.csv
#   `-- constructors.csv
#
# If your CSV files are instead inside a subfolder called "data",
# change the file_path line in read_f1_csv() to:
#
#   file_path <- file.path("data", file_name)
#
# Install the packages once in the R Console if required:
#
# install.packages(c("shiny", "tidytable", "janitor"))
# ============================================================


# ------------------------------------------------------------
# 1. LOAD PACKAGES
# ------------------------------------------------------------

library(shiny)
library(tidytable)


# ------------------------------------------------------------
# 2. CREATE A REUSABLE CSV IMPORT FUNCTION
# ------------------------------------------------------------

read_f1_csv <- function(file_name) {
  
  # This version assumes the CSV files are in the same folder as
  # this R script.
  file_path <- file.path("./data", file_name)
  
  # Stop with a clear message if a required file cannot be found.
  if (!file.exists(file_path)) {
    stop(paste("Missing file:", file_path))
  }
  
  # Import the CSV and convert common missing-value codes to NA.
  data <- read.csv(
    file_path,
    na.strings = c("\\N", "", "NA"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # Convert names such as raceId to race_id.
  janitor::clean_names(data)
}


# ------------------------------------------------------------
# 3. IMPORT THE FOUR TABLES NEEDED FOR THIS APP
# ------------------------------------------------------------

races <- read_f1_csv("races.csv")
results <- read_f1_csv("results.csv")
drivers <- read_f1_csv("drivers.csv")
constructors <- read_f1_csv("constructors.csv")


# ------------------------------------------------------------
# 4. CLEAN VARIABLES USED BY THE APP
# ------------------------------------------------------------

races <- races |>
  tidytable::mutate(year = as.integer(year),
                    round = as.integer(round))

results <- results |>
  tidytable::mutate(position = as.integer(position),
                    position_order = as.integer(position_order))


# ------------------------------------------------------------
# 5. CREATE SMALL LOOKUP TABLES
# ------------------------------------------------------------
#
# A lookup table keeps only the descriptive fields needed for a join.
# The rename() and select() steps are separate to make their purposes
# easier to follow.

race_lookup <- races |>
  tidytable::rename(race_name = name,
                    race_date = date) |>
  tidytable::select(race_id, year, round, race_name, race_date)

driver_lookup <- drivers |>
  tidytable::mutate(driver_name = paste(forename, surname)) |>
  tidytable::select(driver_id,driver_name)

constructor_lookup <- constructors |>
  tidytable::rename(constructor_name = name) |>
  tidytable::select(constructor_id, constructor_name
  )

head_to_head_results <- results |>
  tidytable::left_join(drivers |>
      tidytable::mutate(driver_name = paste(forename, surname)) |>
      tidytable::select(driver_id,driver_name), by = "driver_id") |>
  tidytable::left_join(races |>
      tidytable::select(race_id, year, round, race_name = name), by = "race_id") |>
  tidytable::select(race_id, year, round, race_name, driver_id, driver_name,
                    constructor_id, grid, position, points, status_id) |>
  tidytable::distinct(race_id, driver_id, .keep_all = TRUE)

# ------------------------------------------------------------
# 6. CREATE THE DASHBOARD-READY RACE RESULTS TABLE
# ------------------------------------------------------------
#
# results is the starting table because one row represents one driver
# in one race. The joins replace IDs with readable race, driver and
# constructor information.
#
# Link between source tables:
#
#   races -------- race_id --------\
#                                  \
#   drivers ------ driver_id -------- results
#                                  /
#   constructors - constructor_id -/
#
# race_results still has one row per driver per race.

race_results <- results |>
  tidytable::left_join(race_lookup, by = "race_id") |>
  tidytable::left_join(driver_lookup, by = "driver_id") |>
  tidytable::left_join(constructor_lookup, by = "constructor_id") |>
  tidytable::arrange(year, round, position_order)


# ------------------------------------------------------------
# 7. CREATE THE USER INTERFACE
# ------------------------------------------------------------
#
# The UI creates:
#   1. A season dropdown
#   2. An empty output area for the winners
#
# inputId = "overview_year" creates input$overview_year in server.
#
# uiOutput("overview_winners") is a placeholder. Its content is
# supplied by output$overview_winners in server.

ui <- shiny::fluidPage(
  
  shiny::titlePanel("Formula 1 Race Winners"),
  shiny::selectInput(
    inputId = "overview_year",
    label = "Select season:",
    
    # unique() removes repeated years.
    # sort(..., decreasing = TRUE) puts the newest season first.
    choices = sort(unique(race_results$year), decreasing = TRUE),
    
    # Select the latest available season when the app opens.
    selected = max(race_results$year, na.rm = TRUE)),
  
  shiny::hr(),
  
  shiny::h3(
    "Race winners"),

  shiny::uiOutput(
    outputId = "overview_winners")
)
  nav_panel(
    "Head-to-Head",
    
    layout_sidebar(
      
      sidebar = sidebar(
        
        selectInput(
          inputId = "comparison_year",
          label = "Select season:",
          choices = NULL
        ),
        
        selectizeInput(
          inputId = "comparison_driver_1",
          label = "Select first driver:",
          choices = NULL
        ),
        
        selectizeInput(
          inputId = "comparison_driver_2",
          label = "Select second driver:",
          choices = NULL
        )
      ),
      
      layout_columns(
        
        value_box(
          title = "Race head-to-head",
          value = textOutput("comparison_race_score")
        ),
        
        value_box(
          title = "Wins",
          value = textOutput("comparison_wins")
        ),
        
        value_box(
          title = "Podiums",
          value = textOutput("comparison_podiums")
        ),
        
        value_box(
          title = "Points",
          value = textOutput("comparison_points")
        ),
        
        col_widths = c(3, 3, 3, 3)
      ),
      
      card(
        full_screen = TRUE,
        
        card_header(
          textOutput("comparison_chart_title")
        ),
        
        plotlyOutput(
          outputId = "comparison_position_chart",
          height = "550px"
        )
      ),
      
      card(
        full_screen = TRUE,
        
        card_header("Race-by-race comparison"),
        
        tableOutput("comparison_table")
      )
    )
  )



# ------------------------------------------------------------
# 8. CREATE THE SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # 8A. FILTER RACE RESULTS TO THE SELECTED SEASON
  # ----------------------------------------------------------
  #
  # If the user chooses 2024, input$overview_year contains "2024".
  # selectInput values normally arrive as text, so as.integer()
  # converts the selected value to a number before comparison.
  #
  # Because this is reactive(), it recalculates automatically when
  # the user selects a different season.
  
  overview_data <- shiny::reactive({
    
    # Do not continue until a year selection is available.
    shiny::req(input$overview_year)
    
    race_results |>
      tidytable::filter(year == as.integer(input$overview_year))
  })
  
  
  # ----------------------------------------------------------
  # 8B. FIND AND DISPLAY THE WINNER OF EACH RACE
  # ----------------------------------------------------------
  #
  # output$overview_winners links to:
  #
  #   uiOutput(outputId = "overview_winners")
  #
  # renderUI() is used because the output contains multiple pieces
  # of formatted HTML for every race, rather than one text value.
  
  output$overview_winners <- shiny::renderUI({
    
    # Start with the currently selected season, retain first-place
    # finishes, and put the races into round order.
    winners <- overview_data() |>
      tidytable::filter(position == 1) |>
      tidytable::arrange(round)
    
    
    # Display a helpful message if the selected season has no winners.
    if (nrow(winners) == 0) {
      return(
        shiny::p(
          "No race-winner information is available for this season.")
        )
    }
    
    
    # tagList() combines all the individual race blocks into one UI
    # result that can be displayed by uiOutput().
    shiny::tagList(
      
      # seq_len(nrow(winners)) creates 1, 2, 3 ... up to the number
      # of winner rows. lapply() creates one display block per row.
      lapply(
        seq_len(nrow(winners)),
        function(i) {
          
          shiny::div(
            
            # Basic styling for each race-winner block.
            style = paste0(
              "background-color: #f5f5f5; ",
              "border-left: 5px solid #e10600; ",
              "border-radius: 5px; ",
              "margin-bottom: 10px; ",
              "padding: 12px;"),
            
            # Race heading, for example:
            # Round 1: Bahrain Grand Prix
            shiny::strong(
              paste0("Round ", winners$round[i], ": ", winners$race_name[i])),
            
            shiny::tags$br(),
            
            # Winning driver.
            shiny::span(
              winners$driver_name[i],
              style = "font-size: 16px; font-weight: bold;"),
            
            shiny::tags$br(),
            
            # Winning constructor.
            shiny::span(
              winners$constructor_name[i],
              style = "color: #666666;")
          )
        }
      )
    )
  })
}


# ------------------------------------------------------------
# 9. RUN THE SHINY APP
# ------------------------------------------------------------
#
# This connects the ui object to the server function.

shiny::shinyApp(ui, server)


# ============================================================
# COMPLETE LINK TO REMEMBER
# ============================================================
#
# selectInput(inputId = "overview_year")
#              |
#              v
# input$overview_year
#              |
#              v
# overview_data <- reactive({ filter(year == selected year) })
#              |
#              v
# winners <- overview_data() |> filter(position == 1)
#              |
#              v
# output$overview_winners <- renderUI(...)
#              |
#              v
# uiOutput("overview_winners")
#
# ============================================================
