# ============================================================
# FORMULA 1 SHINY APP
# Race winners and driver head-to-head comparison
# ============================================================
#
# The app contains:
#
#   1. Race Winners tab
#   2. Driver Head-to-Head tab
#
# The head-to-head comparison:
#
#   - covers the latest 20 seasons in the data
#   - only compares drivers within the same season
#   - prevents the same driver being selected twice
#   - displays classified finishing positions
#   - displays DNF for drivers who did not finish
#   - displays DNS for drivers who did not start
#
# Expected folder structure:
#
#   f1_data/
#   |-- race_winners_app.R
#   `-- data/
#       |-- results.csv
#       |-- races.csv
#       |-- drivers.csv
#       |-- constructors.csv
#       `-- status.csv
#
# Install the packages once in the R Console if required:
#
# install.packages(
#   c(
#     "shiny",
#     "tidytable",
#     "janitor",
#     "ggplot2"
#   )
# )
# ============================================================


# ------------------------------------------------------------
# 1. LOAD PACKAGES
# ------------------------------------------------------------

library(shiny)
library(tidytable)
library(ggplot2)


# ------------------------------------------------------------
# 2. CREATE A REUSABLE CSV IMPORT FUNCTION
# ------------------------------------------------------------

read_f1_csv <- function(file_name) {
  
  # This version assumes the CSV files are inside a subfolder
  # called data.
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
# 3. IMPORT THE TABLES NEEDED FOR THE APP
# ------------------------------------------------------------

races <- read_f1_csv("races.csv")
results <- read_f1_csv("results.csv")
drivers <- read_f1_csv("drivers.csv")
constructors <- read_f1_csv("constructors.csv")
status <- read_f1_csv("status.csv")
driver_standings <- read_f1_csv("driver_standings.csv")
constructor_standings <- read_f1_csv("constructor_standings.csv")

# ------------------------------------------------------------
# 4. CLEAN VARIABLES USED BY THE APP
# ------------------------------------------------------------

races <- races |>
  tidytable::mutate(year = as.integer(year),
                    round = as.integer(round))

results <- results |>
  tidytable::mutate(position = as.integer(position),
                    position_order = as.integer(position_order),
                    grid = as.integer(grid),
                    points = as.numeric(points))

# ------------------------------------------------------------
# 5. CREATE SMALL LOOKUP TABLES
# ------------------------------------------------------------
#
# Lookup tables contain the descriptive fields needed for joins.
# They prevent unnecessary columns from being added to the final
# race-results table.

race_lookup <- races |>
  tidytable::rename(race_name = name,
                    race_date = date) |>
  tidytable::select(race_id, year, round, race_name, race_date)

driver_lookup <- drivers |>
  tidytable::mutate(driver_name = paste(forename, surname),
                    driver_label = paste0(surname, ", ", forename)) |>
  tidytable::select(driver_id, driver_name, driver_label)

constructor_lookup <- constructors |>
  tidytable::rename(constructor_name = name) |>
  tidytable::select(constructor_id, constructor_name)

status_lookup <- status |>
  tidytable::rename(race_status = status) |>
  tidytable::select(status_id, race_status)

# ------------------------------------------------------------
# 5b. CREATE CONSTRUCTOR COLOUR LOOKUP
# ------------------------------------------------------------
#
# These colours are used for the background of each race-winner
# card.
#
# Constructors not included here will use the default grey.

constructor_colours <- c(
  "Ferrari" = "#E80020",
  "McLaren" = "#FF8000",
  "Red Bull" = "#3671C6",
  "Mercedes" = "#27F4D2",
  "Aston Martin" = "#00665E",
  "Alpine F1 Team" = "#0090FF",
  "Williams" = "#64C4FF",
  "RB F1 Team" = "#6692FF",
  "Racing Bulls" = "#6692FF",
  "Haas F1 Team" = "#E6002B",
  "Sauber" = "#52E252",
  "Alfa Romeo" = "#981E32",
  "Renault" = "#FFF500",
  "Toro Rosso" = "#469BFF",
  "Force India" = "#F596C8",
  "Racing Point" = "#F596C8"
)

get_text_colour <- function(background_colour) {
  
  rgb_values <- grDevices::col2rgb(background_colour)
  
  brightness <- (
    rgb_values[1] * 299 +
      rgb_values[2] * 587 +
      rgb_values[3] * 114) / 1000
  
  if (brightness > 150) {
    "#000000"
  } else {
    "#FFFFFF"
  }
}
# ------------------------------------------------------------
# 6. CREATE THE DASHBOARD-READY RACE RESULTS TABLE
# ------------------------------------------------------------
#
# results is the starting table because one row represents one
# driver in one race.
#
# The joins add readable race, driver, constructor and result
# status information.
#
# distinct() ensures that there is only one row for each driver
# in each race.

race_results <- results |>
  tidytable::left_join(race_lookup, by = "race_id") |>
  tidytable::left_join(driver_lookup, by = "driver_id") |>
  tidytable::left_join(constructor_lookup, by = "constructor_id") |>
  tidytable::left_join(status_lookup, by = "status_id") |>
  tidytable::distinct(race_id, driver_id, .keep_all = TRUE) |>
  tidytable::arrange(year, round, position_order)


driver_season_constructor <- race_results |>
  tidytable::arrange(year, driver_id,
                     tidytable::desc(round)) |>
  tidytable::distinct(year, driver_id, .keep_all = TRUE) |>
  tidytable::select(year, driver_id, constructor_name)

driver_championship_final <- driver_standings |>
  tidytable::left_join(race_lookup, by = "race_id") |>
  tidytable::left_join(driver_lookup, by = "driver_id") |>
  tidytable::mutate(final_round = max(round, na.rm = TRUE), .by = year) |>
  tidytable::filter(round == final_round) |>
  tidytable::arrange(year, position) |>
  tidytable::left_join(driver_season_constructor, by = c("year", "driver_id"))

constructor_championship_final <- constructor_standings |>
  tidytable::left_join(race_lookup, by = "race_id") |>
  tidytable::left_join(constructor_lookup, by = "constructor_id") |>
  tidytable::mutate(final_round = max(round, na.rm = TRUE), .by = year) |>
  tidytable::filter(round == final_round) |>
  tidytable::arrange(year, position)


# ------------------------------------------------------------
# 7. CREATE AN OVERALL RESULT LABEL
# ------------------------------------------------------------
#
# This function converts each race record into one of:
#
#   - a classified finishing position
#   - DNF: Did Not Finish
#   - DNS: Did Not Start
#
# The result status is converted to lower case so the matching
# is not affected by capitalisation.

create_result_label <- function(position, race_status) {
  
  status_text <- tolower(ifelse(is.na(race_status), "", race_status))
  
  did_not_start <- grepl(
    paste(
      "did not start",
      "did not qualify",
      "did not prequalify",
      "withdrawn",
      "excluded",
      sep = "|"),
    status_text)
  
  tidytable::case_when(
    did_not_start ~ "DNS",
    !is.na(position) ~ as.character(position),
    TRUE ~ "DNF")
}

race_results <- race_results |>
  tidytable::mutate(result_label = create_result_label(position,race_status))


# ------------------------------------------------------------
# 8. IDENTIFY THE LATEST 20 SEASONS
# ------------------------------------------------------------
#
# This uses the latest 20 years available in the data.
#
# It will update automatically when newer seasons are added.

comparison_years <- race_results |>
  tidytable::filter(!is.na(year)) |>
  tidytable::distinct(year) |>
  tidytable::arrange(tidytable::desc(year)) |>
  tidytable::slice_head(n = 20) |>
  tidytable::pull(year)


# ------------------------------------------------------------
# 9. CREATE THE USER INTERFACE
# ------------------------------------------------------------

ui <- shiny::navbarPage(
  
  title = "Formula 1 Dashboard",
  
  
  # ==========================================================
  # RACE WINNERS TAB
  # ==========================================================
  
  shiny::tabPanel(
    
    title = "Race Winners",
    
    shiny::fluidPage(
      
      shiny::titlePanel("Formula 1 Race Winners"),
      
      shiny::selectInput(
        inputId = "overview_year",
        label = "Select season:",
        choices = sort(
          unique(race_results$year),
          decreasing = TRUE),
        selected = max(
          race_results$year,
          na.rm = TRUE)),
      
      shiny::hr(),
      
      shiny::h3("Race winners"),
      
      shiny::uiOutput(outputId = "overview_winners"))
  ),
  
  
  # ==========================================================
  # DRIVER HEAD-TO-HEAD TAB
  # ==========================================================
  
  shiny::tabPanel(
    
    title = "Driver Head-to-Head",
    
    shiny::fluidPage(
      
      shiny::titlePanel(
        "Driver Head-to-Head"),
      
      shiny::p(
        paste(
          "Select one season and two drivers.",
          "Only drivers who competed in the selected season",
          "can be compared.")),
      
      shiny::fluidRow(
        
        shiny::column(width = 4,
                      shiny::selectInput(
                        inputId = "comparison_year",
                        label = "Select season:",
                        choices = comparison_years,
                        selected = max(
                          comparison_years,
                          na.rm = TRUE))),
        
        shiny::column(width = 4,
                      shiny::selectizeInput(
                        inputId = "comparison_driver_1",
                        label = "Select first driver:",
                        choices = NULL)),
        
        shiny::column(width = 4,
                      shiny::selectizeInput(
                        inputId = "comparison_driver_2",
                        label = "Select second driver:",
                        choices = NULL))),
      
      shiny::hr(),
      
      shiny::uiOutput(outputId = "comparison_heading"),
      
      shiny::fluidRow(
        
        shiny::column(width = 3,
                      shiny::uiOutput(outputId = "comparison_race_score")),
        
        shiny::column(width = 3,
                      shiny::uiOutput(outputId = "comparison_wins")),
        
        shiny::column(width = 3,
                      shiny::uiOutput(outputId = "comparison_podiums")),
        
        shiny::column(width = 3,
                      shiny::uiOutput(outputId = "comparison_points"))),
      
      shiny::hr(),
      
      shiny::h3("Finishing position by race"),
      
      shiny::p(
        paste(
          "DNF = Did Not Finish.",
          "DNS = Did Not Start.")),
      
      shiny::plotOutput(
        outputId = "comparison_position_chart",
        height = "550px"),
      
      shiny::hr(),
      
      shiny::h3("Race-by-race comparison"),
      
      shiny::tableOutput(outputId = "comparison_table"))),

# ==========================================================
# CHAMPIONSHIPS TAB
# ==========================================================


shiny::tabPanel(
  
  title = "World Championships",
  
  shiny::fluidPage(
    
    shiny::titlePanel(
      "Final World Championship Standings"
    ),
    
    shiny::selectInput(
      inputId = "championship_year",
      label = "Select season:",
      choices = sort(
        unique(driver_championship_final$year),
        decreasing = TRUE
      ),
      selected = max(
        driver_championship_final$year,
        na.rm = TRUE
      )
    ),
    
    shiny::hr(),
    
    shiny::fluidRow(
      
      shiny::column(
        
        width = 6,
        
        shiny::h3(
          "Drivers' Championship"
        ),
        
        shiny::uiOutput(
          outputId = "driver_championship_table"
        )
      ),
      
      shiny::column(
        
        width = 6,
        
        shiny::h3(
          "Constructors' Championship"
        ),
        
        shiny::uiOutput(
          outputId = "constructor_championship_table"
        )
      )
    )
  )
)
)


# ------------------------------------------------------------
# 10. CREATE THE SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  
  # ==========================================================
  # 10A. RACE-WINNER OVERVIEW
  # ==========================================================
  
  overview_data <- shiny::reactive({
    
    shiny::req(input$overview_year)
    
    race_results |>
      tidytable::filter(
        year == as.integer(input$overview_year))
  })
  output$overview_winners <- shiny::renderUI({
    
    winners <- overview_data() |>
      tidytable::filter(
        position == 1
      ) |>
      tidytable::arrange(round)
    
    
    if (nrow(winners) == 0) {
      
      return(
        shiny::p(
          paste(
            "No race-winner information is available",
            "for this season."
          )
        )
      )
    }
    
    
    shiny::tagList(
      
      lapply(
        seq_len(nrow(winners)),
        
        function(i) {
          
          # ----------------------------------------------------
          # FIND THE WINNING CONSTRUCTOR'S COLOUR
          # ----------------------------------------------------
          
          constructor <- winners$constructor_name[i]
          
          team_colour <- constructor_colours[
            constructor
          ]
          
          
          # Use grey if the constructor does not yet have a
          # colour in constructor_colours.
          if (
            length(team_colour) == 0 ||
            is.na(team_colour)
          ) {
            
            team_colour <- "#666666"
          }
          
          
          # Choose black or white text depending on how light
          # the team colour is.
          text_colour <- get_text_colour(
            team_colour
          )
          
          
          # ----------------------------------------------------
          # CREATE THE RACE WINNER CARD
          # ----------------------------------------------------
          
          shiny::div(
            
            style = paste0(
              "background-color: ",
              team_colour,
              "; ",
              "color: ",
              text_colour,
              "; ",
              "border-radius: 8px; ",
              "margin-bottom: 10px; ",
              "padding: 15px;"
            ),
            
            
            # Race
            shiny::strong(
              paste0(
                "Round ",
                winners$round[i],
                ": ",
                winners$race_name[i]
              )
            ),
            
            
            shiny::tags$br(),
            
            
            # Driver
            shiny::span(
              winners$driver_name[i],
              style = paste0(
                "font-size: 18px; ",
                "font-weight: bold;"
              )
            ),
            
            
            shiny::tags$br(),
            
            
            # Constructor
            shiny::span(
              winners$constructor_name[i],
              style = paste0(
                "font-size: 14px; ",
                "font-weight: bold;"
              )
            )
          )
        }
      )
    )
  }
  
 )
  
  
  # ==========================================================
  # 10B. FIND DRIVERS IN THE SELECTED SEASON
  # ==========================================================
  #
  # Both driver dropdowns are populated only with drivers who
  # have a result record in the selected season.
  
  season_drivers <- shiny::reactive({
    
    shiny::req(input$comparison_year)
    
    race_results |>
      tidytable::filter(
        year == as.integer(input$comparison_year),
        !is.na(driver_id),
        !is.na(driver_label)) |>
      tidytable::distinct(
        driver_id,
        driver_name,
        driver_label) |>
      tidytable::arrange(driver_label)
  })
  
  
  # ----------------------------------------------------------
  # 10C. UPDATE THE FIRST DRIVER DROPDOWN
  # ----------------------------------------------------------
  
  shiny::observeEvent(
    input$comparison_year,
    {
      
      driver_data <- season_drivers()
      
      shiny::req(nrow(driver_data) >= 2)
      
      driver_choices <- stats::setNames(
        driver_data$driver_id,
        driver_data$driver_label)
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "comparison_driver_1",
        choices = driver_choices,
        selected = driver_data$driver_id[1],
        server = TRUE)
    }
  )
  
  
  # ----------------------------------------------------------
  # 10D. UPDATE THE SECOND DRIVER DROPDOWN
  # ----------------------------------------------------------
  #
  # The first selected driver is excluded from the choices for
  # the second dropdown.
  
  shiny::observeEvent(
    list(
      input$comparison_year,
      input$comparison_driver_1),
    {
      
      shiny::req(input$comparison_year)
      shiny::req(input$comparison_driver_1)
      
      second_driver_data <- season_drivers() |>
        tidytable::filter(
          driver_id != as.integer(
            input$comparison_driver_1))
      
      shiny::req(nrow(second_driver_data) >= 1)
      
      second_driver_choices <- stats::setNames(
        second_driver_data$driver_id,
        second_driver_data$driver_label)
      
      current_selection <- input$comparison_driver_2
      
      valid_driver_ids <- as.character(
        second_driver_data$driver_id)
      
      if (is.null(current_selection) ||!current_selection %in% valid_driver_ids) {
        current_selection <- second_driver_data$driver_id[1]
      }
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "comparison_driver_2",
        choices = second_driver_choices,
        selected = current_selection,
        server = TRUE)
    }
  )
  
  
  # ==========================================================
  # 10E. FILTER TO THE SELECTED SEASON AND DRIVERS
  # ==========================================================
  
  selected_comparison <- shiny::reactive({
    
    shiny::req(input$comparison_year)
    shiny::req(input$comparison_driver_1)
    shiny::req(input$comparison_driver_2)
    
    first_driver <- as.integer(input$comparison_driver_1)
    
    second_driver <- as.integer(input$comparison_driver_2)
    
    shiny::validate(
      shiny::need(first_driver != second_driver,
        "Please select two different drivers.")
    )
    
    race_results |>
      tidytable::filter(year == as.integer(input$comparison_year),
                        driver_id %in% c(first_driver, second_driver)) |>
      tidytable::distinct(race_id, driver_id, .keep_all = TRUE) |>
      tidytable::arrange(round, driver_name)
  })
  
  
  # ----------------------------------------------------------
  # 10F. GET THE SELECTED DRIVER NAMES
  # ----------------------------------------------------------
  
  selected_driver_names <- shiny::reactive({
    
    shiny::req(input$comparison_driver_1)
    shiny::req(input$comparison_driver_2)
    
    first_driver_name <- driver_lookup |>
      tidytable::filter(driver_id == as.integer(input$comparison_driver_1)) |>
      tidytable::pull(driver_name)
    
    second_driver_name <- driver_lookup |>
      tidytable::filter(driver_id == as.integer(input$comparison_driver_2)) |>
      tidytable::pull(driver_name)
    
    list(first = first_driver_name[1],
         second = second_driver_name[1])
  })
  
  
  # ----------------------------------------------------------
  # 10G. CREATE THE COMPARISON HEADING
  # ----------------------------------------------------------
  
  output$comparison_heading <- shiny::renderUI({
    
    driver_names <- selected_driver_names()
    
    shiny::h2(paste0(driver_names$first, " vs ", driver_names$second, ": ", input$comparison_year))
  })
  
  
  # ==========================================================
  # 10H. CREATE ONE ROW PER RACE FOR BOTH DRIVERS
  # ==========================================================
  #
  # inner_join() means the direct race score uses races where
  # both selected drivers have a results-table record.
  #
  # position_order provides an official ordering for drivers
  # who have a DNF result.
  
  paired_comparison <- shiny::reactive({
    
    comparison_data <- selected_comparison()
    
    first_driver_id <- as.integer(input$comparison_driver_1)
    
    second_driver_id <- as.integer(input$comparison_driver_2)
    
    driver_names <- selected_driver_names()
    
    first_driver_results <- comparison_data |>
      tidytable::filter(driver_id == first_driver_id) |>
      tidytable::select(race_id, round,race_name, first_grid = grid, first_position = position,
        first_position_order = position_order, first_points = points, first_status = race_status,
        first_result = result_label)
    
    second_driver_results <- comparison_data |>
      tidytable::filter(driver_id == second_driver_id) |>
      tidytable::select(race_id, second_grid = grid, second_position = position,
        second_position_order = position_order, second_points = points, second_status = race_status,
        second_result = result_label)
    
    first_driver_results |>
      tidytable::inner_join(second_driver_results, by = "race_id") |>
      tidytable::mutate(finished_ahead = tidytable::case_when(
        first_position_order < second_position_order ~ driver_names$first,
        second_position_order < first_position_order ~ driver_names$second,
        TRUE ~ "Tie")) |>
      tidytable::arrange(round)
  })
  
  
  # ==========================================================
  # 10I. CREATE THE COMPARISON SUMMARY
  # ==========================================================
  
  comparison_summary <- shiny::reactive({
    
    selected_comparison() |>
      tidytable::summarise(
        wins = sum(position == 1, na.rm = TRUE),
        podiums = sum(position <= 3, na.rm = TRUE),
        points = sum(points, na.rm = TRUE),
        .by = c(driver_id, driver_name))
  })
  
  
  # ----------------------------------------------------------
  # FUNCTION FOR CREATING SUMMARY BOXES
  # ----------------------------------------------------------
  
  comparison_box <- function(title, text) {
    
    shiny::div(
      
      style = paste0(
        "background-color: #f5f5f5; ",
        "border-top: 5px solid #e10600; ",
        "border-radius: 5px; ",
        "min-height: 125px; ",
        "margin-bottom: 15px; ",
        "padding: 15px;"
      ),
      
      shiny::h4(title),
      
      shiny::div(
        text,
        style = paste0(
          "font-size: 16px; ",
          "font-weight: bold;"
        )
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # 10J. RACE HEAD-TO-HEAD SCORE
  # ----------------------------------------------------------
  
  output$comparison_race_score <- shiny::renderUI({
    
    paired_data <- paired_comparison()
    driver_names <- selected_driver_names()
    
    first_score <- sum(
      paired_data$finished_ahead == driver_names$first,
      na.rm = TRUE
    )
    
    second_score <- sum(
      paired_data$finished_ahead == driver_names$second,
      na.rm = TRUE
    )
    
    comparison_box(
      title = "Race head-to-head",
      text = paste0(
        driver_names$first,
        ": ",
        first_score,
        " | ",
        driver_names$second,
        ": ",
        second_score
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # 10K. WINS
  # ----------------------------------------------------------
  
  output$comparison_wins <- shiny::renderUI({
    
    summary_data <- comparison_summary()
    
    text <- paste0(
      summary_data$driver_name,
      ": ",
      summary_data$wins,
      collapse = " | "
    )
    
    comparison_box(
      title = "Wins",
      text = text
    )
  })
  
  
  # ----------------------------------------------------------
  # 10L. PODIUMS
  # ----------------------------------------------------------
  
  output$comparison_podiums <- shiny::renderUI({
    
    summary_data <- comparison_summary()
    
    text <- paste0(
      summary_data$driver_name,
      ": ",
      summary_data$podiums,
      collapse = " | "
    )
    
    comparison_box(
      title = "Podiums",
      text = text
    )
  })
  
  
  # ----------------------------------------------------------
  # 10M. POINTS
  # ----------------------------------------------------------
  
  output$comparison_points <- shiny::renderUI({
    
    summary_data <- comparison_summary()
    
    text <- paste0(
      summary_data$driver_name,
      ": ",
      summary_data$points,
      collapse = " | "
    )
    
    comparison_box(
      title = "Points",
      text = text
    )
  })
  
  
  # ==========================================================
  # 10N. FINISHING-POSITION CHART
  # ==========================================================
  #
  # The y-axis is permanently fixed as:
  #
  #   1
  #   2
  #   ...
  #   22
  #   DNF
  #   DNS
  #
  # This keeps the scale identical for every season and every
  # driver comparison.
  
  output$comparison_position_chart <- shiny::renderPlot({
    
    plot_data <- selected_comparison()
    
    shiny::validate(
      shiny::need(
        nrow(plot_data) > 0,
        "No race results are available for this comparison."
      )
    )
    
    
    # --------------------------------------------------------
    # CREATE A FIXED NUMERIC POSITION FOR DNF AND DNS
    # --------------------------------------------------------
    #
    # Normal finishing positions occupy 1 to 22.
    #
    # DNF is plotted at position 23.
    # DNS is plotted at position 24.
    
    plot_data <- plot_data |>
      tidytable::mutate(
        
        plot_position = tidytable::case_when(
          result_label == "DNF" ~ 23,
          result_label == "DNS" ~ 24,
          TRUE ~ as.numeric(position)
        )
      )
    
    
    # --------------------------------------------------------
    # CREATE THE CHART
    # --------------------------------------------------------
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = round,
        y = plot_position,
        colour = driver_name,
        group = driver_name
      )
    ) +
      
      ggplot2::geom_line(
        linewidth = 0.9
      ) +
      
      ggplot2::geom_point(
        size = 3
      ) +
      
      ggplot2::scale_x_continuous(
        breaks = sort(
          unique(plot_data$round)
        )
      ) +
      
      # ------------------------------------------------------
    # FIX THE Y-AXIS
    # ------------------------------------------------------
    
    ggplot2::scale_y_reverse(
      breaks = 1:24,
      labels = c(
        as.character(1:22),
        "DNF",
        "DNS"
      ),
      limits = c(24, 1)
    ) +
      
      ggplot2::labs(
        x = "Championship round",
        y = "Finishing position",
        colour = "Driver"
      ) +
      
      ggplot2::theme_minimal() +
      
      ggplot2::theme(
        legend.position = "bottom",
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          face = "bold"
        )
      )
  })
  
  
  # ==========================================================
  # 10O. RACE-BY-RACE COMPARISON TABLE
  # ==========================================================
  
  output$comparison_table <- shiny::renderTable({
    
    driver_names <- selected_driver_names()
    
    table_data <- paired_comparison() |>
      tidytable::select(
        round,
        race_name,
        first_result,
        second_result,
        finished_ahead
      )
    
    names(table_data) <- c(
      "Round",
      "Race",
      driver_names$first,
      driver_names$second,
      "Finished ahead"
    )
    
    table_data
    
  },
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  spacing = "s",
  rownames = FALSE
  )
  
  championship_row <- function(
    position,
    name,
    points,
    wins,
    constructor_name
  ) {
    
    team_colour <- constructor_colours[
      constructor_name
    ]
    
    # Constructors not in the colour lookup use grey.
    if (
      length(team_colour) == 0 ||
      is.na(team_colour)
    ) {
      
      team_colour <- "#666666"
    }
    
    text_colour <- get_text_colour(
      team_colour
    )
    
    shiny::div(
      
      style = paste0(
        "display: grid; ",
        "grid-template-columns: 50px 1fr 80px 60px; ",
        "align-items: center; ",
        "background-color: ",
        team_colour,
        "; ",
        "color: ",
        text_colour,
        "; ",
        "border-radius: 5px; ",
        "margin-bottom: 5px; ",
        "padding: 10px;"
      ),
      
      shiny::strong(position),
      
      shiny::strong(name),
      
      shiny::span(
        paste0(
          points,
          " pts"
        )
      ),
      
      shiny::span(
        paste0(
          wins,
          " wins"
        )
      )
    )
  }
  # ==========================================================
  # 10P. DRIVER CHAMPIONSHIP TABLE
  # ==========================================================
  #
  # Only the final championship standings for the selected
  # season are displayed.
  
  output$driver_championship_table <- shiny::renderUI({
    
    shiny::req(input$championship_year)
    
    championship_data <- driver_championship_final |>
      tidytable::filter(year == as.integer(input$championship_year)) |>
      tidytable::arrange(position)
    
    
    shiny::tagList(
      
      # Column headings
      shiny::div(
        
        style = paste0(
          "display: grid; ",
          "grid-template-columns: 50px 1fr 80px 60px; ",
          "font-weight: bold; ",
          "padding: 8px;"
        ),
        
        shiny::span("Pos"),
        shiny::span("Driver"),
        shiny::span("Points"),
        shiny::span("Wins")
      ),
      
      
      # Championship rows
      lapply(
        seq_len(nrow(championship_data)),
        
        function(i) {
          
          championship_row(
            position = championship_data$position[i],
            name = championship_data$driver_name[i],
            points = championship_data$points[i],
            wins = championship_data$wins[i],
            constructor_name =
              championship_data$constructor_name[i]
          )
        }
      )
    )
  })
  
  
  # ==========================================================
  # 10Q. CONSTRUCTOR CHAMPIONSHIP TABLE
  # ==========================================================
  #
  # Only the final constructor championship standings for the
  # selected season are displayed.
  
  output$constructor_championship_table <- shiny::renderUI({
    
    shiny::req(input$championship_year)
    
    championship_data <- constructor_championship_final |>
      tidytable::filter(year == as.integer(input$championship_year)) |>
      tidytable::arrange(position)
    
    
    shiny::tagList(
      
      # Column headings
      shiny::div(
        
        style = paste0(
          "display: grid; ",
          "grid-template-columns: 50px 1fr 80px 60px; ",
          "font-weight: bold; ",
          "padding: 8px;"
        ),
        
        shiny::span("Pos"),
        shiny::span("Constructor"),
        shiny::span("Points"),
        shiny::span("Wins")
      ),
      
      
      # Championship rows
      lapply(
        seq_len(nrow(championship_data)),
        
        function(i) {
          
          championship_row(
            position = championship_data$position[i],
            name = championship_data$constructor_name[i],
            points = championship_data$points[i],
            wins = championship_data$wins[i],
            constructor_name =
              championship_data$constructor_name[i]
          )
        }
      )
    )
  })
  
  
} # <-- THIS closes server()
# ------------------------------------------------------------
# 11. RUN THE SHINY APP
# ------------------------------------------------------------

shiny::shinyApp(
  ui = ui,
  server = server
)