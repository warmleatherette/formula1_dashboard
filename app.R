# ============================================================
# FORMULA 1 SHINY DASHBOARD
# ============================================================
#
# Tabs:
#   1. Race Winners
#   2. Driver Head-to-Head
#   3. World Championships
#   4. Driver Analysis
#
# Required files in ./data:
#   races.csv
#   results.csv
#   drivers.csv
#   constructors.csv
#   status.csv
#   driver_standings.csv
#   constructor_standings.csv
#
# Install packages once if required:
# install.packages(c("shiny", "tidytable", "janitor", "ggplot2"))
# ============================================================

library(shiny)
library(tidytable)
library(ggplot2)


# ------------------------------------------------------------
# 1. IMPORT FUNCTION
# ------------------------------------------------------------

read_f1_csv <- function(file_name) {
  file_path <- file.path("./data", file_name)
  
  if (!file.exists(file_path)) {
    stop(paste("Missing file:", file_path))
  }
  
  data <- read.csv(
    file_path,
    na.strings = c("\\N", "", "NA"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  janitor::clean_names(data)
}


# ------------------------------------------------------------
# 2. IMPORT TABLES
# ------------------------------------------------------------

races <- read_f1_csv("races.csv")
results <- read_f1_csv("results.csv")
drivers <- read_f1_csv("drivers.csv")
constructors <- read_f1_csv("constructors.csv")
status <- read_f1_csv("status.csv")
driver_standings <- read_f1_csv("driver_standings.csv")
constructor_standings <- read_f1_csv("constructor_standings.csv")


# ------------------------------------------------------------
# 3. CLEAN VARIABLES
# ------------------------------------------------------------

races <- races |>
  tidytable::mutate(
    year = as.integer(year),
    round = as.integer(round)
  )

results <- results |>
  tidytable::mutate(
    position = as.integer(position),
    position_order = as.integer(position_order),
    grid = as.integer(grid),
    points = as.numeric(points)
  )

driver_standings <- driver_standings |>
  tidytable::mutate(
    position = as.integer(position),
    points = as.numeric(points),
    wins = as.integer(wins)
  )

constructor_standings <- constructor_standings |>
  tidytable::mutate(
    position = as.integer(position),
    points = as.numeric(points),
    wins = as.integer(wins)
  )


# ------------------------------------------------------------
# 4. LOOKUP TABLES
# ------------------------------------------------------------

race_lookup <- races |>
  tidytable::rename(
    race_name = name,
    race_date = date
  ) |>
  tidytable::select(
    race_id,
    year,
    round,
    race_name,
    race_date
  )

driver_lookup <- drivers |>
  tidytable::mutate(
    driver_name = paste(forename, surname),
    driver_label = paste0(surname, ", ", forename)
  ) |>
  tidytable::select(
    driver_id,
    driver_name,
    driver_label
  )

constructor_lookup <- constructors |>
  tidytable::rename(constructor_name = name) |>
  tidytable::select(
    constructor_id,
    constructor_name
  )

status_lookup <- status |>
  tidytable::rename(race_status = status) |>
  tidytable::select(
    status_id,
    race_status
  )


# ------------------------------------------------------------
# 5. CONSTRUCTOR COLOURS
# ------------------------------------------------------------

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
  "Racing Point" = "#F596C8",
  "Brawn" = "#B8FD6E",
  "BMW Sauber" = "#006EFF",
  "Toyota" = "#CC0000",
  "Lotus F1" = "#FFB800"
)

get_team_colour <- function(constructor_name) {
  team_colour <- unname(constructor_colours[constructor_name])
  
  if (length(team_colour) == 0 || is.na(team_colour)) {
    team_colour <- "#666666"
  }
  
  team_colour
}

get_text_colour <- function(background_colour) {
  rgb_values <- grDevices::col2rgb(background_colour)
  
  brightness <- (
    rgb_values[1] * 299 +
      rgb_values[2] * 587 +
      rgb_values[3] * 114
  ) / 1000
  
  if (brightness > 150) {
    "#000000"
  } else {
    "#FFFFFF"
  }
}


# ------------------------------------------------------------
# 6. DASHBOARD-READY RACE RESULTS
# ------------------------------------------------------------

race_results <- results |>
  tidytable::left_join(race_lookup, by = "race_id") |>
  tidytable::left_join(driver_lookup, by = "driver_id") |>
  tidytable::left_join(constructor_lookup, by = "constructor_id") |>
  tidytable::left_join(status_lookup, by = "status_id") |>
  tidytable::distinct(
    race_id,
    driver_id,
    .keep_all = TRUE
  ) |>
  tidytable::arrange(
    year,
    round,
    position_order
  )

create_result_label <- function(position, race_status) {
  status_text <- tolower(
    ifelse(is.na(race_status), "", race_status)
  )
  
  did_not_start <- grepl(
    paste(
      "did not start",
      "did not qualify",
      "did not prequalify",
      "withdrawn",
      "excluded",
      sep = "|"
    ),
    status_text
  )
  
  tidytable::case_when(
    did_not_start ~ "DNS",
    !is.na(position) ~ as.character(position),
    TRUE ~ "DNF"
  )
}

race_results <- race_results |>
  tidytable::mutate(
    result_label = create_result_label(
      position,
      race_status
    ),
    positions_gained = tidytable::case_when(
      grid > 0 & !is.na(position) ~ grid - position,
      TRUE ~ NA_integer_
    )
  )


# ------------------------------------------------------------
# 7. FINAL CHAMPIONSHIP TABLES
# ------------------------------------------------------------

# Identify the actual final race_id in each season.
final_races <- race_lookup |>
  tidytable::filter(!is.na(year), !is.na(round)) |>
  tidytable::filter(round == max(round, na.rm = TRUE), .by = year) |>
  tidytable::select(year, race_id, final_round = round)

driver_championship_final <- driver_standings |>
  tidytable::inner_join(final_races, by = "race_id") |>
  tidytable::left_join(driver_lookup, by = "driver_id") |>
  tidytable::arrange(year, position)

constructor_championship_final <- constructor_standings |>
  tidytable::inner_join(final_races, by = "race_id") |>
  tidytable::left_join(constructor_lookup, by = "constructor_id") |>
  tidytable::arrange(year, position)

# Use each driver's constructor at their latest race of the season.
driver_season_constructor <- race_results |>
  tidytable::filter(!is.na(year), !is.na(driver_id)) |>
  tidytable::filter(round == max(round, na.rm = TRUE), .by = c(year, driver_id)) |>
  tidytable::arrange(year, driver_id, tidytable::desc(round)) |>
  tidytable::distinct(year, driver_id, .keep_all = TRUE) |>
  tidytable::select(
    year,
    driver_id,
    constructor_name
  )

driver_championship_final <- driver_championship_final |>
  tidytable::left_join(
    driver_season_constructor,
    by = c("year", "driver_id")
  )

comparison_years <- race_results |>
  tidytable::filter(!is.na(year)) |>
  tidytable::distinct(year) |>
  tidytable::arrange(tidytable::desc(year)) |>
  tidytable::slice_head(n = 20) |>
  tidytable::pull(year)


# ------------------------------------------------------------
# 8. USER INTERFACE
# ------------------------------------------------------------

ui <- shiny::navbarPage(
  title = "Formula 1 Dashboard",
  
  # ==========================================================
  # RACE WINNERS
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
          decreasing = TRUE
        ),
        selected = max(
          race_results$year,
          na.rm = TRUE
        )
      ),
      
      shiny::hr(),
      shiny::h3("Race winners"),
      shiny::uiOutput("overview_winners")
    )
  ),
  
  # ==========================================================
  # DRIVER HEAD-TO-HEAD
  # ==========================================================
  
  shiny::tabPanel(
    title = "Driver Head-to-Head",
    
    shiny::fluidPage(
      shiny::titlePanel("Driver Head-to-Head"),
      
      shiny::p(
        paste(
          "Select one season and two drivers.",
          "Only drivers who competed in the selected season",
          "can be compared."
        )
      ),
      
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::selectInput(
            inputId = "comparison_year",
            label = "Select season:",
            choices = comparison_years,
            selected = max(comparison_years, na.rm = TRUE)
          )
        ),
        shiny::column(
          width = 4,
          shiny::selectizeInput(
            inputId = "comparison_driver_1",
            label = "Select first driver:",
            choices = NULL
          )
        ),
        shiny::column(
          width = 4,
          shiny::selectizeInput(
            inputId = "comparison_driver_2",
            label = "Select second driver:",
            choices = NULL
          )
        )
      ),
      
      shiny::hr(),
      shiny::uiOutput("comparison_heading"),
      
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shiny::uiOutput("comparison_race_score")
        ),
        shiny::column(
          width = 3,
          shiny::uiOutput("comparison_wins")
        ),
        shiny::column(
          width = 3,
          shiny::uiOutput("comparison_podiums")
        ),
        shiny::column(
          width = 3,
          shiny::uiOutput("comparison_points")
        )
      ),
      
      shiny::hr(),
      shiny::h3("Finishing position by race"),
      shiny::p("DNF = Did Not Finish. DNS = Did Not Start."),
      shiny::plotOutput(
        outputId = "comparison_position_chart",
        height = "550px"
      ),
      
      shiny::hr(),
      shiny::h3("Race-by-race comparison"),
      shiny::tableOutput("comparison_table")
    )
  ),
  
  # ==========================================================
  # WORLD CHAMPIONSHIPS
  # ==========================================================
  
  shiny::tabPanel(
    title = "World Championships",
    
    shiny::fluidPage(
      shiny::titlePanel("Final World Championship Standings"),
      
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
          shiny::h3("Drivers' Championship"),
          shiny::uiOutput("driver_championship_table")
        ),
        shiny::column(
          width = 6,
          shiny::h3("Constructors' Championship"),
          shiny::uiOutput("constructor_championship_table")
        )
      )
    )
  ),
  
  # ==========================================================
  # DRIVER ANALYSIS
  # ==========================================================
  
  shiny::tabPanel(
    title = "Driver Analysis",
    
    shiny::fluidPage(
      shiny::titlePanel("Driver Career and Performance Analysis"),
      
      shiny::selectizeInput(
        inputId = "analysis_driver",
        label = "Select driver:",
        choices = stats::setNames(
          driver_lookup$driver_id,
          driver_lookup$driver_label
        ),
        selected = driver_lookup$driver_id[1]
      ),
      
      shiny::fluidRow(
        shiny::column(
          width = 2,
          shiny::uiOutput("career_starts")
        ),
        shiny::column(
          width = 2,
          shiny::uiOutput("career_wins")
        ),
        shiny::column(
          width = 2,
          shiny::uiOutput("career_podiums")
        ),
        shiny::column(
          width = 2,
          shiny::uiOutput("career_points")
        ),
        shiny::column(
          width = 2,
          shiny::uiOutput("career_dnfs")
        ),
        shiny::column(
          width = 2,
          shiny::uiOutput("career_finish_rate")
        )
      ),
      
      shiny::hr(),
      
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::h3("Wins and podiums by season"),
          shiny::plotOutput(
            outputId = "career_results_chart",
            height = "420px"
          )
        ),
        shiny::column(
          width = 6,
          shiny::h3("Points by season"),
          shiny::plotOutput(
            outputId = "career_points_chart",
            height = "420px"
          )
        )
      ),
      
      shiny::hr(),
      
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::h3("Grid position vs finishing position"),
          shiny::plotOutput(
            outputId = "grid_finish_chart",
            height = "470px"
          )
        ),
        shiny::column(
          width = 6,
          shiny::h3("Positions gained by season"),
          shiny::plotOutput(
            outputId = "positions_gained_chart",
            height = "470px"
          )
        )
      ),
      
      shiny::hr(),
      shiny::h3("Reliability by season"),
      shiny::tableOutput("driver_reliability_table")
    )
  )
)


# ------------------------------------------------------------
# 9. SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # REUSABLE DISPLAY HELPERS
  # ----------------------------------------------------------
  
  metric_box <- function(title, text) {
    shiny::div(
      style = paste0(
        "background-color: #f5f5f5; ",
        "border-top: 5px solid #e10600; ",
        "border-radius: 5px; ",
        "min-height: 110px; ",
        "margin-bottom: 15px; ",
        "padding: 15px;"
      ),
      shiny::h4(title),
      shiny::div(
        text,
        style = "font-size: 16px; font-weight: bold;"
      )
    )
  }
  
  championship_row <- function(
    position,
    name,
    points,
    wins,
    constructor_name
  ) {
    team_colour <- get_team_colour(constructor_name)
    text_colour <- get_text_colour(team_colour)
    
    shiny::div(
      style = paste0(
        "display: grid; ",
        "grid-template-columns: 50px 1fr 85px 65px; ",
        "align-items: center; ",
        "background-color: ",
        team_colour,
        "; color: ",
        text_colour,
        "; border-radius: 5px; ",
        "margin-bottom: 5px; ",
        "padding: 10px;"
      ),
      shiny::strong(position),
      shiny::strong(name),
      shiny::span(paste0(points, " pts")),
      shiny::span(paste0(wins, " wins"))
    )
  }
  
  championship_header <- function(name_heading) {
    shiny::div(
      style = paste0(
        "display: grid; ",
        "grid-template-columns: 50px 1fr 85px 65px; ",
        "font-weight: bold; padding: 8px;"
      ),
      shiny::span("Pos"),
      shiny::span(name_heading),
      shiny::span("Points"),
      shiny::span("Wins")
    )
  }
  
  
  # ==========================================================
  # 9A. RACE WINNERS
  # ==========================================================
  
  overview_data <- shiny::reactive({
    shiny::req(input$overview_year)
    
    race_results |>
      tidytable::filter(
        year == as.integer(input$overview_year)
      )
  })
  
  output$overview_winners <- shiny::renderUI({
    winners <- overview_data() |>
      tidytable::filter(position == 1) |>
      tidytable::arrange(round)
    
    if (nrow(winners) == 0) {
      return(
        shiny::p(
          "No race-winner information is available for this season."
        )
      )
    }
    
    shiny::tagList(
      lapply(
        seq_len(nrow(winners)),
        function(i) {
          team_colour <- get_team_colour(
            winners$constructor_name[i]
          )
          text_colour <- get_text_colour(team_colour)
          
          shiny::div(
            style = paste0(
              "background-color: ",
              team_colour,
              "; color: ",
              text_colour,
              "; border-radius: 8px; ",
              "margin-bottom: 10px; padding: 15px;"
            ),
            shiny::strong(
              paste0(
                "Round ",
                winners$round[i],
                ": ",
                winners$race_name[i]
              )
            ),
            shiny::tags$br(),
            shiny::span(
              winners$driver_name[i],
              style = "font-size: 18px; font-weight: bold;"
            ),
            shiny::tags$br(),
            shiny::span(
              winners$constructor_name[i],
              style = "font-size: 14px; font-weight: bold;"
            )
          )
        }
      )
    )
  })
  
  
  # ==========================================================
  # 9B. DRIVER HEAD-TO-HEAD
  # ==========================================================
  
  season_drivers <- shiny::reactive({
    shiny::req(input$comparison_year)
    
    race_results |>
      tidytable::filter(
        year == as.integer(input$comparison_year),
        !is.na(driver_id),
        !is.na(driver_label)
      ) |>
      tidytable::distinct(
        driver_id,
        driver_name,
        driver_label
      ) |>
      tidytable::arrange(driver_label)
  })
  
  shiny::observeEvent(input$comparison_year, {
    driver_data <- season_drivers()
    shiny::req(nrow(driver_data) >= 2)
    
    driver_choices <- stats::setNames(
      driver_data$driver_id,
      driver_data$driver_label
    )
    
    shiny::updateSelectizeInput(
      session = session,
      inputId = "comparison_driver_1",
      choices = driver_choices,
      selected = driver_data$driver_id[1],
      server = TRUE
    )
  })
  
  shiny::observeEvent(
    list(
      input$comparison_year,
      input$comparison_driver_1
    ),
    {
      shiny::req(input$comparison_year)
      shiny::req(input$comparison_driver_1)
      
      second_driver_data <- season_drivers() |>
        tidytable::filter(
          driver_id != as.integer(input$comparison_driver_1)
        )
      
      shiny::req(nrow(second_driver_data) >= 1)
      
      choices <- stats::setNames(
        second_driver_data$driver_id,
        second_driver_data$driver_label
      )
      
      valid_ids <- as.character(second_driver_data$driver_id)
      selected_id <- input$comparison_driver_2
      
      if (
        is.null(selected_id) ||
        !selected_id %in% valid_ids
      ) {
        selected_id <- second_driver_data$driver_id[1]
      }
      
      shiny::updateSelectizeInput(
        session = session,
        inputId = "comparison_driver_2",
        choices = choices,
        selected = selected_id,
        server = TRUE
      )
    }
  )
  
  selected_comparison <- shiny::reactive({
    shiny::req(input$comparison_year)
    shiny::req(input$comparison_driver_1)
    shiny::req(input$comparison_driver_2)
    
    first_driver <- as.integer(input$comparison_driver_1)
    second_driver <- as.integer(input$comparison_driver_2)
    
    shiny::validate(
      shiny::need(
        first_driver != second_driver,
        "Please select two different drivers."
      )
    )
    
    race_results |>
      tidytable::filter(
        year == as.integer(input$comparison_year),
        driver_id %in% c(first_driver, second_driver)
      ) |>
      tidytable::distinct(
        race_id,
        driver_id,
        .keep_all = TRUE
      ) |>
      tidytable::arrange(round, driver_name)
  })
  
  selected_driver_names <- shiny::reactive({
    shiny::req(input$comparison_driver_1)
    shiny::req(input$comparison_driver_2)
    
    first_name <- driver_lookup |>
      tidytable::filter(
        driver_id == as.integer(input$comparison_driver_1)
      ) |>
      tidytable::pull(driver_name)
    
    second_name <- driver_lookup |>
      tidytable::filter(
        driver_id == as.integer(input$comparison_driver_2)
      ) |>
      tidytable::pull(driver_name)
    
    list(
      first = first_name[1],
      second = second_name[1]
    )
  })
  
  output$comparison_heading <- shiny::renderUI({
    driver_names <- selected_driver_names()
    
    shiny::h2(
      paste0(
        driver_names$first,
        " vs ",
        driver_names$second,
        ": ",
        input$comparison_year
      )
    )
  })
  
  paired_comparison <- shiny::reactive({
    comparison_data <- selected_comparison()
    first_driver_id <- as.integer(input$comparison_driver_1)
    second_driver_id <- as.integer(input$comparison_driver_2)
    driver_names <- selected_driver_names()
    
    first_results <- comparison_data |>
      tidytable::filter(driver_id == first_driver_id) |>
      tidytable::select(
        race_id,
        round,
        race_name,
        first_position_order = position_order,
        first_result = result_label
      )
    
    second_results <- comparison_data |>
      tidytable::filter(driver_id == second_driver_id) |>
      tidytable::select(
        race_id,
        second_position_order = position_order,
        second_result = result_label
      )
    
    first_results |>
      tidytable::inner_join(second_results, by = "race_id") |>
      tidytable::mutate(
        finished_ahead = tidytable::case_when(
          first_position_order < second_position_order ~
            driver_names$first,
          second_position_order < first_position_order ~
            driver_names$second,
          TRUE ~ "Tie"
        )
      ) |>
      tidytable::arrange(round)
  })
  
  comparison_summary <- shiny::reactive({
    selected_comparison() |>
      tidytable::summarise(
        wins = sum(position == 1, na.rm = TRUE),
        podiums = sum(position <= 3, na.rm = TRUE),
        points = sum(points, na.rm = TRUE),
        .by = c(driver_id, driver_name)
      )
  })
  
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
    
    metric_box(
      "Race head-to-head",
      paste0(
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
  
  output$comparison_wins <- shiny::renderUI({
    summary_data <- comparison_summary()
    
    metric_box(
      "Wins",
      paste0(
        summary_data$driver_name,
        ": ",
        summary_data$wins,
        collapse = " | "
      )
    )
  })
  
  output$comparison_podiums <- shiny::renderUI({
    summary_data <- comparison_summary()
    
    metric_box(
      "Podiums",
      paste0(
        summary_data$driver_name,
        ": ",
        summary_data$podiums,
        collapse = " | "
      )
    )
  })
  
  output$comparison_points <- shiny::renderUI({
    summary_data <- comparison_summary()
    
    metric_box(
      "Points",
      paste0(
        summary_data$driver_name,
        ": ",
        summary_data$points,
        collapse = " | "
      )
    )
  })
  
  output$comparison_position_chart <- shiny::renderPlot({
    plot_data <- selected_comparison() |>
      tidytable::mutate(
        plot_position = tidytable::case_when(
          result_label == "DNF" ~ 23,
          result_label == "DNS" ~ 24,
          TRUE ~ as.numeric(position)
        )
      )
    
    shiny::validate(
      shiny::need(
        nrow(plot_data) > 0,
        "No race results are available for this comparison."
      )
    )
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = round,
        y = plot_position,
        colour = driver_name,
        group = driver_name
      )
    ) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::geom_point(size = 3) +
      ggplot2::scale_x_continuous(
        breaks = sort(unique(plot_data$round))
      ) +
      ggplot2::scale_y_reverse(
        breaks = 1:24,
        labels = c(as.character(1:22), "DNF", "DNS"),
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
        panel.grid.minor = ggplot2::element_blank()
      )
  })
  
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
  
  
  # ==========================================================
  # 9C. WORLD CHAMPIONSHIPS
  # ==========================================================
  
  output$driver_championship_table <- shiny::renderUI({
    shiny::req(input$championship_year)
    
    championship_data <- driver_championship_final |>
      tidytable::filter(
        year == as.integer(input$championship_year)
      ) |>
      tidytable::arrange(position)
    
    shiny::tagList(
      championship_header("Driver"),
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
  
  output$constructor_championship_table <- shiny::renderUI({
    shiny::req(input$championship_year)
    
    championship_data <- constructor_championship_final |>
      tidytable::filter(
        year == as.integer(input$championship_year)
      ) |>
      tidytable::arrange(position)
    
    shiny::tagList(
      championship_header("Constructor"),
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
  
  
  # ==========================================================
  # 9D. DRIVER ANALYSIS
  # ==========================================================
  
  selected_driver_career <- shiny::reactive({
    shiny::req(input$analysis_driver)
    
    race_results |>
      tidytable::filter(
        driver_id == as.integer(input$analysis_driver)
      ) |>
      tidytable::arrange(year, round)
  })
  
  driver_career_summary <- shiny::reactive({
    career_data <- selected_driver_career()
    
    starts <- sum(career_data$result_label != "DNS", na.rm = TRUE)
    finishes <- sum(!career_data$result_label %in% c("DNF", "DNS"))
    
    tidytable::tidytable(
      starts = starts,
      wins = sum(career_data$position == 1, na.rm = TRUE),
      podiums = sum(career_data$position <= 3, na.rm = TRUE),
      points = sum(career_data$points, na.rm = TRUE),
      dnfs = sum(career_data$result_label == "DNF", na.rm = TRUE),
      finish_rate = ifelse(
        starts > 0,
        round(finishes / starts * 100, 1),
        NA_real_
      )
    )
  })
  
  output$career_starts <- shiny::renderUI({
    metric_box("Starts", driver_career_summary()$starts)
  })
  
  output$career_wins <- shiny::renderUI({
    metric_box("Wins", driver_career_summary()$wins)
  })
  
  output$career_podiums <- shiny::renderUI({
    metric_box("Podiums", driver_career_summary()$podiums)
  })
  
  output$career_points <- shiny::renderUI({
    metric_box("Points", driver_career_summary()$points)
  })
  
  output$career_dnfs <- shiny::renderUI({
    metric_box("DNFs", driver_career_summary()$dnfs)
  })
  
  output$career_finish_rate <- shiny::renderUI({
    metric_box(
      "Finish rate",
      paste0(driver_career_summary()$finish_rate, "%")
    )
  })
  
  driver_season_summary <- shiny::reactive({
    selected_driver_career() |>
      tidytable::summarise(
        starts = sum(result_label != "DNS", na.rm = TRUE),
        wins = sum(position == 1, na.rm = TRUE),
        podiums = sum(position <= 3, na.rm = TRUE),
        points = sum(points, na.rm = TRUE),
        dnfs = sum(result_label == "DNF", na.rm = TRUE),
        finishes = sum(
          !result_label %in% c("DNF", "DNS"),
          na.rm = TRUE
        ),
        positions_gained = sum(positions_gained, na.rm = TRUE),
        .by = year
      ) |>
      tidytable::mutate(
        finish_rate = tidytable::case_when(
          starts > 0 ~ round(finishes / starts * 100, 1),
          TRUE ~ NA_real_
        )
      ) |>
      tidytable::arrange(year)
  })
  
  output$career_results_chart <- shiny::renderPlot({
    season_data <- driver_season_summary()
    
    plot_data <- rbind(
      data.frame(
        year = season_data$year,
        measure = "Wins",
        total = season_data$wins
      ),
      data.frame(
        year = season_data$year,
        measure = "Podiums",
        total = season_data$podiums
      )
    )
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = year,
        y = total,
        fill = measure
      )
    ) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::scale_x_continuous(
        breaks = sort(unique(plot_data$year))
      ) +
      ggplot2::labs(
        x = "Season",
        y = "Total",
        fill = NULL
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom",
        axis.text.x = ggplot2::element_text(
          angle = 45,
          hjust = 1
        )
      )
  })
  
  output$career_points_chart <- shiny::renderPlot({
    plot_data <- driver_season_summary()
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = year, y = points)
    ) +
      ggplot2::geom_col(fill = "#E10600") +
      ggplot2::scale_x_continuous(
        breaks = sort(unique(plot_data$year))
      ) +
      ggplot2::labs(
        x = "Season",
        y = "Points"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 45,
          hjust = 1
        )
      )
  })
  
  output$grid_finish_chart <- shiny::renderPlot({
    plot_data <- selected_driver_career() |>
      tidytable::filter(
        grid > 0,
        !is.na(position)
      )
    
    shiny::validate(
      shiny::need(
        nrow(plot_data) > 0,
        "No comparable grid and finishing positions are available."
      )
    )
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = grid,
        y = position,
        colour = year
      )
    ) +
      ggplot2::geom_abline(
        slope = 1,
        intercept = 0,
        linetype = "dashed",
        colour = "grey50"
      ) +
      ggplot2::geom_point(size = 2.5, alpha = 0.75) +
      ggplot2::scale_x_continuous(
        breaks = 1:22,
        limits = c(1, 22)
      ) +
      ggplot2::scale_y_reverse(
        breaks = 1:22,
        limits = c(22, 1)
      ) +
      ggplot2::labs(
        x = "Grid position",
        y = "Finishing position",
        colour = "Season"
      ) +
      ggplot2::theme_minimal()
  })
  
  output$positions_gained_chart <- shiny::renderPlot({
    plot_data <- driver_season_summary()
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = year,
        y = positions_gained,
        fill = positions_gained >= 0
      )
    ) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_hline(
        yintercept = 0,
        colour = "grey40"
      ) +
      ggplot2::scale_x_continuous(
        breaks = sort(unique(plot_data$year))
      ) +
      ggplot2::labs(
        x = "Season",
        y = "Net positions gained"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 45,
          hjust = 1
        )
      )
  })
  
  output$driver_reliability_table <- shiny::renderTable({
    driver_season_summary() |>
      tidytable::select(
        year,
        starts,
        finishes,
        dnfs,
        finish_rate
      ) |>
      tidytable::arrange(tidytable::desc(year)) |>
      tidytable::rename(
        Season = year,
        Starts = starts,
        Finishes = finishes,
        DNFs = dnfs,
        `Finish rate (%)` = finish_rate
      )
  },
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = FALSE
  )
}


# ------------------------------------------------------------
# 10. RUN APP
# ------------------------------------------------------------

shiny::shinyApp(
  ui = ui,
  server = server
)
