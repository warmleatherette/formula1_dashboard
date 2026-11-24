# ============================================================
# FORMULA 1 SHINY DASHBOARD
# Fully integrated and annotated version
# ============================================================
#
# This app contains:
#   1. Race Winners
#   2. Driver Head-to-Head
#   3. World Championships
#   4. Driver Analysis
#   5. Reliability & DNF Analysis
#
# Expected folder structure:
#
#   f1_data/
#   |-- f1_dashboard_annotated_full.R
#   `-- data/
#       |-- races.csv
#       |-- results.csv
#       |-- drivers.csv
#       |-- constructors.csv
#       |-- status.csv
#       |-- driver_standings.csv
#       `-- constructor_standings.csv
#
# ============================================================


# ------------------------------------------------------------
# 0. INSTALL REQUIRED PACKAGES
# ------------------------------------------------------------
#
# The vector below lists every package required by the dashboard.
# installed.packages() is checked first so packages are installed
# only when they are missing.

required_packages <- c(
  "shiny",
  "tidytable",
  "janitor",
  "ggplot2",
  "plotly"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    dependencies = TRUE
  )
}


# ------------------------------------------------------------
# LOAD REQUIRED PACKAGES
# ------------------------------------------------------------
#
# shiny: builds the interactive web application.
# tidytable: performs filtering, joins, mutation and summaries.
# janitor: cleans imported column names.
# ggplot2: creates charts.
# plotly: makes selected ggplot charts interactive.

library(shiny)
library(tidytable)
library(janitor)
library(ggplot2)
library(plotly)

# ============================================================
# 1. IMPORT DATA
# ============================================================

read_f1_csv <- function(file_name) {
  file_path <- file.path("data", file_name)
  
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

races <- read_f1_csv("races.csv")
results <- read_f1_csv("results.csv")
drivers <- read_f1_csv("drivers.csv")
constructors <- read_f1_csv("constructors.csv")
status <- read_f1_csv("status.csv")
driver_standings <- read_f1_csv("driver_standings.csv")
constructor_standings <- read_f1_csv("constructor_standings.csv")

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

# ============================================================
# 2. LOOKUP TABLES
#
# Purpose:
#   Convert database-style IDs into human-readable labels for the app.
#
# race_lookup:
#   Keeps race_id plus year, round, race name and date.
# driver_lookup:
#   Keeps driver_id and creates both a display name and surname-first
#   dropdown label.
# constructor_lookup:
#   Maps constructor_id to constructor_name.
# status_lookup:
#   Maps status_id to the exact result description from status.csv.
#
# These smaller tables are joined onto results later, keeping the data
# lineage clear and avoiding unnecessary source columns.
# ============================================================

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
  tidytable::select(constructor_id, constructor_name)

status_lookup <- status |>
  tidytable::rename(race_status = status) |>
  tidytable::select(status_id, race_status)

# ============================================================
# 3. COLOURS AND DISPLAY FUNCTIONS
#
# Purpose:
#   Keep presentation rules separate from the underlying race data.
#
# constructor_colours:
#   Named vector used for winner cards and championship rows.
# get_team_colour():
#   Looks up a constructor colour and falls back to grey if missing.
# get_text_colour():
#   Calculates whether black or white text gives better contrast.
# create_result_label():
#   Converts each race result into a classified position, DNF or DNS.
# create_dnf_category():
#   Creates broad reliability categories for summary analysis.
# create_dnf_subcategory():
#   Retains the exact status.csv reason for detailed DNF analysis.
# ============================================================

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
  colour <- unname(constructor_colours[constructor_name])
  
  if (length(colour) == 0 || is.na(colour)) {
    colour <- "#666666"
  }
  
  colour
}

get_text_colour <- function(background_colour) {
  rgb_values <- grDevices::col2rgb(background_colour)
  
  brightness <- (
    rgb_values[1] * 299 +
      rgb_values[2] * 587 +
      rgb_values[3] * 114
  ) / 1000
  
  if (brightness > 150) "#000000" else "#FFFFFF"
}

create_result_label <- function(position, race_status) {
  status_text <- tolower(ifelse(is.na(race_status), "", race_status))
  
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

create_dnf_category <- function(result_label, race_status) {
  tidytable::case_when(
    result_label != "DNF" ~ NA_character_,
    grepl(
      "accident|collision|spun",
      race_status,
      ignore.case = TRUE
    ) ~ "Accident / Collision",
    grepl(
      paste0(
        "engine|gearbox|transmission|hydraulic|suspension|",
        "brake|oil|water|fuel|clutch|driveshaft|differential|",
        "power unit|turbo|overheating|radiator"
      ),
      race_status,
      ignore.case = TRUE
    ) ~ "Mechanical",
    grepl(
      "electrical|electronics|battery|alternator",
      race_status,
      ignore.case = TRUE
    ) ~ "Electrical",
    grepl(
      "tyre|tire|puncture|wheel",
      race_status,
      ignore.case = TRUE
    ) ~ "Tyre / Wheel",
    TRUE ~ "Other"
  )
}


create_dnf_subcategory <- function(result_label, race_status) {
  tidytable::case_when(
    result_label == "DNF" ~ race_status,
    TRUE ~ NA_character_
  )
}

# ============================================================
# 4. DASHBOARD-READY DATA
#
# Purpose:
#   Build the main analysis tables used by every dashboard tab.
#
# race_results starts from results because results has one row per driver
# per race. Lookups then add race, driver, constructor and status labels.
# Derived fields add DNF/DNS labels, positions gained, DNF categories and
# retirement timing.
#
# final_races identifies the last race_id in each season. This is used to
# keep only end-of-season driver and constructor championship standings.
#
# driver_season_constructor supplies a constructor colour for each driver
# championship row, using the driver's final constructor in that season.
#
# comparison_years restricts Head-to-Head to the latest 20 seasons.
# ============================================================

race_results <- results |>
  tidytable::left_join(race_lookup, by = "race_id") |>
  tidytable::left_join(driver_lookup, by = "driver_id") |>
  tidytable::left_join(constructor_lookup, by = "constructor_id") |>
  tidytable::left_join(status_lookup, by = "status_id") |>
  tidytable::distinct(race_id, driver_id, .keep_all = TRUE) |>
  tidytable::arrange(year, round, position_order) |>
  tidytable::mutate(
    result_label = create_result_label(position, race_status),
    positions_gained = tidytable::case_when(
      grid > 0 & !is.na(position) ~ grid - position,
      TRUE ~ NA_integer_
    )
  ) |>
  tidytable::mutate(
    dnf_category = create_dnf_category(result_label, race_status),
    dnf_subcategory = create_dnf_subcategory(result_label, race_status),
    mechanical_dnf = result_label == "DNF" &
      dnf_category %in% c("Mechanical", "Electrical", "Tyre / Wheel"),
    completed_laps = as.integer(laps),
    retirement_phase = tidytable::case_when(
      result_label != "DNF" ~ NA_character_,
      completed_laps <= 1 ~ "Opening lap",
      completed_laps <= 10 ~ "Early race",
      completed_laps <= 40 ~ "Middle race",
      completed_laps > 40 ~ "Late race",
      TRUE ~ "Unknown"
    )
  )

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

driver_season_constructor <- race_results |>
  tidytable::filter(!is.na(year), !is.na(driver_id)) |>
  tidytable::filter(
    round == max(round, na.rm = TRUE),
    .by = c(year, driver_id)
  ) |>
  tidytable::distinct(year, driver_id, .keep_all = TRUE) |>
  tidytable::select(year, driver_id, constructor_name)

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

# ============================================================
# 5. USER INTERFACE
#
# Purpose:
#   Define what the user sees and can interact with.
#
# navbarPage() contains five tabs. Controls such as selectInput() create
# input values that the server reads. uiOutput(), tableOutput(),
# plotOutput() and plotlyOutput() are placeholders populated by server.
#
# Race Winners:
#   Season selector plus coloured winner cards.
# Driver Head-to-Head:
#   Season and two driver selectors, comparison metrics, result chart and
#   race-by-race table.
# World Championships:
#   One season selector controlling final driver and constructor tables.
# Driver Analysis:
#   Career metrics and charts, including the interactive grid/finish plot.
# Reliability & DNFs:
#   Driver/constructor reliability filters, DNF charts and detail tables.
# ============================================================

ui <- shiny::navbarPage(
  title = "Formula 1 Dashboard",
  
  shiny::tabPanel(
    title = "Race Winners",
    shiny::fluidPage(
      shiny::titlePanel("Formula 1 Race Winners"),
      shiny::selectInput(
        "overview_year",
        "Select season:",
        choices = sort(unique(race_results$year), decreasing = TRUE),
        selected = max(race_results$year, na.rm = TRUE)
      ),
      shiny::hr(),
      shiny::uiOutput("overview_winners")
    )
  ),
  
  shiny::tabPanel(
    title = "Driver Head-to-Head",
    shiny::fluidPage(
      shiny::titlePanel("Driver Head-to-Head"),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::selectInput(
            "comparison_year",
            "Select season:",
            choices = comparison_years,
            selected = max(comparison_years, na.rm = TRUE)
          )
        ),
        shiny::column(
          4,
          shiny::selectizeInput(
            "comparison_driver_1",
            "Select first driver:",
            choices = NULL
          )
        ),
        shiny::column(
          4,
          shiny::selectizeInput(
            "comparison_driver_2",
            "Select second driver:",
            choices = NULL
          )
        )
      ),
      shiny::uiOutput("comparison_heading"),
      shiny::fluidRow(
        shiny::column(3, shiny::uiOutput("comparison_race_score")),
        shiny::column(3, shiny::uiOutput("comparison_wins")),
        shiny::column(3, shiny::uiOutput("comparison_podiums")),
        shiny::column(3, shiny::uiOutput("comparison_points"))
      ),
      shiny::h3("Finishing position by race"),
      shiny::p("DNF = Did Not Finish. DNS = Did Not Start."),
      shiny::plotOutput("comparison_position_chart", height = "550px"),
      shiny::h3("Race-by-race comparison"),
      shiny::tableOutput("comparison_table")
    )
  ),
  
  shiny::tabPanel(
    title = "World Championships",
    shiny::fluidPage(
      shiny::titlePanel("Final World Championship Standings"),
      shiny::selectInput(
        "championship_year",
        "Select season:",
        choices = sort(
          unique(driver_championship_final$year),
          decreasing = TRUE
        ),
        selected = max(driver_championship_final$year, na.rm = TRUE)
      ),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::h3("Drivers' Championship"),
          shiny::uiOutput("driver_championship_table")
        ),
        shiny::column(
          6,
          shiny::h3("Constructors' Championship"),
          shiny::uiOutput("constructor_championship_table")
        )
      )
    )
  ),
  
  shiny::tabPanel(
    title = "Driver Analysis",
    shiny::fluidPage(
      shiny::titlePanel("Driver Career and Performance Analysis"),
      shiny::selectizeInput(
        "analysis_driver",
        "Select driver:",
        choices = stats::setNames(
          driver_lookup$driver_id,
          driver_lookup$driver_label
        )
      ),
      shiny::fluidRow(
        shiny::column(2, shiny::uiOutput("career_starts")),
        shiny::column(2, shiny::uiOutput("career_wins")),
        shiny::column(2, shiny::uiOutput("career_podiums")),
        shiny::column(2, shiny::uiOutput("career_points")),
        shiny::column(2, shiny::uiOutput("career_dnfs")),
        shiny::column(2, shiny::uiOutput("career_finish_rate"))
      ),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::h3("Wins and podiums by season"),
          shiny::plotOutput("career_results_chart", height = "420px")
        ),
        shiny::column(
          6,
          shiny::h3("Points by season"),
          shiny::plotOutput("career_points_chart", height = "420px")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::h3("Grid position vs finishing position"),
          shiny::selectInput(
            inputId = "grid_finish_year",
            label = "Select season:",
            choices = NULL
          ),
          plotly::plotlyOutput(
            "grid_finish_chart",
            height = "470px"
          )
        ),
        shiny::column(
          6,
          shiny::h3("Positions gained by season"),
          shiny::plotOutput("positions_gained_chart", height = "470px")
        )
      ),
      shiny::h3("Reliability by season"),
      shiny::tableOutput("driver_reliability_table")
    )
  ),
  
  shiny::tabPanel(
    title = "Reliability & DNFs",
    shiny::fluidPage(
      shiny::titlePanel("Reliability and DNF Analysis"),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::radioButtons(
            "reliability_type",
            "Analyse:",
            choices = c("Driver", "Constructor"),
            selected = "Driver",
            inline = TRUE
          )
        ),
        shiny::column(4, shiny::uiOutput("reliability_entity_selector")),
        shiny::column(
          4,
          shiny::selectInput(
            "reliability_year",
            "Season:",
            choices = c(
              "All seasons",
              sort(unique(race_results$year), decreasing = TRUE)
            ),
            selected = "All seasons"
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(3, shiny::uiOutput("reliability_starts")),
        shiny::column(3, shiny::uiOutput("reliability_dnfs")),
        shiny::column(3, shiny::uiOutput("reliability_dnf_rate")),
        shiny::column(3, shiny::uiOutput("reliability_mechanical"))
      ),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::h3("Broad DNF categories"),
          shiny::plotOutput("dnf_category_chart", height = "420px")
        ),
        shiny::column(
          6,
          shiny::h3("Actual status from status.csv"),
          shiny::plotOutput("dnf_subcategory_chart", height = "420px")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::h3("DNF rate by season"),
          shiny::plotOutput("dnf_rate_chart", height = "420px")
        ),
        shiny::column(
          6,
          shiny::h3("When retirements occurred"),
          shiny::plotOutput("retirement_phase_chart", height = "420px")
        )
      ),
      shiny::conditionalPanel(
        condition = "input.reliability_type == 'Constructor'",
        shiny::hr(),
        shiny::h2("Constructor reliability"),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::h3("DNFs by driver"),
            shiny::plotOutput(
              "constructor_driver_dnf_chart",
              height = "420px"
            )
          ),
          shiny::column(
            6,
            shiny::h3("Constructor reliability by season"),
            shiny::tableOutput(
              "constructor_reliability_table"
            )
          )
        )
      ),
      shiny::hr(),
      shiny::h3("Individual DNFs"),
      shiny::tableOutput("dnf_detail_table")
    )
  )
)

# ============================================================
# 6. SERVER
#
# Purpose:
#   Connect UI inputs to filtered data, calculations, charts and tables.
#
# reactive() objects recalculate only when their dependent inputs change.
# observeEvent() is used where changing one input must update another UI
# control, such as rebuilding the available driver or season choices.
# renderUI(), renderTable(), renderPlot() and renderPlotly() create the
# outputs placed in the matching UI placeholders.
# ============================================================

server <- function(input, output, session) {
  # Reusable HTML card used for headline statistics throughout the app.
  metric_box <- function(title, text) {
    shiny::div(
      style = paste0(
        "background-color:#f5f5f5;",
        "border-top:5px solid #e10600;",
        "border-radius:5px;min-height:110px;",
        "margin-bottom:15px;padding:15px;"
      ),
      shiny::h4(title),
      shiny::div(
        text,
        style = "font-size:16px;font-weight:bold;"
      )
    )
  }
  
  # Reusable coloured row used by both championship standings tables.
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
        "display:grid;grid-template-columns:50px 1fr 85px 65px;",
        "align-items:center;background-color:",
        team_colour,
        ";color:",
        text_colour,
        ";border-radius:5px;margin-bottom:5px;padding:10px;"
      ),
      shiny::strong(position),
      shiny::strong(name),
      shiny::span(paste0(points, " pts")),
      shiny::span(paste0(wins, " wins"))
    )
  }
  
  # Creates a consistent four-column header above championship rows.
  championship_header <- function(name_heading) {
    shiny::div(
      style = paste0(
        "display:grid;grid-template-columns:50px 1fr 85px 65px;",
        "font-weight:bold;padding:8px;"
      ),
      shiny::span("Pos"),
      shiny::span(name_heading),
      shiny::span("Points"),
      shiny::span("Wins")
    )
  }
  
  # ----------------------------------------------------------
  # 6A. RACE WINNERS
  # ----------------------------------------------------------
  #
  # overview_data() filters race_results to the selected season.
  # overview_winners then keeps position == 1, orders races by round and
  # creates one constructor-coloured card for every winner.
  
  overview_data <- shiny::reactive({
    shiny::req(input$overview_year)
    
    race_results |>
      tidytable::filter(year == as.integer(input$overview_year))
  })
  
  output$overview_winners <- shiny::renderUI({
    winners <- overview_data() |>
      tidytable::filter(position == 1) |>
      tidytable::arrange(round)
    
    if (nrow(winners) == 0) {
      return(shiny::p("No race-winner information is available."))
    }
    
    shiny::tagList(
      lapply(seq_len(nrow(winners)), function(i) {
        team_colour <- get_team_colour(winners$constructor_name[i])
        text_colour <- get_text_colour(team_colour)
        
        shiny::div(
          style = paste0(
            "background-color:",
            team_colour,
            ";color:",
            text_colour,
            ";border-radius:8px;margin-bottom:10px;padding:15px;"
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
            style = "font-size:18px;font-weight:bold;"
          ),
          shiny::tags$br(),
          shiny::span(winners$constructor_name[i])
        )
      })
    )
  })
  
  # ----------------------------------------------------------
  # 6B. DRIVER HEAD-TO-HEAD
  # ----------------------------------------------------------
  #
  # season_drivers() identifies drivers who actually appear in the chosen
  # season. observeEvent() rebuilds the two dropdowns and prevents the same
  # driver being selected twice. selected_comparison() then filters to one
  # season and those two drivers only. paired_comparison() joins them by
  # race_id so the race result can be compared directly. Summary reactives
  # feed wins, podiums, points, head-to-head score, chart and detail table.
  
  season_drivers <- shiny::reactive({
    shiny::req(input$comparison_year)
    
    race_results |>
      tidytable::filter(
        year == as.integer(input$comparison_year),
        !is.na(driver_id),
        !is.na(driver_label)
      ) |>
      tidytable::distinct(driver_id, driver_name, driver_label) |>
      tidytable::arrange(driver_label)
  })
  
  shiny::observeEvent(input$comparison_year, {
    driver_data <- season_drivers()
    shiny::req(nrow(driver_data) >= 2)
    
    shiny::updateSelectizeInput(
      session,
      "comparison_driver_1",
      choices = stats::setNames(
        driver_data$driver_id,
        driver_data$driver_label
      ),
      selected = driver_data$driver_id[1],
      server = TRUE
    )
  })
  
  shiny::observeEvent(
    list(input$comparison_year, input$comparison_driver_1),
    {
      shiny::req(input$comparison_driver_1)
      
      driver_data <- season_drivers() |>
        tidytable::filter(
          driver_id != as.integer(input$comparison_driver_1)
        )
      
      shiny::updateSelectizeInput(
        session,
        "comparison_driver_2",
        choices = stats::setNames(
          driver_data$driver_id,
          driver_data$driver_label
        ),
        selected = driver_data$driver_id[1],
        server = TRUE
      )
    }
  )
  
  selected_comparison <- shiny::reactive({
    shiny::req(
      input$comparison_year,
      input$comparison_driver_1,
      input$comparison_driver_2
    )
    
    first_driver <- as.integer(input$comparison_driver_1)
    second_driver <- as.integer(input$comparison_driver_2)
    
    shiny::validate(
      shiny::need(
        first_driver != second_driver,
        "Select two different drivers."
      )
    )
    
    race_results |>
      tidytable::filter(
        year == as.integer(input$comparison_year),
        driver_id %in% c(first_driver, second_driver)
      ) |>
      tidytable::arrange(round, driver_name)
  })
  
  selected_driver_names <- shiny::reactive({
    shiny::req(input$comparison_driver_1, input$comparison_driver_2)
    
    names <- driver_lookup |>
      tidytable::filter(
        driver_id %in% as.integer(
          c(input$comparison_driver_1, input$comparison_driver_2)
        )
      )
    
    list(
      first = names$driver_name[
        names$driver_id == as.integer(input$comparison_driver_1)
      ][1],
      second = names$driver_name[
        names$driver_id == as.integer(input$comparison_driver_2)
      ][1]
    )
  })
  
  output$comparison_heading <- shiny::renderUI({
    names <- selected_driver_names()
    shiny::h2(
      paste0(
        names$first,
        " vs ",
        names$second,
        ": ",
        input$comparison_year
      )
    )
  })
  
  paired_comparison <- shiny::reactive({
    data <- selected_comparison()
    names <- selected_driver_names()
    
    first <- data |>
      tidytable::filter(
        driver_id == as.integer(input$comparison_driver_1)
      ) |>
      tidytable::select(
        race_id,
        round,
        race_name,
        first_order = position_order,
        first_result = result_label
      )
    
    second <- data |>
      tidytable::filter(
        driver_id == as.integer(input$comparison_driver_2)
      ) |>
      tidytable::select(
        race_id,
        second_order = position_order,
        second_result = result_label
      )
    
    first |>
      tidytable::inner_join(second, by = "race_id") |>
      tidytable::mutate(
        finished_ahead = tidytable::case_when(
          first_order < second_order ~ names$first,
          second_order < first_order ~ names$second,
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
    data <- paired_comparison()
    names <- selected_driver_names()
    
    metric_box(
      "Race head-to-head",
      paste0(
        names$first,
        ": ",
        sum(data$finished_ahead == names$first),
        " | ",
        names$second,
        ": ",
        sum(data$finished_ahead == names$second)
      )
    )
  })
  
  output$comparison_wins <- shiny::renderUI({
    data <- comparison_summary()
    metric_box(
      "Wins",
      paste0(data$driver_name, ": ", data$wins, collapse = " | ")
    )
  })
  
  output$comparison_podiums <- shiny::renderUI({
    data <- comparison_summary()
    metric_box(
      "Podiums",
      paste0(data$driver_name, ": ", data$podiums, collapse = " | ")
    )
  })
  
  output$comparison_points <- shiny::renderUI({
    data <- comparison_summary()
    metric_box(
      "Points",
      paste0(data$driver_name, ": ", data$points, collapse = " | ")
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
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        round,
        plot_position,
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
      ggplot2::theme(legend.position = "bottom")
  })
  
  output$comparison_table <- shiny::renderTable({
    names <- selected_driver_names()
    
    data <- paired_comparison() |>
      tidytable::select(
        round,
        race_name,
        first_result,
        second_result,
        finished_ahead
      )
    
    names(data) <- c(
      "Round",
      "Race",
      names$first,
      names$second,
      "Finished ahead"
    )
    
    data
  },
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = FALSE
  )
  
  # ----------------------------------------------------------
  # 6C. WORLD CHAMPIONSHIPS
  # ----------------------------------------------------------
  #
  # These outputs use driver_championship_final and
  # constructor_championship_final, which were already restricted to each
  # season's last race. The renderUI outputs create coloured standings rows
  # using constructor_colours.
  
  output$driver_championship_table <- shiny::renderUI({
    data <- driver_championship_final |>
      tidytable::filter(year == as.integer(input$championship_year)) |>
      tidytable::arrange(position)
    
    shiny::tagList(
      championship_header("Driver"),
      lapply(seq_len(nrow(data)), function(i) {
        championship_row(
          data$position[i],
          data$driver_name[i],
          data$points[i],
          data$wins[i],
          data$constructor_name[i]
        )
      })
    )
  })
  
  output$constructor_championship_table <- shiny::renderUI({
    data <- constructor_championship_final |>
      tidytable::filter(year == as.integer(input$championship_year)) |>
      tidytable::arrange(position)
    
    shiny::tagList(
      championship_header("Constructor"),
      lapply(seq_len(nrow(data)), function(i) {
        championship_row(
          data$position[i],
          data$constructor_name[i],
          data$points[i],
          data$wins[i],
          data$constructor_name[i]
        )
      })
    )
  })
  
  # ----------------------------------------------------------
  # 6D. DRIVER ANALYSIS
  # ----------------------------------------------------------
  #
  # selected_driver_career() is the single filtered career dataset for the
  # selected driver. driver_season_summary() aggregates it by season for
  # starts, finishes, wins, podiums, points, DNFs and positions gained.
  #
  # grid_finish_year is rebuilt for the selected driver's career. The
  # Plotly scatter chart then shows one season at a time, with hover text
  # giving race, grid, finish and position change. The 1-22 axes stay fixed
  # while the plot uses the full width of its Shiny column.
  
  selected_driver_career <- shiny::reactive({
    shiny::req(input$analysis_driver)
    
    race_results |>
      tidytable::filter(
        driver_id == as.integer(input$analysis_driver)
      ) |>
      tidytable::arrange(year, round)
  })
  
  driver_season_summary <- shiny::reactive({
    selected_driver_career() |>
      tidytable::summarise(
        starts = sum(result_label != "DNS", na.rm = TRUE),
        finishes = sum(!result_label %in% c("DNF", "DNS")),
        wins = sum(position == 1, na.rm = TRUE),
        podiums = sum(position <= 3, na.rm = TRUE),
        points = sum(points, na.rm = TRUE),
        dnfs = sum(result_label == "DNF", na.rm = TRUE),
        positions_gained = sum(positions_gained, na.rm = TRUE),
        .by = year
      ) |>
      tidytable::mutate(
        finish_rate = round(finishes / starts * 100, 1)
      ) |>
      tidytable::arrange(year)
  })
  
  career_summary <- shiny::reactive({
    data <- selected_driver_career()
    starts <- sum(data$result_label != "DNS")
    finishes <- sum(!data$result_label %in% c("DNF", "DNS"))
    
    list(
      starts = starts,
      wins = sum(data$position == 1, na.rm = TRUE),
      podiums = sum(data$position <= 3, na.rm = TRUE),
      points = sum(data$points, na.rm = TRUE),
      dnfs = sum(data$result_label == "DNF"),
      finish_rate = round(finishes / starts * 100, 1)
    )
  })
  
  output$career_starts <- shiny::renderUI({
    metric_box("Starts", career_summary()$starts)
  })
  output$career_wins <- shiny::renderUI({
    metric_box("Wins", career_summary()$wins)
  })
  output$career_podiums <- shiny::renderUI({
    metric_box("Podiums", career_summary()$podiums)
  })
  output$career_points <- shiny::renderUI({
    metric_box("Points", career_summary()$points)
  })
  output$career_dnfs <- shiny::renderUI({
    metric_box("DNFs", career_summary()$dnfs)
  })
  output$career_finish_rate <- shiny::renderUI({
    metric_box("Finish rate", paste0(career_summary()$finish_rate, "%"))
  })
  
  output$career_results_chart <- shiny::renderPlot({
    data <- driver_season_summary()
    plot_data <- rbind(
      data.frame(year = data$year, measure = "Wins", total = data$wins),
      data.frame(
        year = data$year,
        measure = "Podiums",
        total = data$podiums
      )
    )
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(year, total, fill = measure)
    ) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::labs(x = "Season", y = "Total", fill = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")
  })
  
  output$career_points_chart <- shiny::renderPlot({
    ggplot2::ggplot(
      driver_season_summary(),
      ggplot2::aes(year, points)
    ) +
      ggplot2::geom_col(fill = "#E10600") +
      ggplot2::labs(x = "Season", y = "Points") +
      ggplot2::theme_minimal()
  })
  
  shiny::observeEvent(input$analysis_driver, {
    driver_years <- selected_driver_career() |>
      tidytable::filter(!is.na(year)) |>
      tidytable::distinct(year) |>
      tidytable::arrange(tidytable::desc(year)) |>
      tidytable::pull(year)
    
    shiny::req(length(driver_years) > 0)
    
    shiny::updateSelectInput(
      session = session,
      inputId = "grid_finish_year",
      choices = driver_years,
      selected = max(driver_years, na.rm = TRUE)
    )
  })
  
  output$grid_finish_chart <- plotly::renderPlotly({
    shiny::req(input$grid_finish_year)
    
    plot_data <- selected_driver_career() |>
      tidytable::filter(
        year == as.integer(input$grid_finish_year),
        grid > 0,
        !is.na(position)
      ) |>
      tidytable::mutate(
        position_change = grid - position,
        movement = tidytable::case_when(
          position_change > 0 ~ "Gained",
          position_change < 0 ~ "Lost",
          TRUE ~ "No change"
        ),
        hover_text = paste0(
          "Race: ", race_name,
          "<br>Round: ", round,
          "<br>Grid: ", grid,
          "<br>Finish: ", position,
          "<br>Positions gained/lost: ",
          tidytable::case_when(
            position_change > 0 ~ paste0("+", position_change),
            TRUE ~ as.character(position_change)
          )
        )
      )
    
    shiny::validate(
      shiny::need(
        nrow(plot_data) > 0,
        "No grid and finishing-position data are available."
      )
    )
    
    chart <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = grid,
        y = position,
        colour = movement,
        text = hover_text
      )
    ) +
      ggplot2::geom_abline(
        slope = 1,
        intercept = 0,
        linetype = "dashed",
        colour = "grey50"
      ) +
      ggplot2::geom_point(size = 4, alpha = 0.85) +
      ggplot2::scale_colour_manual(
        values = c(
          "Gained" = "#2E8B57",
          "Lost" = "#E10600",
          "No change" = "#808080"
        )
      ) +
      ggplot2::scale_x_continuous(
        breaks = 1:22,
        limits = c(1, 22),
        expand = ggplot2::expansion(
          mult = c(0.03, 0.03)
        )
      ) +
      ggplot2::scale_y_reverse(
        breaks = 1:22,
        limits = c(22, 1),
        expand = ggplot2::expansion(
          mult = c(0.03, 0.03)
        )
      ) +
      ggplot2::labs(
        title = paste0(
          input$grid_finish_year,
          " grid vs finishing position"
        ),
        subtitle = paste(
          "Green points gained positions;",
          "red points lost positions."
        ),
        x = "Starting position",
        y = "Finishing position",
        colour = NULL
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom",
        panel.grid.minor = ggplot2::element_blank()
      )
    
    plotly::ggplotly(chart, tooltip = "text") |>
      plotly::config(displaylogo = FALSE)
  })
  
  output$positions_gained_chart <- shiny::renderPlot({
    ggplot2::ggplot(
      driver_season_summary(),
      ggplot2::aes(year, positions_gained, fill = positions_gained >= 0)
    ) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
      ggplot2::labs(x = "Season", y = "Net positions gained") +
      ggplot2::theme_minimal()
  })
  
  output$driver_reliability_table <- shiny::renderTable({
    driver_season_summary() |>
      tidytable::select(year, starts, finishes, dnfs, finish_rate) |>
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
  
  # ----------------------------------------------------------
  # 6E. RELIABILITY AND DNF ANALYSIS
  # ----------------------------------------------------------
  #
  # reliability_type switches between Driver and Constructor. The matching
  # selector is generated dynamically by reliability_entity_selector.
  # selected_reliability() then filters race_results by entity and optional
  # season. reliability_summary() calculates starts, total DNFs, DNF rate
  # and technical DNFs.
  #
  # The DNF charts show broad categories, exact status.csv descriptions,
  # retirement phase and DNF rate over time. Constructor mode additionally
  # shows DNFs by driver and season-level constructor reliability statistics.
  # The final table retains the original race_status so source detail is not
  # lost by the broader analytical categories.
  
  output$reliability_entity_selector <- shiny::renderUI({
    if (input$reliability_type == "Driver") {
      shiny::selectizeInput(
        "reliability_entity",
        "Select driver:",
        choices = stats::setNames(
          driver_lookup$driver_id,
          driver_lookup$driver_label
        )
      )
    } else {
      data <- constructor_lookup |>
        tidytable::arrange(constructor_name)
      
      shiny::selectizeInput(
        "reliability_entity",
        "Select constructor:",
        choices = stats::setNames(
          data$constructor_id,
          data$constructor_name
        )
      )
    }
  })
  
  selected_reliability <- shiny::reactive({
    shiny::req(
      input$reliability_type,
      input$reliability_entity,
      input$reliability_year
    )
    
    entity_id <- as.integer(input$reliability_entity)
    
    if (input$reliability_type == "Driver") {
      data <- race_results |>
        tidytable::filter(driver_id == entity_id)
    } else {
      data <- race_results |>
        tidytable::filter(constructor_id == entity_id)
    }
    
    if (input$reliability_year != "All seasons") {
      data <- data |>
        tidytable::filter(year == as.integer(input$reliability_year))
    }
    
    data
  })
  
  reliability_summary <- shiny::reactive({
    data <- selected_reliability()
    starts <- sum(data$result_label != "DNS", na.rm = TRUE)
    dnfs <- sum(data$result_label == "DNF", na.rm = TRUE)
    
    list(
      starts = starts,
      dnfs = dnfs,
      dnf_rate = ifelse(
        starts > 0,
        round(dnfs / starts * 100, 1),
        NA_real_
      ),
      mechanical_dnfs = sum(
        data$mechanical_dnf,
        na.rm = TRUE
      )
    )
  })
  
  output$reliability_starts <- shiny::renderUI({
    metric_box("Starts", reliability_summary()$starts)
  })
  output$reliability_dnfs <- shiny::renderUI({
    metric_box("DNFs", reliability_summary()$dnfs)
  })
  output$reliability_dnf_rate <- shiny::renderUI({
    metric_box(
      "DNF rate",
      paste0(reliability_summary()$dnf_rate, "%")
    )
  })
  output$reliability_mechanical <- shiny::renderUI({
    metric_box(
      "Technical DNFs",
      reliability_summary()$mechanical_dnfs
    )
  })
  
  output$dnf_category_chart <- shiny::renderPlot({
    data <- selected_reliability() |>
      tidytable::filter(result_label == "DNF") |>
      tidytable::summarise(
        dnfs = tidytable::n(),
        .by = dnf_category
      ) |>
      tidytable::arrange(dnfs)
    
    shiny::validate(
      shiny::need(nrow(data) > 0, "No DNFs for this selection.")
    )
    
    ggplot2::ggplot(
      data,
      ggplot2::aes(
        dnfs,
        stats::reorder(dnf_category, dnfs),
        fill = dnf_category
      )
    ) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::labs(x = "DNFs", y = NULL) +
      ggplot2::theme_minimal()
  })
  
  
  output$dnf_subcategory_chart <- shiny::renderPlot({
    data <- selected_reliability() |>
      tidytable::filter(result_label == "DNF") |>
      tidytable::summarise(
        dnfs = tidytable::n(),
        .by = dnf_subcategory
      ) |>
      tidytable::arrange(dnfs)
    
    shiny::validate(
      shiny::need(nrow(data) > 0, "No DNFs for this selection.")
    )
    
    ggplot2::ggplot(
      data,
      ggplot2::aes(
        dnfs,
        stats::reorder(dnf_subcategory, dnfs)
      )
    ) +
      ggplot2::geom_col(fill = "#E10600") +
      ggplot2::labs(x = "DNFs", y = NULL) +
      ggplot2::theme_minimal()
  })
  
  output$retirement_phase_chart <- shiny::renderPlot({
    data <- selected_reliability() |>
      tidytable::filter(result_label == "DNF") |>
      tidytable::summarise(
        dnfs = tidytable::n(),
        .by = retirement_phase
      )
    
    phase_levels <- c(
      "Opening lap",
      "Early race",
      "Middle race",
      "Late race",
      "Unknown"
    )
    
    data$retirement_phase <- factor(
      data$retirement_phase,
      levels = phase_levels
    )
    
    ggplot2::ggplot(
      data,
      ggplot2::aes(retirement_phase, dnfs)
    ) +
      ggplot2::geom_col(fill = "#3671C6") +
      ggplot2::labs(x = NULL, y = "DNFs") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
      )
  })
  
  output$dnf_rate_chart <- shiny::renderPlot({
    shiny::req(input$reliability_entity)
    entity_id <- as.integer(input$reliability_entity)
    
    if (input$reliability_type == "Driver") {
      data <- race_results |>
        tidytable::filter(driver_id == entity_id)
    } else {
      data <- race_results |>
        tidytable::filter(constructor_id == entity_id)
    }
    
    data <- data |>
      tidytable::summarise(
        starts = sum(result_label != "DNS", na.rm = TRUE),
        dnfs = sum(result_label == "DNF", na.rm = TRUE),
        .by = year
      ) |>
      tidytable::filter(starts > 0) |>
      tidytable::mutate(dnf_rate = round(dnfs / starts * 100, 1)) |>
      tidytable::arrange(year)
    
    ggplot2::ggplot(data, ggplot2::aes(year, dnf_rate)) +
      ggplot2::geom_line(linewidth = 1, colour = "#E10600") +
      ggplot2::geom_point(size = 3, colour = "#E10600") +
      ggplot2::labs(x = "Season", y = "DNF rate (%)") +
      ggplot2::theme_minimal()
  })
  
  
  output$constructor_driver_dnf_chart <- shiny::renderPlot({
    shiny::req(input$reliability_type == "Constructor")
    shiny::req(input$reliability_entity)
    
    plot_data <- selected_reliability() |>
      tidytable::filter(result_label == "DNF") |>
      tidytable::summarise(
        dnfs = tidytable::n(),
        mechanical_dnfs = sum(mechanical_dnf, na.rm = TRUE),
        .by = driver_name
      ) |>
      tidytable::arrange(dnfs)
    
    shiny::validate(
      shiny::need(
        nrow(plot_data) > 0,
        "No constructor DNFs are available for this selection."
      )
    )
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = dnfs,
        y = stats::reorder(driver_name, dnfs)
      )
    ) +
      ggplot2::geom_col(fill = "#E10600") +
      ggplot2::geom_text(
        ggplot2::aes(label = dnfs),
        hjust = -0.2,
        size = 3.5
      ) +
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.12))
      ) +
      ggplot2::labs(
        x = "DNFs",
        y = NULL
      ) +
      ggplot2::theme_minimal()
  })
  
  constructor_reliability_by_season <- shiny::reactive({
    shiny::req(input$reliability_type == "Constructor")
    shiny::req(input$reliability_entity)
    
    constructor_id_selected <- as.integer(input$reliability_entity)
    
    race_results |>
      tidytable::filter(
        constructor_id == constructor_id_selected
      ) |>
      tidytable::summarise(
        entries = tidytable::n(),
        starts = sum(result_label != "DNS", na.rm = TRUE),
        classified_finishes = sum(
          !result_label %in% c("DNF", "DNS"),
          na.rm = TRUE
        ),
        dnfs = sum(result_label == "DNF", na.rm = TRUE),
        mechanical_dnfs = sum(mechanical_dnf, na.rm = TRUE),
        accident_collision_dnfs = sum(
          dnf_category == "Accident / Collision",
          na.rm = TRUE
        ),
        .by = year
      ) |>
      tidytable::filter(starts > 0) |>
      tidytable::mutate(
        finish_rate = round(
          classified_finishes / starts * 100,
          1
        ),
        dnf_rate = round(dnfs / starts * 100, 1),
        mechanical_dnf_rate = round(
          mechanical_dnfs / starts * 100,
          1
        )
      ) |>
      tidytable::arrange(tidytable::desc(year))
  })
  
  output$constructor_reliability_table <- shiny::renderTable({
    table_data <- constructor_reliability_by_season()
    
    if (input$reliability_year != "All seasons") {
      table_data <- table_data |>
        tidytable::filter(
          year == as.integer(input$reliability_year)
        )
    }
    
    table_data |>
      tidytable::select(
        year,
        starts,
        classified_finishes,
        dnfs,
        mechanical_dnfs,
        accident_collision_dnfs,
        finish_rate,
        dnf_rate,
        mechanical_dnf_rate
      ) |>
      tidytable::rename(
        Season = year,
        Starts = starts,
        `Classified finishes` = classified_finishes,
        DNFs = dnfs,
        `Mechanical DNFs` = mechanical_dnfs,
        `Accident/collision DNFs` = accident_collision_dnfs,
        `Finish rate (%)` = finish_rate,
        `DNF rate (%)` = dnf_rate,
        `Mechanical DNF rate (%)` = mechanical_dnf_rate
      )
  },
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = FALSE
  )
  
  output$dnf_detail_table <- shiny::renderTable({
    selected_reliability() |>
      tidytable::filter(result_label == "DNF") |>
      tidytable::select(
        year,
        round,
        race_name,
        driver_name,
        constructor_name,
        completed_laps,
        retirement_phase,
        race_status,
        dnf_category,
      ) |>
      tidytable::arrange(tidytable::desc(year), round) |>
      tidytable::rename(
        Season = year,
        Round = round,
        Race = race_name,
        Driver = driver_name,
        Constructor = constructor_name,
        `Laps completed` = completed_laps,
        `Race phase` = retirement_phase,
        Status = race_status,
        Category = dnf_category,
      )
  },
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  rownames = FALSE
  )
}

shiny::shinyApp(ui = ui, server = server)
