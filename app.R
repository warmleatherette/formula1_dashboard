# ============================================================
# FORMULA 1 SHINY DASHBOARD
# Fully annotated version
# ============================================================
#
# Tabs:
#   1. Race Podiums
#   2. Driver Head-to-Head
#   3. World Championships
#   4. Driver Analysis
#   5. Reliability & DNFs
#
# Required CSV files are listed in README.md.
# ============================================================

# ------------------------------------------------------------
# 0. INSTALL AND LOAD PACKAGES
# ------------------------------------------------------------
# Install only packages that are not already available.

required_packages <- c(
  "shiny", "tidytable", "janitor", "ggplot2", "plotly"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

library(shiny)
library(tidytable)
library(janitor)
library(ggplot2)
library(plotly)


# ------------------------------------------------------------
# 1. IMPORT SOURCE TABLES
# ------------------------------------------------------------
# read_f1_csv() applies the same path, missing-value and naming
# rules to every source file.

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

# Convert variables used in arithmetic and comparisons.
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
    points = as.numeric(points),
    laps = as.integer(laps)
  )

for (object_name in c("driver_standings", "constructor_standings")) {
  object_value <- get(object_name) |>
    tidytable::mutate(
      position = as.integer(position),
      points = as.numeric(points),
      wins = as.integer(wins)
    )
  assign(object_name, object_value)
}


# ------------------------------------------------------------
# 2. CREATE LOOKUP TABLES
# ------------------------------------------------------------
# These tables replace IDs with readable labels while keeping
# the joins small and easy to inspect.

race_lookup <- races |>
  tidytable::rename(
    race_name = name,
    race_date = date
  ) |>
  tidytable::select(race_id, year, round, race_name, race_date)

driver_lookup <- drivers |>
  tidytable::mutate(
    driver_name = paste(forename, surname),
    driver_label = paste0(surname, ", ", forename)
  ) |>
  tidytable::select(driver_id, driver_name, driver_label)

constructor_lookup <- constructors |>
  tidytable::rename(constructor_name = name) |>
  tidytable::select(constructor_id, constructor_name)

status_lookup <- status |>
  tidytable::rename(race_status = status) |>
  tidytable::select(status_id, race_status)


# ------------------------------------------------------------
# 3. DISPLAY METADATA AND RESULT CLASSIFICATION
# ------------------------------------------------------------
# Constructor colours are presentation metadata. The podium
# background communicates position; the inner driver card keeps
# the selected driver's constructor colour.

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
  if (length(colour) == 0 || is.na(colour)) "#666666" else colour
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
      "did not start", "did not qualify", "did not prequalify",
      "withdrawn", "excluded", sep = "|"
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
  status_text <- tolower(ifelse(is.na(race_status), "", race_status))
  
  tidytable::case_when(
    result_label != "DNF" ~ NA_character_,
    grepl("accident|collision|spun", status_text) ~
      "Accident / Collision",
    grepl(
      paste0(
        "engine|power unit|turbo|gearbox|transmission|clutch|",
        "driveshaft|differential|hydraulic|suspension|brake|",
        "steering|throttle|fuel|oil|water|radiator|overheating"
      ),
      status_text
    ) ~ "Mechanical",
    grepl("electrical|electronics|battery|alternator", status_text) ~
      "Electrical",
    grepl("puncture|tyre|tire|wheel", status_text) ~ "Tyre / Wheel",
    TRUE ~ "Other"
  )
}

# Keep the exact source description for detailed DNF reporting.
create_dnf_subcategory <- function(result_label, race_status) {
  tidytable::case_when(
    result_label == "DNF" ~ race_status,
    TRUE ~ NA_character_
  )
}


# ------------------------------------------------------------
# 4. CREATE DASHBOARD-READY TABLES
# ------------------------------------------------------------
# results.csv is the central fact table: one row represents one
# driver in one race. The lookups add readable labels.

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
    ),
    dnf_category = create_dnf_category(result_label, race_status),
    dnf_subcategory = create_dnf_subcategory(
      result_label,
      race_status
    ),
    technical_dnf = result_label == "DNF" &
      dnf_category %in% c(
        "Mechanical", "Electrical", "Tyre / Wheel"
      ),
    retirement_phase = tidytable::case_when(
      result_label != "DNF" ~ NA_character_,
      laps <= 1 ~ "Opening lap",
      laps <= 10 ~ "Early race",
      laps <= 40 ~ "Middle race",
      laps > 40 ~ "Late race",
      TRUE ~ "Unknown"
    )
  )

# The standings files contain records after every round. Join
# them to the last race_id in each season to keep final results.
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

# Use each driver's final constructor of the season to colour
# their championship row.
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


# ------------------------------------------------------------
# 5. USER INTERFACE
# ------------------------------------------------------------
# Each inputId becomes input$<id> in server. Each output
# placeholder links to an output$<id> render function.

ui <- shiny::navbarPage(
  title = "Formula 1 Dashboard",
  
  # Race Podiums replaces the former Race Winners tab.
  shiny::tabPanel(
    title = "Race Podiums",
    shiny::fluidPage(
      shiny::titlePanel("Formula 1 Race Podiums"),
      shiny::selectInput(
        "overview_year",
        "Select season:",
        choices = sort(unique(race_results$year), decreasing = TRUE),
        selected = max(race_results$year, na.rm = TRUE)
      ),
      shiny::hr(),
      shiny::uiOutput("overview_podiums")
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
            "grid_finish_year",
            "Select season:",
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
        shiny::column(3, shiny::uiOutput("reliability_technical"))
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
          shiny::plotOutput("dnf_status_chart", height = "420px")
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
            shiny::h3("Reliability by season"),
            shiny::tableOutput("constructor_reliability_table")
          )
        )
      ),
      shiny::h3("Individual DNFs"),
      shiny::tableOutput("dnf_detail_table")
    )
  )
)


# ------------------------------------------------------------
# 6. SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # Reusable summary card.
  metric_box <- function(title, text) {
    shiny::div(
      style = paste0(
        "background-color:#f5f5f5;",
        "border-top:5px solid #e10600;",
        "border-radius:5px;min-height:110px;",
        "margin-bottom:15px;padding:15px;"
      ),
      shiny::h4(title),
      shiny::div(text, style = "font-size:16px;font-weight:bold;")
    )
  }
  
  # Reusable coloured championship row.
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
        "align-items:center;background-color:", team_colour,
        ";color:", text_colour,
        ";border-radius:5px;margin-bottom:5px;padding:10px;"
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
  # 6A. RACE PODIUMS
  # ----------------------------------------------------------
  # overview_data() filters once. overview_podiums keeps the top
  # three and builds one responsive three-column card per race.
  
  overview_data <- shiny::reactive({
    shiny::req(input$overview_year)
    race_results |>
      tidytable::filter(year == as.integer(input$overview_year))
  })
  
  output$overview_podiums <- shiny::renderUI({
    podiums <- overview_data() |>
      tidytable::filter(position %in% 1:3) |>
      tidytable::arrange(round, position)
    
    shiny::validate(
      shiny::need(
        nrow(podiums) > 0,
        "No podium information is available for this season."
      )
    )
    
    races_with_podiums <- podiums |>
      tidytable::distinct(race_id, round, race_name) |>
      tidytable::arrange(round)
    
    shiny::tagList(
      lapply(seq_len(nrow(races_with_podiums)), function(i) {
        race_data <- podiums |>
          tidytable::filter(
            race_id == races_with_podiums$race_id[i]
          ) |>
          tidytable::arrange(position)
        
        create_podium_column <- function(
    position_number,
    podium_colour,
    position_label,
    driver_font_size
        ) {
          result <- race_data |>
            tidytable::filter(position == position_number)
          
          if (nrow(result) == 0) {
            return(
              shiny::div(
                style = paste0(
                  "flex:1;background-color:", podium_colour,
                  ";padding:15px;text-align:center;"
                ),
                shiny::strong(position_label),
                shiny::tags$br(),
                shiny::span("No result available")
              )
            )
          }
          
          team_colour <- get_team_colour(result$constructor_name[1])
          team_text_colour <- get_text_colour(team_colour)
          
          shiny::div(
            style = paste0(
              "flex:1;background-color:", podium_colour,
              ";padding:15px;text-align:center;min-width:0;"
            ),
            shiny::div(
              position_label,
              style = paste0(
                "font-size:18px;font-weight:bold;",
                "margin-bottom:12px;color:#202124;"
              )
            ),
            shiny::div(
              style = paste0(
                "background-color:", team_colour,
                ";color:", team_text_colour,
                ";border-radius:6px;padding:12px 8px;"
              ),
              shiny::div(
                result$driver_name[1],
                style = paste0(
                  "font-size:", driver_font_size,
                  ";font-weight:bold;"
                )
              ),
              shiny::div(
                result$constructor_name[1],
                style = "font-size:13px;margin-top:4px;"
              )
            )
          )
        }
        
        shiny::div(
          style = paste0(
            "border:1px solid #dddddd;border-radius:8px;",
            "overflow:hidden;margin-bottom:18px;"
          ),
          shiny::div(
            style = paste0(
              "background-color:#202124;color:white;",
              "padding:12px 15px;"
            ),
            shiny::strong(
              paste0(
                "Round ", races_with_podiums$round[i], ": ",
                races_with_podiums$race_name[i]
              )
            )
          ),
          shiny::div(
            style = "display:flex;width:100%;",
            create_podium_column(1, "#FFD700", "1st", "20px"),
            create_podium_column(2, "#C0C0C0", "2nd", "18px"),
            create_podium_column(3, "#CD7F32", "3rd", "16px")
          )
        )
      })
    )
  })
  
  # ----------------------------------------------------------
  # 6B. DRIVER HEAD-TO-HEAD
  # ----------------------------------------------------------
  
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
    ids <- as.integer(
      c(input$comparison_driver_1, input$comparison_driver_2)
    )
    shiny::validate(
      shiny::need(ids[1] != ids[2], "Select two different drivers.")
    )
    race_results |>
      tidytable::filter(
        year == as.integer(input$comparison_year),
        driver_id %in% ids
      ) |>
      tidytable::arrange(round, driver_name)
  })
  
  selected_driver_names <- shiny::reactive({
    ids <- as.integer(
      c(input$comparison_driver_1, input$comparison_driver_2)
    )
    data <- driver_lookup |>
      tidytable::filter(driver_id %in% ids)
    list(
      first = data$driver_name[data$driver_id == ids[1]][1],
      second = data$driver_name[data$driver_id == ids[2]][1]
    )
  })
  
  output$comparison_heading <- shiny::renderUI({
    names <- selected_driver_names()
    shiny::h2(
      paste0(
        names$first, " vs ", names$second, ": ",
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
        race_id, round, race_name,
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
        names$first, ": ",
        sum(data$finished_ahead == names$first), " | ",
        names$second, ": ",
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
    data <- selected_comparison() |>
      tidytable::mutate(
        plot_position = tidytable::case_when(
          result_label == "DNF" ~ 23,
          result_label == "DNS" ~ 24,
          TRUE ~ as.numeric(position)
        )
      )
    
    ggplot2::ggplot(
      data,
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
        breaks = sort(unique(data$round))
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
        round, race_name, first_result,
        second_result, finished_ahead
      )
    names(data) <- c(
      "Round", "Race", names$first,
      names$second, "Finished ahead"
    )
    data
  }, striped = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE)
  
  # ----------------------------------------------------------
  # 6C. WORLD CHAMPIONSHIPS
  # ----------------------------------------------------------
  
  output$driver_championship_table <- shiny::renderUI({
    data <- driver_championship_final |>
      tidytable::filter(year == as.integer(input$championship_year)) |>
      tidytable::arrange(position)
    shiny::tagList(
      championship_header("Driver"),
      lapply(seq_len(nrow(data)), function(i) {
        championship_row(
          data$position[i], data$driver_name[i], data$points[i],
          data$wins[i], data$constructor_name[i]
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
          data$position[i], data$constructor_name[i],
          data$points[i], data$wins[i],
          data$constructor_name[i]
        )
      })
    )
  })
  
  # ----------------------------------------------------------
  # 6D. DRIVER ANALYSIS
  # ----------------------------------------------------------
  
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
        finishes = sum(
          !result_label %in% c("DNF", "DNS"),
          na.rm = TRUE
        ),
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
    starts <- sum(data$result_label != "DNS", na.rm = TRUE)
    finishes <- sum(
      !data$result_label %in% c("DNF", "DNS"),
      na.rm = TRUE
    )
    list(
      starts = starts,
      wins = sum(data$position == 1, na.rm = TRUE),
      podiums = sum(data$position <= 3, na.rm = TRUE),
      points = sum(data$points, na.rm = TRUE),
      dnfs = sum(data$result_label == "DNF", na.rm = TRUE),
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
    metric_box(
      "Finish rate",
      paste0(career_summary()$finish_rate, "%")
    )
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
    years <- selected_driver_career() |>
      tidytable::distinct(year) |>
      tidytable::arrange(tidytable::desc(year)) |>
      tidytable::pull(year)
    shiny::updateSelectInput(
      session,
      "grid_finish_year",
      choices = years,
      selected = max(years, na.rm = TRUE)
    )
  })
  
  output$grid_finish_chart <- plotly::renderPlotly({
    shiny::req(input$grid_finish_year)
    data <- selected_driver_career() |>
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
          "<br>Positions gained/lost: ", position_change
        )
      )
    
    chart <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        grid,
        position,
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
        expand = ggplot2::expansion(mult = c(0.03, 0.03))
      ) +
      ggplot2::scale_y_reverse(
        breaks = 1:22,
        limits = c(22, 1),
        expand = ggplot2::expansion(mult = c(0.03, 0.03))
      ) +
      ggplot2::labs(
        x = "Starting position",
        y = "Finishing position",
        colour = NULL
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")
    
    plotly::ggplotly(chart, tooltip = "text") |>
      plotly::config(displaylogo = FALSE)
  })
  
  output$positions_gained_chart <- shiny::renderPlot({
    ggplot2::ggplot(
      driver_season_summary(),
      ggplot2::aes(
        year,
        positions_gained,
        fill = positions_gained >= 0
      )
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
  }, striped = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE)
  
  # ----------------------------------------------------------
  # 6E. RELIABILITY AND DNF ANALYSIS
  # ----------------------------------------------------------
  
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
      technical_dnfs = sum(data$technical_dnf, na.rm = TRUE)
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
  output$reliability_technical <- shiny::renderUI({
    metric_box("Technical DNFs", reliability_summary()$technical_dnfs)
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
  
  output$dnf_status_chart <- shiny::renderPlot({
    data <- selected_reliability() |>
      tidytable::filter(result_label == "DNF") |>
      tidytable::summarise(
        dnfs = tidytable::n(),
        .by = dnf_subcategory
      ) |>
      tidytable::arrange(dnfs)
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
      tidytable::mutate(dnf_rate = round(dnfs / starts * 100, 1))
    ggplot2::ggplot(data, ggplot2::aes(year, dnf_rate)) +
      ggplot2::geom_line(linewidth = 1, colour = "#E10600") +
      ggplot2::geom_point(size = 3, colour = "#E10600") +
      ggplot2::labs(x = "Season", y = "DNF rate (%)") +
      ggplot2::theme_minimal()
  })
  
  output$retirement_phase_chart <- shiny::renderPlot({
    data <- selected_reliability() |>
      tidytable::filter(result_label == "DNF") |>
      tidytable::summarise(
        dnfs = tidytable::n(),
        .by = retirement_phase
      )
    data$retirement_phase <- factor(
      data$retirement_phase,
      levels = c(
        "Opening lap", "Early race", "Middle race",
        "Late race", "Unknown"
      )
    )
    ggplot2::ggplot(data, ggplot2::aes(retirement_phase, dnfs)) +
      ggplot2::geom_col(fill = "#3671C6") +
      ggplot2::labs(x = NULL, y = "DNFs") +
      ggplot2::theme_minimal()
  })
  
  output$constructor_driver_dnf_chart <- shiny::renderPlot({
    shiny::req(input$reliability_type == "Constructor")
    data <- selected_reliability() |>
      tidytable::filter(result_label == "DNF") |>
      tidytable::summarise(
        dnfs = tidytable::n(),
        .by = driver_name
      ) |>
      tidytable::arrange(dnfs)
    ggplot2::ggplot(
      data,
      ggplot2::aes(dnfs, stats::reorder(driver_name, dnfs))
    ) +
      ggplot2::geom_col(fill = "#E10600") +
      ggplot2::labs(x = "DNFs", y = NULL) +
      ggplot2::theme_minimal()
  })
  
  output$constructor_reliability_table <- shiny::renderTable({
    shiny::req(input$reliability_type == "Constructor")
    constructor_id_selected <- as.integer(input$reliability_entity)
    race_results |>
      tidytable::filter(constructor_id == constructor_id_selected) |>
      tidytable::summarise(
        starts = sum(result_label != "DNS", na.rm = TRUE),
        finishes = sum(
          !result_label %in% c("DNF", "DNS"),
          na.rm = TRUE
        ),
        dnfs = sum(result_label == "DNF", na.rm = TRUE),
        technical_dnfs = sum(technical_dnf, na.rm = TRUE),
        .by = year
      ) |>
      tidytable::mutate(
        finish_rate = round(finishes / starts * 100, 1),
        dnf_rate = round(dnfs / starts * 100, 1)
      ) |>
      tidytable::arrange(tidytable::desc(year)) |>
      tidytable::rename(
        Season = year,
        Starts = starts,
        Finishes = finishes,
        DNFs = dnfs,
        `Technical DNFs` = technical_dnfs,
        `Finish rate (%)` = finish_rate,
        `DNF rate (%)` = dnf_rate
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE)
  
  output$dnf_detail_table <- shiny::renderTable({
    selected_reliability() |>
      tidytable::filter(result_label == "DNF") |>
      tidytable::select(
        year, round, race_name, driver_name,
        constructor_name, laps, retirement_phase,
        race_status, dnf_category
      ) |>
      tidytable::arrange(tidytable::desc(year), round) |>
      tidytable::rename(
        Season = year,
        Round = round,
        Race = race_name,
        Driver = driver_name,
        Constructor = constructor_name,
        `Laps completed` = laps,
        `Race phase` = retirement_phase,
        `Actual status` = race_status,
        Category = dnf_category
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, rownames = FALSE)
}


# ------------------------------------------------------------
# 7. RUN THE APP
# ------------------------------------------------------------

shiny::shinyApp(ui = ui, server = server)
