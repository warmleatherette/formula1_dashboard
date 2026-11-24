# Formula 1 Shiny Dashboard Guide

**Architecture, data lineage, analysis and implementation examples**

> **Purpose:** This guide explains how the dashboard turns relational Formula 1 CSV files into
> interactive race, championship, driver and reliability analysis. It is intended as both technical
> documentation and a teaching resource.

## Dashboard areas

| Dashboard area | Main question answered |
|---|---|
| Race Podiums | Who finished first, second and third in each race? |
| Driver Head-to-Head | How did two drivers compare within the same season? |
| World Championships | What were the final driver and constructor standings? |
| Driver Analysis | How did one driver perform across their career? |
| Reliability & DNFs | How often and why did drivers or constructors fail to finish? |

## Contents

1. [Dashboard overview](#1-dashboard-overview)
2. [Required files and packages](#2-required-files-and-packages)
3. [Relational data model](#3-relational-data-model)
4. [Importing and preparing the data](#4-importing-and-preparing-the-data)
5. [Building the dashboard-ready tables](#5-building-the-dashboard-ready-tables)
6. [Shiny architecture](#6-shiny-architecture)
7. [Race Podiums tab](#7-race-podiums-tab)
8. [Driver Head-to-Head tab](#8-driver-head-to-head-tab)
9. [World Championships tab](#9-world-championships-tab)
10. [Driver Analysis tab](#10-driver-analysis-tab)
11. [Reliability and DNF analysis](#11-reliability-and-dnf-analysis)
12. [Interactive chart design](#12-interactive-chart-design)
13. [Validation and common failure points](#13-validation-and-common-failure-points)
14. [Extending the dashboard](#14-extending-the-dashboard)
15. [Appendix: complete application flow](#appendix-complete-application-flow)

## 1. Dashboard overview

The application is built around a single joined race-results table plus two final championship
tables. The key design principle is to keep the original source tables separate, join only the
fields needed for analysis, and reuse reactive datasets rather than repeating filters inside every
output.

> **Core Shiny flow:** UI input → reactive filter or calculation → render function → UI output
> placeholder.

### Example: a season selector feeding a reactive dataset

```r
shiny::selectInput(
  inputId = "overview_year",
  label = "Select season:",
  choices = sort(unique(race_results$year), decreasing = TRUE)
)

# In server()
overview_data <- shiny::reactive({
  race_results |>
    tidytable::filter(year == as.integer(input$overview_year))
})
```

### Dashboard design principles

- Use `results.csv` as the central race-level fact table because one row represents one driver in
  one race.
- Use lookup tables to replace IDs with readable race, driver, constructor and status labels.
- Restrict driver head-to-head comparisons to one selected season.
- Use the final race of each season to select final championship standings.
- Retain the original `status.csv` description for detailed DNF analysis.
- Separate presentation metadata, such as constructor colours, from analytical data.

## 2. Required files and packages

### Folder structure

```text
f1_data/
|-- f1_dashboard_fully_annotated_every_section.R
|-- README.md
|-- F1_Shiny_Dashboard_Guide.md
`-- data/
    |-- races.csv
    |-- results.csv
    |-- drivers.csv
    |-- constructors.csv
    |-- status.csv
    |-- driver_standings.csv
    `-- constructor_standings.csv
```

### Packages

| Package | Role in the dashboard |
|---|---|
| `shiny` | Creates the web application, inputs, reactives and outputs. |
| `tidytable` | Performs joins, filters, mutation, summaries and ordering. |
| `janitor` | Converts source names such as `raceId` to `race_id`. |
| `ggplot2` | Builds static charts and the ggplot base for Plotly. |
| `plotly` | Adds interactive hover behaviour to selected charts. |

### Install only packages that are missing

```r
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
```

## 3. Relational data model

The source files form a relational model. IDs identify the relationships; readable labels are stored
in separate tables. Understanding these links is the key to understanding the dashboard data flow.

| Source table | Grain | Join keys used | Dashboard contribution |
|---|---|---|---|
| `results.csv` | One driver in one race | `race_id`, `driver_id`, `constructor_id`, `status_id` | Position, grid, points, laps and result status ID |
| `races.csv` | One race | `race_id` | Season, round, race name and date |
| `drivers.csv` | One driver | `driver_id` | Driver name and dropdown label |
| `constructors.csv` | One constructor | `constructor_id` | Constructor name |
| `status.csv` | One result status | `status_id` | Exact reason or classification of the result |
| `driver_standings.csv` | One driver after one round | `race_id`, `driver_id` | Championship position, points and wins |
| `constructor_standings.csv` | One constructor after one round | `race_id`, `constructor_id` | Constructor position, points and wins |

> **Important:** `status.csv` contains the reason, but `results.csv` connects that reason to a
> specific driver and race through `status_id`.

### The four lookup joins that create readable race results

```r
race_results <- results |>
  tidytable::left_join(race_lookup, by = "race_id") |>
  tidytable::left_join(driver_lookup, by = "driver_id") |>
  tidytable::left_join(
    constructor_lookup,
    by = "constructor_id"
  ) |>
  tidytable::left_join(status_lookup, by = "status_id")
```

## 4. Importing and preparing the data

### Reusable import function

The import function centralises file-path handling, missing-value conversion and column-name
cleaning. This means every CSV is imported consistently.

```r
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
```

### Type conversion

Fields used in comparisons or calculations are explicitly converted. This avoids character ordering,
failed arithmetic and mismatched input comparisons.

```r
results <- results |>
  tidytable::mutate(
    position = as.integer(position),
    position_order = as.integer(position_order),
    grid = as.integer(grid),
    points = as.numeric(points)
  )
```

## 5. Building the dashboard-ready tables

### Lookup tables

The lookup tables keep only the descriptive fields required by the dashboard. This makes the joins
easier to inspect and avoids pulling unnecessary source columns into `race_results`.

```r
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
```

### One row per driver per race

After the joins, `distinct(race_id, driver_id)` protects the expected grain. It removes accidental
duplicates while retaining two different drivers from the same constructor in the same race.

```r
race_results <- results |>
  tidytable::left_join(race_lookup, by = "race_id") |>
  tidytable::left_join(driver_lookup, by = "driver_id") |>
  tidytable::left_join(
    constructor_lookup,
    by = "constructor_id"
  ) |>
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
```

### DNF and DNS labels

A classified numeric position is retained. Explicit did-not-start style statuses become DNS.
Remaining records without a numeric position become DNF.

```r
create_result_label <- function(position, race_status) {
  status_text <- tolower(
    ifelse(is.na(race_status), "", race_status)
  )

  did_not_start <- grepl(
    "did not start|did not qualify|withdrawn|excluded",
    status_text
  )

  tidytable::case_when(
    did_not_start ~ "DNS",
    !is.na(position) ~ as.character(position),
    TRUE ~ "DNF"
  )
}
```

### Final championship standings

The standings files contain a record after every round. The dashboard identifies the final `race_id`
in each season and joins only standings attached to that race.

```r
final_races <- race_lookup |>
  tidytable::filter(
    round == max(round, na.rm = TRUE),
    .by = year
  ) |>
  tidytable::select(
    year,
    race_id,
    final_round = round
  )

driver_championship_final <- driver_standings |>
  tidytable::inner_join(final_races, by = "race_id") |>
  tidytable::left_join(driver_lookup, by = "driver_id")
```

## 6. Shiny architecture

The user interface defines what appears on screen. The server defines how inputs change the data and
outputs. Matching IDs connect the two sides.

| UI element | Server counterpart | Purpose |
|---|---|---|
| `selectInput("overview_year")` | `input$overview_year` | Selected race-podium season |
| `uiOutput("overview_podiums")` | `output$overview_podiums` | Dynamic podium cards |
| `plotlyOutput("grid_finish_chart")` | `output$grid_finish_chart` | Interactive grid/finish plot |
| `tableOutput("dnf_detail_table")` | `output$dnf_detail_table` | Detailed retirement records |

> **Reactive reuse:** Create one clearly named reactive dataset for a tab, then use it in multiple
> outputs. This keeps filtering consistent and makes the code easier to teach and debug.

## 7. Race Podiums tab

The Race Podiums tab filters `race_results` to the selected season, keeps positions 1 to 3
and creates one three-column podium card for every race.

The outer column background communicates the podium position:

| Column | Background | Driver text size |
|---|---|---:|
| 1st | Gold (`#FFD700`) | 20 px |
| 2nd | Silver (`#C0C0C0`) | 18 px |
| 3rd | Bronze (`#CD7F32`) | 16 px |

Each column contains an inner driver panel using the constructor colour. This keeps the two
visual encodings separate: gold, silver and bronze show finishing position, while the inner
team colour identifies the constructor.

```r
podiums <- overview_data() |>
  tidytable::filter(position %in% 1:3) |>
  tidytable::arrange(round, position)
```

The three columns are created with the same helper function but different podium colours and
font sizes:

```r
create_podium_column(1, "#FFD700", "1st", "20px")
create_podium_column(2, "#C0C0C0", "2nd", "18px")
create_podium_column(3, "#CD7F32", "3rd", "16px")
```

The team colour remains visible inside each podium column:

```r
team_colour <- get_team_colour(result$constructor_name[1])
team_text_colour <- get_text_colour(team_colour)
```

> **Visual hierarchy:** First place uses the largest driver text, second place is slightly
> smaller, and third place is smaller again. The constructor name remains readable at 13 px.

## 8. Driver Head-to-Head tab

The season is selected first. The app then rebuilds both driver dropdowns using only drivers who
competed in that season. This prevents cross-season comparisons by design.

```r
season_drivers <- shiny::reactive({
  race_results |>
    tidytable::filter(
      year == as.integer(input$comparison_year)
    ) |>
    tidytable::distinct(
      driver_id,
      driver_name,
      driver_label
    )
})
```

### Direct race comparison

Each driver is filtered separately, and the two result sets are inner-joined by `race_id`. Only
races with a result row for both selected drivers enter the direct head-to-head score.

```r
first_results |>
  tidytable::inner_join(second_results, by = "race_id") |>
  tidytable::mutate(
    finished_ahead = tidytable::case_when(
      first_order < second_order ~ names$first,
      second_order < first_order ~ names$second,
      TRUE ~ "Tie"
    )
  )
```

### Fixed result axis

DNF and DNS are mapped to numeric plotting positions so they remain visible alongside classified
results.

```r
plot_position = tidytable::case_when(
  result_label == "DNF" ~ 23,
  result_label == "DNS" ~ 24,
  TRUE ~ as.numeric(position)
)

ggplot2::scale_y_reverse(
  breaks = 1:24,
  labels = c(
    as.character(1:22),
    "DNF",
    "DNS"
  ),
  limits = c(24, 1)
)
```

## 9. World Championships tab

The tab displays only final end-of-season classifications. A single season input controls both
driver and constructor championship outputs. Each row is rendered as UI so its background can use a
constructor colour.

```r
championship_data <- driver_championship_final |>
  tidytable::filter(
    year == as.integer(input$championship_year)
  ) |>
  tidytable::arrange(position)
```

> **Driver colour assumption:** If a driver raced for more than one team during a season, the row
> uses the constructor associated with their latest race result in that season.

## 10. Driver Analysis tab

The selected driver is filtered once in `selected_driver_career()`. Career totals and season
summaries are then derived from that common dataset.

```r
driver_season_summary <- shiny::reactive({
  selected_driver_career() |>
    tidytable::summarise(
      starts = sum(result_label != "DNS", na.rm = TRUE),
      finishes = sum(
        !result_label %in% c("DNF", "DNS")
      ),
      wins = sum(position == 1, na.rm = TRUE),
      podiums = sum(position <= 3, na.rm = TRUE),
      points = sum(points, na.rm = TRUE),
      dnfs = sum(result_label == "DNF", na.rm = TRUE),
      .by = year
    )
})
```

### Interactive grid vs finishing-position chart

A driver-specific season selector reduces clutter. The scatter plot uses a fixed 1–22 analytical
scale but no fixed physical aspect ratio, so it expands to the width of its Shiny column. Hover text
explains each race observation.

```r
position_change = grid - position

movement = tidytable::case_when(
  position_change > 0 ~ "Gained",
  position_change < 0 ~ "Lost",
  TRUE ~ "No change"
)

plotly::ggplotly(chart, tooltip = "text")
```

| Point colour | Meaning |
|---|---|
| Green | The driver finished higher than their grid position. |
| Red | The driver finished lower than their grid position. |
| Grey | The driver finished in the same position as they started. |

## 11. Reliability and DNF analysis

The reliability tab can analyse either a driver or a constructor. Starts exclude DNS records. DNFs
include races that were started but not classified as finishes. The DNF rate is therefore DNFs
divided by starts.

```r
starts <- sum(
  data$result_label != "DNS",
  na.rm = TRUE
)

dnfs <- sum(
  data$result_label == "DNF",
  na.rm = TRUE
)

dnf_rate <- round(dnfs / starts * 100, 1)
```

### Detailed reason from `status.csv`

The detailed reason uses the exact `race_status` joined from `status.csv`. Broad categories can
still be retained for high-level charts, but they do not replace the source description.

```r
create_dnf_subcategory <- function(result_label, race_status) {
  tidytable::case_when(
    result_label == "DNF" ~ race_status,
    TRUE ~ NA_character_
  )
}
```

### Constructor reliability

Constructor calculations use individual driver starts. A two-car constructor entering 24 races can
therefore have up to 48 starts. This denominator makes rates comparable between seasons with
different calendar lengths.

| Measure | Definition |
|---|---|
| Starts | Constructor result rows excluding DNS |
| Classified finishes | Results that are neither DNF nor DNS |
| DNF rate | DNFs / starts × 100 |
| Technical DNF rate | Technical DNFs / starts × 100 |
| Accident/collision DNFs | DNFs assigned to the accident/collision broad category |

### Retirement timing

| Phase | Completed laps |
|---|---:|
| Opening lap | 0–1 |
| Early race | 2–10 |
| Middle race | 11–40 |
| Late race | 41+ |
| Unknown | Missing lap information |

## 12. Interactive chart design

Plotly is used where individual observations benefit from hover detail. Static ggplot charts remain
appropriate for aggregated season summaries. This mixed approach avoids adding unnecessary
interactivity to every output.

```r
hover_text = paste0(
  "Race: ", race_name,
  "<br>Round: ", round,
  "<br>Grid: ", grid,
  "<br>Finish: ", position,
  "<br>Positions gained/lost: ", position_change
)
```

> **Layout fix:** Do not use `coord_fixed()` for the grid/finish Plotly chart. Removing it allows
> the plot to use the full width of the Shiny column while the data scales remain fixed at 1–22.

## 13. Validation and common failure points

| Symptom | Likely cause | Fix |
|---|---|---|
| `object 'output' not found` | An output block is outside `server()` | Move every `output$...` assignment inside the server function. |
| A tab does not appear | `tabPanel()` was added outside `navbarPage()` | Check closing brackets and keep every tab inside `navbarPage()`. |
| Duplicate race results | A lookup table contains repeated join keys | Check uniqueness and use `distinct(race_id, driver_id, .keep_all = TRUE)`. |
| A driver disappears from the chart | Missing positions were filtered out | Map DNF and DNS to explicit plotting positions. |
| Championship table shows mid-season data | All standings rounds were retained | Join standings to the final `race_id` for each season. |
| Grid/finish chart is squashed | `coord_fixed()` forces a square aspect ratio | Remove `coord_fixed()` and keep fixed axis limits only. |
| Too many tyre/wheel DNFs | Broad text matching over-classifies statuses | Use the exact `race_status` for detailed reasons and inspect grouped statuses. |

### Useful data checks

```r
# Duplicate driver/race rows
race_results |>
  tidytable::summarise(
    n = tidytable::n(),
    .by = c(race_id, driver_id)
  ) |>
  tidytable::filter(n > 1)

# Review exact DNF statuses
race_results |>
  tidytable::filter(result_label == "DNF") |>
  tidytable::summarise(
    n = tidytable::n(),
    .by = race_status
  ) |>
  tidytable::arrange(tidytable::desc(n))
```

## 14. Extending the dashboard

The current structure is suitable for additional tabs because `race_results` provides a consistent
race-level base and the server already uses reusable reactives.

Good next additions include:

- Circuit analysis: wins, podiums and reliability by circuit.
- Teammate-only comparisons: automatically restrict driver choices to the same constructor and
  season.
- Championship progression: use round-by-round standings rather than final standings only.
- Pit-stop analysis: join `pit_stops.csv` by `race_id` and `driver_id`.
- Qualifying analysis: join `qualifying.csv` and compare qualifying position with race result.
- Constructor performance: points, wins, podiums and driver contribution by season.

> **Recommended development pattern:** Prepare one clean dashboard-ready dataset before `ui` and
> `server`, create one reactive filter for each tab, and make every output consume that reactive
> dataset. This keeps the data lineage visible and reduces inconsistent filtering.

## Appendix: complete application flow

```text
Source CSV tables
        |
        v
Clean names and convert types
        |
        v
Create race, driver, constructor and status lookups
        |
        v
Join lookups to results -> race_results
        |
        +----------------------+----------------------+
        |                      |                      |
        v                      v                      v
Race/driver analysis     Final standings       Reliability analysis
        |                      |                      |
        v                      v                      v
Reactive filters         Season filter          Entity + year filter
        |                      |                      |
        v                      v                      v
UI cards/charts/tables   Championship rows      DNF metrics/charts
```
