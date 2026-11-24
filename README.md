# Formula 1 Shiny Dashboard

An interactive Formula 1 dashboard built in R using Shiny, tidytable, ggplot2 and Plotly.

The dashboard combines historical race results, championship standings and result-status data to support race-winner summaries, driver comparisons, career analysis and reliability analysis for drivers and constructors.

## Dashboard features

### Race Winners

- Select any available season.
- Display the winner of every race in round order.
- Show the winning driver and constructor.
- Colour each winner card using the constructor colour lookup.
- Automatically switch between black and white text for readability.

### Driver Head-to-Head

- Compare any two drivers within the same selected season.
- Restrict the season selector to the latest 20 seasons available in the data.
- Populate the driver selectors only with drivers who competed in that season.
- Prevent the same driver from being selected twice.
- Compare:
  - race head-to-head score
  - wins
  - podiums
  - points
  - race-by-race finishing results
- Display classified positions from 1 to 22 plus DNF and DNS.

### World Championships

- Display final end-of-season standings only.
- Show Drivers' World Championship results.
- Show Constructors' World Championship results.
- Include championship position, points and wins.
- Colour standings rows using constructor colours.

### Driver Analysis

- Select a driver using a searchable surname-first dropdown.
- Display career totals for:
  - starts
  - wins
  - podiums
  - points
  - DNFs
  - finish rate
- Analyse wins, podiums and points by season.
- Show positions gained or lost by season.
- Compare grid position with finishing position in an interactive Plotly chart.
- Select a season for the grid-versus-finish chart.
- Hover over race points to view race, round, grid, finish and position change.

### Reliability and DNF Analysis

- Switch between driver and constructor analysis.
- Analyse all seasons or one selected season.
- Display:
  - starts
  - DNFs
  - DNF rate
  - technical DNFs
- Analyse broad DNF categories.
- Retain the exact result reason from `status.csv` for detailed analysis.
- Show when retirements occurred during the race.
- Display individual DNF records.
- For constructors, also show:
  - DNFs by driver
  - classified finishes
  - mechanical or technical DNF rate
  - accident and collision DNFs
  - season-by-season reliability

## Required folder structure

```text
formula1_dashboard/
|-- README.md
|-- data/
    |-- races.csv
    |-- results.csv
    |-- drivers.csv
    |-- constructors.csv
    |-- status.csv
    |-- driver_standings.csv
    `-- constructor_standings.csv
```

The script expects the CSV files to be inside a folder named `data` relative to the working directory.

## Required R packages

The dashboard uses:

- `shiny`
- `tidytable`
- `janitor`
- `ggplot2`
- `plotly`

The script checks for missing packages and installs only those that are not already installed.

To install them manually:

```r
install.packages(
  c(
    "shiny",
    "tidytable",
    "janitor",
    "ggplot2",
    "plotly"
  )
)
```

## Running the dashboard

### RStudio

1. Open the project folder in RStudio.
2. Open `f1_dashboard_fully_annotated_every_section.R`.
3. Confirm that the `data` folder contains all seven required CSV files.
4. Click **Run App**.

### R console

Set the working directory to the project folder and run:

```r
shiny::runApp("f1_dashboard_fully_annotated_every_section.R")
```

Alternatively, source the script:

```r
source("f1_dashboard_fully_annotated_every_section.R")
```

## Data model

The dashboard keeps the source tables separate and joins them using their ID fields.

| Source table | Grain | Main join keys | Dashboard use |
|---|---|---|---|
| `results.csv` | One driver in one race | `race_id`, `driver_id`, `constructor_id`, `status_id` | Grid, finish, points, laps and result status |
| `races.csv` | One race | `race_id` | Season, round, race name and date |
| `drivers.csv` | One driver | `driver_id` | Driver name and dropdown label |
| `constructors.csv` | One constructor | `constructor_id` | Constructor name |
| `status.csv` | One result status | `status_id` | Exact result or retirement reason |
| `driver_standings.csv` | One driver after one round | `race_id`, `driver_id` | Championship position, points and wins |
| `constructor_standings.csv` | One constructor after one round | `race_id`, `constructor_id` | Constructor position, points and wins |

## Main joined table

`race_results` is the central dashboard-ready table. It starts from `results.csv` because one row represents one driver in one race.

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
  )
```

The `distinct()` step protects the expected grain of one row per driver per race without removing the second driver from the same constructor.

## DNF and DNS logic

Each race result is assigned one of three display outcomes:

- a numeric classified finishing position
- `DNF` for Did Not Finish
- `DNS` for Did Not Start

The detailed DNF reason comes directly from the `status` field in `status.csv`, joined through `status_id` in `results.csv`.

```r
create_dnf_subcategory <- function(result_label, race_status) {
  tidytable::case_when(
    result_label == "DNF" ~ race_status,
    TRUE ~ NA_character_
  )
}
```

Broad DNF categories are retained for summary charts, while the exact `race_status` is retained in the detailed table.

## Championship logic

The standings CSV files contain championship positions after every round. The dashboard identifies the final race in each season and keeps only standings attached to that race.

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
```

This prevents mid-season standings from appearing in the World Championships tab.

## Interactive grid-versus-finish chart

The chart uses a fixed analytical range from 1 to 22 on both axes but does not use `coord_fixed()`. This allows the chart to expand across the available Shiny column width.

Point colours mean:

- green: positions gained
- red: positions lost
- grey: no change

Hover text includes:

- race name
- round
- grid position
- finishing position
- positions gained or lost

## Constructor colours

Constructor colours are stored separately from race results because they are display metadata rather than analytical data.

Constructors that are not included in the colour lookup use a neutral grey fallback.

## Reliability definitions

### Starts

A result counts as a start when it is not labelled `DNS`.

### Classified finishes

A classified finish is a result that is neither `DNF` nor `DNS`.

### DNF rate

```text
DNF rate = DNFs / starts * 100
```

### Constructor starts

Constructor reliability uses individual driver starts. For example, a two-car constructor entering 24 races can have up to 48 starts.

## Common issues

### `object 'output' not found`

An `output$...` block is probably outside the `server()` function. Move every rendered output inside:

```r
server <- function(input, output, session) {
  # All output$... objects must be here.
}
```

### A tab does not appear

Check that its `tabPanel()` is inside `navbarPage()` and that the preceding closing brackets have not closed the navigation page early.

### Duplicate race results

Check for duplicate combinations of `race_id` and `driver_id`:

```r
race_results |>
  tidytable::summarise(
    n = tidytable::n(),
    .by = c(race_id, driver_id)
  ) |>
  tidytable::filter(n > 1)
```

### Mid-season championship standings appear

Confirm that `driver_standings` and `constructor_standings` are joined to the final `race_id` for each season.

### Grid-versus-finish chart is squashed

Remove `ggplot2::coord_fixed()`. Keep the axis limits fixed, but allow Plotly to use the full chart width.

### Too many results fall into one DNF category

Review the exact statuses before changing group rules:

```r
race_results |>
  tidytable::filter(result_label == "DNF") |>
  tidytable::summarise(
    n = tidytable::n(),
    .by = race_status
  ) |>
  tidytable::arrange(tidytable::desc(n))
```

## Possible extensions

- circuit-level driver and constructor analysis
- teammate-only comparisons
- round-by-round championship progression
- qualifying-versus-race performance
- pit-stop analysis using `pit_stops.csv`
- fastest-lap analysis
- constructor driver-contribution charts
- season and circuit reliability comparisons

## Development approach

When adding a new dashboard area:

1. Prepare a clean analysis table before `ui` and `server` where possible.
2. Create one clearly named reactive dataset for the tab.
3. Make the tab's summary boxes, charts and tables reuse that reactive dataset.
4. Keep presentation metadata, such as colours, separate from analytical data.
5. Validate the expected grain after joins.
6. Use the original source status alongside any grouped reliability categories.

## Documentation

The project also includes a Word guide covering the dashboard architecture, data lineage, analysis and implementation examples:

`F1_Shiny_Dashboard_Guide.docx`

## Licence and data source

Add the licence and original dataset source applicable to your copy of the Formula 1 CSV files before publishing or distributing the dashboard.
