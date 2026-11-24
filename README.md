# Formula 1 Shiny Dashboard

An interactive Formula 1 dashboard built with R, Shiny, tidytable, ggplot2 and Plotly.

## Features

### Race Podiums

- Select any available season.
- Show first, second and third for every race.
- Use gold, silver and bronze podium columns.
- Use slightly smaller driver text for second and third.
- Retain constructor colours inside each podium column.

### Driver Head-to-Head

- Compare any two drivers within one selected season.
- Show wins, podiums, points and direct race score.
- Display positions 1 to 22 plus DNF and DNS.

### World Championships

- Show final end-of-season driver and constructor standings.
- Colour rows using constructor colours.

### Driver Analysis

- Career starts, wins, podiums, points, DNFs and finish rate.
- Season summaries and an interactive grid-versus-finish chart.

### Reliability & DNFs

- Analyse drivers or constructors.
- Show DNF rates, categories, exact statuses and retirement timing.

## Project structure

```text
f1_data/
|-- f1_dashboard_fully_annotated_podiums.R
|-- README.md
|-- F1_Shiny_Dashboard_Guide_100col_podiums.md
`-- data/
    |-- races.csv
    |-- results.csv
    |-- drivers.csv
    |-- constructors.csv
    |-- status.csv
    |-- driver_standings.csv
    `-- constructor_standings.csv
```

## Packages

```r
install.packages(
  c("shiny", "tidytable", "janitor", "ggplot2", "plotly")
)
```

The dashboard script also installs any missing packages automatically.

## Run the dashboard

Open `f1_dashboard_fully_annotated_podiums.R` in RStudio and click **Run App**, or run:

```r
source("f1_dashboard_fully_annotated_podiums.R")
```

## Podium design

Each race is shown as three columns:

| 1st | 2nd | 3rd |
|---|---|---|
| Gold background | Silver background | Bronze background |
| 20 px driver text | 18 px driver text | 16 px driver text |

The team colour is shown inside each podium column, so the podium background represents the
finishing position and the inner panel represents the constructor.

## Data model

`results.csv` is the central table. It joins to:

- `races.csv` through `race_id`
- `drivers.csv` through `driver_id`
- `constructors.csv` through `constructor_id`
- `status.csv` through `status_id`

`driver_standings.csv` and `constructor_standings.csv` supply final championship results.

## Documentation

See `F1_Shiny_Dashboard_Guide_100col_podiums.md` for the detailed architecture, data lineage,
code examples and troubleshooting guidance.
