
# ============================================================
# Jolpica / Ergast F1 Data Downloader
# Seasons: 1999-2024
# Outputs:
#   drivers.csv
#   constructors.csv
#   circuits.csv
#   seasons.csv
#   results.csv
# ============================================================


library(jsonlite)
library(dplyr)
library(purrr)
library(readr)

# ============================================================
# SETTINGS
# ============================================================

output_dir <- "ergast_csv"

dir.create(output_dir, showWarnings = FALSE)

years <- 2000:2026


# ============================================================
# SAFE DOWNLOAD WITH RETRY
# ============================================================

safe_json <- function(url,
                      retries = 5,
                      wait_seconds = 5) {
  
  for (i in seq_len(retries)) {
    
    result <- tryCatch(
      fromJSON(url, flatten = TRUE),
      error = function(e) e
    )
    
    if (!inherits(result, "error")) {
      return(result)
    }
    
    cat(
      "Attempt",
      i,
      "failed. Waiting",
      wait_seconds,
      "seconds...\n"
    )
    
    Sys.sleep(wait_seconds)
  }
  
  stop(paste("Failed:", url))
}

# ============================================================
# PAGINATED DRIVER DOWNLOAD
# ============================================================

get_all_drivers <- function() {
  
  limit <- 100
  offset <- 0
  
  out <- list()
  
  repeat {
    
    url <- sprintf(
      "https://api.jolpi.ca/ergast/f1/drivers?limit=%s&offset=%s",
      limit,
      offset
    )
    
    cat("Drivers offset:", offset, "\n")
    
    x <- safe_json(url)
    
    drivers <- x$MRData$DriverTable$Drivers
    
    if (nrow(drivers) == 0)
      break
    
    out[[length(out) + 1]] <- drivers
    
    total <- as.numeric(x$MRData$total)
    
    offset <- offset + limit
    
    if (offset >= total)
      break
    
    Sys.sleep(1)
  }
  
  bind_rows(out)
}

# ============================================================
# PAGINATED CONSTRUCTORS
# ============================================================

get_all_constructors <- function() {
  
  limit <- 100
  offset <- 0
  
  out <- list()
  
  repeat {
    
    url <- sprintf(
      "https://api.jolpi.ca/ergast/f1/constructors?limit=%s&offset=%s",
      limit,
      offset
    )
    
    cat("Constructors offset:", offset, "\n")
    
    x <- safe_json(url)
    
    constructors <- x$MRData$ConstructorTable$Constructors
    
    if (nrow(constructors) == 0)
      break
    
    out[[length(out) + 1]] <- constructors
    
    total <- as.numeric(x$MRData$total)
    
    offset <- offset + limit
    
    if (offset >= total)
      break
    
    Sys.sleep(1)
  }
  
  bind_rows(out)
}

# ============================================================
# PAGINATED CIRCUITS
# ============================================================

get_all_circuits <- function() {
  
  limit <- 100
  offset <- 0
  
  out <- list()
  
  repeat {
    
    url <- sprintf(
      "https://api.jolpi.ca/ergast/f1/circuits?limit=%s&offset=%s",
      limit,
      offset
    )
    
    cat("Circuits offset:", offset, "\n")
    
    x <- safe_json(url)
    
    circuits <- x$MRData$CircuitTable$Circuits
    
    if (nrow(circuits) == 0)
      break
    
    out[[length(out) + 1]] <- circuits
    
    total <- as.numeric(x$MRData$total)
    
    offset <- offset + limit
    
    if (offset >= total)
      break
    
    Sys.sleep(1)
  }
  
  bind_rows(out)
}

# ============================================================
# SEASONS
# ============================================================

get_all_seasons <- function() {
  
  x <- safe_json(
    "https://api.jolpi.ca/ergast/f1/seasons?limit=1000")
  
  x$MRData$SeasonTable$Seasons
}

# ============================================================
# RACE RESULTS
# ============================================================

get_results <- function(years) {
  
  all_results <- list()
  
  for (yr in years) {
    
    cat("Processing season:", yr, "\n")
    
    url <- sprintf(
      "https://api.jolpi.ca/ergast/f1/%s/results",
      yr
    )
    
    x <- safe_json(url)
    
    races <- x$MRData$RaceTable$Races
    
    if (nrow(races) == 0)
      next
    
    season_results <- map_dfr(
      seq_len(nrow(races)),
      function(i) {
        
        res <- races$Results[[i]]
        
        if (is.null(res))
          return(NULL)
        
        if (nrow(res) == 0)
          return(NULL)
        
        bind_cols(
          races[i,
                c(
                  "season",
                  "round",
                  "raceName",
                  "date"
                )
          ],
          res
        )
      }
    )
    
    all_results[[length(all_results) + 1]] <- season_results
    
    Sys.sleep(1)
  }
  
  bind_rows(all_results)
}

# ============================================================
# DOWNLOAD EVERYTHING
# ============================================================

cat("Downloading drivers...\n")
drivers <- get_all_drivers()

cat("Downloading constructors...\n")
constructors <- get_all_constructors()

cat("Downloading circuits...\n")
circuits <- get_all_circuits()

cat("Downloading seasons...\n")
seasons <- get_all_seasons()

cat("Downloading race results...\n")
results <- get_results(years)

# ============================================================
# SAVE FILES
# ============================================================

write_csv(
  drivers,
  file.path(output_dir, "drivers.csv")
)

write_csv(
  constructors,
  file.path(output_dir, "constructors.csv")
)

write_csv(
  circuits,
  file.path(output_dir, "circuits.csv")
)

write_csv(
  seasons,
  file.path(output_dir, "seasons.csv")
)

write_csv(
  results,
  file.path(output_dir, paste0("results.csv")
  )
  
  cat("\nDONE\n")
  cat("Files written to:", normalizePath(output_dir), "\n") cat("Drivers:", nrow(drivers), "\n") cat("Constructors:", nrow(constructors), "\n") cat("Circuits:", nrow(circuits), "\n") cat("Results:", nrow(results), "\n")
  