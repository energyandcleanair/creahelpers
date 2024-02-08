library(httr)
library(readr)
library(dplyr)
library(lubridate)
library(progress)

api.get <- function(endpoint, date_from=NULL, date_to=NULL, split_by=NULL, format="csv", ...,
                    max_retries = 3, initial_sleep = 1, backoff_factor = 2) {
  results_list <- list()

  # Add "https://" to endpoint if not present
  if (!grepl("^http[s]?://", endpoint)) {
    endpoint <- paste0("https://", endpoint)
  }

  if (is.null(date_to)) {
    date_to <- Sys.Date()
  }

  # Create date intervals
  if (!is.null(date_from) && !is.null(split_by)) {
    start_date <- as.Date(date_from)
    end_date <- as.Date(date_to)
    dates <- seq(start_date, end_date, by = split_by)
    adjusted_dates <- c(dates[-1], end_date + 1) - 1
    date_intervals <- Map(function(start, end) list(start, end), dates, adjusted_dates)
  } else {
    date_intervals <- list(list(date_from, date_to))
  }

  # Initialize progress bar
  pb <- progress_bar$new(
    format = "[:bar] :percent :current/:total (:elapsed/:eta)",
    total = length(date_intervals),
    clear = FALSE
  )

  # Function to get data for a given interval
  get_data_for_interval <- function(interval) {
    params <- list(format = format, ...)
    if (!is.null(interval[[1]])) params$date_from <- interval[[1]]
    if (!is.null(interval[[2]])) params$date_to <- interval[[2]]

    attempt <- 1
    current_sleep <- initial_sleep
    while (attempt <= max_retries) {
      try({
        response <- GET(url = endpoint, query = params)
        if (status_code(response) == 200) {
          return(read_csv(content(response, "text"), col_types = cols()))
        }
      }, silent = TRUE)
      if (attempt < max_retries) {
        Sys.sleep(current_sleep)
        current_sleep <- current_sleep * backoff_factor
      }
      attempt <- attempt + 1
    }
    stop("API request failed after ", max_retries, " attempts")
  }

  # Retrieve data for each interval
  for (interval in date_intervals) {
    results_list[[length(results_list) + 1]] <- get_data_for_interval(interval)
    pb$tick()
  }

  bind_rows(results_list)
}

# Example usage
# result <- api.get("example.com/api/data", "2020-01-01", NULL, "month")
