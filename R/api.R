#' A wrapper function to call CREA API with the following functionalities:
#'
#' - split by chunk e.g. by month or year when data is too heavy to query in one go
#' - retry functionality
#' - use either explicit args or a params list
#'
#' @param endpoint
#' @param date_from
#' @param date_to
#' @param split_by
#' @param format
#' @param ...
#' @param params
#' @param max_retries
#' @param initial_sleep
#' @param backoff_factor
#'
#' @return tibble
#' @export
#'
#' @examples
#'
#' api.get("api.energyandcleanair.org/power/generation",
#'         params = list(aggregate_by="country,source,date",
#'                       country="DE",
#'                       date_from="2021-01-01"),
#'          split_by="year")
#'
#' api.get("api.energyandcleanair.org/power/generation",
#'         aggregate_by="country,source,date",
#'         country="DE",
#'         date_from="2021-01-01",
#'         split_by="year")
#'
#'
api.get <- function(endpoint, date_from=NULL, date_to=NULL, split_by=NULL, format="csv", ...,
                    params=list(),
                    max_retries = 3, initial_sleep = 1, backoff_factor = 2) {
  results_list <- list()

  # Add "https://" to endpoint if not present
  if (!grepl("^http[s]?://", endpoint)) {
    endpoint <- paste0("https://", endpoint)
  }

  # Update params with specific function arguments
  default_params <- list(date_from = date_from, date_to = date_to, format = format)
  params <- modifyList(default_params, params)

  # Merge ... arguments with params
  args <- list(...)
  params <- modifyList(params, args)

  if (is.null(params$date_to)) {
    params$date_to <- Sys.Date()
  }


  # Create date intervals
  if (!is.null(date_from) && !is.null(split_by)) {
    start_date <- as.Date(params$date_from)
    end_date <- as.Date(params$date_to)
    dates <- seq(start_date, end_date, by = split_by)
    adjusted_dates <- c(dates[-1], end_date + 1) - 1
    date_intervals <- Map(function(start, end) list(start, end), dates, adjusted_dates)
  } else {
    date_intervals <- list(list(params$date_from, params$date_to))
  }

  # Initialize progress bar
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent :current/:total (:elapsed/:eta)",
    total = length(date_intervals),
    clear = FALSE
  )

  # Function to get data for a given interval
  get_data_for_interval <- function(interval) {
    interval_params <- params
    if (!is.null(interval[[1]])) interval_params$date_from <- interval[[1]]
    if (!is.null(interval[[2]])) interval_params$date_to <- interval[[2]]

    # If any parameter is a list, convert it to a string
    interval_params <- lapply(interval_params, function(x) if (is.vector(x)) paste(x, collapse = ",") else x)

    attempt <- 1
    current_sleep <- initial_sleep
    while (attempt <= max_retries) {
      # try({
        response <- httr::GET(url = endpoint, query = interval_params)
        if (httr::status_code(response) == 200) {
          return(read_csv(httr::content(response, "text"), col_types = cols()))
        }
      # }, silent = TRUE)
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
