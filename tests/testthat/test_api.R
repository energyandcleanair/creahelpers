test_that("api.get works for split_by and params", {

  date_from <- "2023-01-01"
  start <- Sys.time()
  onego <- creahelpers::api.get("api.energyandcleanair.org/power/generation",
            date_from=date_from,
            aggregate_by="country,source,date",
            country="DE",
            use_cache = F)
  onego_time <- Sys.time() - start

  start <- Sys.time()
  by_week <- creahelpers::api.get(endpoint="api.energyandcleanair.org/power/generation",
                                   date_from=date_from,
                                   aggregate_by="country,source,date",
                                   country="DE",
                                   split_by="week",
                                  use_cache = F)
  by_week_time <- Sys.time() - start

  start <- Sys.time()
  by_month <- creahelpers::api.get(endpoint="api.energyandcleanair.org/power/generation",
                 date_from=date_from,
                 aggregate_by="country,source,date",
                 country="DE",
                 split_by="month",
                 use_cache = F)
  by_month_time <- Sys.time() - start

  start <- Sys.time()
  by_year <- creahelpers::api.get(endpoint="api.energyandcleanair.org/power/generation",
                      date_from=date_from,
                      aggregate_by="country,source,date",
                      country="DE",
                      split_by="year",
                      use_cache = F)
  by_year_time <- Sys.time() - start


  # Test the params instead of ...
  start <- Sys.time()
  by_year_params <- api.get(endpoint="api.energyandcleanair.org/power/generation",
                     date_from=date_from,
                     params = list(aggregate_by="country,source,date",
                                   country="DE"),
                     split_by="year",
                     use_cache = F)
  by_year_params_time <- Sys.time() - start

  # Test vector
  by_year_params_vector <- api.get(endpoint="api.energyandcleanair.org/power/generation",
                            date_from=date_from,
                            params = list(aggregate_by=c('country','source','date'),
                                          country="DE"),
                            split_by="year",
                            use_cache = F)


  start <- Sys.time()
  # Test that params$date_from overrides date_from
  by_year_params_shorter <- api.get(endpoint="api.energyandcleanair.org/power/generation",
                            date_from=date_from,
                            params = list(aggregate_by="country,source,date",
                                          country="DE",
                                          date_from=as.Date(date_from) + lubridate::days(100)),
                            split_by="year",
                            use_cache = F)
  by_year_params_different_time <- Sys.time() - start

  testthat::expect_gt(nrow(onego), 2e3)

  testthat::expect_equal(nrow(onego), nrow(by_week))
  testthat::expect_equal(nrow(onego), nrow(by_month))
  testthat::expect_equal(nrow(onego), nrow(by_year))
  testthat::expect_equal(nrow(onego), nrow(by_year_params))
  testthat::expect_equal(nrow(onego), nrow(by_year_params_vector))
  testthat::expect_gt(nrow(onego), nrow(by_year_params_shorter))

  testthat::expect_equal(sum(onego$value_mwh), sum(by_week$value_mwh))
  testthat::expect_equal(sum(onego$value_mwh), sum(by_month$value_mwh))
  testthat::expect_equal(sum(onego$value_mwh), sum(by_year$value_mwh))
  testthat::expect_equal(sum(onego$value_mwh), sum(by_year_params$value_mwh))
  testthat::expect_equal(sum(onego$value_mwh), sum(by_year_params_vector$value_mwh))
  testthat::expect_gt(sum(onego$value_mwh), sum(by_year_params_shorter$value_mwh))


  # Proxy way to check that it did split. Not perfect...
  # testthat::expect_gt(by_month_time, onego_time * 2)
  # testthat::expect_gt(by_year_time, onego_time * 2)
  # testthat::expect_gt(by_year_params_time, onego_time * 2)

})

test_that("api.get works with null date_from", {

  endpoint_default_date_from = "2022-01-01"
  allde <- creahelpers::api.get("api.energyandcleanair.org/power/generation",
                                aggregate_by="country,source,date",
                                country="DE",
                                use_cache = F)
  testthat::expect_equal(min(allde$date), as.Date(endpoint_default_date_from))
})

test_that("api.get caching works", {

  date_from <- "2023-01-01"
  start <- Sys.time()
  first <- creahelpers::api.get("api.energyandcleanair.org/power/generation",
                                date_from=date_from,
                                aggregate_by="country,source,date",
                                country="DE",
                                use_cache = T,
                                refresh_cache = T)
  first_time <- Sys.time() - start

  start <- Sys.time()
  second <- api.get("api.energyandcleanair.org/power/generation",
                   date_from=date_from,
                   aggregate_by="country,source,date",
                   country="DE",
                   use_cache = T,
                   refresh_cache = F)
  second_time <- Sys.time() - start

  testthat::expect_equal(first, second)
  testthat::expect_gt(first_time, second_time * 10)

})

test_that("cache paths describe the endpoint and effective date range", {
  params <- list(
    country = "DE",
    date_from = as.Date("2023-01-01"),
    date_to = as.Date("2023-12-31"),
    format = "csv"
  )

  path <- creahelpers:::api._cache_path(
    "https://api.example.org/power/generation/",
    params,
    "cache"
  )

  expect_match(
    basename(path),
    "^generation_2023-01-01_2023-12-31_[0-9a-f]{16}\\.parquet$"
  )

  dateless_path <- creahelpers:::api._cache_path(
    "https://api.example.org/power/hourly%20generation/",
    list(country = "DE", date_from = NULL, date_to = as.Date("2023-12-31")),
    "cache"
  )
  expect_match(
    basename(dateless_path),
    "^hourly_generation_[0-9a-f]{16}\\.parquet$"
  )
})

test_that("cache hashes use normalized request details only", {
  endpoint <- "https://api.example.org/power/generation"
  first <- creahelpers:::api._cache_path(
    endpoint,
    list(country = "DE", aggregate_by = c("country", "date")),
    "cache"
  )
  reordered <- creahelpers:::api._cache_path(
    endpoint,
    list(aggregate_by = "country,date", country = "DE"),
    "cache"
  )
  changed <- creahelpers:::api._cache_path(
    endpoint,
    list(aggregate_by = "country,date", country = "FR"),
    "cache"
  )

  expect_identical(first, reordered)
  expect_false(identical(first, changed))
})

test_that("api.get reads, refreshes, and separates parquet cache entries", {
  cache_folder <- file.path(tempdir(), paste0("creahelpers-cache-", uuid::UUIDgenerate()))
  calls <- 0
  local_mocked_bindings(
    api._get_raw = function(endpoint, params, max_retries, initial_sleep,
                            backoff_factor, verbose = FALSE) {
      calls <<- calls + 1
      tibble::tibble(country = params$country, call = calls)
    },
    .package = "creahelpers"
  )

  request <- function(country = "DE", refresh_cache = FALSE, verbose = FALSE) {
    creahelpers::api.get(
      "api.example.org/power/generation",
      country = country,
      use_cache = TRUE,
      cache_folder = cache_folder,
      refresh_cache = refresh_cache,
      verbose = verbose
    )
  }

  first <- request()
  second <- request(verbose = TRUE)
  expect_equal(calls, 1)
  expect_identical(first, second)
  expect_length(list.files(cache_folder, pattern = "\\.parquet$"), 1)

  refreshed <- request(refresh_cache = TRUE)
  expect_equal(calls, 2)
  expect_equal(refreshed$call, 2)
  expect_length(list.files(cache_folder, pattern = "\\.parquet$"), 1)

  request(country = "FR")
  expect_equal(calls, 3)
  expect_length(list.files(cache_folder, pattern = "\\.parquet$"), 2)
})

test_that("api.get caching round-trips an empty tibble", {
  cache_folder <- file.path(tempdir(), paste0("creahelpers-cache-", uuid::UUIDgenerate()))
  calls <- 0
  local_mocked_bindings(
    api._get_raw = function(...) {
      calls <<- calls + 1
      tibble::tibble()
    },
    .package = "creahelpers"
  )

  first <- creahelpers::api.get(
    "api.example.org/power/generation",
    use_cache = TRUE,
    cache_folder = cache_folder
  )
  second <- creahelpers::api.get(
    "api.example.org/power/generation",
    use_cache = TRUE,
    cache_folder = cache_folder
  )

  expect_equal(calls, 1)
  expect_identical(first, tibble::tibble())
  expect_identical(second, first)
})

test_that("api.get does not create a cache folder when caching is disabled", {
  cache_folder <- file.path(tempdir(), paste0("creahelpers-cache-", uuid::UUIDgenerate()))
  local_mocked_bindings(
    api._get_raw = function(...) tibble::tibble(value = 1),
    .package = "creahelpers"
  )

  result <- creahelpers::api.get(
    "api.example.org/power/generation",
    use_cache = FALSE,
    cache_folder = cache_folder
  )

  expect_equal(result$value, 1)
  expect_false(dir.exists(cache_folder))
})

test_that("split requests create one parquet file per interval", {
  cache_folder <- file.path(tempdir(), paste0("creahelpers-cache-", uuid::UUIDgenerate()))
  local_mocked_bindings(
    api._get_raw = function(endpoint, params, ...) {
      tibble::tibble(date_from = as.Date(params$date_from))
    },
    .package = "creahelpers"
  )

  creahelpers::api.get(
    "api.example.org/power/generation",
    date_from = "2023-01-01",
    date_to = "2023-01-03",
    split_by = "day",
    use_cache = TRUE,
    cache_folder = cache_folder
  )

  cache_files <- sort(list.files(cache_folder))
  expect_length(cache_files, 3)
  expect_true(all(grepl(
    "^generation_2023-01-0[1-3]_2023-01-0[1-3]_[0-9a-f]{16}\\.parquet$",
    cache_files
  )))
})
