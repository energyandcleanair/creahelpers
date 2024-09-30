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
