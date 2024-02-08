test_that("api.get works", {

  date_from <- "2023-01-01"
  start <- Sys.time()
  onego <- creahelpers::api.get("api.energyandcleanair.org/power/generation",
            date_from=date_from,
            aggregate_by="country,source,date",
            country="DE")
  onego_time <- Sys.time() - start

  start <- Sys.time()
  by_week <- creahelpers::api.get(endpoint="api.energyandcleanair.org/power/generation",
                                   date_from=date_from,
                                   aggregate_by="country,source,date",
                                   country="DE",
                                   split_by="week")
  by_week_time <- Sys.time() - start

  start <- Sys.time()
  by_month <- creahelpers::api.get(endpoint="api.energyandcleanair.org/power/generation",
                 date_from=date_from,
                 aggregate_by="country,source,date",
                 country="DE",
                 split_by="month")
  by_month_time <- Sys.time() - start

  start <- Sys.time()
  by_year <- creahelpers::api.get(endpoint="api.energyandcleanair.org/power/generation",
                      date_from=date_from,
                      aggregate_by="country,source,date",
                      country="DE",
                      split_by="year")
  by_year_time <- Sys.time() - start


  # Test the params instead of ...
  start <- Sys.time()
  by_year_params <- api.get(endpoint="api.energyandcleanair.org/power/generation",
                     date_from=date_from,
                     params = list(aggregate_by="country,source,date",
                                   country="DE"),
                     split_by="year")
  by_year_params_time <- Sys.time() - start

  # Test vector
  by_year_params_vector <- api.get(endpoint="api.energyandcleanair.org/power/generation",
                            date_from=date_from,
                            params = list(aggregate_by=c('country','source','date'),
                                          country="DE"),
                            split_by="year")


  start <- Sys.time()
  # Test that params$date_from overrides date_from
  by_year_params_shorter <- api.get(endpoint="api.energyandcleanair.org/power/generation",
                            date_from=date_from,
                            params = list(aggregate_by="country,source,date",
                                          country="DE",
                                          date_from=date_from + lubridate::days(100)),
                            split_by="year")
  by_year_params_different_time <- Sys.time() - start


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
  testthat::expect_gt(by_month_time, onego_time * 10)
  testthat::expect_gt(by_year_time, onego_time * 2)
  testthat::expect_gt(by_year_params_time, onego_time * 2)

})
