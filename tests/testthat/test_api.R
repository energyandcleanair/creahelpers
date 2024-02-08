test_that("api.get works", {

start <- Sys.time()
onego <- api.get("api.energyandcleanair.org/power/generation",
          date_from="2020-01-01",
          aggregate_by="country,source,date",
          country="DE")
onego_time <- Sys.time() - start

start <- Sys.time()
by_month <- api.get(endpoint="api.energyandcleanair.org/power/generation",
               date_from="2020-01-01",
               aggregate_by="country,source,date",
               country="DE",
               split_by="month")
by_month_time <- Sys.time() - start

start <- Sys.time()
by_year <- api.get(endpoint="api.energyandcleanair.org/power/generation",
                    date_from="2020-01-01",
                    aggregate_by="country,source,date",
                    country="DE",
                    split_by="year")
by_year_time <- Sys.time() - start


testthat::expect_equal(nrow(onego), nrow(by_month))
testthat::expect_equal(nrow(onego), nrow(by_year))

testthat::expect_equal(sum(onego$value_mwh), sum(by_month$value_mwh))
testthat::expect_equal(sum(onego$value_mwh), sum(by_year$value_mwh))

# Proxy way to check that it did split. Not perfect...
testthat::expect_gt(by_month_time, onego_time * 10)
testthat::expect_gt(by_year_time, onego_time * 2)

})
