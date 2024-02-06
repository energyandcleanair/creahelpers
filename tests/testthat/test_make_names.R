test_that("make_unique and make_name work and do not lose any information in the process", {


  to_crea <- function(x) tolower(gsub("\\.", "_", x))

  tricky_ones <- list(
    c("A", "a"),
    c("A", "a.1", "a_1"),
    c("A", "a", "a.1", "a_1")
  )

  # repeat each item
  tricky_ones <- c(tricky_ones, (lapply(tricky_ones, function(x) rep(x, 2))))


  lapply(tricky_ones, function(x){

    base_y_names <- make.names(x)
    crea_y_names <- make_names(x)

    # Test that it does what we want...
    testthat::expect_equal(crea_y_names, to_crea(crea_y_names))

    # Without losing info
    testthat::expect_equal(length(unique(crea_y_names)), length(unique(base_y_names)))

    base_y_unique <- make.unique(x)
    crea_y_unique <- make_unique(x)

    # Test that it does what we want...
    testthat::expect_equal(crea_y_unique, to_crea(crea_y_unique))

    # Without losing info
    testthat::expect_equal(length(unique(crea_y_unique)), length(unique(base_y_unique)))

  })


})
