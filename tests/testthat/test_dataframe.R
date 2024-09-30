# Test dataframe.R functions


df1 <- data.frame(
  ID = c(1, 2, 3),
  Value1 = c(NA, 20, NA),
  Value2 = c(10, NA, 30)
)

df2 <- data.frame(
  ID = c(1, 2, 3),
  Value1 = c(100, 200, 300),
  Value2 = c(NA, 25, 35)
)

# Writing tests for fill_df1_with_df2
test_that("fill_df1_with_df2 correctly fills NA values from df2", {

  result <- fill_df1_with_df2(df1, df2, "ID", c("Value1", "Value2"))

  expected_result <- data.frame(
    ID = c(1, 2, 3),
    Value1 = c(100, 20, 300),
    Value2 = c(10, 25, 30)
  )

  expect_equal(result, expected_result)
})

test_that("fill_df1_with_df2 keeps original values if no NA present", {

  df1_full <- data.frame(
    ID = c(1, 2, 3),
    Value1 = c(10, 20, 30),
    Value2 = c(10, 20, 30)
  )

  result <- fill_df1_with_df2(df1_full, df2, "ID", c("Value1", "Value2"))

  expect_equal(result, df1_full)
})

test_that("fill_df1_with_df2 handles empty df2 gracefully", {

  df2_empty <- data.frame(
    ID = integer(),
    Value1 = numeric(),
    Value2 = numeric()
  )

  result <- fill_df1_with_df2(df1, df2_empty, "ID", c("Value1", "Value2"))

  expect_equal(result, df1)
})

test_that("fill_df1_with_df2 adds columns to df1 if need be", {

  df2_extra_col <- data.frame(
    ID = c(1, 2, 3),
    Value1 = c(100, 200, 300),
    Value2 = c(NA, 25, 35),
    Value3 = c(1, 2, 3)
  )

  result <- fill_df1_with_df2(df1, df2_extra_col, "ID", c("Value1", "Value2", "Value3"))

  expected_result <- data.frame(
    ID = c(1, 2, 3),
    Value1 = c(100, 20, 300),
    Value2 = c(10, 25, 30),
    Value3 = c(1, 2, 3)
  )

  expect_equal(result, expected_result)
})

test_that("fill_df1_with_df2 throws error if it adds rows", {

  df2_extra_row <- data.frame(
    ID = c(1, 2, 3, 3),
    Value1 = c(100, 200, 300, 400),
    Value2 = c(NA, 25, 35, 45)
  )

  expect_error(fill_df1_with_df2(df1, df2_extra_row, "ID", c("Value1", "Value2")))
})


test_that("fill_df1_with_df2 works if df2 is NULL", {

  result <- fill_df1_with_df2(df1, NULL, "ID", c("Value1", "Value2"))

  expect_equal(result, df1)
})
