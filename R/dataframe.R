#'  Fill values in df1 with values from df2 if df1 values are NA
#'
#' @param df1
#' @param df2
#' @param join_cols column(s) to join on
#' @param value_cols column(s) to update
#'
#' @return
#' @export
#'
fill_df1_with_df2 <- function(df1, df2, join_cols, value_cols){

  # ensure suffix is unique...
  suffix <- uuid::UUIDgenerate()

  # Perform the update
  df1 %>%
    left_join(df2 %>% select_at(c(join_cols, value_cols)),
              by = join_cols, suffix = c("", suffix)) %>%
    mutate(across(all_of(value_cols), ~ coalesce(get(cur_column()), get(paste0(cur_column(), suffix))))) %>%
    select(-ends_with(suffix))
}

