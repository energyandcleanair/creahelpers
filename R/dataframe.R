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

  if(is.null(df2)) return(df1)

  # ensure suffix is unique...
  suffix <- uuid::UUIDgenerate()

  # Only consider columns existing in df2
  value_cols <- intersect(value_cols, colnames(df2))

  # Fill df1 with value_cols if they're not already there
  missing_cols <- setdiff(value_cols, colnames(df1))
  df1[missing_cols] <- NA

  # Perform the update
  df1 %>%
    left_join(df2 %>% select_at(c(join_cols, value_cols)),
              by = join_cols, suffix = c("", suffix)) %>%
    mutate(across(all_of(value_cols), ~ coalesce(get(cur_column()), get(paste0(cur_column(), suffix))))) %>%
    select(-ends_with(suffix)) %T>%
    # Throw error if more or fewer rows than before
    {
      if(nrow(.) != nrow(df1)){
        stop("Number of rows changed")
      }
    }

}

