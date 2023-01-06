sel <- dplyr::select

'%notin%' <- function(x,y)!('%in%'(x,y))

#' Capitalize the first letter of each word
#'
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
capitalize_first <- function(x, rest_lower=T) {
  sapply(x, function(x) {
    if(is.na(x)) return(x)
    tolower_if = function(x) {if(rest_lower) {return(tolower(x))}else return(x)}
    sapply(x, function(x) {
      s <- strsplit(x, " ")[[1]]
      paste(toupper(substring(s, 1,1)), tolower_if(substring(s, 2)),
            sep="", collapse=" ")
    } )
  })
}

#' Replace NA values in a vector with the corresponding values of another vector
#'
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
na.cover <- function(x, x.new) { ifelse(is.na(x), x.new, x) }


#' Round a number down to specified number of significant digits
#'
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
sigfloor <- function(x,sigdig=1) {
  mag <- 10^floor(log10(x)-sigdig+1)
  return(floor(x/mag)*mag)
}

#' Order factor levels based on another variable
#'
#' Create a factor from a factor or character string, with levels ordered based on another variable.
#' @param var Factor or character vector with the values of the output factor.
#' @param by A vector that can be ordered with base::order.
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
orderfactor <- function(var, by) {
  var = factor(var, levels = var[rev(order(by))])
}


expand_dates <- function(df, datecol='date', targetdates=NULL) {
  groupvarlist <- df %>% select(all_of(dplyr::group_vars(df))) %>% as.list() %>% lapply(unique)

  if(is.null(targetdates))
    targetdates <- seq.Date(min(df[[datecol]]),
                            max(df[[datecol]]),
                            by='day')

  if(!is.list(targetdates)) targetdates %<>% list

  names(targetdates) <- datecol
  full_join(df, expand.grid(c(groupvarlist, targetdates)))
}

get_yoy <- function(x, date) {
  lastyr <- date
  year(lastyr) %<>% subtract(1)
  ind = match(lastyr, date)
  x / x[ind] - 1
}

