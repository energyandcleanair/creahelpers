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

#' Recode a character variable using regex
#'
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
disambiguate = function(x, keys, unique.values=F, match.all=F, stop.error=F, warn.error=F, ...) {
  if(is.null(names(keys))) names(keys)=keys
  for(k in keys) {
    m=grep(k, x, ...)
    hits=length(m)

    if(hits != 1) {
      msg=paste(k,'matched',hits, 'times')
      if(hits==0 & match.all) {
        if(stop.error) stop(smg)
        if(warn.error) warning(msg)
      }

      if(hits>1 & unique.values) {
        if(stop.error) stop(smg)
        if(warn.error) warning(msg)
      }
      print(msg)
    }

    x[m] <- names(keys)[keys==k]
  }
  return(x)
}


#' Wrappers for countrycode
#'
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
get_iso2 = function(x) {
  require(countrycode)
  countrycode(x, "country.name.en", "iso2c", custom_match = c(Kosovo='XK'))
}

#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
get_iso3 = function(x) {
  require(countrycode)
  countrycode(x, 'country.name', 'iso3c', custom_match = c(Kosovo='XKX'))
}


x_at_zero <- function(headroom=.05, ...) {list(ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,headroom)), ...), expand_limits(y=0))}
snug_x <- list(ggplot2::scale_x_continuous(expand=ggplot2::expansion(mult=0)))
snug_x_date <- list(ggplot2::scale_x_date(expand=ggplot2::expansion(mult=0)))
snug_x_datetime <- list(ggplot2::scale_x_datetime(expand=ggplot2::expansion(mult=0)))

col.a <- function(colorname,alpha) {
  colorname %>% col2rgb %>% divide_by(255) -> cn
  rgb(cn[1,],cn[2,],cn[3,],alpha)
}


#show all pch symbols
pchcheat <- function() {
  plot(x=rep(1:5,5),y=sapply(1:5,rep,5),pch=1:25)
  text(x=rep(1:5,5),y=sapply(1:5,rep,5),1:25,pos=c(rep(3,5),rep(1,20)))
}

quicksave <- function(file, width=8, height=6, bg='white', ...) {
  ggsave(file, width=width, height=height, bg=bg, ...)
}

#show all named colors
colcheat <- function(cl=colors(), bg = "grey",
                           cex = 0.75, rot = 30) {
  m <- ceiling(sqrt(n <-length(cl)))
  length(cl) <- m*m; cm <- matrix(cl, m)
  require("grid")
  grid.newpage(); vp <- viewport(w = .92, h = .92)
  grid.rect(gp=gpar(fill=bg))
  grid.text(cm, x = col(cm)/m, y = rev(row(cm))/m, rot = rot,
            vp=vp, gp=gpar(cex = cex, col = cm,font=2))
}


force_numeric = function(x) {
  stringr::str_extract(x, '[+-]?[0-9,]+\\.?[0-9]*') %>% gsub(',', '', .) %>% as.numeric
}

dms_to_dec <- function(x) {
  x %>%
    lapply(function(x) {
      xn = x %>% strsplit('[^0-9.,]+') %>% unlist %>% gsub(',', '.', .) %>% as.numeric
      if(is.na(xn[3])) xn[3] <- 0
      x_dec = xn[1]+xn[2]/60+xn[3]/3600
      if(grepl("S$|W$", x)) x_dec %<>% multiply_by(-1)
      return(x_dec)
    }) %>%
    unlist
}

statmode <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(table(match(x, ux)))]
}

is.outlier <- function(x, SDs=10,na.rm=F) {
  abs((x - mean(x, na.rm=na.rm)) / sd(x, na.rm = na.rm)) -> devs
  return(is.na(devs) | devs > SDs)
}

mean.maxna <- function(x,maxna) {
  if(sum(is.na(x))>maxna) { return(as.numeric(NA))
  } else return(mean(x,na.rm=T))
}

cluster <- function(sp, distKM) {
  require(sp)
  require(geosphere)
  sp <- spdf(sp)
  hc <- sp %>% coordinates %>% distm %>% as.dist %>% hclust
  cutree(hc,h=distKM*1000)
}


na.cover <- function(x, x.new) { ifelse(is.na(x), x.new, x) }

'%whichin%' <- function(x,y) x[x %in% y]
'%notin%' <- function(x,y)!('%in%'(x,y))
'%whichnotin%' <- function(x,y) x[x %notin% y]

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

x_at_zero <- function(headroom=.05, ...) {
  list(ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,headroom)), ...),
       ggplot2::expand_limits(y=0))
}
