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

#show all named colors
colcheat <- function(cl=colors(), bg = "grey",
                           cex = 0.75, rot = 30) {
  m <- ceiling(sqrt(n <-length(cl)))
  length(cl) <- m*m; cm <- matrix(cl, m)
  require(grid)
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
  warning('This is the old version of the function, maintained for backward compatibility, which uses standard deviations from mean as the criterion. Please switch to using is_outlier.')
  is_outlier(x, deviation_threshold=SDs, na.rm=na.rm, FUN=mean)
}

is_outlier <- function(x, deviation_threshold=10, na.rm=F, FUN=median) {
  mean_value <- FUN(x, na.rm=na.rm)
  deviations <- abs(x - mean_value)
  mean_deviation <- FUN(deviations, na.rm=na.rm)
  relative_deviations <- deviations/mean_deviation
  return(is.na(relative_deviations) | relative_deviations > deviation_threshold)
}

mean.maxna <- function(x,maxna) {
  if(sum(is.na(x))>maxna) { return(as.numeric(NA))
  } else return(mean(x,na.rm=T))
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

#function to read in an excel file with multiple header rows, either combining into one row (wide=T) or pivoting longer
silent_read <- function(...) suppressMessages(read_xlsx(...))

read_wide_xlsx <- function(path, sheet=NULL,
                           header_rows = length(header_row_names),
                           skip=0,
                           info_columns=1,
                           header_row_names=paste0('V', 1:header_rows),
                           stop_at_blank_row=T, discard_empty_columns=T,
                           wide_format=F,
                           ...) {
  header <- silent_read(path, sheet=sheet, skip=skip, n_max=header_rows, col_names = F)
  data <- silent_read(path, sheet=sheet, skip=header_rows+skip, col_names = F, ...)

  if(stop_at_blank_row) {
    first_empty_row <- data %>% apply(1, function(x) sum(!is.na(x))) %>% equals(0) %>% which %>% min
    data %<>% slice_head(n=first_empty_row-1)
  }

  if(discard_empty_columns) {
    empty_columns <- data %>% apply(2, function(x) sum(!is.na(x))) %>% equals(0) %>% which
    data %<>% select(-all_of(empty_columns))
  }

  header %>% t %>% as_tibble() %>%
    fill(everything(), .direction='down') %>%
    apply(1, function(x) x %>% na.omit %>% matrix(nrow=1) %>% as_tibble) %>%
    bind_rows() ->
    header_df

  header_df %>% select(matches('^V[0-9]+$')) %>%
    apply(1, function(x) x %>% na.omit %>% paste(collapse='_')) ->
    header_colnames

  if(!is.null(header_row_names)) names(header_df) <- header_row_names
  header_df %<>% mutate(col=names(header))

  if(wide_format) names(data) <- header_colnames

  if(!wide_format) {
    names(data)[info_columns] <- header_colnames[info_columns]

    data %>%
      mutate(across(-all_of(info_columns), as.character)) %>%
      pivot_longer(-all_of(info_columns), names_to='col') %>%
      right_join(header_df, .) %>% select(-col)
  }
}

#rolling mean for data by date
rollmean_date <- function(x, dates, width=7) {
  x.out <- x
  x.out[] <- NA
  for(i in 1:length(x))
    x.out[i] <- sum(x[dates %in% (dates[i]-0:(width-1))], na.rm=T)/width
  return(x.out)
}

#equals operator that treats NA's as a value to be compared instead of returning NA
'%eqna%' <- function(x, y) (!is.na(x) & !is.na(y) & x==y) | (is.na(x) & is.na(y))

#read the BP Statistical Review data
read_bp <- function(file_path,
                    sheet_name=NULL,
                    year=2022,
                    startRow=3, ignore.case=T, read_multiple=F) {
  require(readxl)
  excel_sheets(file_path) -> sheets

  if(is.null(sheet_name)) {
    message(paste('no sheet given. options:', paste(sheets, collapse='; ')))
    return(sheets)
  }

  if(sheet_name %notin% sheets) sheet_name = grep(sheet_name, sheets, value=T, ignore.case=ignore.case)
  if(length(sheet_name)>1 & !read_multiple) stop(paste('sheet_name corresponds to more than one sheet: ', paste(sheet_name, collapse='; ')))
  if(length(sheet_name)==0) stop(paste('sheet_name does not match a sheet. options: ', paste(sheets, collapse='; ')))

  lapply(sheet_name,
         function(sn) {
           read_xlsx(file_path,
                     sheet=sn, #rowIndex=NULL,
                     skip=startRow-1,
                     .name_repair = function(x) x %>% make.names %>% make.unique) -> d

           names(d)[1] <- 'country'
           d$is.country <- with(d,!(is.na(country) | grepl("Other|Total|Africa", country)))
           d$is.country[d$country=="South Africa"] <- T
           d$is.country[d$country=="Central America"] <- F
           world.total.index <- match("Total World", d$country)
           if(is.na(world.total.index))
             world.total.index <- grep("Total", d$country) %>% tail(1)
           d$is.country[world.total.index:nrow(d)] <- F
           last.data.row = d[[paste0('X', year-1)]] %>% is.na() %>% not %>% which %>% max
           d <- d[1:last.data.row,]
           EU.index <- grep("European Union", d$country)
           d$country[EU.index] <- "EU"
           d$is.country[EU.index] <- T
           d$country[d$country=="United Kingdom"] <- "UK"
           d$country[grep("Russia", d$country)] <- "Russia"

           d %>% select(-matches("X[0-9]*\\.")) %>%
             filter(!is.na(country)) %>%
             pivot_longer(starts_with('X'), names_to='year', values_transform=list(value=as.numeric)) %>%
             mutate(across(value, as.numeric), across(year, force_numeric),
                    variable=sn)
         }) -> d.out

  if(!read_multiple) d.out <- d.out[[1]]
  return(d.out)
}

#clean up memory, interactively choosing which objects to delete
cleanup <- function(protect,threshold=1) {
  sort( sapply(ls(envir = .GlobalEnv),function(x){object.size(get(x))}),decreasing = T)/1e6 -> memsizes
  trash <- rep(F,length(memsizes))

  for(o in which(memsizes>threshold)) {
    print(paste0("total memory size: ",round(sum(memsizes[!trash]),1),"Mb"))
    print(paste0(ifelse(o==1,"","next "),"largest object: ",names(memsizes)[o],", ",round(memsizes[o],1),"Mb"))
    readline("remove y/n/q ") -> reply
    if(substr(reply,1,1) %in% c("", "q")) break()

    if(substr(reply,1,1) == "y") trash[o] <- T
  }

  if(sum(trash==T)==0) return()

  rmObjs <- names(memsizes[trash])
  print(paste("removing",paste(rmObjs,collapse=" ")))
  readline("continue y/n ") -> reply2
  #rmObjs <- names(memsizes[memsizes>10e6 & !(names(memsizes) %in% c(protectObjects,"total"))])
  if(substr(reply2,1,1) == "y") rm(list=rmObjs, envir = .GlobalEnv)
}

#paste an Excel table from clipboard into a data.frame
paste.xl <- function(header=T, ...)
  read.table('clipboard', sep='\t', header=header, ...)

#copy a data.frame as a string that can be pasted into an Excel spreadsheet
copy.xl <- function(df, col.names=T, quote=F, row.names=F, ...)
  write.table(df, 'clipboard', sep='\t',
              col.names=col.names, quote=quote, row.names=row.names, ...)
