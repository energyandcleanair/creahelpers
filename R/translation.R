#translation and language functions

#translate using a pre-provided lookup table
trans <- function(x,
                  lang=get("lang", .GlobalEnv),
                  trans_file = 'data/label_translations.xlsx',
                  wrap_chars=NULL,
                  ignore.case=T,
                  ignore.non.ascii=T,
                  when_missing='warn') {

  if(lang=='EN') return(x)

  read_xlsx(trans_file) -> dict

  if(!is.null(wrap_chars)) dict[[lang]] %<>% strsplit_lang(width=wrap_chars, lang=lang)

  if(ignore.case) {
    if(is.factor(x)) { x <- factor(tolower(x), levels=tolower(levels(x)))
    } else x %<>% tolower

    dict$EN %<>% tolower
  }

  erase_non_ascii <- function(str) gsub('[^A-Za-z0-9]+', ' ', str)
  if(ignore.non.ascii) {
    if(is.factor(x)) { x <- factor(erase_non_ascii(x), levels=erase_non_ascii(levels(x)))
    } else x %<>% erase_non_ascii
    dict$EN %<>% erase_non_ascii
  }

  dictvect <- dict[[lang]]
  names(dictvect) <- dict$EN

  #identify values not translated
  missing <- x %whichnotin% dict$EN
  if(length(missing)>0) {
    msg = warning(paste('these values were not matched:', paste(missing, collapse='; ')))
    if(when_missing=='warn') warning(msg)
    if(when_missing=='stop') stop(msg)
  }

  if(is.character(x)) x %<>% recode(!!!dictvect)
  if(is.factor(x)) x %<>% recode_factor(!!!dictvect, .ordered=T)

  x
}

#string wrapping that also works with Chinese
strsplit_lang <- function(x, width, lang=get('lang', envir=.GlobalEnv)) {
  if(lang!='ZH') return(stringr::str_wrap(x, width=width))

  starts <- seq(1, nchar(x), by=width) %>% c(nchar(x)) %>% unique

  # chop it up
  out <- character()
  for(i in 1:(length(starts)-1)) {
    if(substr(x, starts[i], starts[i]) %in% c("，", "。","：")) starts[i] %<>% add(1)
    out[i] <- substr(x, starts[i], starts[i+1])
  }
  out %>% paste(collapse='\n')
}

#date labeling functions that work with Chinese and English
monthlab <- function(x, lang=get('lang', envir=.GlobalEnv)) {
  if(lang=='EN') x %<>% format.Date('%b')
  if(lang=='ZH') x %<>% month() %>% paste0('月')
  return(x)
}

yearlab <- function(x, lang=get('lang', envir=.GlobalEnv)) {
  if(lubridate::is.Date(x) | lubridate::is.POSIXt(x)) x %<>% year
  if(lang=='ZH') x %<>% paste0('年')
  x
}

monthyearlab <- function(x, lang=get('lang', envir=.GlobalEnv)) {
  if(lang=='EN') x %<>% format.Date('%b %Y')
  if(lang=='ZH') x <- glue("{year(as.Date(x))} 年 {month(as.Date(x))} 月")
  return(x)
}