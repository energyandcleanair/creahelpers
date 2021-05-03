sel <- dplyr::select

'%notin%' <- function(x,y)!('%in%'(x,y))
