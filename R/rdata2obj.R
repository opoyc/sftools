#' RData to object
#'
#' This simple function receives a RData and passes as object to be assigned
#'
#' @param path String. Path to RData file
#'
#' @return
#'
#' @examples
#' \dontrun{
#' rdata2object("path")
#' }
rdata2obj <- function(path){
  tmp_env <- new.env()
  load(path, envir = tmp_env)
  as.list(tmp_env)[[1]]
}
