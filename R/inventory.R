#' Inventory from [www.dataseries.org](http://www.dataseries.org)
#' 
#' Read latest time series inventory from
#' [www.dataseries.org](http://www.dataseries.org) as a `data.frame`.
#'
#' The downloaded data are **cached in memory** as long as the
#' R session is open. If you want to force an update, either restart the R
#' session or run `cache_rm` to emtpy the cache. `cache_ls` is a utility
#' function that lists all cached objects.
#' 
#' @examples
#' \dontrun{
#' inv <- inventory()  # will be cached
#' # search in the inventory
#' inv[grepl("consumer conf", inv$description, ignore.case = TRUE), ]
#' }
#' 
#' @export
#' @importFrom utils download.file
inventory <- function(){
  is.cached <- isTRUE("inventory" %in% names(env.cache))
  if (!is.cached){
    tfile <- tempfile(fileext = ".RData")
    remotefile <- paste0(base.url, "inventory.RData")
    download.file(remotefile, tfile)
    load(tfile, envir = env.cache)
    unlink(tfile)
  }
  get("inventory", envir = env.cache)
}

