# Environment to cache downloaded series
env.cache <- new.env(parent = emptyenv())

base.url <- "http://www.dataseries.org.s3-website-eu-west-1.amazonaws.com/"

#' Download Time Series from [www.dataseries.org](http://www.dataseries.org)
#' 
#' `ds` downloads time series from
#' [www.dataseries.org](http://www.dataseries.org). Data can be imported as a 
#' `data.frame` or an `xts` object. 
#' 
#' Downloaded series are **cached in memory** as long as the
#' R session is open. If you want to force an update, either restart the R
#' session or run `cache_rm` to emtpy the cache. `cache_ls` is a utility
#' function that lists all cached objects (see examples).
#'
#' @param id  one ore more IDs, as given by
#'   [www.dataseries.org](http://www.dataseries.org). 
#' @param class  class of the return value, either a `"data.frame"` (default) or 
#'   an `"xts"` object (requires the `xts` package to be installed).
#' @examples
#' \dontrun{
#' ds(c("CCI.AIK", "CCI.ASSS"))
#' ds(c("CCI.AIK", "CCI.ASSS"), class = "xts")
#'
#' # list cached objects
#' cache_ls()
#' 
#' # empty in-memory cache, which forces a fresh download
#' cache_rm()
#' }
#' 
#' @export
#' @importFrom utils read.csv
ds <- function(id, class = c("data.frame", "xts")){

  class <- match.arg(class)
  stopifnot(inherits(id, "character"))
  
  # output structure
  z <- as.list(id)
  names(z) <- id

  is.cached <- id %in% names(env.cache)
  if (!all(is.cached)){
    fname.not.cached <- paste0(base.url, id[!is.cached], ".csv")
    z[!is.cached] <- lapply(fname.not.cached, function(e) try(read.csv(e, row.names = NULL, colClasses = c("Date", "numeric")))) 
  }
  z[is.cached] <- as.list(env.cache)[id[is.cached]]

  is.err <- vapply(z, function(e) inherits(e, "try-error"), FALSE)
  if (length(z[is.err]) > 0){
    message("Error when downloading: ", paste(id[is.err], collapse = ", "))
    message(z[is.err])
  }

  to.chache <- z[!is.err & !is.cached]

  assign_to_cache <- function(x, value) assign(x, value, envir = env.cache)
  Map(assign_to_cache, x = names(to.chache), value = to.chache)

  zz <- z[!is.err]
  if (length(zz) == 0) return(NULL)

  combine <- if (class == "data.frame"){
    combine_df
  } else if (class == "xts") {
    combine_xts
  } else {
    stop("wrong class.")
  }
  combine(zz)
}


#' @rdname ds
#' @export
cache_ls <- function(){
  ls(envir = env.cache)
}

#' @rdname ds
#' @export
cache_rm <- function(){
  rm(list = cache_ls(), envir = env.cache)
}



# combine single time series data frames in a single data.frame, using merge.
combine_df <- function(ll){
  # rename 'value' column to id
  ll <- Map(function(e, name) {names(e)[2] <- name; e}, e = ll, name = names(ll))
  df <- Reduce(function(df1, df2) merge(df1, df2, by = "time", all = TRUE), ll)
  df[order(df[['time']]), ]
}

# transform to xts and cbind
combine_xts <- function(ll){

  if (!requireNamespace('xts', quietly = TRUE)) {
    stop("The 'xts' package is not installed. To install, run:\n  install.packages(\"xts\")", call. = FALSE)
  }

  ll.xts <- lapply(ll, function(e) xts::as.xts(e$value, order.by = e$time))
  do.call(cbind, ll.xts)
}


