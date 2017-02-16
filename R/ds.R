#' Download time series from
#' [www.dataseries.org](http://www.dataseries.org)
#' 
#' Download time series from
#' [www.dataseries.org](http://www.dataseries.org) as a `data.frame` or and 
#' `xts` object.
#' 
#' Downloaded series are **cached in the memory** and kept there as long as your
#' R session is open. If you want to force an update, either restart your R
#' session or delete the caching enivronment, `.in.memory.cache` (see examples).
#'
#' @param id  one ore more IDs, as shown on
#'   [www.dataseries.org](http://www.dataseries.org). 
#' @param class  class of the return value, either a `"data.frame"` (default) or 
#'   an `"xts"` object.
#' @examples
#' /dontrun{
#' ds(c("CCI.AIK", "CCI.ASSS"))
#' ds(c("CCI.AIK", "CCI.ASSS"), class = "xts")
#'
#' # delete in-memory cache, to force a fresh download
#' rm(".in.memory.cache")
#' }
#' 
#' @export
ds <- function(id, class = c("data.frame", "xts")){

  class <- match.arg(class)
  stopifnot(inherits(id, "character"))

  base.url <- "http://www.dataseries.org.s3-website-eu-west-1.amazonaws.com/"
  
  ## in memory cache
  if (!exists(".in.memory.cache")){
    .in.memory.cache <<- new.env(parent = emptyenv())
  }

  # output structure
  z <- as.list(id)
  names(z) <- id

  is.cached <- id %in% names(.in.memory.cache)
  if (!all(is.cached)){
    fname.not.cached <- paste0(base.url, id[!is.cached], ".csv")
    z[!is.cached] <- lapply(fname.not.cached, function(e) try(read.csv(e, row.names = NULL, colClasses = c("Date", "numeric")))) 
  }
  z[is.cached] <- as.list(.in.memory.cache)[id[is.cached]]

  is.err <- vapply(z, function(e) inherits(e, "try-error"), FALSE)
  if (length(z[is.err]) > 0){
    message("Error when downloading: ", paste(id[is.err], collapse = ", "))
    message(z[is.err])
  }

  to.chache <- z[!is.err & !is.cached]

  assign_to_cache <- function(x, value) assign(x, value, envir = .in.memory.cache)
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


# combine single time series data frames in a single data.frame, using merge.
combine_df <- function(ll){
  # rename 'value' column to id
  ll <- Map(function(e, name) {names(e)[2] <- name; e}, e = ll, name = names(ll))
  df <- Reduce(function(df1, df2) merge(df1, df2, by = "time", all = TRUE), ll)
  df[order(df[['time']]), ]
}

# transform to xts and cbind
combine_xts <- function(ll){

  if (requireNamespace('xts')) {
    stop("'xts' package is not installed but required to return series as 'xts' objects.", call. = FALSE)
  }

  ll.xts <- lapply(ll, function(e) xts::as.xts(e$value, order.by = e$time))
  do.call(cbind, ll.xts)
}


