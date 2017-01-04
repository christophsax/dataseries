# download

# sid <- c("CCI.AIK", "CCI.ASSS", "CNS.TTL")

#' @export
ds <- function(sid){
  base.url <- "http://data.dataseries.org.s3-website.eu-central-1.amazonaws.com/"
  
  ## in memory cache
  if (!exists(".in.memory.cache")){
    .in.memory.cache <<- new.env(parent = emptyenv())
  }

  # output structure
  z <- as.list(sid)
  names(z) <- sid

  is.cached <- sid %in% names(.in.memory.cache)
  if (!all(is.cached)){
    fname.not.cached <- paste0(base.url, sid[!is.cached], ".csv")
    z[!is.cached] <- lapply(fname.not.cached, function(e) try(read.csv(e, row.names = NULL, colClasses = c("Date", "numeric")))) 
  }
  z[is.cached] <- as.list(.in.memory.cache)[sid[is.cached]]

  is.err <- vapply(z, function(e) inherits(e, "try-error"), FALSE)
  if (length(z[is.err]) > 0){
    message("Error when downloading: ", paste(sid[is.err], collapse = ", "))
    message(z[is.err])
  }


  to.chache <- z[!is.err & !is.cached]

  assign_to_cache <- function(x, value) assign(x, value, envir = .in.memory.cache)
  Map(assign_to_cache, x = names(to.chache), value = to.chache)

  if (length(z[!is.err]) == 0) return(NULL)

  z.xts <- lapply(z[!is.err], function(e) as.xts(e$value, order.by = e$time))
  ans <- do.call(cbind, z.xts)
  ans
}

# ds(c("CCI.AIK", "CCI.ASSS"))


