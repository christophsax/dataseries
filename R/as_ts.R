# convert data.frame to ts object

as_ts <- function(x) {
  tsp <- Date_POSIXct_to_tsp(x[['time']])
  cdta <- x[['value']]
  if (NCOL(cdta) == 1) cdta <- as.numeric(cdta)
  ts(cdta, start = tsp[1], frequency = tsp[3])
}


# # utility function to find POSIXct range (for coding only)
# find_range <- function(x){
#   ser <- ts(rep(1, 1000), f = x, start = 1800)
#   range(diff(as.numeric(as.POSIXct(ts_to_Date_POSIXct(ser)))))
# }

# find_range(12)

in_range <- function(x, min, max, tol = 1000){
  (all(x < (max + tol)) & all(x > (min - tol)))
}

POSIXct_to_tsp <- function(x){
  check_regularity(x)

  stopifnot(inherits(x, "POSIXct"))
  start <- POSIXct_to_dectime(x[1])
  end <- POSIXct_to_dectime(x[length(x)])
  f <- (length(x) - 1) / (end - start)
  c(start, end, f)
}

Date_POSIXct_to_tsp <- function(x){ 

  st <- as.POSIXlt(x[1])
  y <- st$year + 1900L
  m <- st$mon + 1L
  d <- st$mday

  ds <- range(diff(as.numeric(as.POSIXct(x))))
  
  if (in_range(ds, 31536000, 31622400)){
    f <- 1
    start <- y + (1 / (m - 1))
  } else if (in_range(ds, 7776000, 7948800)){
    f <- 4
    if (!(m %in% (c(1, 4, 7, 10)))) { 
      stop("Quarterly data needs to specified as start of period (currently)")
    }
    # 3*((1:4)-1)+1   ## oposite
    start <- c(y, ((m - 1) / 3) + 1)
  } else if (in_range(ds, 2419200, 2678400)){
    f <- 12
    start <- c(y, m)
  } else {
    f <- NULL
  }

  if (!is.null(f)) {
    if (d != 1){
      stop("Data needs to specified as start of period (currently)")
    }
    z <- tsp(ts(x, frequency = f, start = start))  # a bit inefficient
  } else {
    # non heuristic conversion
    z <- POSIXct_to_tsp(as.POSIXct(x))
  }
  z
}





