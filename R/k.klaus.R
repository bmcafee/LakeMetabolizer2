# ---Author: Bennett McAfee, 2026-06-08 --- 
# Last update: 2026-06-08 

#'@export
k.klaus = function(ts.data, wnd.z, lake.area, spatial.int, method = c("linear", "power")){
  
  if(!has.vars(ts.data, 'wnd')){
    stop('k.klaus requires a "wnd" (wind speed) column in the supplied data')
  }
  
  wind = get.vars(ts.data, 'wnd')
  
  k600 = k.klaus.base(wind[,2], wnd.z, lake.area, spatial.int, method = method)
  
  return(data.frame(datetime=ts.data$datetime, k600=k600))
}


#'@export
k.klaus.base <- function(wnd, wnd.z, lake.area, spatial.int, method = c("linear", "power")) {
  
  method <- match.arg(method)
  
  # converting m2 to km2
  lake.area <- lake.area / 1e6 
  
  # Converting uz to u10
  if (wnd.z != 10){
    wnd <- wind.scale.base(wnd = wnd, wnd.z = wnd.z)
  }
  
  # helper logit function
  logit.custom <- function(x){
    return(log(x / (1 - x)))
  }
  
  # sanity checks
  if (any(spatial.int <= 0 | spatial.int >= 1)) {
    stop("spatial.int must be between 0 and 1 (exclusive)")
  }
  
  if (method == "linear") {
    k600 <- (0.328 * log10(lake.area) + 1.581) * wnd - 0.066 * logit.custom(spatial.int) + 1.266
  } else if (method == "power") {
    k600 <- (0.281 * log10(lake.area) + 1.361) * (wnd ^ 1.097) - 0.072 * logit.custom(spatial.int) + 1.401
  } else if (method == "exp") {
    # if (is.null(sdi)) {
    #   stop("sdi must be provided for exponential model")
    # }
    # k600 <- (-0.057 * logit.custom(spatial.int) + 2.366) * exp(wnd * (0.144 * log10(sdi) + 0.156))
    stop('The exponential model was removed per Klaus and Vachon (2020): "...[We] do not recommend using our
models that include SDI."')
  }
  
  k600 <- k600 * (24/100) # convert cm/hr to m/day
  
  return(k600)
}


# -- References
# Klaus, Marcus, and Dominic Vachon. 2020. 
# Challenges of Predicting Gas Transfer Velocity from Wind Measurements over Global Lakes. 
# Aquatic Sciences 82 (3): 53. https://doi.org/10.1007/s00027-020-00729-9
