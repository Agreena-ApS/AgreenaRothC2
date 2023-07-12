#' @export
load_base_yields <- function() {
  if (!exists("base_yields")) {
    base_yields <<- read.csv(file.path(getOption("calib_inputs"), "base_yields.csv"))
  }
  return(base_yields)
}

#' @export
load_resid_mgmt <- function() {
  if (!exists("resid_mgmt")) {
    resid_mgmt <<- read.csv(file.path(getOption("calib_inputs"), "resid_mgmt.csv"),row.names = 1)
  }
  return(resid_mgmt)
}

#' @export
load_crop_names <- function() {
  if (!exists("crop_names")) {
    crop_names <<- read.csv(file.path(getOption("calib_inputs"), "crop_names.csv"), sep = ";")
  }
  return(crop_names)
}

#' @export
load_yld2bio <- function() {
  if (!exists("yld2bio")) {
    yld2bio <<- read.csv(file.path(getOption("calib_inputs"), "yld2bio.csv"), sep = ";", row.names = 1)
  }
  return(yld2bio)
}

#' @export
load_tillage_convert <- function() {
  if (!exists("tillage_convert")) {
    tillage_convert <<- read.csv(file.path(getOption("calib_inputs"), "tillage_convert.csv"))
  }
  return(tillage_convert)
}

#' @export
load_res_fractions <- function() {
  if (!exists("res_fractions")) {
    res_fractions <<- read.csv(file.path(getOption("calib_inputs"), "res_fractions.csv"), row.names = 1, sep = ";")
  }
  return(res_fractions)
}

#' @export
load_cc <- function() {
  if (!exists("cc")) {
    cc <<- read.csv(file.path(getOption("calib_inputs"), "cc.csv"), row.names = 1)
  }
  return(cc)
}

