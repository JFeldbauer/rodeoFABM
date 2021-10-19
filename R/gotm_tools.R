#' Recieve parameter from a netcdf file created by GOTM
#'
#' This function fetches variables for given depths from a netcdf file created by running GOTM-FABM
#'
#' @param file Path to the netcdf file
#' @param var Name of the variable to get
#' @param z_out Vector of depths from which to get the variable. If no depths are selected
#'    (NULL, the default) all depths from the lowest to the highest depth in a equal distance of
#'    'res'
#' @param t_out Vector of times (as POSIX) for which the variable should be fetched. If no times
#'    are selected (NULL, the default) all available times from the netcdf file are returned
#' @param res Spatial resolution for depths, if no depths are explicitly specified by 'z_out'
#' @param reference Either "surface" or "bottom". The reference point for the depths sepified in
#'    'z_out'
#' @param long Boolean, output the variable in long format? Defaults to FALSE
#' @keywords FABM, GOTM, get variable
#' @author Johannes Feldbauer
#' @import ncdf4
#' @importFrom tidyr pivot_longer
#' @export
#' @examples
#' \dontrun{
#' temp <- get_var(file = "output.nc", var = "temp")
#' }
#'

get_var <- function(file = "output.nc", var = "temp", z_out = NULL, t_out = NULL, res = 0.5,
                       reference = "surface", long = FALSE) {

  # check if reference is correct
  if(!(reference %in% c("surface", "bottom"))) {
    stop(paste0('Reference "', reference, '" unknown. Must bei either "surface" or "bottom"'))
  }

  # set flag if a output depth is given
  z_user <- length(z_out) > 0

  # open connection to ncdf file
  nc_file <- nc_open(file)

  # get dimension variables
  z_raw <- ncvar_get(nc_file, "z")
  meta_z <- ncatt_get(nc_file, "z")
  t_raw <- ncvar_get(nc_file, "time")
  meta_t <- ncatt_get(nc_file, "time")
  # get variable
  v_raw <- ncvar_get(nc_file, var)
  meta_v <- ncatt_get(nc_file, var)
  # check if variable is 1D or 2D (time or time + depth)
  var_dim <- ifelse(grepl("z", meta_v$coordinates), "2D", "1D")
  # close the connection to the ncdf file
  nc_close(nc_file)

  # convert raw time to POSIX
  t_raw <- as.POSIXct(t_raw, origin = gsub("seconds since ", "", meta_t$units), tz = "UTC")

    if(var_dim == "2D") {
      # if no output depths are given go from highest to lowest in 'res' steps
    if(!z_user) {
      z_out <- seq(round(max(z_raw), 0), round(min(z_raw), 0), by = -res)
      if(reference == "surface") {
        z_out <- z_out - max(z_out)
      } else {
        z_out <- z_out - min(z_out)
      }
    }

    # if output depth is given check sign of the depths
    if(z_user) {
      # sort user defined output
      z_out <- z_out[order(z_out)]
      # for "surface" the signs should be negative
      if(reference == "surface" & all(z_out >= 0)) {
        z_out <- -z_out
      }
      # for "bottom" the signs should be positive
      if(reference == "bottom" & all(z_out <= 0)) {
        z_out <- -z_out
      }
      # for bottom the values should be decreasing to make the plot nice
      if(reference == "bottom" & all(diff(z_out) > 0)) {
        z_out <- rev(z_out)
      }
    }
    } else {
    z_out <- NA
  }

  # if output times are given remove other values
  if(length(t_out) > 0) {
    z_raw <- z_raw[, t_raw %in% t_out]
    v_raw <- v_raw[, t_raw %in% t_out]
    t_out <- t_raw[t_raw %in% t_out]
  } else {
    t_out <- t_raw
  }

  if (var_dim == "2D") {
      # matrix for output data
    v_out <- matrix(NA,length(t_out),length(z_out))
  } else {
      v_out <- rep(NA, length(t_out))
  }

  if (var_dim == "2D") {
    # loop through all time steps
    for(i in 1:length(t_out)){
      if(reference == "surface") {
        z_r <- z_raw[, i] - max(z_raw[, i])
      } else {
        z_r <- z_raw[, i] - min(z_raw[, i])
      }
      v_out[i, ] <- approx(z_r, v_raw[, i], z_out, rule = 1)$y
    }

    # switch NAs to start of matrix
    if(length(z_out) > 1) {
      v_out <- t(apply(v_out, 1, rev))
      z_out <- rev(z_out)
    }

    colnames(v_out) <- z_out
    rownames(v_out) <- format(t_out, "%Y-%m-%d %H:%M:%S")

    # if long format wanted as output
    if(long) {

      v_out <- pivot_longer(setNames(data.frame(time = t_out,
                               v_out), c("time", z_out)), cols = 2:(ncol(v_out)+1),
                            names_transform = list(name = as.numeric),
                            values_drop_na = TRUE)
      colnames(v_out) <- c("time", "z", "C")
    }
  } else {
    v_out <-  v_raw[t_raw %in% t_out]
    names(v_out) <- var
  }


  return(list(var = v_out,
              z = z_out,
              time = t_out,
              unit = meta_v$units,
              name = meta_v$long_name))
}

# nice color palette
mycol <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, 'Spectral')))

#' Plot parameter from a netcdf file created by GOTM
#'
#' This function plots variables for given depths and times from a netcdf file created
#' by running GOTM-FABM
#'
#' @param file Path to the netcdf file
#' @param var Name of the variable to get
#' @param reference Either "surface" or "bottom". The reference point for the depths sepified in
#'    'z_out'
#' @param z_out Vector of depths from which to get the variable. If no depths are selected
#'    (NULL, the default) all depths from the lowest to the highest depth in a equal distance of
#'    'res'
#' @param t_out Vector of times (as POSIX) for which the variable should be fetched. If no times
#'    are selected (NULL, the default) all available times from the netcdf file are returned
#' @param res Spatial resolution for depths, if no depths are explicitly specified by 'z_out'
#' @param add If only one depth is selected, add to an existing plot
#' @param col If only one depth is selected, color of the line to plot
#' @param main Title of the plot
#' @param colp Color palette to use for the 2D image
#' @param auto.xaxt Automatically create x axis labels. If set to FALSE no x axis labels are
#'    plotted.
#' @param ... additional arguments to pass to image2D or plot (if only one depth is selected)
#' @keywords FABM, GOTM, get variable
#' @author Johannes Feldbauer
#' @import ncdf4
#' @import plot3D
#' @export
#' @examples
#' \dontrun{
#' plot_var(file = "output.nc", var = "temp")
#' }
#'

plot_var <- function(file = "output.nc", var = "temp", reference = "surface", z_out = NULL,
                        t_out = NULL, res = 0.25, add = FALSE, col = 1, main = TRUE,
                        colp = mycol(100), auto.xaxt = TRUE, ...) {
  # get variable
  var <- get_var(file, var, z_out, t_out, res, reference)

  # get duation in days
  dt <- (as.numeric(max(var$time)) - as.numeric(min(var$time)))/86400

  if (dt > 1500) {
    frm <- "%Y"
  } else if (dt <= 1500 & dt >= 367) {
    frm <- "%Y.%m.%d"
  } else if (dt < 367 & dt > 2) {
    frm <- "%m.%d"
  } else if (dt <= 2) {
    frm <- "%m.%d %H:00"
  }

  if (length(var$z) > 1) {
    yl <- "Depth below surface (m)"
    if(reference == "bottom") {
      yl <- "Height above bottom (m)"
    }
    image2D(var$var, var$time, var$z, main = var$name, clab = var$unit, xlab = "Date",
            ylab = yl, col = colp, xaxt = "n", las = 1, ...)
    axis(1, at = pretty(var$time), labels = FALSE)
    if(auto.xaxt) {
      text(pretty(var$time), par("usr")[3] - 3, labels = format(pretty(var$time), frm),
           xpd = NA, srt = 336, adj = 0.0, cex = 0.8)
    }
  } else {
    if (main) {
      ml <- paste0(z_out, " m below surface")
      if(reference == "bottom") {
        ml <- paste0(z_out, " m above bottom")
      }
    } else {
      ml <- ""
    }
    if(!add) {
      plot(var$time, var$var, "n", xlab = "Date", ylab = paste0(var$name, " (", var$unit, ")"),
           main = ml, xaxt = ifelse(auto.xaxt, "t", "n"), ...)
      abline(h = pretty(var$var), col = "grey", lty = 15)
      abline(v = pretty(var$time), col = "grey", lty = 15)
    }
    lines(var$time, var$var, lwd = 1.5, col = col)
  }
}
