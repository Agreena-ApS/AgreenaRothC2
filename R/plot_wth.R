#' @title Plot weather climate Normals
#' @param wth data output from \link{get_wth_power_nasa}
#' @return Long formatted soil carbon values that can be ploted by \link{plot_rothc}
#' @details Plots weather data Climate normals download from nasa power
#' @author Marcos Alves
#' @seealso \link{get_wth_power_nasa}
#' @import ggplot2
#' @import ggpubr
#' @export
#'

plot_wth <- function(wth) {
  p1 <- ggplot(data = wth, aes(x = MM)) +
    geom_line(aes(y = TS_AV, ), size = 1) +
    geom_ribbon(aes(y = TS_AV, ymin = TS_AV - TS_SD, ymax = TS_AV + TS_SD), alpha = .2) +
    ggtitle("Temperature") +
    xlab("Months") +
    ylab(expression("Temperature (" * ~ degree * C * ")"))

  p2 <- ggplot(data = wth, aes(x = MM)) +
    geom_line(aes(y = PRECTOTCORR_AV, ), size = 1) +
    geom_ribbon(aes(y = PRECTOTCORR_AV, ymin = PRECTOTCORR_AV - PRECTOTCORR_SD, ymax = PRECTOTCORR_AV + PRECTOTCORR_SD), alpha = .2) +
    ggtitle("Precipitation") +
    xlab("Months") +
    ylab("Precipitation (mm)")

  p3 <- ggplot(data = wth, aes(x = MM)) +
    geom_line(aes(y = EVPTRNS_AV, ), size = 1) +
    geom_ribbon(aes(y = EVPTRNS_AV, ymin = EVPTRNS_AV - EVPTRNS_SD, ymax = EVPTRNS_AV + EVPTRNS_SD), alpha = .2) +
    ggtitle("Evapotranspiration") +
    xlab("Months") +
    ylab("Evapotranspiration (mm)")

  figure <- ggarrange(p1, p2, p3,
    labels = c("A", "B", "C"),
    ncol = 2, nrow = 2
  )
  annotate_figure(figure,
    top = text_grob(paste0("Avg. weather conditions  between ", attr(wth, "meta")$dates[1], " and ", attr(wth, "meta")$dates[2])),
    bottom = text_grob(paste0(
      "lat: ",
      round(as.numeric(attr(wth, "meta")$latitude), 2),
      "\nlon: ", round(as.numeric(attr(wth, "meta")$longitude), 2)
    ))
  )
}
