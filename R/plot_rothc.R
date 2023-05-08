#' @title Plot AgreenaRothC diagnostics plots
#' @param ... list of objects output from \link{AgreenaRothC}
#' @return Long formatted soil carbon values that can be ploted by \link{plot_rothc}
#' @details Receives as inputs a list of data frames containing the output of AgreenaRothC and plots diagnostics graphs comparing different runs
#' @author Marcos Alves
#' @seealso \link{AgreenaRothC}
#' @import ggplot2
#' @import ggpubr
#' @import ggnewscale
#' @export
#'

plot_rothc <- function(...) {
  z <- tidy_rothc(...)
  p1 <- ggplot(data = z[z$Type == "scenario", ], aes(x = month)) +
    # geom_line(aes(y = mean, color = run_code_mean ), size = 0.5) +
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = run_code_mean), alpha = .2) +
    geom_smooth(aes(y = mean, color = run_code_mean), size = 0.5) +
    ggnewscale::new_scale_color() +
    geom_smooth(data = z[z$Type == "baseline", ], aes(x = month, y = mean, color = run_code_mean), size = 0.5) +
    xlab("N Months") +
    ylab("tC/ha")


  p2 <- ggplot(data = tail(z[z$Type == "scenario", ], 12), aes(x = run_code_mean, y = mean)) +
    geom_boxplot(aes(fill = run_code_mean), alpha = 0.9) +
    xlab("Simulations") +
    ylab("tC/ha")
  # geom_dotplot(binaxis='y', stackdir='center', binwidth = 0.7, alpha = 0.2, dotsize = 0.05)

  figure <- ggarrange(p1, p2,
    labels = c("A", "B"),
    ncol = 2, nrow = 1
  )
  print(figure)
}
