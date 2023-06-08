#' Inverted decomposition ma
#'
#' This function implements the RothC model of Jenkinson et al. It is a wrapper
#' for the more general function \code{\link{GeneralModel}}.
#'
#' @title CeqlRoth
#' @name CeqlRoth
#' @param ks A vector of length 5 containing the values of the decomposition
#' rates for the different pools
#' @param C0 A vector of length 5 containing the initial amount of carbon for
#' the 5 pools.
#' @param In A scalar or data.frame object specifying the amount of litter
#' inputs by time.
#' @param DR A scalar representing the ratio of decomposable plant material to
#' resistant plant material (DPM/RPM).
#' @param clay Percent clay in mineral soil.
#' @param xi A scalar or data.frame object specifying the external
#' (environmental and/or edaphic) effects on decomposition rates.
#' @param FYM A scalar or data.frame object specifying the amount of Farm Yard
#' Manure inputs by time.
#' @return Initial carbon equilibrium status of RothC carbvon pools for adefined biomass input
#' @author Marcos Alves (adapted from SoilR::RothCmodel)
#' @export

CeqlRoth <- function(ks = c(
                       k.DPM = 10, k.RPM = 0.3, k.BIO = 0.66, k.HUM = 0.02,
                       k.IOM = 0
                     ), C0 = c(0, 0, 0, 0, 2.7), In = 1.7, FYM = 0,
                     DR = 1.44, clay = 23.4, xi = 1) {
  if (length(ks) != 5) {
    stop("ks must be of length = 5")
  }
  if (length(C0) != 5) {
    stop("the vector with initial conditions must be of length = 5")
  }
  if (length(In) != 1) {
    stop("The initialization input value must be lenght 1")
  }
  inputFluxes <-
    matrix(nrow = 5, ncol = 1, c(
      In * (DR / (DR + 1)) +
        (FYM * 0.49), In * (1 / (DR + 1)) + (FYM * 0.49),
      0, (FYM * 0.02), 0
    ))

  x <- 1.67 * (1.85 + 1.6 * exp(-0.0786 * clay))
  B <- 0.46 / (x + 1)
  H <- 0.54 / (x + 1)
  ai3 <- B * ks
  ai4 <- H * ks
  A <- diag(-ks)
  A[3, ] <- A[3, ] + ai3
  A[4, ] <- A[4, ] + ai4
  if (length(xi) == 1) {
    fX <- function(t) {
      xi
    }
  }
  if (is.data.frame(xi)) {
    X <- xi[, 1]
    Y <- xi[, 2]
    fX <- splinefun(X, Y)
  }
  fX <- mean(xi[1:12, 2])
  A1 <- fX * A[1:4, 1:4]
  A1 <- solve(A1)
  A1 <- cbind(A1, A[1:4, 5])
  A1 <- rbind(A1, t(A[5, ]))

  Ceql <- as.vector(-A1 %*% inputFluxes)
  Ceql[5] <- C0["IOM"]
  return(Ceql)
}
