#' Implementation of the RothCModel
#'
#' This function implements the RothC model of Jenkinson et al. It is a wrapper
#' for the more general function \code{\link{GeneralModel}}.
#'
#'
#' @param t A vector containing the points in time where the solution is
#' sought.
#' @param ks A vector of length 5 containing the values of the decomposition
#' rates for the different pools
#' @param C0 A vector of length 5 containing the initial amount of carbon for
#' the 5 pools.
#' @param In A scalar or data.frame object specifying the amount of litter
#' inputs by time.
#' @param FYM A scalar or data.frame object specifying the amount of Farm Yard
#' Manure inputs by time.
#' @param DR A scalar representing the ratio of decomposable plant material to
#' resistant plant material (DPM/RPM).
#' @param clay Percent clay in mineral soil.
#' @param xi A scalar or data.frame object specifying the external
#' (environmental and/or edaphic) effects on decomposition rates.
#' @param solver A function that solves the system of ODEs. This can be
#' \code{\link{euler}} or \code{\link{deSolve.lsoda.wrapper}} or any other user
#' provided function with the same interface.
#' @param pass if TRUE forces the constructor to create the model even if it is
#' invalid
#' @return A Model Object that can be further queried
#' @seealso There are other \code{\link{predefinedModels}} and also more
#' general functions like \code{\link{Model}}.
#' @references Jenkinson, D. S., S. P. S. Andrew, J. M. Lynch, M. J. Goss, and
#' P. B. Tinker. 1990. The Turnover of Organic Carbon and Nitrogen in Soil.
#' Philosophical Transactions: Biological Sciences 329:361-368. Sierra, C.A.,
#' M. Mueller, S.E. Trumbore. 2012. Models of soil organic matter
#' decomposition: the SoilR package version 1.0. Geoscientific Model
#' Development 5, 1045-1060.
#' @importFrom SoilR BoundInFluxes BoundLinDecompOp GeneralModel
#' @export

RothCModel2 <- function(
    t,
    ks = c(k.DPM = 10, k.RPM = 0.3, k.BIO = 0.66, k.HUM = 0.02, k.IOM = 0),
    C0 = c(0, 0, 0, 0, 2.7),
    In = 1.7,
    FYM = 0,
    DR = 1.44,
    clay = 23.4,
    xi = 1,
    solver = deSolve.lsoda.wrapper,
    pass = FALSE) {
  t_start <- min(t)
  t_end <- max(t)
  if (length(ks) != 5) stop("ks must be of length = 5")
  if (length(C0) != 5) stop("the vector with initial conditions must be of length = 5")
  if (class(In) != class(FYM)) stop("Inputs In and FYM must be of the same class, either scalars or data.frame")
  if (class(FYM) != class(DR)) stop("Inputs In and FYM and DR must be of the same class, either scalars or data.frame")
  if (length(In) == 1) {
    inputFluxes <- SoilR::BoundInFluxes(
      function(t) {
        matrix(
          c(
            In * (DR / (DR + 1)) + (FYM * 0.49),
            In * (1 / (DR + 1)) + (FYM * 0.49),
            0,
            (FYM * 0.02),
            0
          ),
          nrow = 5,
          ncol = 1
        )
      },
      t_start,
      t_end
    )
  }

  # Fix me. This code needs to be refactored to do interpolation by TimeMap instead of manually.
  if (is.data.frame(In)) {
    inputFlux <- splinefun(In[, 1], In[, 2])
    FYMflux <- splinefun(FYM[, 1], FYM[, 2])
    DRs <- splinefun(DR[, 1], DR[, 2])

    inputFluxes <- SoilR::BoundInFluxes(
      function(t) {
        matrix(
          c(
            inputFlux(t) * (DRs(t) / (DRs(t) + 1)) + (FYMflux(t) * 0.49),
            inputFlux(t) * (1 / (DRs(t) + 1)) + (FYMflux(t) * 0.49),
            0,
            FYMflux(t) * 0.02,
            0
          ),
          nrow = 5,
          ncol = 1
        )
      },
      min(In[, 1]),
      max(In[, 1])
    )
  }

  x <- 1.67 * (1.85 + 1.60 * exp(-0.0786 * clay))
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

  Af <- SoilR::BoundLinDecompOp(
    function(t) {
      fX(t) * A
    },
    t_start,
    t_end
  )

  Mod <- SoilR::GeneralModel(
    t = t,
    A = Af,
    ivList = C0,
    inputFluxes = inputFluxes,
    solverfunc = solver,
    pass = pass
  )

  return(Mod)
}
