% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yield_to_resid2.R
\name{yield_to_resid2}
\alias{yield_to_resid2}
\title{Yield to residues}
\usage{
yield_to_resid2(yield, cropname, cf1 = 1, cf2 = 1)
}
\arguments{
\item{yield}{vector with crop yields}

\item{cropname}{vector with cropnames}

\item{cf1}{Coefficient factor for slope yield inputs}

\item{cf2}{Coefficient factor Intercept yield inputs}
}
\value{
vector with crop residues in t C/ha
}
\description{
Calculate crop specific biomass residues
baseline and scenario runs.
}
\author{
Marcos Alves
}
