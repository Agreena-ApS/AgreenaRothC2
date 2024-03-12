"""
Test to parse entire functions
"""

import rpy2.robjects as robjects

robjects.r(

'''
library(SoilR)
i <- read.csv("inputs.csv")
w <- read.csv("weather.csv")

t <- seq(1/12, i$Years, by = 1/12)
fw <- fW.RothC(P=w$precipitation,E = w$evaporation,S.Thick = i$Soil_thickness,pE =i$pE,pClay = i$clay ,bare = i$bare)$b
ft <- fT.RothC(w$temperature)
xi <-  data.frame(t,rep_len(ft * fw, length.out = length(t)))

model <- RothCModel(t = t,
           ks = c(k.DPM = 10, k.RPM = 0.3, k.BIO = 0.66, k.HUM = 0.02, k.IOM = 0),
           C0 = c(0, 0, 0, 0, 2.7),
           In = as.numeric(i$biomass_inputs),
           FYM = as.numeric(i$FYM),
           DR = i$DR,
           clay = i$clay,
           xi = xi,
           solver = deSolve.lsoda.wrapper,
           pass = FALSE)
Ct <- getC(model)
)
'''
)