"""Test to compute calculatio of xi"""

import numpy as np
import rpy2
import rpy2.robjects as robjects
import rpy2.robjects.packages as rpackages

# Define the input variables
precipitation = robjects.FloatVector(
    (
        1.49516129032258,
        0.862258064516129,
        0.509677419354839,
        1.19225806451613,
        2.70935483870968,
        1.77645161290323,
        1.16096774193548,
        1.33354838709677,
        1.60516129032258,
        0.975806451612903,
        1.53161290322581,
        2.74870967741935,
    )
)
temperature = robjects.FloatVector(
    (
        27.2545161290323,
        27.561935483871,
        28.5738709677419,
        29.8109677419355,
        29.528064516129,
        27.6951612903226,
        26.3851612903226,
        26.6535483870968,
        27.0687096774194,
        27.7848387096774,
        28.328064516129,
        28.0874193548387,
    )
)
evaporation = robjects.FloatVector(
    (
        1.98,
        1.71,
        3.47,
        7,
        11.4,
        14.9,
        17.3,
        17.6,
        14.7,
        10.6,
        6.55,
        3.37,
    )
)
years = 1000
decomposition_rates = robjects.FloatVector((10, 0.3, 0.66, 0.02, 0))
initial_carbon = robjects.FloatVector((0, 0, 0, 0, 2.7))
input_carbon = 1.7
farmyard_manure = 0
clay = 32.5
soil_thickness = 30.0
plant_material_ratio = 1.44
evaporation_coefficient = 1
bare = False
solver = "euler"

# library(SoilR)
# robjects.r('library(SoilR)')
# Get the fT.RothC() and fW.RothC() functions
# fT = robjects.globalenv['SoilR.fT.RothC']
# fW = robjects.globalenv['SoilR.fW.RothC']
SoilR = rpackages.importr("SoilR")


# Compute the RothC model
fw = SoilR.fW_RothC(
    precipitation, evaporation, soil_thickness, plant_material_ratio, clay, bare
)[0]
ft = SoilR.fT_RothC(temperature)
xi = np.repeat(np.array(ft) * np.array(fw), len(temperature))
