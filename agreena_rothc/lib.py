"""Test to compute RothCModel from SoilR"""

import pandas as pd
import numpy as np
import rpy2.robjects as robjects
import rpy2.robjects.packages as rpackages


# Import the SoilR package
SoilR = rpackages.importr("SoilR")

from rpy2.robjects import numpy2ri, pandas2ri
from rpy2.robjects import default_converter
from rpy2.robjects.conversion import localconverter

with localconverter(default_converter + numpy2ri.converter + pandas2ri.converter): 

    # Define the input variables
    input_variables = {
        "Years": 10,
        "Soil_thickness": 30.0,
        "pE": 1.44,
        "clay": 32.5,
        "bare": 0.0,
        "biomass_inputs": 1.7,
        "FYM": 0.0,
        "DR": np.array((10.0, 0.3, 0.66, 0.02, 0.0)),
    }

    # Define the RothC parameters
    rothc_parameters = pd.DataFrame({
        "k.DPM": 10.0, "k.RPM": 0.3, "k.BIO": 0.66, "k.HUM": 0.02, "k.IOM": 0.0
    }, index=[0])

    #
    #### Compute xi
    #
    # Define the input variables
    precipitation = np.array(
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
    temperature = np.array(
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
    evaporation = np.array(
        (
            1.98,
            1.71,
            3.47,
            7.0,
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
    decomposition_rates = np.array((10.0, 0.3, 0.66, 0.02, 0.0))
    initial_carbon = np.array((0.0, 0.0, 0.0, 0.0, 2.7))
    input_carbon = 1.7
    farmyard_manure = 0.0
    clay = 32.5
    soil_thickness = 30.0
    plant_material_ratio = 1.44
    evaporation_coefficient = 1.0
    bare = False
    solver = "euler"

    # returns a dataframe
    #     Acc.TSMD     b
    # 1  -62.854839 -1.534252
    # 2  ...        ...
    # ...
    # 12 ...        ...
    fw = SoilR.fW_RothC(
        precipitation, evaporation, soil_thickness, plant_material_ratio, clay, bare
    )
    ft = SoilR.fT_RothC(temperature)  # returns a single value

    multiply = fw.multiply(ft, axis=0)
    xi = multiply.to_numpy()
    print(xi)


    # Create a RothCModel object
    model = SoilR.RothCModel(
        1 / 12,
        rothc_parameters,
        np.array([0.0, 0.0, 0.0, 0.0, 2.7]),
        input_variables["biomass_inputs"],
        input_variables["FYM"],
        input_variables["DR"],
        input_variables["clay"],
        xi,
        SoilR.deSolve_lsoda_wrapper,
        robjects.NULL,
    )

    # Trigger the RothC calculations
    model.compute()

    # Get the carbon stores
    carbon_stores = model.getC()
    print(carbon_stores)
