"""Test to compute RothCModel from SoilR

Agreena implementation at
https://github.com/Agreena-ApS/AgreenaRothC2/blob/main/R/AgreenaProgramme2.R#L259-L278
"""
from pathlib import Path
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
    input_variables = pd.read_csv(Path(__file__).parent / "data/inputs.csv")
    weather_data = pd.read_csv(Path(__file__).parent / "data/weather.csv")

    # Define the RothC parameters
    rothc_parameters = pd.DataFrame({
        "k.DPM": 10.0, "k.RPM": 0.3, "k.BIO": 0.66, "k.HUM": 0.02, "k.IOM": 0.0
    }, index=[0])

    #
    #### Compute xi
    #
    # Define the input variables
    # decomposition_rates = np.array((10.0, 0.3, 0.66, 0.02, 0.0))
    initial_carbon = np.array((0.0, 0.0, 0.0, 0.0, 2.7))
    # input_carbon = 1.7
    # farmyard_manure = 0.0
    # clay = 32.5
    # soil_thickness = 30.0
    # plant_material_ratio = 1.44
    # evaporation_coefficient = 1.0
    # bare = False
    # solver = "euler"
    t = np.arange(1/12, int(input_variables["Years"].iloc[0]), 1/12)

    # `SoliR.fW_RothC` returns wrong results
    fw_b = SoilR.fW_RothC(
        P=np.array(weather_data["precipitation"]),
        E=np.array(weather_data["evaporation"]),
        S_Thick=int(input_variables["Soil_thickness"].iloc[0]),
        pClay=float(input_variables["clay"].iloc[0]),
        pE=float(input_variables["pE"].iloc[0]),    # plant_material_ratio,
        bare=False if input_variables["bare"].iloc[0] == "FALSE" else True
    )["b"]

    ft = SoilR.fT_RothC(weather_data["temperature"])  # returns a single value
    breakpoint()
    # np.multiply(np.array(fw_b), ft)

    def np_rep_len(x, length_out):
        return np.tile(x, length_out)[:length_out]
        # return np.resize(x, length_out)
        # return np.array(list(x) * (int(length_out / len(x))+1))[:length_out] 
        # return np.repeat(x, length_out)[:length_out]
        # return robjects.r('''rep_len(x, length.out = lenght_out)''')

    xi = pd.DataFrame(
        np_rep_len(np.multiply(np.array(fw_b), ft), len(t)),
        index=t
    )
    
    # 1 0.08333333                                 4.259312
    # 2 0.16666667                                 4.320228
    # 3 0.25000000                                 4.520404
    # 4 0.33333333                                 4.764131
    # 5 0.41666667                                 4.708510
    # ...
    # 11995 999.5833                                2.5064024
    # 11996 999.6667                                1.1017760
    # 11997 999.7500                                0.8444951
    # 11998 999.8333                                0.8728741
    # 11999 999.9167                                0.8943673
    print(xi)
    breakpoint()


    # Create a RothCModel object
    model = SoilR.RothCModel(
        t,                                               # t
        rothc_parameters,                                # ks
        initial_carbon,                                  # C0
        np.array(input_variables["biomass_inputs"]),     # In
        np.array(input_variables["FYM"]),                # FYM
        np.array(input_variables["DR"]),                 # DR
        np.array(input_variables["clay"]),               # Clay
        xi,                                              # xi
        SoilR.deSolve_lsoda_wrapper,                     # solver
        robjects.NULL,                                   # pass
    )

    # Trigger the RothC calculations
    model.compute()

    # Get the carbon stores
    carbon_stores = model.getC()
    print(carbon_stores)
