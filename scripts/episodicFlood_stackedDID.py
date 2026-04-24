import numpy as np
import pandas as pd
import geopandas as gpd
import os
import re
from functools import reduce
import osmnx as ox
import copy
from datetime import datetime
import sys
import time

# Get the absolute path to the directory containing the module
module_dir = os.path.abspath(r"C:\Users\hypak\OneDrive - Singapore Management University\Documents\Projects\Risk Assessment\Impact of flood on property prices")
# Add the directory to the system path
sys.path.append(module_dir)

import importlib

import helper_functions.residential_attributes
import helper_functions.mrt_buffer
import helper_functions.data
import helper_functions.DEM
import helper_functions.flood_drainage_work
import helper_functions.stacked_DID

importlib.reload(helper_functions.residential_attributes)
importlib.reload(helper_functions.mrt_buffer)
importlib.reload(helper_functions.data)
importlib.reload(helper_functions.DEM)
importlib.reload(helper_functions.flood_drainage_work)
importlib.reload(helper_functions.stacked_DID)

import helper_functions.residential_attributes as residential_attributes
import helper_functions.mrt_buffer as mrt_buffer
import helper_functions.data as Data
import helper_functions.DEM as DEM
import helper_functions.flood_drainage_work as FloodDrainage
import helper_functions.utils as utils
import helper_functions.stacked_DID as stackedDID

def main(residential_df, small_buffer=200, big_buffer=500, depth=5, 
         save_dir = r"Exported_Data",
         pickle_data = True):
    """
    Run this script to execute the analysis pipeline to obtain the dataframe in csv
    
    :param buffer: buffer around points/polygons so that it will intersect with residential building. higher buffer radius implies buffer area will intersect with residential building
    :param depth: BFS depth, used for road network. TODO: If you don't want to use road network, assign as None
    :param save_dir: directory of where to save the exported csv file
    :param pickle_data: bool. whether to pickle data, if True, save as .pkl, else, save as .csv
    """
    start_time = time.perf_counter()

    if depth is not None:
        road_network_buffer = True
    else:
        road_network_buffer = False
    
    # save results as save_fp filepath
    save_fp = os.path.join(save_dir,
                        f"PrivRes_{datetime.today().strftime('%Y%m%d')}_dsmall{small_buffer:03d}dbig{big_buffer:03d}_networkDepth{depth}.csv")
    print(f"Save file as: {save_fp}")

    # import flood data
    G_car = ox.load_graphml(Data.G_CAR_FP)

    # historical flood
    if road_network_buffer:
        # # (polygon gdf) - buffer around downstream of flooded roads
        flood_df_small = Data.get_flood_network_buffer(G_car, Data.FLOOD_FP, reverse = False, 
                                    depth_limit = depth,
                                    buffer_dist=small_buffer)
        flood_df_big = Data.get_flood_network_buffer(G_car, Data.FLOOD_FP, reverse = False, 
                                    depth_limit = depth,
                                    buffer_dist=big_buffer)
    else:
        # # (polygon gdf) - buffer around flooded roads
        flood_df_small = Data.get_flood_buffer(G_car, Data.FLOOD_FP, radius=small_buffer)
        flood_df_big = Data.get_flood_buffer(G_car, Data.FLOOD_FP, radius=big_buffer)

    flood_columns = ["flooded_location","Flood_Date","geometry","Flood_ID"]
    # create stacked DID df
    master_df = stackedDID.add_historical_flooding(residential_df, 
                                   flood_df_small[flood_columns], 
                                   flood_df_big[flood_columns])
    
    columns_unkeep = ['Area (SQFT)','Unit Price ($ PSF)', 'Nett Price($)',
                    'Number of Units','Purchaser Address Indicator','SEARCHVAL', 'BLK_NO',
                    'ADDRESS', 'POSTAL', 'X', 'Y',
                    'LATITUDE', 'LONGITUDE', 'geometry','nodesID_property']
    master_df_minimal =  master_df.drop(columns = columns_unkeep)
    # rename columns for readability in R
    rename_columns = {c: re.sub(r"\s|/","_",c) for c in master_df_minimal.columns}
    master_df_minimal = master_df_minimal.rename(columns = rename_columns)
    print(master_df_minimal.columns)
    print("length of df: ", len(master_df_minimal))
    master_df_minimal.head()

    # Export Data
    if pickle_data:
        utils.pickle_data(master_df_minimal, save_fp)
    else:
        master_df_minimal.to_csv(save_fp,index=False)
    
    end_time = time.perf_counter()
    print(f"Saving file into...: {save_fp}\n=====Execution time: {int((end_time - start_time)/60)} mins.=====")
    return master_df_minimal

if __name__ == '__main__':
    # buffer = 500
    depth = None #5
    save_dir = r"Exported_Data\stacked_did"
    if not os.path.exists(save_dir):
        os.mkdir(save_dir)
    pickle_data = False
    small_buffer = 200
    big_buffer_fn = lambda x: int(round(x*(2**0.5),-1)) # sqrt2 * small_radius (rounded up to the nearest hundred)
    big_buffer = big_buffer_fn(small_buffer)

    # Import Data
    ## Import planning area and subzone
    # planningArea_shp = gpd.read_file(Data.PLANNING_AREA_FP)
    subzone_shp = gpd.read_file(Data.SUBZONE_FP)

    G_car = ox.load_graphml(Data.G_CAR_FP)

    ## Import residential
    residential_df = Data.get_private_residential_df(Data.PRIVATE_RESIDENTIAL_FP)
    # add subzone
    # spatial join with subzone
    residential_df = residential_df.sjoin(subzone_shp[["SUBZONE_N","geometry"]],how="left")
    residential_df['nodesID_property'] = ox.nearest_nodes(G_car,X=residential_df['LONGITUDE'], Y=residential_df['LATITUDE'])
    print("length of residential df: ", len(residential_df))
    # drop index_right
    residential_df = residential_df.drop(columns=["index_right"])
    print(f"Number of unique project name: {len(residential_df["Project Name"].unique())}")
    print(f"Number of unique Address: {len(residential_df["Address"].unique())}")
    print(f"Number of unique building name: {len(residential_df["Building Name"].unique())}")
    print(f"Number of tenure types: {len(residential_df['Tenure'].unique())}")
    print(f"Number of sale type types: {len(residential_df['Type of Sale'].unique())}")
    print(f"Number of property types: {len(residential_df['Property Type'].unique())}")
    print(f"Number of different floor number: {len(residential_df['Floor_level'].unique())}")
    print(f"Number of ground floor units: {residential_df['is_ground_floor'].sum()}")
    print(f"Number of NAs in floor level: {residential_df['Floor_level'].isna().sum()}")

    for small_buffer in np.arange(50,370,20):
        big_buffer = big_buffer_fn(small_buffer)
        main(residential_df, small_buffer, big_buffer, depth=depth, 
            save_dir = save_dir,
            pickle_data = pickle_data)