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

def main(buffer=200, depth=5, 
         save_dir = r"Exported_Data",
         pickle_data = True,
         private_residential_arg = True,
         HDB_residential_arg = True,
         flood_arg = True,
         flood_hotspot_arg = True,
         landuse_arg = True,
         tide_prone_arg = False,
         drainage_density_arg = False,
         centrality_arg = True,
         DEM_arg = True):
    """
    Run this script to execute the analysis pipeline to obtain the dataframe in csv
    
    :param buffer: buffer around points/polygons so that it will intersect with residential building. higher buffer radius implies buffer area will intersect with residential building
    :param depth: BFS depth, used for road network. TODO: If you don't want to use road network, assign as None
    :param save_dir: directory of where to save the exported csv file
    :param pickle_data: bool. whether to pickle data, if True, save as .pkl, else, save as .csv
    """
    start_time = time.perf_counter()

    # save results as save_fp filepath
    save_fp = os.path.join(save_dir,
                        f"Adaptation_{datetime.today().strftime('%Y%m%d')}_buffer{buffer:03d}_networkDepth{depth}.csv")
    print(f"Save file as: {save_fp}")


    if depth is not None:
        road_network_buffer = True
    else:
        road_network_buffer = False

    subzone_shp = gpd.read_file(Data.SUBZONE_FP)
    G_car = ox.load_graphml(Data.G_CAR_FP)

    # import adaptation df as master_df
    road_raising_works_buffer_df = Data.get_road_raising_buffer_df(G_car, Data.DRAINAGE_WORKS_FP,
                                                               buffer_dist=buffer)
    drainage_works_df = Data.get_drain_improvement_buffer_df(G_car, Data.DRAINAGE_WORKS_FP, 
                                                            buffer_dist=buffer)
    master_df = pd.concat([road_raising_works_buffer_df,drainage_works_df])
    master_df = master_df.reset_index(names="Drainage_ID")

    if private_residential_arg:
        G_car = ox.load_graphml(Data.G_CAR_FP)
        residential_df = Data.get_private_residential_df(Data.PRIVATE_RESIDENTIAL_FP)
        # spatial join with subzone
        residential_df = residential_df.sjoin(subzone_shp[["SUBZONE_N","geometry"]],how="left")

    if HDB_residential_arg:
        # import HDB
        HDB_residential_df = Data.get_hdb_residential_df(Data.HDB_RESALE_RESIDENTIAL_FP)
    
    if flood_arg:
        # historical flood
        if road_network_buffer:
            # # (polygon gdf) - buffer around downstream of flooded roads
            flooding_df = Data.get_flood_network_buffer(G_car, Data.FLOOD_FP, reverse = False, 
                                        depth_limit = depth,
                                        buffer_dist=buffer)
        else:
            # # (polygon gdf) - buffer around flooded roads
            flooding_df = Data.get_flood_buffer(G_car, Data.FLOOD_FP, radius=buffer)
    
    if flood_hotspot_arg:
        flooding_hotspot_buffer = Data.get_flooding_hotspot_buffer(G_car, Data.FLOODING_HOTSPOT_FP, radius=buffer)

    if centrality_arg:
        Gcar_edge_betweeness_centrality = utils.load_pickle(r"Data\Gcar_edge_betweeness_centrality.pkl")
        Gcar_edge_closeness_centrality = utils.load_pickle(r"Data\Gcar_edge_closeness_centrality.pkl")

    if tide_prone_arg:
        tide_3m_polygon = Data.get_coastal_flood_prone(Data.TIDE_3M_FP)

    if landuse_arg:
        landuse = gpd.read_file(r"C:\Users\hypak\OneDrive - Singapore Management University\Documents\Data\SG_Landuse\MP2019_LU_DESC.shp")

    # column names
    private_residential_columns = ['Sale_Date','Type of Area', 'Area (SQM)', 'Unit Price ($ PSM)', 'Nett Price($)',
       'Property Type', 'Number of Units','Completion Date','REGION_N', 'PLN_AREA_N','geometry']
    HDB_residential_columns = ['Sale_Date','Property Type','Area (SQM)', 'flat_model', 'lease_commence_date',
        'Transacted Price ($)','geometry']
    drainage_columns = ['Drainage_Date','work_types','work_categories','geometry','u','v','key']
    flood_columns = ['Flood_Date','geometry']
    flood_hotspot_columns = ['Flood_Hotspot_Date','geometry']
    landuse_columns = ['LU_DESC', 'geometry']

    if private_residential_arg:
        # merge with private residential
        master_df = master_df[drainage_columns].sjoin(residential_df[private_residential_columns],how="left").drop(columns=['index_right'])
    
    if tide_prone_arg:
        # merge with tide prone
        master_df["prone_to_high_tide"] = master_df['geometry'].apply(lambda x: tide_3m_polygon.intersects(x))
    
    if centrality_arg:
        # merge with centrality metrics
        master_df['betweeness_centrality'] = master_df.apply(lambda x: Gcar_edge_betweeness_centrality[(x['u'],x['v'],x['key'])],axis=1)
        master_df['closeness_centrality'] = master_df.apply(lambda x: Gcar_edge_closeness_centrality[(x['u'],x['v'],x['key'])],axis=1)
    
    if DEM_arg:
        # Merge with DEM
        # drop NAs in coordinates
        master_df = master_df.dropna(subset=["LATITUDE","LONGITUDE"])
        master_df["DEM"] = master_df.apply(lambda x: DEM.get_DEM_value(x["LONGITUDE"],x["LATITUDE"]),axis=1)
    
    if HDB_residential_arg:
        # merge with HDB residential
        master_df = master_df.sjoin(HDB_residential_df[HDB_residential_columns],how="left",
                    lsuffix='private',rsuffix='HDB').drop(columns=['index_HDB'])
    
    if flood_arg:
        # merge with flood data
        master_df = master_df.sjoin(flooding_df[flood_columns],how="left").drop(columns=['index_right'])
    
    if flood_hotspot_arg:
        # merge with flooding hotspot data
        master_df = master_df.sjoin(flooding_hotspot_buffer[flood_hotspot_columns],how="left").drop(columns=['index_right'])
    
    if landuse_arg:
        # merge with land use
        master_df = master_df.sjoin(landuse[landuse_columns],how="left").drop(columns=['index_right'])
    
    print(master_df.columns)
    print(f"Length of df: {len(master_df)}")

    # Export Data
    if pickle_data:
        utils.pickle_data(master_df, save_fp)
    else:
        master_df.to_csv(save_fp,index=False)
    
    end_time = time.perf_counter()
    print(f"Saving file into...: {save_fp}\n=====Execution time: {int((end_time - start_time)/60)} mins.=====")
    return master_df

if __name__ == '__main__':
    # buffer = 500
    depth = None #5
    save_dir = r"Exported_Data\adaptation"
    if not os.path.exists(save_dir):
        os.mkdir(save_dir)
    pickle_data = False
    buffer = 200

    private_residential_arg = True
    HDB_residential_arg = True
    flood_arg = True
    flood_hotspot_arg = True
    landuse_arg = True
    tide_prone_arg = True
    drainage_density_arg = True
    centrality_arg = True
    DEM_arg = True

    main(buffer=buffer, depth=depth, 
         save_dir = save_dir,
         pickle_data = pickle_data,
         private_residential_arg = private_residential_arg,
         HDB_residential_arg = HDB_residential_arg,
         flood_arg = flood_arg,
         flood_hotspot_arg = flood_hotspot_arg,
         landuse_arg = landuse_arg,
         tide_prone_arg = tide_prone_arg,
         drainage_density_arg = drainage_density_arg,
         centrality_arg = centrality_arg,
         DEM_arg = DEM_arg)

