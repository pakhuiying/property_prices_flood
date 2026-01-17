import numpy as np
import pandas as pd
import geopandas as gpd
import os
import re
# import fnmatch
# import matplotlib.pyplot as plt
from functools import reduce
# from bs4 import BeautifulSoup
# import requests
import osmnx as ox
# from API_KEY import get_OneMap_token
# import networkx as nx
import copy
# import matplotlib
# import matplotlib as mpl
from datetime import datetime
# from matplotlib_scalebar.scalebar import ScaleBar
# from matplotlib_map_utils.core.north_arrow import NorthArrow, north_arrow

import sys

# Get the absolute path to the directory containing the module
module_dir = os.path.abspath(r"C:\Users\hypak\OneDrive - Singapore Management University\Documents\Projects\Risk Assessment\Impact of flood on property prices")
# Add the directory to the system path
sys.path.append(module_dir)


# import helper_functions.serviceArea as serviceArea
# import helper_functions.amenities_dict as amenities_dict
# import helper_functions.plot_utils as plot_utils
# import helper_functions.OneMapAPI as OneMapAPI
import helper_functions.residential_attributes as residential_attributes
import helper_functions.flood as Flood
import helper_functions.mrt_buffer as mrt_buffer
import helper_functions.drainage_work as drainage_work
import helper_functions.data as Data
import helper_functions.DEM as DEM


def main(buffer=500, depth=5, adaptation_groupby_column='Building Name',
         save_dir = r"Exported_Data"):
    """
    Run this script to execute the analysis pipeline to obtain the dataframe in csv
    
    :param buffer: buffer around points/polygons so that it will intersect with residential building. higher buffer radius implies buffer area will intersect with residential building
    :param depth: BFS depth, used for road network. TODO: If you don't want to use road network, assign as None
    :param adaptation_groupby_column: Default is Building Name - better if fixed effect is building, else use Project Name but effects may be diluted because adaption work will be a lot more extension
    :param save_dir: directory of where to save the exported csv file
    """
    if depth is not None:
        road_network_buffer = True
    else:
        road_network_buffer = False
    
    # save results as save_fp filepath
    save_fp = os.path.join(save_dir,
                        f"floodHDBResidential_{datetime.today().strftime('%Y%m%d')}_buffer{buffer}_networkDepth{depth}_adaptation{adaptation_groupby_column.replace(' ','')}.csv")
    print(f"Save file as: {save_fp}")

    # Import Data
    ## Import planning area and subzone
    planningArea_shp = gpd.read_file(Data.PLANNING_AREA_FP)
    subzone_shp = gpd.read_file(Data.SUBZONE_FP)

    ## import network
    G_walk = ox.load_graphml(Data.G_WALK_FP)
    G_car = ox.load_graphml(Data.G_CAR_FP)

    ## Import residential
    residential_df = Data.get_hdb_residential_df(Data.HDB_RESALE_RESIDENTIAL_FP)
    print(f"Length of residential_df: {len(residential_df)}")
    # drop duplicates where all content across columns are the same!
    residential_df = residential_df.drop_duplicates(keep="first")
    print(f"Length of residential_df after dropping duplicates: {len(residential_df)}")
    # create a unique index that would identify each unique transaction so there is no need to keep quoting 5 column names to identify unique columns
    residential_df['unique_index'] = residential_df[['Address','month_year','Transacted Price ($)','Area (SQM)','remaining_lease']].astype(str).apply(lambda x: ''.join(x), axis=1)
    # add subzone
    # spatial join with subzone, planning area and region
    residential_df = residential_df.sjoin(subzone_shp[["SUBZONE_N","PLN_AREA_N",'REGION_N',"geometry"]],how="left")

    # add nodesID for properties
    residential_df['nodesID_property'] = ox.nearest_nodes(G_car,X=residential_df['LONGITUDE'], Y=residential_df['LATITUDE'])

    print("length of residential df: ", len(residential_df))
    # drop index_right
    residential_df = residential_df.drop(columns=["index_right"])
    print(f"Number of unique project name: {len(residential_df["Project Name"].unique())}")
    print(f"Number of unique Address: {len(residential_df["Address"].unique())}")
    print(f"Number of unique building name: {len(residential_df["Building Name"].unique())}")
    print(f"Number of property types: {len(residential_df['Property Type'].unique())}")
    print(f"Number of different floor number: {len(residential_df['Floor_level'].unique())}")
    print(f"Number of ground floor units: {residential_df['is_ground_floor'].sum()}")


    # import flood data
    # historical flood
    flood_df = Data.get_flood_df(Data.FLOOD_FP)
    # update from 2013 to 2025
    # spatial join with subzone
    flood_df = flood_df.sjoin(subzone_shp[["SUBZONE_N","PLN_AREA_N","geometry"]],how="left")
    flood_df.head()

    ## Import PUB-compiled flooding hotspot
    flooding_hotspot_buffer = Data.get_flooding_hotspot_buffer(G_car, Data.FLOODING_HOTSPOT_FP, radius=buffer)
    print("Earliest record of hotspot date:", flooding_hotspot_buffer["Flood_Hotspot_Date"].sort_values().values[0])
    flooding_hotspot_buffer.head()

    ## Import area near the coast prone to flooding during high-tide (3m)
    tide_3m_polygon = Data.get_coastal_flood_prone(Data.TIDE_3M_FP)

    ### Import road raising buffer
    road_raising_works_buffer_df = Data.get_road_raising_buffer_df(G_car, Data.DRAINAGE_WORKS_FP,
                                                               buffer_dist=buffer)
    print(road_raising_works_buffer_df.columns)

    ### Import downstream road raising network buffer
    if road_network_buffer:
        road_raising_network_buffer_df = Data.get_road_raising_network_buffer_df(G_car, Data.DRAINAGE_WORKS_FP,
                                                                                    reverse = False, 
                                                                                    depth_limit = depth,
                                                                                    buffer_dist=buffer)
        
        road_raising_network_buffer_df.columns
    else:
        road_raising_network_buffer_df = None

    ### import drainage works
    # includes both drainage works on roadside and outlet drains + culvert improvement works
    drainage_works_df = Data.get_drain_improvement_buffer_df(G_car, Data.DRAINAGE_WORKS_FP, 
                                                            buffer_dist=buffer)
    # drainage_works_df = Data.get_drainage_works_buffer_df(Data.DRAINAGE_WORKS_FP)
    print("Type of works: ",drainage_works_df['work_categories'].unique())
    print(drainage_works_df.columns)

    ### Combine road raising + drainage works df
    if road_network_buffer: # ise use road net work buffer
        road_drainage_works = pd.concat([road_raising_network_buffer_df,drainage_works_df])
    else:
        road_drainage_works = pd.concat([road_raising_works_buffer_df,drainage_works_df])
    # convert to a geodataframe
    road_drainage_works = gpd.GeoDataFrame(data=road_drainage_works, geometry=road_drainage_works['geometry'])
    print("Unique type of adaptation works: ", road_drainage_works["work_categories"].unique())

    ## Import drainage density
    drainage_density_df = Data.get_drainage_density_df(Data.DRAINAGE_DENSITY_FP)

    ## Import MRT and buffer
    mrt_buffer_df = Data.get_MRT_buffer_df(Data.MRT_FP, buffer_dist=buffer)
    mrt_buffer_df.head()

    # Merge all datasets
    master_df = copy.deepcopy(residential_df)
    print(master_df.columns)
    print("length of df: ", len(master_df))

    ## Merge with centrality metrics
    master_df = residential_attributes.add_centrality_metrics(master_df,residential_nodes_column_name='nodesID_property',
                                                            standardise=True)
    print(master_df.columns)
    print("length of df: ", len(master_df))

    ## Merge with MRT
    master_df = mrt_buffer.add_mrt_buffer(master_df,mrt_buffer_df,
                                address_column = "Address", sale_date_column = "Sale_Date",
                                    construction_period = 6)
    print(master_df.columns)
    print("length of df: ", len(master_df))

    ## Merge with DEM
    master_df["DEM"] = master_df.apply(lambda x: DEM.get_DEM_value(x["LONGITUDE"],x["LATITUDE"]),axis=1)
    print(master_df.columns)
    print("length of df: ", len(master_df))

    ## Merge with flood event

    # - Use 6/12/18 months to flag transaction records that has floods occurring within 6/12/18 months
    # - Include continuous weeks-since-flood as robustness checks (*Note: this is added after checking whether location is in a flood prone area as a check to see if the area has experienced repeated flooding. Reduce noise in dataset by only examining areas with repeated flooding)

    master_df = master_df.merge(flood_df[["flooded_location","Flood_Date","SUBZONE_N"]],how="left",
                    left_on=["SUBZONE_N",'Sale_Date'], 
                    right_on=["SUBZONE_N",'Flood_Date'],suffixes=('_property','_flood'))
    # master_df = master_df.drop_duplicates(subset=['Project Name','Address','Sale_Date'])
    master_df = master_df.drop_duplicates(subset=['unique_index'])
    master_df = Flood.add_flood_within_months(master_df,sale_date_column="Sale_Date",months_after_flood=6,
                                            groupby_column=["SUBZONE_N"],
                                            drop_duplicate_column=["unique_index"])
    master_df = Flood.add_flood_within_months(master_df,sale_date_column="Sale_Date",months_after_flood=12,
                                            groupby_column=["SUBZONE_N"],
                                            drop_duplicate_column=["unique_index"])
    master_df = Flood.add_flood_within_months(master_df,sale_date_column="Sale_Date",months_after_flood=18,
                                            groupby_column=["SUBZONE_N"],
                                            drop_duplicate_column=["unique_index"])
    master_df = Flood.add_weeks_since_flood(master_df,sale_date_column="Sale_Date", 
                                            groupby_column=["SUBZONE_N"],
                                            drop_duplicate_column=["unique_index"],
                                            fillna=np.nan)
    print(master_df.columns)
    print("length of df: ", len(master_df))

    ## Merge with PUB-compiled flooding hotspot
    # - Identifies if an area is within a flood prone area for that transaction year
    #     - if publication date of the flooding hotspot is after the transaction date, it means the location is in a flood prone area
    # - Obtain the latest flooding hotspot date
    # - Directly examine whether flood occurs in a flood-prone area or not, and if flood recency has an impact on property price for area that is flood-prone and for area that is not flood-prone

    master_df = Flood.add_within_flooding_hotspot_buffer(master_df,flooding_hotspot_buffer,
                                                        #  groupby_column = ["Project Name"], 
                                                        groupby_column=[adaptation_groupby_column],
                                                        sale_date_column="Sale_Date")
    print(master_df.columns)
    print("length of df: ", len(master_df))

    ## Merge with coastal flood prine area
    master_df["prone_to_high_tide"] = master_df['geometry'].apply(lambda x: tide_3m_polygon.intersects(x))
    print(master_df.columns)
    print("length of df: ", len(master_df))

    # spatial join drainage works with master_df - this will produce duplicated rows because different drianage work categories can intersect with the same residential location at the same time
    #  (i.e. multiple works carried out simultaneously at the same location)
    master_drainage_df = master_df.sjoin(road_drainage_works[["work_categories","year","month","ROAD_NAME","geometry"]],how="left",
                                                            rsuffix="drainage",lsuffix="property",on_attribute=["year","month"])

    print("length of df before merging: ", len(master_drainage_df))
    print("Number of unique work categories: ", master_drainage_df['work_categories'].unique())
    print(master_drainage_df['work_categories'].value_counts())
    # aggregate drainage works by the same location e.g. work A and B are carried out at the same location but exists as separate rows in the df, 
    # aggregate them such that it shows A & B works in one row instead of 2 row
    master_df = master_df.merge(master_drainage_df.groupby(['unique_index']).agg({'work_categories': lambda x: ','.join([str(i) for i in set(x.tolist())])}).reset_index(level=[0]),
            on=['unique_index'])
    # after aggregating work categories, the NaNs will join to become a string - nan, and it would read as a string instead of dtype==np.nan
    master_df['work_categories'] = master_df['work_categories'].replace({"nan":np.nan})
    print("length of df after merging: ", len(master_df))
    print("Number of unique work categories: ", master_df['work_categories'].unique())
    print(master_df['work_categories'].value_counts())

    ## Merge with drainage period
    # Identify if drainage work took place before and after Sale Date
    master_df = drainage_work.add_drainage_period(master_df, construction_period=0,
                                                groupby_column=[adaptation_groupby_column])
    # replace construction with before
    master_df['drainage_period'] = master_df['drainage_period'].apply(lambda x: "before" if x=="construction" else x)
    print(master_df.columns)
    print("length of df: ", len(master_df))
    print("Number of unique work categories: ", master_df['work_categories'].unique())
    print(master_df['work_categories'].value_counts())

    ## Merge with drainage density
    master_df = master_df.merge(drainage_density_df,how="left",left_on=["SUBZONE_N","year"],right_on=["SUBZONE_N","year_drainage_density"])
    master_df = drainage_work.add_drainage_density(master_df,drainage_density_df,sale_date_column = "Sale_Date",noData=0,
                                                groupby_column=["SUBZONE_N"])
    print(master_df.columns)
    print("length of df: ", len(master_df))

    # Simplify Data
    # only keep relevant columns in master_df

    columns_unkeep = ['town','block',"lease_commence_date",'remaining_lease','SEARCHVAL', 'BLK_NO',
       'POSTAL', 'X', 'Y','BUILDING',
       'LATITUDE', 'LONGITUDE', 'geometry','nodesID_property','year_drainage_density',
       'drainage_length_km', 'area_km2','flooded_location','Flood_Date',"latest_flooding_hotspot_date"]
    master_df_minimal =  master_df.drop(columns = columns_unkeep)
    # rename columns for readability in R
    rename_columns = {c: re.sub(r"\s|/","_",c) for c in master_df_minimal.columns}
    master_df_minimal = master_df_minimal.rename(columns = rename_columns)
    print(master_df_minimal.columns)
    print("length of df: ", len(master_df_minimal))
    master_df_minimal.head()

    # Export Data
    master_df_minimal.to_csv(save_fp,index=False)
    print(f"Saving file into...: {save_fp}")
    return master_df_minimal.columns

if __name__ == '__main__':
    for buffer in [200,500]:
        for depth in [None,2,5]:
            for adaptation_groupby_column in ['Building Name', 'Project Name']:
                main(buffer=buffer, depth=depth, adaptation_groupby_column=adaptation_groupby_column,
         save_dir = r"Exported_Data")
