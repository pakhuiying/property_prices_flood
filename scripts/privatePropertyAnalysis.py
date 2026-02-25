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
         months_after_flood_list = [1,2,3, 6, 9, 12], flood_bins = None,
         save_dir = r"Exported_Data",
         flood_arg = True,
         flood_hotspot_arg = True,
         tide_prone_arg = False,
         flood_adaptation_arg = False,
         drainage_density_arg = False,
         primary_school_arg = False,
         workplace_cluster_arg = False,
         mall_arg = False,
         park_arg = False,
         MRT_arg = False,
         centrality_arg = False,
         DEM_arg = False
         ):
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
                        f"floodPrivateResidential_{datetime.today().strftime('%Y%m%d')}_buffer{buffer}_networkDepth{depth}_adaptation{adaptation_groupby_column.replace(' ','')}.csv")
    print(f"Save file as: {save_fp}")

    # Import Data
    ## Import planning area and subzone
    planningArea_shp = gpd.read_file(Data.PLANNING_AREA_FP)
    subzone_shp = gpd.read_file(Data.SUBZONE_FP)

    ## import network
    G_walk = ox.load_graphml(Data.G_WALK_FP)
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
    # residential_df.head()

    # import flood data
    if flood_arg:
        # historical flood
        if road_network_buffer:
            # # (polygon gdf) - buffer around downstream of flooded roads
            flood_df = Data.get_flood_network_buffer(G_car, Data.FLOOD_FP, reverse = False, 
                                        depth_limit = depth,
                                        buffer_dist=buffer)
        else:
            # # (polygon gdf) - buffer around flooded roads
            flood_df = Data.get_flood_buffer(G_car, Data.FLOOD_FP, radius=buffer)
        # flood_df.head()

    ## Import PUB-compiled flooding hotspot
    if flood_hotspot_arg:
        flooding_hotspot_buffer = Data.get_flooding_hotspot_buffer(G_car, Data.FLOODING_HOTSPOT_FP, radius=buffer)
        print("Earliest record of hotspot date:", flooding_hotspot_buffer["Flood_Hotspot_Date"].sort_values().values[0])
        flooding_hotspot_buffer.head()

    if tide_prone_arg:
        ## Import area near the coast prone to flooding during high-tide (3m)
        tide_3m_polygon = Data.get_coastal_flood_prone(Data.TIDE_3M_FP)

    if flood_adaptation_arg:
        ### Import road raising buffer
        road_raising_works_buffer_df = Data.get_road_raising_buffer_df(G_car, Data.DRAINAGE_WORKS_FP,
                                                                buffer_dist=buffer)
        print(road_raising_works_buffer_df.columns)

    ### Import downstream road raising network buffer
    if flood_adaptation_arg:
        if road_network_buffer:
            road_raising_network_buffer_df = Data.get_road_raising_network_buffer_df(G_car, Data.DRAINAGE_WORKS_FP,
                                                                                        reverse = False, 
                                                                                        depth_limit = depth,
                                                                                        buffer_dist=buffer)
            
            # road_raising_network_buffer_df.columns
        else:
            road_raising_network_buffer_df = None

    ### import drainage works
    if drainage_density_arg:
        # includes both drainage works on roadside and outlet drains + culvert improvement works
        drainage_works_df = Data.get_drain_improvement_buffer_df(G_car, Data.DRAINAGE_WORKS_FP, 
                                                                buffer_dist=buffer)
        # drainage_works_df = Data.get_drainage_works_buffer_df(Data.DRAINAGE_WORKS_FP)
        print("Type of works: ",drainage_works_df['work_categories'].unique())
        print(drainage_works_df.columns)

    if flood_adaptation_arg:
        ### Combine road raising + drainage works df
        if road_network_buffer: # ise use road net work buffer
            road_drainage_works = pd.concat([road_raising_network_buffer_df,drainage_works_df])
        else:
            road_drainage_works = pd.concat([road_raising_works_buffer_df,drainage_works_df])
        # convert to a geodataframe
        road_drainage_works = gpd.GeoDataFrame(data=road_drainage_works, geometry=road_drainage_works['geometry'])
        print("Unique type of adaptation works: ", road_drainage_works["work_categories"].unique())

    ## Import drainage density
    if drainage_density_arg:
        drainage_density_df = Data.get_drainage_density_df(Data.DRAINAGE_DENSITY_FP)

    ## Import residential attributes

    ### Distance to top primary schools
    if primary_school_arg:
        travel_time_school_car = Data.get_travel_time_to_schools(Data.PRI_SCH_CAR_TRAVEL_FP,suffix="car")
        travel_time_school_walk = Data.get_travel_time_to_schools(Data.PRI_SCH_WALK_TRAVEL_FP,suffix="walk")
        # only select within 1/2km and min distance
        travel_time_school_car = travel_time_school_car[["Address","Sale_Date"]+travel_time_school_car.columns[-3:].to_list()]
        travel_time_school_walk = travel_time_school_walk[["Address","Sale_Date"]+travel_time_school_walk.columns[-3:].to_list()]
        # travel_time_school_walk.head()

    ### Workplace cluster
    if workplace_cluster_arg:
        workplace_cluster_df = Data.get_workplace_cluster(G_car,Data.WORKPLACE_CLUSTER_FP)
        travel_time_work_cluster = residential_attributes.get_travel_time_to_destination(G_car,residential_df,residential_nodes_column_name='nodesID_property',
                                                        destination_df=workplace_cluster_df[workplace_cluster_df['PLN_AREA_N'].isin(['DOWNTOWN CORE','TAMPINES','JURONG EAST','ANG MO KIO', 'WOODLANDS'])],
                                                        destination_nodes_column_name="node_ID")
    
    ### Malls
    if mall_arg:
        malls_df = Data.get_mall_df(Data.MALLS_FP)

    ### Parks
    if park_arg:
        parks_df = Data.get_park_df(Data.PARKS_FP)

    ## Import MRT and buffer
    if MRT_arg:
        mrt_buffer_df = Data.get_MRT_buffer_df(Data.MRT_FP, buffer_dist=buffer)
        # mrt_buffer_df.head()

    # Merge all datasets
    master_df = copy.deepcopy(residential_df)
    print(master_df.columns)
    print("length of df: ", len(master_df))

    # Merge with travel time to top 5 workplace cluster
    if workplace_cluster_arg:
        master_df = master_df.merge(travel_time_work_cluster)
        print(master_df.columns)

    # Merge with travel time to top primary schools
    if primary_school_arg:
        master_df = reduce(lambda  left,right: pd.merge(left,right,how="left",on=["Address","Sale_Date"]),[master_df,travel_time_school_car,travel_time_school_walk])
        print(master_df.columns)
        print("length of df: ", len(master_df))

    ## Merge with centrality metrics
    if centrality_arg:
        master_df = residential_attributes.add_centrality_metrics(master_df,residential_nodes_column_name='nodesID_property',
                                                                standardise=True)
        print(master_df.columns)
        print("length of df: ", len(master_df))

    ## Merge with malls
    if mall_arg:
        mall_buffer_df = residential_attributes.get_malls(G_walk,malls_df,radius=buffer,plot=False)
        master_df = residential_attributes.add_malls(master_df,mall_buffer_df,groupby_columns = ['Project Name','Address','Sale_Date'])
        print(master_df.columns)
        print("length of df: ", len(master_df))

    ## Merge with Parks
    if park_arg:
        master_df = residential_attributes.add_parks(master_df, parks_df, radius=buffer, plot=False)
        print(master_df.columns)
        print("length of df: ", len(master_df))

    ## Merge with MRT
    if MRT_arg:
        master_df = mrt_buffer.add_mrt_buffer(master_df,mrt_buffer_df,
                                    address_column = "Address", sale_date_column = "Sale_Date",
                                        construction_period = 6)
        print(master_df.columns)
        print("length of df: ", len(master_df))

    ## Merge with DEM
    if DEM_arg:
        master_df["DEM"] = master_df.apply(lambda x: DEM.get_DEM_value(x["LONGITUDE"],x["LATITUDE"]),axis=1)
        print(master_df.columns)
        print("length of df: ", len(master_df))

    ## Merge with flood event

    # merging of master df with flood df by location intersection ONLY
    if flood_arg:
        master_df = Flood.add_historical_flooding(master_df, 
                                            flood_df[["flooded_location","Flood_Date","geometry"]],
                                            sale_date_column="Sale_Date",
                                            drop_duplicate_column = ["Project Name","Address","Sale_Date"])
        print("Number of flooded entries: ",len(master_df[~master_df['flooded_location'].isna()]))
        print("Length of df: ",len(master_df))

        # add boolean flags on whether obs has recent flood occurrences
        for months_after_flood in months_after_flood_list:
            master_df = Flood.add_flood_within_months(master_df,sale_date_column="Sale_Date",
                                                    months_after_flood=months_after_flood,
                                                    groupby_column=adaptation_groupby_column,
                                                    drop_duplicate_column = ["Project Name","Address","Sale_Date"])
            
            print(f"Number of flood within months: {master_df[f'within_{months_after_flood}_months_post_flood'].sum()}")

        master_df = Flood.add_time_since_flood(master_df,sale_date_column="Sale_Date",
                                            groupby_column=adaptation_groupby_column,
                                            drop_duplicate_column = ["Project Name","Address","Sale_Date"], 
                                            fillna=np.nan,
                                            units="months", bins=flood_bins)
        
        # add column to label treated properties i.e. within flood_buffer of radius (buffer)
        flood_union = flood_df['geometry'].union_all()
        master_df['flood_buffer'] = master_df['geometry'].apply(lambda x: flood_union.intersects(x))
        print(f"Number of properties within {buffer} m buffer: {master_df['flood_buffer'].sum()}")
        print(f"Number of transactions with flood: {len(master_df[~master_df['Flood_Date'].isna()])}")
        print(master_df.columns)
        print("length of df: ", len(master_df))
    # - Use 6/12/18 months to flag transaction records that has floods occurring within 6/12/18 months
    # - Include continuous weeks-since-flood as robustness checks (*Note: this is added after checking whether location is in a flood prone area as a check to see if the area has experienced repeated flooding. Reduce noise in dataset by only examining areas with repeated flooding)

    # master_df = master_df.merge(flood_df[["flooded_location","Flood_Date","SUBZONE_N"]],how="left",
    #                     left_on=["SUBZONE_N",'Sale_Date'], 
    #                     right_on=["SUBZONE_N",'Flood_Date'],suffixes=('_property','_flood'))
    # master_df = master_df.drop_duplicates(subset=['Project Name','Address','Sale_Date'])
    # master_df = Flood.add_flood_within_months(master_df,sale_date_column="Sale_Date",months_after_flood=6,
    #                                         groupby_column=["SUBZONE_N"])
    # master_df = Flood.add_flood_within_months(master_df,sale_date_column="Sale_Date",months_after_flood=12,
    #                                         groupby_column=["SUBZONE_N"])
    # master_df = Flood.add_flood_within_months(master_df,sale_date_column="Sale_Date",months_after_flood=18,
    #                                         groupby_column=["SUBZONE_N"])
    # master_df = Flood.add_weeks_since_flood(master_df,sale_date_column="Sale_Date", 
    #                                         groupby_column=["SUBZONE_N"],fillna=np.nan)
    # print(master_df.columns)
    # print("length of df: ", len(master_df))

    ## Merge with PUB-compiled flooding hotspot
    # - Identifies if an area is within a flood prone area for that transaction year
    #     - if publication date of the flooding hotspot is after the transaction date, it means the location is in a flood prone area
    # - Obtain the latest flooding hotspot date
    # - Directly examine whether flood occurs in a flood-prone area or not, and if flood recency has an impact on property price for area that is flood-prone and for area that is not flood-prone
    if flood_hotspot_arg:
        master_df = Flood.add_within_flooding_hotspot_buffer(master_df, 
                                                flooding_hotspot_buffer[['Flood_Hotspot_Date', 'flooded_locations','geometry']],
                                            groupby_column = [adaptation_groupby_column], sale_date_column="Sale_Date",
                                            drop_duplicate_column = ["Project Name","Address","Sale_Date"])
        print(f"Number of obs within flooding hotspot: {master_df['within_flooding_hotspot'].sum()}")
        print(master_df.columns)
        print("length of df: ", len(master_df))

    # master_df = Flood.add_within_flooding_hotspot_buffer(master_df,flooding_hotspot_buffer,
    #                                                     #  groupby_column = ["Project Name"], 
    #                                                     groupby_column=[adaptation_groupby_column],
    #                                                     sale_date_column="Sale_Date")
    # print(master_df.columns)
    # print("length of df: ", len(master_df))

    ## Merge with coastal flood prine area
    if tide_prone_arg:
        master_df["prone_to_high_tide"] = master_df['geometry'].apply(lambda x: tide_3m_polygon.intersects(x))
        print(master_df.columns)
        print("length of df: ", len(master_df))

    ## Merge with drainage work type
    if flood_adaptation_arg:
        master_df = master_df.sjoin(road_drainage_works[["work_categories","year","month","ROAD_NAME","geometry"]],how="left",
                                                                rsuffix="drainage",lsuffix="property",on_attribute=["year","month"])
        print("length of df: ", len(master_df))
        print(master_df['work_categories'].value_counts())
        # remove duplicated index - this method corresponds with merging the aggregated data ie. HDB resale method
        master_df = master_df[~master_df.index.duplicated(keep="first")]
        master_df = master_df.drop(columns=['index_drainage'])
        print("length of df: ", len(master_df))
        print(master_df['work_categories'].value_counts())

        ## Merge with drainage period
        # Identify if drainage work took place before and after Sale Date
        
        master_df = drainage_work.add_road_drainage_works(master_df, 
                                    road_drainage_works[["work_categories","ROAD_NAME","geometry","Drainage_Date"]],
                                    sale_date_column="Sale_Date",
                                    groupby_column = [adaptation_groupby_column],
                                    drop_duplicate_column = ["Project Name","Address","Sale_Date"])
        print(master_df['work_categories'].value_counts())
        print(master_df['drainage_period'].value_counts())
        print(master_df.columns)
        print("length of df: ", len(master_df))
        print("Number of unique work categories: ", master_df['work_categories'].unique())
        print(master_df['work_categories'].value_counts())
    # master_df = drainage_work.add_drainage_period(master_df, construction_period=0,
    #                                             groupby_column=[adaptation_groupby_column])
    # # replace construction with before
    # master_df['drainage_period'] = master_df['drainage_period'].apply(lambda x: "before" if x=="construction" else x)
    # print(master_df.columns)
    # print("length of df: ", len(master_df))
    # print("Number of unique work categories: ", master_df['work_categories'].unique())
    # print(master_df['work_categories'].value_counts())

    ## Merge with drainage density
    if drainage_density_arg:
        master_df = master_df.merge(drainage_density_df,how="left",left_on=["SUBZONE_N","year"],right_on=["SUBZONE_N","year_drainage_density"])
        master_df = drainage_work.add_drainage_density(master_df,drainage_density_df,sale_date_column = "Sale_Date",noData=0,
                                                    groupby_column=["SUBZONE_N"])
        print(master_df.columns)
        print("length of df: ", len(master_df))

    # Simplify Data
    # only keep relevant columns in master_df

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
    master_df_minimal.to_csv(save_fp,index=False)
    print(f"Saving file into...: {save_fp}")
    return master_df_minimal.columns

if __name__ == '__main__':
    buffer = 500
    depth = None #5
    adaptation_groupby_column = 'Project Name'#'Building Name'
    months_after_flood_list = np.arange(1,13,dtype=int).tolist()
    flood_bins = None
    save_dir = r"Exported_Data\flood_buffer_dist"
    # boolean flag to indicate which processing should be conducted
    flood_arg = True
    flood_hotspot_arg = True
    tide_prone_arg = False
    flood_adaptation_arg = False
    drainage_density_arg = False
    primary_school_arg = False
    workplace_cluster_arg = False
    mall_arg = False
    park_arg = False
    MRT_arg = False
    centrality_arg = False
    DEM_arg = False

    for buffer in np.arange(50,1050,step=50,dtype=int):
        main(buffer=buffer, depth=depth, adaptation_groupby_column=adaptation_groupby_column,
            months_after_flood_list = months_after_flood_list, flood_bins = flood_bins,
            save_dir = save_dir,
            flood_arg = flood_arg,
            flood_hotspot_arg = flood_hotspot_arg,
            tide_prone_arg = tide_prone_arg,
            flood_adaptation_arg = flood_adaptation_arg,
            drainage_density_arg = drainage_density_arg,
            primary_school_arg = primary_school_arg,
            workplace_cluster_arg = workplace_cluster_arg,
            mall_arg = mall_arg,
            park_arg = park_arg,
            MRT_arg = MRT_arg,
            centrality_arg = centrality_arg,
            DEM_arg = DEM_arg
            )
    
    # for buffer in [200,500]:
    #     for depth in [None,2,5]:
    #         for adaptation_groupby_column in ['Building Name', 'Project Name']:
    #             main(buffer=buffer, depth=depth, adaptation_groupby_column=adaptation_groupby_column,
    #      save_dir = r"Exported_Data")
