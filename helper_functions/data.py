import re
import numpy as np
import pandas as pd
import osmnx as ox
import networkx as nx
from helper_functions import utils
from functools import reduce
import os
import geopandas as gpd
import helper_functions.serviceArea as serviceArea
import helper_functions.road_raising_works as RoadRaisingWorks
import helper_functions.drainage_work as DrainageWork
import copy

# filepath variables

# planning/subzone maps
PLANNING_AREA_FP = r"C:\Users\hypak\OneDrive - Singapore Management University\Documents\Data\SG_Map\planningArea.shp"
SUBZONE_FP = r"C:\Users\hypak\OneDrive - Singapore Management University\Documents\Data\SG_Map\subzone.shp"

# road network
G_CAR_FP = os.path.join(os.getcwd(),"Data","Road_Networks","SG_car_network.graphml")
G_WALK_FP = os.path.join(os.getcwd(),"Data","Road_Networks","SG_walk_network.graphml")

# flood related data
# DRAINAGE_WORKS_FP = r"Exported_Data\drainage_works_buffer200m.shp"
# FLOOD_FP = r"Data\precipitation_levels_during_flood_events.csv"
DRAINAGE_WORKS_FP = r"Exported_Data\drainage_works_roads_2012_2024.csv"
FLOOD_FP = r"Data\flood_events_2013_2025.csv"
FLOODING_HOTSPOT_FP = r"Data\flooding_hotspots_2011_2024.shp"
DRAINAGE_DENSITY_FP = os.path.join(r"Data","drainage_density_subzone.csv")
TIDE_3M_FP = r"Data\water_depth_3m.shp"

# residential/work attributes data
PRIVATE_RESIDENTIAL_FP = r"Exported_Data\Residential_transaction_2014_2024.csv"
WORKPLACE_CLUSTER_FP = r"Data\workplace_cluster_planningArea.shp"
TOP_PRIMARY_SCHOOLS_FP = os.path.join(os.getcwd(),"Data","topPrimarySchools.csv")
PRI_SCH_CAR_TRAVEL_FP = r"Data\topPrimarySchools_travel_time_car.csv"
PRI_SCH_WALK_TRAVEL_FP = r"Data\topPrimarySchools_travel_time_walk.csv"
AMENITIES_FP = os.path.join(os.getcwd(),"Data",'SG_amenities.geojson')
MALLS_FP = os.path.join(os.getcwd(),"Data",'SG_malls.geojson')
PARKS_FP = r"C:\Users\hypak\OneDrive - Singapore Management University\Documents - Heat Risk Index Development\Data\Parks\NParksParksandNatureReserves.geojson"
MRT_FP = r"Exported_Data\MRT_opening\MRT_stations_opening.csv"

def get_private_residential_df(fp):
    residential_df = pd.read_csv(fp)
    # columns to change to numeric
    numeric_columns = [c for c in residential_df.columns if bool(re.search(".*Price.*|^Area.*",c))]
    residential_df[numeric_columns] = residential_df[numeric_columns].apply(lambda x: pd.to_numeric(x.str.replace(',', ''),errors="coerce"),axis=0)
    # change date to datetime format
    residential_df['Sale Date'] = residential_df['Sale Date'].apply(lambda x: pd.to_datetime(x, format='%d %b %Y', errors='coerce')) 
    # get year and month columns
    residential_df["year"] = residential_df['Sale Date'].dt.year.astype(int)
    residential_df["month"] = residential_df['Sale Date'].dt.month.astype(int)
    # rename columns
    rename_columns = {"Planning Region": "REGION_N", "Planning Area":"PLN_AREA_N","Sale Date":"Sale_Date"}
    residential_df = residential_df.rename(columns=rename_columns)
    # strip leading and trailing white spaces from PLN_AREA_N because Changi has trailing white spaces
    residential_df['PLN_AREA_N'] = residential_df['PLN_AREA_N'].str.strip()
    # drop rows with missing longitude and latitude because they are either land or enbloc properties - removal of 494 rows
    residential_df = residential_df.dropna(subset=['LONGITUDE','LATITUDE'])
    # convert cordinates to point geometry
    residential_df = gpd.GeoDataFrame(residential_df, 
                                                geometry=gpd.points_from_xy(residential_df['LONGITUDE'], residential_df['LATITUDE']), 
                                                crs="EPSG:4326")
    return residential_df

# def get_flood_df(fp):
#     flood_df = pd.read_csv(fp)
#     # cast as datetime
#     flood_df['time'] = flood_df['time'].apply(lambda x: pd.to_datetime(x, format='%Y-%m-%d', errors='coerce'))
#     flood_df = gpd.GeoDataFrame(flood_df, geometry=gpd.points_from_xy(flood_df.longitude, flood_df.latitude),crs="EPSG:4326")
#     # rename columns
#     rename_columns = {"time":"Flood_Date","latitude":"LATITUDE","longitude":"LONGITUDE"}
#     flood_df = flood_df.rename(columns=rename_columns)

#     return flood_df

def get_flood_df(fp):
    flood_df = pd.read_csv(fp)
    columns_keep = ['Date','SEARCHVAL', 'BLK_NO', 'ROAD_NAME', 'BUILDING', 'ADDRESS', 'POSTAL',
       'X', 'Y', 'LATITUDE', 'LONGITUDE', 'flooded_location']
    flood_df = flood_df[columns_keep]
    # cast as datetime
    flood_df['Date'] = flood_df['Date'].apply(lambda x: pd.to_datetime(x, format='%Y-%m-%d', errors='coerce'))
    flood_df = gpd.GeoDataFrame(flood_df, geometry=gpd.points_from_xy(flood_df.LONGITUDE, flood_df.LATITUDE),crs="EPSG:4326")
    # rename columns
    rename_columns = {"Date":"Flood_Date"}
    flood_df = flood_df.rename(columns=rename_columns)

    return flood_df

def get_coastal_flood_prone(fp):
    """returns a multipolygon"""
    tide_3m = gpd.read_file(fp)
    return tide_3m.union_all()

def get_flooding_hotspot(fp):
    flooding_hotspot = gpd.read_file(fp)
    # rename columns
    rename_columns = {"year":"Flood_Hotspot_Date","flooded_lo":"flooded_locations","latitude":"LATITUDE","longitude":"LONGITUDE"}
    flooding_hotspot = flooding_hotspot.rename(columns=rename_columns)
    # convert to datetime
    flooding_hotspot["Flood_Hotspot_Date"] = pd.to_datetime(flooding_hotspot["Flood_Hotspot_Date"],format="%b-%y",errors="coerce")
    flooding_hotspot['year'] = flooding_hotspot["Flood_Hotspot_Date"].dt.year
    flooding_hotspot['month'] = flooding_hotspot["Flood_Hotspot_Date"].dt.month
    return flooding_hotspot

def get_flooding_hotspot_buffer(fp,radius=200):
    flooding_hotspot = get_flooding_hotspot(fp)
    flooding_hotspot_buffer = flooding_hotspot.copy()
    flooding_hotspot_buffer['geometry'] = serviceArea.add_buffer(flooding_hotspot,buffer_dist=radius, crs="EPSG:4326",plot=False)
    return flooding_hotspot_buffer

def get_drainage_works_df(fp):
    """
    Args:
        fp (str): filepath to csv of drainage works between 2012 to 2024
    """
    completed_drainage_works = pd.read_csv(fp)
    # drop rows with NAs in coordinates
    completed_drainage_works = completed_drainage_works.dropna(subset=["LATITUDE","LONGITUDE"])
    completed_drainage_works = completed_drainage_works.rename(columns={"Year":"year","Month":"month","Date":"Drainage_Date"})
    return completed_drainage_works

def get_road_raising_buffer_df(G, fp, buffer_dist=400):
    """
    Args:
        G (networkx.Graph): graph representing the car network
        fp (str): filepath to csv of drainage works between 2012 to 2024
        reverse (bool): If True traverse a directed graph in the reverse direction
        depth_limit (float): Specify the maximum search depth
        buffer_dist (None or float): if None, return the non-buffered downstream roads, else return the buffered downstream roads
    Returns:
        gpd.GeoDataFrame: buffer around downstream roads
    """
    df = get_drainage_works_df(fp)
    road_raising_df = RoadRaisingWorks.get_road_raising_works(G, df)
    # rename and simplify work category
    road_raising_df['work_categories'] = "Road Raising"
    
    if buffer_dist is not None:
        # add buffer
        road_raising_works_buffer_df = copy.deepcopy(road_raising_df)
        road_raising_works_buffer = serviceArea.add_buffer(road_raising_df,buffer_dist=buffer_dist,plot=False)
        road_raising_works_buffer_df.loc[road_raising_works_buffer.index,"geometry"] = road_raising_works_buffer
        return road_raising_works_buffer_df
    else:
        return road_raising_df
    
def get_road_raising_network_buffer_df(G, fp, reverse = False, depth_limit = 2, 
                                     buffer_dist=400):
    """
    Args:
        G (networkx.Graph): graph representing the car network
        fp (str): filepath to csv of drainage works between 2012 to 2024
        reverse (bool): If True traverse a directed graph in the reverse direction
        depth_limit (float): Specify the maximum search depth
        buffer_dist (None or float): if None, return the non-buffered downstream roads, else return the buffered downstream roads
    Returns:
        gpd.GeoDataFrame: buffer around downstream roads
    """
    df = get_drainage_works_df(fp)
    road_raising_df = RoadRaisingWorks.get_road_raising_works(G, df)
    # rename and simplify work category
    road_raising_df['work_categories'] = "Road Raising"
    # get downstream edges from road raising edges
    road_raising_works_df = RoadRaisingWorks.get_road_raising_works_df(G, road_raising_df,
                                reverse = reverse, depth_limit = depth_limit,
                                plot = False)
    if buffer_dist is not None:
        # add buffer
        road_raising_works_buffer_df = copy.deepcopy(road_raising_works_df)
        road_raising_works_buffer = serviceArea.add_buffer(road_raising_works_df,buffer_dist=buffer_dist,plot=False)
        road_raising_works_buffer_df.loc[road_raising_works_buffer.index,"geometry"] = road_raising_works_buffer
        return road_raising_works_buffer_df
    else:
        return road_raising_works_df
    
def get_drain_improvement_buffer_df(G, fp, buffer_dist=400):
    """
    Args:
        G (networkx.Graph): graph representing the car network
        fp (str): filepath to csv of drainage works between 2012 to 2024
        buffer_dist (None or float): if None, return the non-buffered downstream roads, else return the buffered downstream roads
    Returns:
        gpd.GeoDataFrame: buffer around roadside/outlet drains/culverts
    """
    df = get_drainage_works_df(fp)
    # filter to get culvert, improvements to roadside/outlet drains
    drains_df = df[df["work_categories"].apply(lambda x: bool(re.match(".*Outlet Drain.*|.*Roadside Drain.*",x, flags=re.IGNORECASE)))]
    culvert_df = df[df["work_categories"].apply(lambda x: bool(re.match(".*Culvert.*",x, flags=re.IGNORECASE)))]
    # rename and simplify work category
    drains_df['work_categories'] = "Improvement to Roadside/Outlet Drains"
    culvert_df['work_categories'] = "Improvement to Culvert"
    # get the gdfs of drains_df and culvert_df
    drains_df = DrainageWork.get_drainage_works_gdf(G, drains_df)
    culvert_df = DrainageWork.get_drainage_works_gdf(G, culvert_df)
    df = pd.concat([drains_df,culvert_df],axis=0) # concat vertically
    # reset index because there are duplicate index integers because calling DrainageWork.get_drainage_works_df resets the index on both dfs
    df = df.reset_index(drop=True)
    # add buffer
    drainage_works_buffer_df = copy.deepcopy(df)
    drainage_works_buffer = serviceArea.add_buffer(df,buffer_dist=buffer_dist,plot=False)
    drainage_works_buffer_df.loc[drainage_works_buffer.index,"geometry"] = drainage_works_buffer
    return drainage_works_buffer_df

# def get_drainage_works_buffer_df(G, fp, reverse = False, depth_limit = 2, 
#                                      buffer_dist=400):
#     """
#     Args:
#         G (networkx.Graph): graph representing the car network
#         fp (str): filepath to csv of drainage works between 2012 to 2024
#         reverse (bool): If True traverse a directed graph in the reverse direction
#         depth_limit (float): Specify the maximum search depth
#         buffer_dist (None or float): if None, return the non-buffered downstream roads, else return the buffered downstream roads
#     Returns:
#         gpd.GeoDataFrame: buffer around downstream roads
#     """
#     road_raising_df = get_road_raising_works_buffer_df(G, fp, reverse = reverse, depth_limit = depth_limit, 
#                                      buffer_dist=buffer_dist)
#     drain_improvement_df = get_drain_improvement_buffer_df(fp, buffer_dist=buffer_dist)
#     drainage_works_buffer_df = pd.concat([road_raising_df, drain_improvement_df],axis=0)
#     return drainage_works_buffer_df

# def get_drainage_works_buffer_df(fp):
#     """
#     Args:
#         fp (str): filepath to shp file to the buffer around drainage works
#     """
#     completed_drainage_works = gpd.read_file(fp)
#     completed_drainage_works = completed_drainage_works.rename(columns={"Year":"year","Month":"month","Date":"Drainage_Date"})
#     return completed_drainage_works

def get_drainage_density_df(fp):
    drainage_den_df = pd.read_csv(fp)
    return drainage_den_df

def get_MRT_buffer_df(fp):
    trainStations = pd.read_csv(fp)
    # remove stns that are not open yet
    trainStations = trainStations[~trainStations["mrt_line"].isin(["JE","JS","JW","CR"])]
    # convert cordinates to point geometry
    trainStations_400m = gpd.GeoDataFrame(trainStations,
                                    geometry=gpd.points_from_xy(trainStations['LONGITUDE'], trainStations['LATITUDE']), 
                                                crs="EPSG:4326")
    # calculate buffer for train station using euclidean distance
    trainStations_400m_buffer = serviceArea.add_buffer(trainStations_400m,buffer_dist=400,plot=False)
    trainStations_400m.loc[trainStations_400m_buffer.index,"geometry"] = trainStations_400m_buffer
    trainStations_400m["opening_date"] = pd.to_datetime(trainStations_400m["opening_date"])
    return trainStations_400m

def get_workplace_cluster(G_car,fp):
    """
    Args:
        G_car (networkx.Graph): graph representing the car network
        fp (str): filepath to shp file to workplace cluster in each planning area
    """
    workplace_cluster = gpd.read_file(fp)
    # replace latitude and longitude coordinates for the row that corresponds to the PLN_AREA_N "TENGAH"
    workplace_cluster.loc[workplace_cluster['PLN_AREA_N'] == 'TENGAH', ['latitude','longitude']] = [1.357293, 103.733877]
    workplace_cluster.loc[workplace_cluster['PLN_AREA_N'] == 'TENGAH', 'node_ID'] = ox.distance.nearest_nodes(G_car,X = 103.733877, Y = 1.357293)
    # print(workplace_cluster[workplace_cluster['PLN_AREA_N'] == 'TENGAH'])
    # planningArea_shp = gpd.read_file(r"C:\Users\hypak\OneDrive - Singapore Management University\Documents\Data\SG_Masterplan\MasterPlan2019PlanningAreaBoundaryNoSea.geojson")
    workplace_cluster = workplace_cluster.sort_values(['REGION_N','PLN_AREA_N'])
    # print("Length of df: ",len(workplace_cluster.index))
    # print("Number of unique regions: ",len(workplace_cluster['PLN_AREA_C'].unique()))

    # sembawang has index 36 1.456735,103.809257, node_ID = 6673057834
    # update sembawang's coordinates and node_ID so that route can be found for car routing. Bus routing is not required as we are using OneMap API
    workplace_cluster.loc[36,["latitude","longitude"]] = [1.450397, 103.802968]
    workplace_cluster.loc[36,"node_ID"] = ox.nearest_nodes(G_car,X=workplace_cluster.loc[36,"longitude"],Y=workplace_cluster.loc[36,"latitude"])
    # rename columns
    rename_columns = {"latitude":"LATITUDE","longitude":"LONGITUDE"}
    workplace_cluster = workplace_cluster.rename(columns=rename_columns)
    return workplace_cluster

def get_mall_df(fp):
    malls_df = gpd.read_file(fp)
    malls_df['nodesID_walk_mall'] = pd.to_numeric(malls_df['nodesID_walk_mall'], errors='coerce')
    # print("Length of malls df: ", len(malls_df))
    # print(malls_df.columns)
    return malls_df

def get_park_df(fp):
    """
    Args:
        fp (str): filepath to shp file to workplace cluster in each planning area
    """
    return gpd.read_file(fp)

def get_top_schools(fp,top=50):
    """
    Args:
        fp (str): filepath to csv file describing list of top pri school and their locations
        top (int): select top e.g. 30 or 50 of primary school
    """
    topPrimarySch = pd.read_csv(fp)
    # select top 50 primary schools
    topPrimarySch = topPrimarySch.head(top)
    return topPrimarySch

def get_travel_time_to_schools(fp,suffix):
    """
    Args:
        fp (str): filepath to csv file describing travel time to each school and if school is within 1/2km to residential unit
        suffix (str): for renaming columns to differentiate walk vs car
    """
    travel_time_to_sch = pd.read_csv(fp)
    rename_columns = ["min_distance","within_1km","within_2km"]
    rename_columns = {c: f"sch_{c}_{suffix}" for c in rename_columns}
    travel_time_to_sch['Sale_Date'] = pd.to_datetime(travel_time_to_sch['Sale_Date'],errors="coerce")
    travel_time_to_sch = travel_time_to_sch.rename(columns=rename_columns)
    return travel_time_to_sch

if __name__ == "__main__":
    print(os.getcwd())