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
import copy
from scipy.stats import rankdata

# HDB resale helper functions
def get_recategorised_flat_model(text):
    """ apply column wise on flat model column. Consolidates flat model types"""
    text = text.strip()
    if re.match(r".*MAISONETTE.*|.*ADJOINED.*", text, flags=re.IGNORECASE):
        return "maisonette/adjoined"
    elif re.match(r".*GEN.*|MODEL\s+A\d+", text, flags=re.IGNORECASE):
        return 'generation/Model A'
    elif re.match(r".*PREMIUM.*|DBSS|^TYPE\s+S\d+|.*IMPROVE.*", text, flags=re.IGNORECASE):
        return 'premium/DBSS/Type S1/2'
    elif re.match(r".*TERRACE.*", text, flags=re.IGNORECASE):
        return "terrace"
    elif re.match(r".*SIMPLIFIED.*|.*2-ROOM.*", text, flags=re.IGNORECASE):
        return "simplified/2-Room"
    else:
        return "standard"
    
def get_hdb_building_age(text,lease_max_year = 99):
    """
    apply column wise on flat model column. 
    estimates building age using 99 years (default) - remaining lease
    """
    if isinstance(text, (float, int)):
        return lease_max_year - text
    else:
        try:
            text = text.strip()
            m = re.match(r"^(.*?)\syear.*",text, flags=re.IGNORECASE)
            if m:
                lease = m.group(1) # part before year
            else:
                lease = text
            return lease_max_year - int(lease.strip())
        except:
            return np.nan

# private property helper functions

def get_sale_type(text):
    """ apply column wise on Type of Sale column. Consolidates New & Sub Sale"""

    if re.search(r"New Sale|Sub Sale",text,re.IGNORECASE):
        return "New/Sub Sale"
    elif re.search(r"Resale",text,re.IGNORECASE):
        return "Resale"
    else:
        return text
    
def get_property_type(text):
    """
    apply column wise on Property Type column. 
    Consolidates condominium etc, and consolidates landed
    """
    if re.search(r"Condominium|Apartment|Executive Condominium|Non-landed Properties",text,re.IGNORECASE):
        return "Condominium/Apartment"
    else:
        return "Landed"

def get_building_name(text):
    """apply column wise on the address column. Assume everything infront the unit number is the building name"""
    match = re.match(r"^(.*?)\s*#",text)
    if match:
        return match.group(1).strip()
    else:
        return text
    
def get_project_name(row):
    """
    # TODO: Deprecated function
    for project name ==N.A., get address before unit unit number #
    """
    if (row['Project Name'] == "N.A.") or (row['Project Name'] == "NIL"):
        match = re.match(r"^(.*?)\s*#",row['Address'])
        if match:
            return match.group(1).strip()
        else:
            return row['Address']
    else:
        return row['Project Name']

def get_tenure(text):
    """ 
    # TODO: Deprecated function if using strong building or project FEs
    apply column wise on Tenure column to get freehold or leasehold in its exact years
    """
    match = re.search(r"(freehold)|(\d+\s+yrs\b)",text,re.IGNORECASE)
    if match:
        return match.group(0).strip().upper()
    else:
        return "None"
    
def get_building_age(completion_year_column, sale_year_column):
    """ column wise operation on Completion Date column
    Returns:
        pd.Series
    """
    completion_date = pd.to_numeric(completion_year_column,errors="coerce").fillna(value=0)
    return sale_year_column - completion_date

def get_floor_number(row):
    """use apply on address to get floor number"""
    # extract floor number based on Address
    # if row['Property Type'] in ['Apartment', 'Condominium', 'Executive Condominium']:
    if row['Property Type'] == 'Condominium/Apartment': # ensure that property type has been simplified!
        address = row['Address']
        result = re.search('#(.*)-',address)
        if result is None:
            return np.nan
        else:
            floor_str = result.group(1)
            # if numeric val, cast to numeric
            try:
                floor_num = int(floor_str)
            except:
                # if floor number has a letter, it will throw an exception, then keep the original str
                try:
                    # if address is in the basement, convert it the negative number to represent basement
                    floor_num = int(floor_str.replace('B','')) * -1
                except:
                    floor_num = floor_str
            return floor_num
    else:
        return 0 # zero floor to represent landed properties

def get_ground_floor(row):
    """
    # TODO: Deprecated function because Property Type has been simplified
    create a binary ground-floor variable - landed property are all ground floors, but some condos have residential units on the ground floor
    """
    
    if row['Property Type'] in ['Terrace House', 'Semi-Detached House','Detached House']:
        return True
    elif row['Floor_level'] < 2: # properties that are apartments etc
        return True
    else:
        return False 
    
def get_travel_time_to_destination(G_car,residential_df,residential_nodes_column_name,destination_df, destination_nodes_column_name,
                                   address_column_name="Address"):
    """ 
    Args:
        G_car (networkx.Graph): graph representing the car network
        residential_df (pd.DataFrame): DataFrame containing residential transaction
        residential_nodes_column_name (str): column name in residential_df that contains the nodesID
        destination_df (pd.DataFrame): dataframe containing workplace pln_area, coordinates and node_ID
        destination_nodes_column_name (str): column name in destination_df that contains the nodesID
    Returns:
        pd.DataFrame: with the shortest travel time from property location to each workplace cluster in each planning area
    """
    df_list = []
    for row_ix, row in destination_df.iterrows():
        workplace_node = row[destination_nodes_column_name]
        pln_area = row['PLN_AREA_N']
        shortest_time = nx.shortest_path_length(G_car, source=None, target=workplace_node, weight='travel_time') # keys are all nodes in G_car, values are travel time to target
        shortest_time_property_workplace = []
        # for _, row_trans in residential_df[[residential_nodes_column_name,'Address','Sale_Date']].iterrows():
        for ix in residential_df.index:
            nodes_key = residential_df.loc[ix, residential_nodes_column_name]
            address = residential_df.loc[ix, address_column_name]
            date = residential_df.loc[ix, "Sale_Date"]

            try:
                shortest_time_property_workplace.append({residential_nodes_column_name:nodes_key, f'{pln_area}_travel_time':shortest_time[nodes_key], 
                                                         address_column_name:address, 'Sale_Date':date})
            except:
                shortest_time_property_workplace.append({residential_nodes_column_name:nodes_key, f'{pln_area}_travel_time':np.nan,
                                                         address_column_name:address, 'Sale_Date':date})

        df_travel_time = pd.DataFrame(shortest_time_property_workplace)
        df_list.append(df_travel_time)

    # merge all dataframes, combining on nodesID_property
    travel_time_df = pd.concat([d.set_index([residential_nodes_column_name,address_column_name,"Sale_Date"]) for d in df_list], axis=1, join='outer').reset_index()
    # create a column that shows the planning area work cluster with the minimum travel time
    travel_time_df['min_travel_time_work_region'] = travel_time_df.iloc[:,3:].idxmin(axis=1).str.replace("_travel_time","")
    # create a column that shows the minimum travel time to the closest work cluster
    travel_time_df['min_travel_time_work'] = travel_time_df.iloc[:,3:-1].min(axis=1)

    return travel_time_df

def get_travel_time_to_school(G,residential_df,residential_nodes_column_name,destination_df, destination_nodes_column_name):
    """ 
    Args:
        G_car (networkx.Graph): graph representing the car network
        residential_df (pd.DataFrame): DataFrame containing residential transaction
        residential_nodes_column_name (str): column name in residential_df that contains the nodesID
        destination_df (pd.DataFrame): dataframe containing school location, coordinates and node_ID
        destination_nodes_column_name (str): column name in destination_df that contains the nodesID
    Returns:
        pd.DataFrame: with the shortest travel time from property location to each top primary school
    """
    df_list = []
    for row_ix, row in destination_df.iterrows():
        sch = row['School Name']
        workplace_node = row[destination_nodes_column_name]
        shortest_time = nx.shortest_path_length(G, source=None, target=workplace_node, weight='length') # keys are all nodes in G, values are travel time to target
        shortest_time_property_workplace = []
        for ix in residential_df.index:
            nodes_key = residential_df.loc[ix, residential_nodes_column_name]
            address = residential_df.loc[ix, "Address"]
            date = residential_df.loc[ix, "Sale_Date"]
            try:
                shortest_time_property_workplace.append({residential_nodes_column_name: nodes_key, f'{sch}_distance':shortest_time[nodes_key],
                                                         'Address':address, "Sale_Date":date})
            except:
                shortest_time_property_workplace.append({residential_nodes_column_name: nodes_key, f'{sch}_distance':np.nan,
                                                         'Address':address, "Sale_Date":date})

        df_travel_time = pd.DataFrame(shortest_time_property_workplace)
        df_list.append(df_travel_time)

    # merge all dataframes, combining on nodesID_property
    travel_time_df = pd.concat([d.set_index([residential_nodes_column_name,"Address","Sale_Date"]) for d in df_list], axis=1, join='outer').reset_index()
    
    oneKm_thresh = 1000 # 1km threshold
    twoKm_thresh = 2000 # 2km threshold
    # school columns
    school_columns = travel_time_df.columns[3:] # first 3 columns are property attributes
    # check if there are any values less than 1 km or 2km, then add a boolean column
        
    travel_time_df['within_1km'] = travel_time_df[school_columns].apply(lambda x: x<oneKm_thresh, axis=1).any(axis=1)
    travel_time_df['within_2km'] = travel_time_df[school_columns].apply(lambda x: x<twoKm_thresh, axis=1).any(axis=1)
    # minimum distance (closest sch) to top 50 primary schools
    travel_time_df['min_distance'] = travel_time_df[school_columns].min(axis=1)
    return travel_time_df


# travel_time_school_walk = get_travel_time_to_school(G_walk,residential_df,'nodesID_property',
#                                                      topPrimarySch, # destination are top 50 primary schools
#                                                      destination_nodes_column_name='nodesID_school_walk')
# travel_time_school_car = get_travel_time_to_school(G_car,residential_df,'nodesID_property',
#                                                      topPrimarySch, # destination are top 50 primary schools
#                                                      destination_nodes_column_name='nodesID_school_car')


def add_centrality_metrics(residential_df,standardise=True,
                           residential_nodes_column_name='nodesID_property'):
    """
    Args:
        residential_df (pd.DataFrame): DataFrame containing residential transaction
        residential_nodes_column_name (str): column name in residential_df that contains the nodesID
        standardise (bool): whether to standardise the betweeness and closeness centrality. Standardisation method using fractional rank_i/N
    Returns:
        pd.DataFrame: add additional columns that describes centrality metrics to residential_df
    """
    residential_df_copy = copy.deepcopy(residential_df)
    betweeness_centrality = utils.load_pickle(r"Data\Gcar_node_betweeness_centrality.pkl")
    closeness_centrality = utils.load_pickle(r"Data\Gcar_node_closeness_centrality.pkl")
    # print("Number of centrality nodes: ", len(list(betweeness_centrality)))

    betweeness_centrality = pd.DataFrame({residential_nodes_column_name: list(betweeness_centrality),'betweeness_centrality':list(betweeness_centrality.values())})
    closeness_centrality = pd.DataFrame({residential_nodes_column_name: list(closeness_centrality),'closeness_centrality':list(closeness_centrality.values())})
    residential_df_copy  = reduce(lambda  left,right: pd.merge(left,right),[residential_df_copy ,betweeness_centrality,closeness_centrality])
    if standardise:
        # return fractional rank
        N = len(residential_df_copy)
        residential_df_copy['betweeness_centrality'] = rankdata(residential_df_copy['betweeness_centrality'])/N
        residential_df_copy['closeness_centrality'] = rankdata(residential_df_copy['closeness_centrality'])/N
    return residential_df_copy 

def get_malls(G_walk,malls_df, radius=400, plot=True):
    """
    Args:
        G_walk (networkx.Graph): graph representing the car network
        malls_df (pd.DataFrame): df is linked to malls_df SG_malls.geojson
        radius (float): buffer radius
    Returns:
        GeoDataFrame: polygon geometry describing the servuce area of the mall
    """
    
    GSA_malls = serviceArea.GetServiceArea(G_walk, malls_df, radius=radius)
    # make copy of df
    malls_400m_polygon = malls_df.copy()
    malls_400m_polygon_geom = GSA_malls.get_serviceArea_polygons(malls_df['nodesID_walk_mall'].to_list())
    # reassign geometry from point to polygon to capture geometry of polygon
    malls_400m_polygon['geometry'] = malls_400m_polygon_geom['geometry']
    if plot:
        malls_400m_polygon.plot()

    return malls_400m_polygon

def add_malls(residential_df,mall_buffer_df,groupby_columns = ['Project Name','Address','Sale_Date']):
    """
    Args:
        residential_df (pd.DataFrame): DataFrame containing residential transaction
        mall_buffer_df (gpd.GeoDataFrame): dataframe describing the radius buffer around mall
        groupby_columns (list of str): columns that describe a unique transaction
    Returns:
        pd.Dataframe: dataframe describing additional columns that describe the number of malls within 400m of residential
    """
    residential_df_copy = copy.deepcopy(residential_df)
    mall_columns = ['Mall Name', 'nodesID_car_mall','nodesID_walk_mall', 'geometry']
    # Obtain number of properties within service area catchment
    mallsCount = mall_buffer_df[mall_columns].sjoin(residential_df_copy,how="right")
    mallsCount = mallsCount.groupby(groupby_columns).size().rename("malls within 400m").reset_index()
    # mallsCount[mallsCount['malls within 400m']>0]
    residential_df_copy = residential_df_copy.merge(mallsCount)
    return residential_df_copy

def add_parks(residential_df, park_df, radius=400, plot=True):
    """
    Args:
        residential_df (pd.DataFrame): DataFrame containing residential transaction
        park_df (gpd.GeoDataFrame): df is linked to parks_df = NParksParksandNatureReserves.geojson
        radius (float): buffer radius
    Returns:
        pd.Dataframe: dataframe describing additional columns that describe if residential is within 400m radius of a park
    """
    residential_df_copy = copy.deepcopy(residential_df)
    parks_400m_buffer = park_df.copy()
    parks_400m_buffer['geometry'] = serviceArea.add_buffer(park_df,buffer_dist=radius, crs="EPSG:4326",plot=False)
    if plot:
        parks_400m_buffer.plot(ec='k',fc="None")
    parks_buffer_union = parks_400m_buffer.union_all()
    residential_df_copy['parks within 400m'] = residential_df_copy['geometry'].apply(lambda x: parks_buffer_union.contains(x))
    return residential_df_copy