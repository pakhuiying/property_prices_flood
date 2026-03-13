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

def get_past_flood(sale_date, flood_date_list):
    """
    Args:
        sale_date (datetime): dt corresponding to property sale date
        flood_date_list (list of datetime): list of flood dates that intersect with the property's location
    Returns:
        pd.Series: latest_flood_date (most recent flood date that occurred before sale date), 
        past_flood_count (number of past floods that occurred before sale date), 
        total_flood_count (total number of floods that occurred in the same location as property, past and future floods)
    """
    save_dict = dict()
    flood_date_list = pd.Series(flood_date_list).sort_values() # sort from oldest to most recent dates
    if flood_date_list.isna().all(): # if flood date list is all NA
        return pd.Series(np.nan, index=['latest_flood_date','past_flood_count','total_flood_count'])
    else:
        # get mask to identify which sale dates occur AFTER flood
        post_flood = sale_date > flood_date_list
        if post_flood.any():
            # get the flood date that occurred closest to the sale date
            latest_flood_date = flood_date_list[post_flood].values[-1]
            save_dict['latest_flood_date'] = latest_flood_date
            # get number of floods that occurred before sale date
            save_dict['past_flood_count'] = post_flood.sum()
        else:
            save_dict['latest_flood_date'] = np.nan
            save_dict['past_flood_count'] = np.nan
        # get number of floods (past and future)
        save_dict['total_flood_count'] = len(flood_date_list)
        return pd.Series(save_dict, index=list(save_dict))
    
def get_past_adaptation(sale_date, adaptation_date_list, work_categories_list):
    """
    Args:
        sale_date (datetime): dt corresponding to property sale date
        adaptation_date_list (list of datetime): list of adaptation dates that intersect with the property's location
        work_categories_list (list of work categories): list of adaptation works that has the same length as adaptation_date_list
    Returns:
        pd.Series: past_work_categories (concatenated list of adaptation works that have occurred BEFORE the sale date),
        future_work_categories (concatenated list of adaptation works that have occurred AFTER the sale date),
    """
    save_dict = {'past_work_categories':"none", 
                 'future_work_categories':"none"}
    if adaptation_date_list != [pd.NaT]: # if adaptation date list is all NA
        # if unit sale occurred AFTER drainage work
        adapt_list = [adapt for adapt_date, adapt in zip(adaptation_date_list, work_categories_list) if sale_date > adapt_date]
        if len(adapt_list) > 0:
            save_dict['past_work_categories'] = ','.join(adapt_list)
        # if unit sale occurred BEFORE drainage work
        adapt_list = [adapt for adapt_date, adapt in zip(adaptation_date_list, work_categories_list) if sale_date <= adapt_date]
        if len(adapt_list) > 0:
            save_dict['future_work_categories'] = ','.join(adapt_list)
    return pd.Series(save_dict, index=list(save_dict))
    
def get_event_within_months(sale_date, event_date_list, months_after_event=6):
    """whether the sale occurred 6 months pre or post event
    Args:
        sale_date (datetime): dt corresponding to property sale date
        event_date_list (list of datetime): list of event dates that intersect with the property's location
        months_after_event (int): if < 0, locate event occurrence AFTER sale date, else, locate event occurrence BEFORE sale date
    Returns:
        pd.Series: whether the sale occurred 6 months pre or post flood
    """
    event_date_list = pd.Series(event_date_list).sort_values() # sort from oldest to most recent dates
    
    if event_date_list.isna().all(): # if event date list is all NA
        return False
    else:
        event_date_months_later = event_date_list + pd.DateOffset(months=months_after_event)
        if months_after_event > 0: # post event check
            mask = (sale_date > event_date_list) & (sale_date <= event_date_months_later)
        else: # pre-event check
            mask = (sale_date <= event_date_list) & (sale_date >= event_date_months_later)

        return mask.any()
    
def get_hotspot_designation_date(sale_date, event_date_list):
    """check if transaction sale is before hotspot designation, if True, means the place is already a hotspot
    Args:
        sale_date (datetime): dt corresponding to property sale date
        event_date_list (list of datetime): list of flooding hotspot publication dates that intersect with the property's location
    Returns:
        pd.Series: pre_hotspot_designation_date (sale date BEFORE hotspot designation. Returns the hotspot designation after the sale date),
        post_hotspot_designation_date (sale date AFTER hotspot designation. Returns the hotspot designation date just before the sale date)
    """
    event_date_list = pd.Series(event_date_list).sort_values() # sort from oldest to most recent dates
    save_dict = dict()
    if event_date_list.isna().all(): # if event date list is all NA
        save_dict['pre_hotspot_designation_date'] = pd.NaT
        save_dict['post_hotspot_designation_date'] = pd.NaT
    else:
        # PRE FLOOD HOTSPOT DESIGNATION
        hotspot_designation_mask = sale_date <= event_date_list #sale date that occurs before flood hotspot designation date
        if hotspot_designation_mask.any(): # there are sale dates that occur before flood hotspot designation date
            # get earliest (oldest) flood hotspot designation date that occurred just after sale
            # could imply that this area has already experienced flooding
            save_dict['pre_hotspot_designation_date'] = event_date_list[hotspot_designation_mask].values[0] # get the flood hotspot designation date closest to transaction date
        else:
            save_dict['pre_hotspot_designation_date'] = pd.NaT
        # POST FLOOD HOTSPOT DESIGNATION
        hotspot_designation_mask = sale_date > event_date_list # sale date after flood hotspot designation date
        if hotspot_designation_mask.any():
            # get most recent hotspot designation date
            # post flood hotspot designation may have an effect on the price or people's perception
            save_dict['post_hotspot_designation_date'] = event_date_list[hotspot_designation_mask].values[-1]
        else:
            save_dict['post_hotspot_designation_date'] = pd.NaT
    return pd.Series(save_dict, index=list(save_dict))

def add_event_within_months(df,sale_date_column="Sale_Date", event_date_column="Flood_Date", months_after_event=6):
    """
    Assign True/False if transaction occurs within 6 months of event for each groupby_column
    Args:
        df (pd.DataFrame): dataframe after merging event_df to residential df
        sale_date_column (str): name of sale date column
        event_date_column (str): name of event date column e.g. flood or adaptation
        months_after_event (int): list of months to identify if event occurs within timeframe
    Returns:
        pd.DataFrame with added columns describing True/False if transaction occurs within 6 months of event
    """
    df_copy = copy.deepcopy(df)
    post_event_column = df_copy.apply(lambda x: get_event_within_months(x[sale_date_column],x[event_date_column],
                                                                        months_after_event=months_after_event),axis=1)
    
    df_copy[f"within_{months_after_event}_months_post_{event_date_column}"] = post_event_column

    return df_copy

def add_historical_flooding(df_property, df_flooding_buffer,sale_date_column="Sale_Date",
                                       drop_duplicate_column = ["Project Name","Address","Sale_Date"]):
    """
    merge with empirical historical flooding data by intersection of locations ONLY
    Args:
        df_property (gpd.GeoDataFrame): df describing property transaction info and property attributes
        df_flooding_buffer (pd.GeoDataFrame): df describing flooding_buffer
        drop_duplicate_column (list of str): list of columns that identify a unique sale observation
    Returns:
        pd.DataFrame: that adds columns describing whether residential area is within a flood prone area for that year
    """
    df_copy = copy.deepcopy(df_property)
    # merge residential with flood data by location ONLY
    # groupby column is not needed because only areas that intersect with df_flooding_buffer will have non NA flooded_location and Flood_Date data
    # areas that don't intersect with df_flooding_buffer will just have NA values
    residential_flood = df_property.sjoin(df_flooding_buffer, how="left")
    residential_flood_agg = residential_flood.groupby(drop_duplicate_column).agg({'Flood_Date': lambda x: list(x),
                                                                              'flooded_location': lambda x: list(x)}).reset_index()
    # get past flood stats e.g. 'latest_flood_date','past_flood_count','total_flood_count'
    flood_summary = residential_flood_agg.apply(lambda x: get_past_flood(x[sale_date_column],x['Flood_Date']),axis=1)
    residential_flood_agg[flood_summary.columns] = flood_summary
    df_copy = df_copy.merge(residential_flood_agg)
    return df_copy

def add_within_flooding_hotspot_buffer(df_property, df_hotspot_buffer,sale_date_column="Sale_Date",
                                       drop_duplicate_column = ["Project Name","Address","Sale_Date"]):
    """
    Args:
        df_property (gpd.GeoDataFrame): df describing property transaction info and property attributes
        df_hotspot_buffer (pd.GeoDataFrame): df describing flooding_hotspot_buffer 
        drop_duplicate_column (list of str): list of columns that identify a unique sale observation
    Returns:
        pd.DataFrame: that adds columns describing whether residential area is within a flood prone area for that year
    """
    df_copy = copy.deepcopy(df_property)
    # merge residential with flood data by location ONLY
    # groupby column is not needed because only areas that intersect with df_hotspot_buffer will have non NA flooded_location and Flood_Date data
    # areas that don't intersect with df_hotspot_buffer will just have NA values
    residential_flood = df_property.sjoin(df_hotspot_buffer, how="left")
    residential_flood_agg = residential_flood.groupby(drop_duplicate_column).agg({'Flood_Hotspot_Date': lambda x: list(x),
                                                                              'flooded_locations': lambda x: list(x)}).reset_index()
    
    # flood hotspot designation dates
    flood_summary = residential_flood_agg.apply(lambda x: get_hotspot_designation_date(x[sale_date_column], x['Flood_Hotspot_Date']),axis=1)
    residential_flood_agg[flood_summary.columns] = flood_summary
    df_copy = df_copy.merge(residential_flood_agg)
    return df_copy

def add_road_drainage_works(df_property, df_road_drainage, sale_date_column="Sale_Date",
                            drop_duplicate_column = ["Project Name","Address","Sale_Date"]):
    """
    Args:
        df_property (gpd.GeoDataFrame): df describing property transaction info and property attributes
        df_road_drainage (pd.GeoDataFrame): df describing buffer location of road raising and drainage works
    Returns:
        pd.DataFrame: that adds columns describing whether residential area is within a flood prone area for that year
    """
    df_copy = copy.deepcopy(df_property)
    # merge residential with flood data
    residential_drainage = df_property.sjoin(df_road_drainage, how="left",
                                             rsuffix="drainage",lsuffix="property")
    # aggregate drainage adaptation events by property location
    residential_drainage_agg = residential_drainage.groupby(drop_duplicate_column).agg({'Drainage_Date': lambda x: list(x),
                                                                                        'work_categories': lambda x: list(x),
                                                                              'ROAD_NAME_drainage': lambda x: list(x)}).reset_index()
    df_copy = df_copy.merge(residential_drainage_agg)
    # add past drainage adaptation work that occurred BEFORE sale date
    drainage_summary = df_copy.apply(lambda x: get_past_adaptation(x[sale_date_column], x['Drainage_Date'], x['work_categories']),axis=1)
    df_copy[drainage_summary.columns] = drainage_summary
    
    return df_copy

