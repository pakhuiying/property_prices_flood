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

last_flooded_date_planningArea = {'JURONG WEST':'2013-11-30',
                                  'BEDOK': '2013-11-24',
                                  'BUKIT TIMAH': '2013-11-04',
                                  'QUEENSTOWN':'2013-10-30',
                                  'GEYLANG':'2013-10-20',
                                  'TAMPINES':'2013-10-15',
                                  'CLEMENTI':'2013-09-05',
                                  'PUNGGOL':'2013-02-07',
                                  'WOODLANDS':'2013-01-02',
                                  'ANG MO KIO':'2012-12-13',
                                  'SERANGOON':'2012-11-01',
                                  'TOA PAYOH':'2012-11-01',
                                  'ORCHARD':'2012-11-01',
                                  'BUKIT MERAH':'2012-09-27',
                                  'DOWNTOWN CORE':'2012-09-27',
                                  'MARINA SOUTH':'2012-09-27',
                                  'OUTRAM':'2012-09-27',
                                  'NEWTON':'2012-09-27',
                                  'CHOA CHU KANG':'2012-05-05',
                                  'HOUGANG':'2012-04-10',
                                  'MANDAI':'2012-01-21',
                                  'SENGKANG':'2012-01-21',
                                  'ROCHOR':'2012-01-20',
                                  'BISHAN':'2012-01-20',
                                  'TANGLIN':'2012-01-20',
                                  'NOVENA':'2011-12-23',
                                  'JURONG EAST':'2011-06-09',
                                  'CHANGI':'2011-01-30',
                                  'MARINE PARADE':'2010-07-17',
                                  'KALLANG':'2010-06-25',
}

def get_flood_within_months(df_subset,sale_date_column="Sale_Date",months_after_flood=6):
    """check if transaction sale is within e.g. 6 months of flood
    Args:
        df_subset (pd.DataFrame) refers to the specific residential areas within specific subzone/planning area
        months (int): months within flood date
    Returns:
        pd.Series: boolean column that describes whether there is flood within 6 months
    """
    df_subset = df_subset.sort_values(by=sale_date_column) # sort oldest date to most recent date
    
    has_flood_within_months = pd.Series(False,index=df_subset.index,name=f"within_{months_after_flood}_months_post_flood")
    if months_after_flood > 0:
        flood_date = df_subset["Flood_Date"].ffill() 
        sale_date = df_subset[sale_date_column]
        flood_date_months_later = flood_date + pd.DateOffset(months=months_after_flood)
        mask = (sale_date >= flood_date) & (sale_date <= flood_date_months_later)
    else:
        flood_date = df_subset["Flood_Date"].bfill() 
        sale_date = df_subset[sale_date_column]
        flood_date_months_later = flood_date + pd.DateOffset(months=months_after_flood)
        mask = (sale_date < flood_date) & (sale_date >= flood_date_months_later)
    # print(mask.sum())
    if mask.any(): # if there's any transaction within 6 months of flood/or no floods in an area
        has_flood_within_months.loc[mask] = True
    return has_flood_within_months

def add_flood_within_months(df,sale_date_column="Sale_Date",months_after_flood=6,
                            groupby_column=["SUBZONE_N"],
                            drop_duplicate_column = ["Project Name","Address","Sale_Date"]):
    """
    Assign True/False if transaction occurs within 6 months of flood for each groupby_column
    Args:
        df (pd.DataFrame): dataframe after merging flood_df to residential df
        groupby_column (list of str): column to split the df by to examine for each specific location e.g. subzone, or building, or project name
        drop_duplicate_column (list of str): culumns from where to identify duplicates and drop duplicate rows
    Returns:
        pd.DataFrame with added columns describing True/False if transaction occurs within 6 months of flood
    """
    df_copy = copy.deepcopy(df)
    # drop duplicates which occur because multiple flood locations in the same subzone can happen during the same day
    df_copy = df_copy.drop_duplicates(subset=drop_duplicate_column)
    df_copy[f"within_{months_after_flood}_months_post_flood"] = pd.Series(False,index=df.index,name=f"within_{months_after_flood}_months_post_flood")
    flood_within_months = df_copy.groupby(groupby_column).apply(lambda x: get_flood_within_months(x,sale_date_column=sale_date_column,months_after_flood=months_after_flood)).reset_index(level=[0])
    df_copy.loc[flood_within_months.index,f"within_{months_after_flood}_months_post_flood"] = flood_within_months[f"within_{months_after_flood}_months_post_flood"]
    return df_copy

def get_within_flooding_hotspot_buffer(df_subset,sale_date_column="Sale_Date"):
    """
    Args:
        df_subset (pd.DataFrame): df for a specific Project Name
    Returns:
        pd.Series: that describes whether residential area is within a flood prone area for that year
    """
    df_subset = df_subset.sort_values(by=[sale_date_column])
    # intialise a new column
    within_flooding_hotspot = pd.Series(False,index=df_subset.index)
    if df_subset['Flood_Hotspot_Date'].notna().any():
        
        sale_date = df_subset[sale_date_column]
        # get the latest flood hotspot date
        flood_hotspot_date = df_subset['Flood_Hotspot_Date'].max()
        # if flooding hotspot publication date is after the transaction date, it means the area has previously flooded many times
        within_flooding_hotspot.loc[flood_hotspot_date>=sale_date] = True
        
        return pd.DataFrame({"latest_flooding_hotspot_date":pd.Series(flood_hotspot_date,index=df_subset.index),
                             "within_flooding_hotspot":within_flooding_hotspot})
    # if residential area is not within flooding hotspot buffer
    return pd.DataFrame({"latest_flooding_hotspot_date":pd.Series(np.nan,index=df_subset.index,dtype='datetime64[ns]'),
                             "within_flooding_hotspot":within_flooding_hotspot})


# def add_within_flooding_hotspot_buffer(df_property, df_flooding_hotspot_buffer,
#                                        groupby_column = ["Project Name"], sale_date_column="Sale_Date"):
#     """
#     Args:
#         df_property (gpd.GeoDataFrame): df describing property transaction info and property attributes
#         df_flooding_hotspot_buffer (pd.GeoDataFrame): df describing flooding_hotspot_buffer 
#         groupby_column (list of str): column to split the df by to examine for each specific location e.g. subzone, or building, or project name
#     Returns:
#         pd.DataFrame: that adds columns describing whether residential area is within a flood prone area for that year
#     """
#     df_copy = copy.deepcopy(df_property)
#     # merge residential with flood data
#     residential_flood_hotspot = df_property.sjoin(df_flooding_hotspot_buffer,how="left",on_attribute=["year","month"])
#     # apply to each project name, assume each project will be equally affected by floods
#     # for each project, get the most recent flooding hotspot date and compare with each transaction record,
#     # if flooding hotspot publication date occurred after transaction date, assume that for that transaction record, that location is still a flood prone area
#     within_flooding_hotspot = residential_flood_hotspot.groupby(groupby_column).apply(lambda x: get_within_flooding_hotspot_buffer(x,sale_date_column=sale_date_column)).reset_index(level=[0])
#     within_flooding_hotspot = within_flooding_hotspot[["latest_flooding_hotspot_date",'within_flooding_hotspot']]
#     # drop duplicated index because multiple flood location can hit the same properties, resulting in duplicated rows
#     within_flooding_hotspot = within_flooding_hotspot.loc[~within_flooding_hotspot.index.duplicated(keep='last')]
#     df_copy.loc[within_flooding_hotspot.index,["latest_flooding_hotspot_date",'within_flooding_hotspot']] = within_flooding_hotspot
#     return df_copy

def add_within_flooding_hotspot_buffer(df_property, df_flooding_hotspot_buffer,
                                       groupby_column = ["Project Name"], sale_date_column="Sale_Date",
                                       drop_duplicate_column = ["Project Name","Address","Sale_Date"]):
    """
    Args:
        df_property (gpd.GeoDataFrame): df describing property transaction info and property attributes
        df_flooding_hotspot_buffer (pd.GeoDataFrame): df describing flooding_hotspot_buffer 
        groupby_column (list of str): column to split the df by to examine for each specific location e.g. subzone, or building, or project name
    Returns:
        pd.DataFrame: that adds columns describing whether residential area is within a flood prone area for that year
    """
    df_copy = copy.deepcopy(df_property)
    # merge residential with flood data by location ONLY
    residential_flood_hotspot = df_property.sjoin(df_flooding_hotspot_buffer,how="left")
    # sort sale date and flood hotspot date by latest to oldest date
    residential_flood_hotspot = residential_flood_hotspot.sort_values(by=[sale_date_column,'Flood_Hotspot_Date'],ascending=False)
    # remove duplicated rows
    # drop duplicates by keeping only the first row because the df has been sorted by Sale and Flood dates 
    # by latest date first (closest date between Sale and Flood Date), and NAs at the bottom (so we do not accidentally keep the NA also)
    unique_residential_flood = residential_flood_hotspot.drop_duplicates(subset=drop_duplicate_column,keep='first')
    # apply to each project name, assume each project will be equally affected by floods
    # for each project, get the most recent flooding hotspot date and compare with each transaction record,
    # if flooding hotspot publication date occurred after transaction date, assume that for that transaction record, that location is still a flood prone area
    # reset index by level=0 doesn't reset the original index
    within_flooding_hotspot = unique_residential_flood.groupby(groupby_column).apply(lambda x: get_within_flooding_hotspot_buffer(x,sale_date_column=sale_date_column)).reset_index(level=[0])
    within_flooding_hotspot = within_flooding_hotspot[["latest_flooding_hotspot_date",'within_flooding_hotspot']]
    # # drop duplicated index because multiple flood location can hit the same properties, resulting in duplicated rows
    # add columns matching index
    df_copy[['Flood_Hotspot_Date','flooded_locations']] = unique_residential_flood[['Flood_Hotspot_Date','flooded_locations']]
    df_copy[["latest_flooding_hotspot_date",'within_flooding_hotspot']] = within_flooding_hotspot[["latest_flooding_hotspot_date",'within_flooding_hotspot']]
    
    return df_copy

def add_historical_flooding(df_property, df_flooding_buffer,sale_date_column="Sale_Date",
                                       drop_duplicate_column = ["Project Name","Address","Sale_Date"]):
    """
    merge with empirical historical flooding data by intersection of locations ONLY
    Args:
        df_property (gpd.GeoDataFrame): df describing property transaction info and property attributes
        df_flooding_buffer (pd.GeoDataFrame): df describing flooding_buffer
    Returns:
        pd.DataFrame: that adds columns describing whether residential area is within a flood prone area for that year
    """
    # merge just based on location, do not use on_attribute because this further adds joining constraints
    # which may cause severe underestimation e.g.  
    # joining on_attribute = "Sale_Date" is too restrictive because on_attribute is an additional join restriction, 
    # which means it only picks residential units with Sale Date that coincides with Flood_Date AND within the flood_df buffer,
    # this doesn't consider residential units where Sale Date occurs after the Flood_Date but is located within the flood_df buffer
    # same for joining on_attribute = "Year" and "Month" - it doesn't consider residential units where Sale Date occurs after the year and month but is within the flood_df buffer
    # this can lead to severe underestimation because properties with sale transaction that occur within e.g. 6 months of post flood will not be picked up in the analysis - they will be shown as "unflooded"
    # only properties with sale transaction that coincides with Flood_Date or year and month will then be flagged as flooded, and then their subsequent transactions may be flagged as flooded - but this will result in severe underestimation
    # because properties in the same flooded location should be picked up even if the Sale Date doesn;t coincide with flood date
    
    df_copy = copy.deepcopy(df_property)
    # merge residential with flood data by location ONLY
    # groupby column is not needed because only areas that intersect with df_flooding_buffer will have non NA flooded_location and Flood_Date data
    # areas that don't intersect with df_flooding_buffer will just have NA values
    residential_flood = df_property.sjoin(df_flooding_buffer, how="left")
    # set flood columns as NA if flood date occurs after sale date 
    # - this prevents false joining of residential place with flood that occurs after sale date
    mask = residential_flood["Flood_Date"] > residential_flood[sale_date_column]
    residential_flood.loc[mask, ["flooded_location","Flood_Date"]] = np.nan
    # remove duplicated rows
    unique_residential_flood = residential_flood[drop_duplicate_column+["flooded_location","Flood_Date"]].drop_duplicates()
    # unique_residential_flood = residential_flood.drop_duplicates()
    # sort df by Sale_Date and Flood_Date so that the closer Sale_Date and Flood_Date are arranged as the top row
    unique_residential_flood = unique_residential_flood.sort_values(by=[sale_date_column,"Flood_Date"], ascending=False)
    # drop duplicates by keeping only the first row because the df has been sorted by Sale and Flood dates by latest date first, and NAs at the bottom (so we do not accidentally keep the NA also)
    unique_residential_flood = unique_residential_flood.drop_duplicates(subset=drop_duplicate_column,keep='first')
    # assign flooded location and flood date to placeholder columns
    for col in ["flooded_location","Flood_Date"]:
        df_copy[col] = unique_residential_flood[col]

    return df_copy

def get_time_since_flood(df_subset,sale_date_column="Sale_Date",
                         fillna=np.nan,units="months", bins=None, labels=None):
    """calculates weeks between transaction sale date and last occurred flood date
    Args:
        df_subset (pd.DataFrame) refers to the specific residential areas within specific subzone/planning area
        units (str): obtain units in months or weeks
        bins (np.ndarray or list): to bin data into categorical intervals
        labels (list or None): to bin data into categorical intervals
    Returns:
        pd.DataFrame: 
            boolean column that looks at if area on a planning area scale has ever flooded before to capture structural flood risk stigma
            float column that calculates weeks since flood to estimate recency from floods
    """
    df_subset = df_subset.sort_values(by=sale_date_column)
    flood_date = df_subset["Flood_Date"]
    # get sale date
    sale_date = df_subset[sale_date_column]
    if units == "weeks":
        time_since_flood = sale_date - flood_date
        # convert dt to weeks
        time_since_flood = time_since_flood/np.timedelta64(1,'W')
    elif units == "months":
        time_since_flood = sale_date - flood_date
        # convert dt to weeks
        time_since_flood = (time_since_flood/np.timedelta64(1,'W'))/30
    # replace NA with placeholder weeks
    time_since_flood = time_since_flood.fillna(value=fillna)
    # set name for panda series
    time_since_flood.name = "time_since_flood"
    # round data
    time_since_flood = time_since_flood.round(0)
    # cut into bins and labels
    if bins is not None:
        time_since_flood = pd.cut(time_since_flood, bins=bins, labels=labels)
        # check if there is at least one flood incidence
        if (flood_date.notna().any()):
            # if there is at least one flood incidence within this obs period, we can assume it has flooded before the obs period
            # so NA values just means that the last occurred flood occurred way before obs period, doesn't mean that it has never flooded
            # get max label to show the largest time period category (i.e. flood > 12 months) to impute NA data
            max_label = time_since_flood.cat.categories.astype(str)[-1]
            # convert categorical dtype to str
            time_since_flood = time_since_flood.astype(str).fillna(max_label).str.replace("nan",max_label)
    return time_since_flood


def add_time_since_flood(df,sale_date_column="Sale_Date", 
                          groupby_column=["SUBZONE_N"],
                          drop_duplicate_column = ["Project Name","Address","Sale_Date"],
                          fillna=np.nan, units="months", bins=None, labels=None):
    """
    calculates weeks (continuous variable) since flood. Use pre-2014 flood data to identify if flood has occured pre-2014, otherwise, assign fillna value
    Args:
        df (pd.DataFrame): dataframe after merging flood_df to residential df
        groupby_column (list of str): column to split the df by to examine for each specific location e.g. subzone, or building, or project name
        drop_duplicate_column (list of str): culumns from where to identify duplicates and drop duplicate rows
        fillna (np.nan or int): value for records that did not have any floods throughout the observation period from 2012-2024
        units (str): obtain units in months or weeks
        bins (np.ndarray or list): to bin data into categorical intervals
        labels (list or None): to bin data into categorical intervals
    Returns:
        pd.DataFrame with added columns calculates weeks (continuous variable) since flood pre Sale Date
    """
    df_copy = copy.deepcopy(df)
    # drop duplicates which occur because multiple flood locations in the same subzone can happen during the same day
    df_copy = df_copy.drop_duplicates(subset=drop_duplicate_column)
    df_copy["time_since_flood"] = pd.Series(fillna,index=df.index,name=f"time_since_flood")
    time_since_flood = df_copy.groupby(groupby_column).apply(lambda x: get_time_since_flood(x,sale_date_column=sale_date_column,
                                                                                            fillna=fillna,
                                                                                            units=units, bins=bins, labels=labels)).reset_index(level=[0])
    df_copy.loc[time_since_flood.index,"time_since_flood"] = time_since_flood["time_since_flood"]
    return df_copy