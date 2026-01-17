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
    df_subset = df_subset.sort_values(by=sale_date_column)
    flood_date = df_subset["Flood_Date"].ffill()
    sale_date = df_subset[sale_date_column]
    flood_date_months_later = flood_date + pd.DateOffset(months=months_after_flood)
    has_flood_within_months = pd.Series(False,index=df_subset.index,name=f"within_{months_after_flood}_months_post_flood")
    mask = (sale_date >= flood_date) & (sale_date <= flood_date_months_later)
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


def add_within_flooding_hotspot_buffer(df_property, df_flooding_hotspot_buffer,
                                       groupby_column = ["Project Name"], sale_date_column="Sale_Date"):
    """
    Args:
        df_property (gpd.GeoDataFrame): df describing property transaction info and property attributes
        df_flooding_hotspot_buffer (pd.GeoDataFrame): df describing flooding_hotspot_buffer 
        groupby_column (list of str): column to split the df by to examine for each specific location e.g. subzone, or building, or project name
    Returns:
        pd.DataFrame: that adds columns describing whether residential area is within a flood prone area for that year
    """
    df_copy = copy.deepcopy(df_property)
    # merge residential with flood data
    residential_flood_hotspot = df_property.sjoin(df_flooding_hotspot_buffer,how="left",on_attribute=["year","month"])
    # apply to each project name, assume each project will be equally affected by floods
    # for each project, get the most recent flooding hotspot date and compare with each transaction record,
    # if flooding hotspot publication date occurred after transaction date, assume that for that transaction record, that location is still a flood prone area
    within_flooding_hotspot = residential_flood_hotspot.groupby(groupby_column).apply(lambda x: get_within_flooding_hotspot_buffer(x,sale_date_column=sale_date_column)).reset_index(level=[0])
    within_flooding_hotspot = within_flooding_hotspot[["latest_flooding_hotspot_date",'within_flooding_hotspot']]
    # drop duplicated index because multiple flood location can hit the same properties, resulting in duplicated rows
    within_flooding_hotspot = within_flooding_hotspot.loc[~within_flooding_hotspot.index.duplicated(keep='last')]
    df_copy.loc[within_flooding_hotspot.index,["latest_flooding_hotspot_date",'within_flooding_hotspot']] = within_flooding_hotspot
    return df_copy

def get_weeks_since_flood(df_subset,sale_date_column="Sale_Date",fillna=np.nan):
    """calculates weeks between transaction sale date and last occurred flood date
    Args:
        df_subset (pd.DataFrame) refers to the specific residential areas within specific subzone/planning area
    Returns:
        pd.DataFrame: 
            boolean column that looks at if area on a planning area scale has ever flooded before to capture structural flood risk stigma
            float column that calculates weeks since flood to estimate recency from floods
    """
    df_subset = df_subset.sort_values(by=sale_date_column)
    flood_date = df_subset["Flood_Date"].ffill()
    # impute with the most recent flood event based on the
    try:
        # impute with the most recent flood event that occurred in the planning area (but very coarse resolution) as compared to the groupby
        # the groupby is by subsone
        last_flooded_date = last_flooded_date_planningArea[df_subset["PLN_AREA_N"].values[0]]
    except:
        last_flooded_date = np.nan
    flood_date.iloc[0] = last_flooded_date
    # fill forward with the last flooded date
    flood_date = flood_date.ffill()
    # # check if location has ever flooded
    # ever_flooded = flood_date.notna().any()
    # ever_flooded.name = "ever_flooded"
    # get sale date
    sale_date = df_subset[sale_date_column]
    weeks_since_flood = sale_date - flood_date
    # convert dt to weeks
    weeks_since_flood = weeks_since_flood/np.timedelta64(1,'W')
    # replace NA with placeholder weeks
    weeks_since_flood = weeks_since_flood.fillna(value=fillna)
    # set name for panda series
    weeks_since_flood.name = "weeks_since_flood"
    # return pd.concat([ever_flooded, weeks_since_flood],axis=1)
    return weeks_since_flood


def add_weeks_since_flood(df,sale_date_column="Sale_Date", 
                          groupby_column=["SUBZONE_N"],
                          drop_duplicate_column = ["Project Name","Address","Sale_Date"],
                          fillna=np.nan):
    """
    calculates weeks (continuous variable) since flood. Use pre-2014 flood data to identify if flood has occured pre-2014, otherwise, assign fillna value
    Args:
        df (pd.DataFrame): dataframe after merging flood_df to residential df
        groupby_column (list of str): column to split the df by to examine for each specific location e.g. subzone, or building, or project name
        drop_duplicate_column (list of str): culumns from where to identify duplicates and drop duplicate rows
        fillna (np.nan or int): value for records that did not have any floods throughout the observation period from 2012-2024
    Returns:
        pd.DataFrame with added columns calculates weeks (continuous variable) since flood pre Sale Date
    """
    df_copy = copy.deepcopy(df)
    # drop duplicates which occur because multiple flood locations in the same subzone can happen during the same day
    df_copy = df_copy.drop_duplicates(subset=drop_duplicate_column)
    df_copy["weeks_since_flood"] = pd.Series(fillna,index=df.index,name=f"weeks_since_flood")
    weeks_since_flood = df_copy.groupby(groupby_column).apply(lambda x: get_weeks_since_flood(x,sale_date_column=sale_date_column,fillna=fillna)).reset_index(level=[0])
    df_copy.loc[weeks_since_flood.index,"weeks_since_flood"] = weeks_since_flood["weeks_since_flood"]
    return df_copy