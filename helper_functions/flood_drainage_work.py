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
import helper_functions.data as Data
import copy

"""
In the context of the Urban Redevelopment Authority (URA) REALIS (Real Estate Information System) in Singapore, the sale date is defined as the date on which the Option to Purchase (OTP) is exercised or the Sale and Purchase Agreement (S&PA) is signed. 

Key Details regarding REALIS Sale Date:
Data Source: REALIS records transactions based on caveats lodged with the Singapore Land Authority (SLA) and stamp duty data from the Inland Revenue Authority of Singapore (IRAS).
Un-caveated Transactions: If no caveat is lodged, the date of the contract submitted to IRAS for stamp duty payment is used.
Transaction Timing: The date indicates when the price was agreed upon between the buyer and seller, rather than the legal completion date.
New Sale vs. Resale: For new projects, it is often the date the developer issues the Option to Purchase. For resale, it is usually the date the option is exercised. 
"""

def get_year_month_date(date_column):
    """
    convert date column to year-month-01 
    e.g. 2020-01-13 to 2020-01-01
    """
    return pd.to_datetime(date_column.dt.strftime("%Y-%m-01"))

def get_past_work_categories(df,sale_date_column="Sale_Date", event_date_column="Drainage_Date",
                             work_category_column="work_categories"):
    """
    Returns:
        pd.Series (str): identifies work_categories where sale date occurs strictly after Drainage_Date
    """
    past_work = df[sale_date_column] > df[event_date_column]
    past_work_categories = pd.Series(np.nan, dtype="str", index = past_work.index)
    past_work_categories.loc[past_work] = df[work_category_column].loc[past_work]
    return past_work_categories

def get_future_work_categories(df,sale_date_column="Sale_Date", event_date_column="Drainage_Date",
                             work_category_column="work_categories"):
    """
    Returns:
        pd.Series (str): identifies work_categories where sale date occurs before or equals to Drainage_Date
    """
    future_work = df[sale_date_column] <= df[event_date_column]
    future_work_categories = pd.Series(np.nan, dtype="str", index = future_work.index)
    future_work_categories.loc[future_work] = df[work_category_column].loc[future_work]
    return future_work_categories

def get_within_flooding_hotspot(df,sale_date_column="Sale_Date", event_date_column="Flood_Hotspot_Date"):
    """
    Returns:
        pd.Series (bool): identifies Flood_Hotspot_Date where sale date occurs before or equals to Flood_Hotspot_Date
    """
    return df[sale_date_column] <= df[event_date_column]

def get_pre_hotspot_designation_date(df,sale_date_column="Sale_Date", event_date_column="Flood_Hotspot_Date"):
    """
    Returns:
        pd.Series (pd.datetime): identifies Flood_Hotspot_Date where sale date occurs before or equals to Flood_Hotspot_Date
    """
    pre_hotspot = df[sale_date_column] <= df[event_date_column]
    pre_hotspot_designation_date = pd.Series(pd.NaT, index=pre_hotspot.index)
    pre_hotspot_designation_date.loc[pre_hotspot] = df[event_date_column].loc[pre_hotspot]
    return pre_hotspot_designation_date

def get_post_hotspot_designation_date(df,sale_date_column="Sale_Date", event_date_column="Flood_Hotspot_Date"):
    """
    Returns:
        pd.Series (pd.datetime): identifies Flood_Hotspot_Date where sale date occurs strictly after Flood_Hotspot_Date
    """
    post_hotspot = df[sale_date_column] > df[event_date_column]
    post_hotspot_designation_date = pd.Series(pd.NaT, index=post_hotspot.index)
    post_hotspot_designation_date.loc[post_hotspot] = df[event_date_column].loc[post_hotspot]
    return post_hotspot_designation_date

def get_past_flood_count(df,sale_date_column="Sale_Date", event_date_column="Flood_Date"):
    """
    Returns:
        pd.Series (bool): identifies where sale date occurs strictly after flood date
    """
    # return get_year_month_date(df[sale_date_column]) >= get_year_month_date(df[event_date_column])
    return df[sale_date_column] >= df[event_date_column]

def get_latest_flood_date(df,sale_date_column="Sale_Date", event_date_column="Flood_Date"):
    """
    Returns:
        pd.Series (pd.datetime): identifies flood dates where sale date occurs strictly after flood date
    """
    # only identify rows where sale date occurs strictly after flood date
    post_flood = get_past_flood_count(df,sale_date_column, event_date_column)
    latest_flood_date = pd.Series(pd.NaT, index=post_flood.index)
    latest_flood_date.loc[post_flood] = df[event_date_column].loc[post_flood]
    return latest_flood_date

def get_event_within_months(df,sale_date_column="Sale_Date", event_date_column="Flood_Date", months_after_event=6):
    """
    Assign True/False if transaction occurs within 6 months of event for each groupby_column
    Args:
        df (pd.DataFrame): dataframe after merging event_df to residential df
        sale_date_column (str): name of sale date column
        event_date_column (str): name of event date column e.g. flood or adaptation
        months_after_event (int): months to identify if event occurs within timeframe
    Returns:
        pd.Series: column describing True/False if transaction occurs within 6 months of event
    """
    def months_after_event_func(sale_date, event_date, months_after_event):
        event_date_months_later = event_date + pd.DateOffset(months=months_after_event)
        return (sale_date > event_date) & (sale_date <= event_date_months_later)
    
    def months_before_event_func(sale_date, event_date, months_before_event):
        event_date_months_before = event_date + pd.DateOffset(months=months_before_event)
        return (sale_date <= event_date) & (sale_date >= event_date_months_before)

    if months_after_event > 0:
        post_event_column = months_after_event_func(df[sale_date_column], 
                                                    df[event_date_column], 
                                                    months_after_event)  
    else:
        post_event_column = months_before_event_func(df[sale_date_column], 
                                                df[event_date_column], 
                                                months_after_event)
    
    return post_event_column

def get_D_event(period_flood,period_sale, lower=None, upper=None):
    """
    get relative time lead/lag with reference to FLOOD EVENT i.e. flood event:= t=0
    Args:
        period_flood (pd.Series): flood event on timeline t
        period_sale (pd.Series): sale event on timeline t
        lower (float): Minimum threshold value. All values below this threshold will be set to it. 
        upper (float): Maximum threshold value. All values above this threshold will be set to it. 
    """
    period_D = period_sale.astype("Int64") - period_flood.astype("Int64")
    return period_D.clip(lower=lower,upper=upper)

def get_never_treated(period_flood):
    """
    if observation has a flood date, it means the flood buffer intersected with unit
    unit is considered never treated if there are no intersections with flood buffer
    """
    return period_flood.isna()

def get_not_yet_treated(period_D):
    return (period_D < 0).fillna(True)

def get_treated(period_D):
    """
    if observation has a flood date, it means the flood buffer intersected with unit
    unit is considered treated if sale date occurs after flood date
    # residential_flood['never_treated'] = get_never_treated(residential_flood['period_flood'])
    # residential_flood['not_yet_treated'] = get_not_yet_treated(residential_flood['period_D'])
    # residential_flood['treated'] = get_treated(residential_flood['period_D'])
    """
    return (period_D >= 0).fillna(False)

# def add_historical_flooding(df_property, df_flooding_buffer,
#                             months_after_event = [6],
#                             sale_date_column="Sale_Date",
#                                        drop_duplicate_column = ["Project Name","Address","Sale_Date"]):
#     """
#     merge with empirical historical flooding data by intersection of locations ONLY
#     Args:
#         df_property (gpd.GeoDataFrame): df describing property transaction info and property attributes
#         df_flooding_buffer (pd.GeoDataFrame): df describing flooding_buffer
#         months_after_event (list of int): list of months to identify if event occurs within timeframe
#         drop_duplicate_column (list of str): list of columns that identify a unique sale observation
#     Returns:
#         pd.DataFrame: that adds columns describing whether residential area is within a flood prone area for that year
#     """
#     df_copy = copy.deepcopy(df_property)
#     # merge residential with flood data by location ONLY
#     # groupby column is not needed because only areas that intersect with df_flooding_buffer will have non NA flooded_location and Flood_Date data
#     # areas that don't intersect with df_flooding_buffer will just have NA values
#     residential_flood = df_copy.sjoin(df_flooding_buffer, how="left")
#     # add post flood bool col
#     for month_after_event in months_after_event:
#         residential_flood[f"within_{month_after_event}_months_post_Flood_Date"] = get_event_within_months(residential_flood,sale_date_column=sale_date_column, 
#                                 event_date_column="Flood_Date", months_after_event=month_after_event)
#     # get past flood stats e.g. 'latest_flood_date','past_flood_count','total_flood_count'
#     residential_flood['latest_flood_date'] = get_latest_flood_date(residential_flood,sale_date_column=sale_date_column, 
#                                 event_date_column="Flood_Date")
#     residential_flood['past_flood_count'] = get_past_flood_count(residential_flood,sale_date_column=sale_date_column, 
#                                 event_date_column="Flood_Date")
#     # identify obs that have a flood date, which would indicate that a flood occurred
#     residential_flood['total_flood_count'] = residential_flood['Flood_Date'].notna()
#     # collapse duplicate rows into unique observations
#     residential_flood_agg = residential_flood.groupby(drop_duplicate_column).agg({'Flood_Date': lambda x: list(x),
#                                                                               'flooded_location': lambda x: list(x),
#                                                                               'latest_flood_date': lambda x: x.sort_values(ascending=False).values[0],
#                                                                               'past_flood_count': lambda x: x.sum(),
#                                                                               'total_flood_count': lambda x: x.sum()
#                                                                               } | {f"within_{i}_months_post_Flood_Date": lambda x: x.any() for i in months_after_event}).reset_index()
    
#     df_copy = df_copy.merge(residential_flood_agg)
#     return df_copy

def add_historical_flooding(df_property, df_flooding_buffer,
                            Dt_min = -12, Dt_max = 12,
                            sale_date_column="Sale_Date",
                                       drop_duplicate_column = ["Project Name","Address","Sale_Date"],
                                       prefix="Dt"):
    """
    merge with empirical historical flooding data by intersection of locations ONLY
    Args:
        df_property (gpd.GeoDataFrame): df describing property transaction info and property attributes
        df_flooding_buffer (pd.GeoDataFrame): df describing flooding_buffer
        Dt_min (int): minmum cut off point to consider pre-flood event time. values smaller than this is clipped to this value.
        Dt_max (int): maximum cut off point to consider post-flood event time. values bigger than this is clipped to this value.
        drop_duplicate_column (list of str): list of columns that identify a unique sale observation
        prefix (str): prefix for columns describing lead/lags wrt to flood event
    Returns:
        pd.DataFrame: that adds columns describing whether residential area is within a flood prone area for that year
    """
    df_copy = copy.deepcopy(df_property)
    # merge residential with flood data by location ONLY
    # groupby column is not needed because only areas that intersect with df_flooding_buffer will have non NA flooded_location and Flood_Date data
    # areas that don't intersect with df_flooding_buffer will just have NA values
    residential_flood = df_copy.sjoin(df_flooding_buffer, how="left")
    # sort sale and flood date from oldest to latest
    # residential_flood = residential_flood.sort_values(by=['Sale_Date','Flood_Date'])
    # get year-month columns
    residential_flood['Sale_Date_corrected'] = get_year_month_date(residential_flood['Sale_Date'])
    residential_flood['Flood_Date_corrected'] = get_year_month_date(residential_flood['Flood_Date'])
    
    # get objective timeline - period_t, which is an absolute timeline (not relative to any event)
    start_date = residential_flood['Sale_Date_corrected'].min() - pd.DateOffset(years=2) #residential_flood['Sale_Date_corrected'].min() if residential_flood['Sale_Date_corrected'].min() < residential_flood['Flood_Date_corrected'].min() else residential_flood['Flood_Date_corrected'].min()
    end_date = residential_flood['Sale_Date_corrected'].max() + pd.DateOffset(years=2) #residential_flood['Sale_Date_corrected'].max() if residential_flood['Sale_Date_corrected'].max() > residential_flood['Flood_Date_corrected'].max() else residential_flood['Flood_Date_corrected'].max()
    unique_dates = Data.get_unique_dates(start_date=start_date, end_date=end_date+pd.DateOffset(months=1)) # add 1 month offset to be inclusive of end_date
    
    # map the absolute time line to the sale and flood dates
    residential_flood = residential_flood.merge(unique_dates.rename(columns={'unique_dates':'Sale_Date_corrected'}),
            how="left").rename(columns={'period_t':'period_sale'})
    residential_flood = residential_flood.merge(unique_dates.rename(columns={'unique_dates':'Flood_Date_corrected'}),
            how="left").rename(columns={'period_t':'period_flood'})

    # get lead and lags wrt to FLOOD EVENT
    period_D = get_D_event(residential_flood['period_flood'],residential_flood['period_sale'], 
                                                lower=Dt_min,upper=Dt_max)
    residential_flood['period_D'] = period_D

    # convert period_D column into wide format i.e. dummies
    residential_flood = pd.get_dummies(residential_flood,columns=['period_D'],dtype=int, prefix=prefix)
    # add back period_D
    residential_flood['period_D'] = period_D
    # get past flood stats e.g. 'latest_flood_date','past_flood_count','total_flood_count'
    residential_flood['past_flood_count'] = get_past_flood_count(residential_flood,
                                                                 sale_date_column='Sale_Date_corrected', 
                                event_date_column="Flood_Date_corrected")
    period_flood_recent = get_latest_flood_date(residential_flood,
                                                sale_date_column='Sale_Date_corrected', 
                                event_date_column="Flood_Date_corrected").to_frame(name='period_flood_recent')
    # map first and recent flood dates to flood period
    # period_flood_recent['period_flood_recent'] = get_year_month_date(period_flood_recent['period_flood_recent'])
    period_flood_recent = period_flood_recent.merge(unique_dates,left_on='period_flood_recent', right_on='unique_dates',how='left')
    residential_flood['period_flood_recent'] = period_flood_recent['period_t']
    residential_flood['period_flood_first'] = residential_flood['period_flood_recent']
    # collapse duplicate rows into unique observations
    Dt_column_names = [i for i in residential_flood.columns if i.startswith(f'{prefix}_')]
    residential_flood_agg = residential_flood.groupby(drop_duplicate_column+['period_sale']).agg({'Flood_Date': lambda x: list(x),
                                                                                'period_flood': lambda x: list(x),
                                                                                'period_flood_first': lambda x: x.min(), # record the first flood in observation period
                                                                                'period_flood_recent': lambda x: x.max(), # record most recent flood that occurred before sale date
                                                                              'flooded_location': lambda x: list(x),
                                                                              'past_flood_count': lambda x: x.sum(), # flood intensity based on previous flood count before sale date
                                                                              } | {i: lambda x: x.sum() for i in Dt_column_names}).reset_index()
    
    # get treatment status of units
    Dt_columns = [i for i in residential_flood_agg.columns if i.startswith(f'{prefix}_')]
    Dt_pre_columns = [i for i in residential_flood_agg.columns if i.startswith(f'{prefix}_-')]
    Dt_post_columns = list(set(Dt_columns)-set(Dt_pre_columns))
    residential_flood_agg['treated'] = residential_flood_agg[Dt_post_columns].sum(axis=1) > 0
    residential_flood_agg['never_treated'] = residential_flood_agg[Dt_columns].sum(axis=1) < 1
    residential_flood_agg['not_yet_treated'] = ~residential_flood_agg['never_treated'] * ~residential_flood_agg['treated']
    residential_flood_agg.loc[residential_flood_agg['never_treated']==True,'not_yet_treated'] = True

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
    residential_flood['pre_hotspot_designation_date'] = get_pre_hotspot_designation_date(residential_flood,
                                                                                         sale_date_column=sale_date_column,
                                                                                         event_date_column="Flood_Hotspot_Date")
    residential_flood['post_hotspot_designation_date'] = get_post_hotspot_designation_date(residential_flood,
                                                                                         sale_date_column=sale_date_column,
                                                                                         event_date_column="Flood_Hotspot_Date")
    # collapse duplicate rows into unique observations
    residential_flood_agg = residential_flood.groupby(drop_duplicate_column).agg({'Flood_Hotspot_Date': lambda x: list(x),
                                                                              'flooded_locations': lambda x: list(x),
                                                                              'pre_hotspot_designation_date': lambda x: x.sort_values().values[0],
                                                                              'post_hotspot_designation_date': lambda x: x.sort_values(ascending=False).values[0],
                                                                              }).reset_index()
    
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
    residential_drainage['past_work_categories'] = get_past_work_categories(residential_drainage,
                                                                                         sale_date_column=sale_date_column,
                                                                                         event_date_column="Drainage_Date",
                                                                                         work_category_column="work_categories")
    residential_drainage['future_work_categories'] = get_future_work_categories(residential_drainage,
                                                                                         sale_date_column=sale_date_column,
                                                                                         event_date_column="Drainage_Date",
                                                                                         work_category_column="work_categories")
    
    # aggregate drainage adaptation events by property location
    residential_drainage_agg = residential_drainage.groupby(drop_duplicate_column).agg({'Drainage_Date': lambda x: list(x),
                                                                                        'work_categories': lambda x: list(x),
                                                                              'ROAD_NAME_drainage': lambda x: list(x),
                                                                              'past_work_categories': lambda x: x.sort_values().str.cat(sep=","),
                                                                              'future_work_categories': lambda x: x.sort_values().str.cat(sep=",")
                                                                              }).reset_index()
    # replace empty strings with none
    residential_drainage_agg = residential_drainage_agg.replace('',"none")
    df_copy = df_copy.merge(residential_drainage_agg)
    
    return df_copy

