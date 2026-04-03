import re
import numpy as np
import pandas as pd
import osmnx as ox
import networkx as nx
from helper_functions import utils
from functools import reduce
import os
import geopandas as gpd
import helper_functions.data as Data
import copy

def get_year_month_date(date_column):
    """
    convert date column to year-month-01 
    e.g. 2020-01-13 to 2020-01-01
    """
    return pd.to_datetime(date_column.dt.strftime("%Y-%m-01"))

def get_contaminated_properties(df_subset):
    """
    contamination occurs when observations of a specific unit and transaction date which has already been treated by an earlier flood so it is not a good control unit for the next flood
    Args:
        df_subset (pd.DataFrame): df of a property ID i.e. property unit of a specific sale date
    
    """
    # contamination occurs if
    contamination_series = pd.Series(0, index=df_subset.index)
    # check if there is at least one obs where treat and post == 1, AND if there are different treat variables aka it serves as a treat and control for different floods
    treated_mask = (df_subset['treat']==1) & (df_subset['post']==1) # treated group
    if (df_subset['treat'].nunique() > 1) & (treated_mask.any()):
        # for rows corresponding to treat == 1, obtain the earliest flood date
        earliest_flood_treated = df_subset.loc[treated_mask,"period_flood"].min()
        # sale_date = df_subset['period_sale'].values[0]
        # print(earliest_flood_treated)
        # for rows corresponding to treat == 0, check if the sale date occurs after the earlier flood
        # if property is a control for another flood that occurs after the earliest flood. it is not suitable as a control because it already has been treated
        control_untreated = (df_subset['treat']==0) & (df_subset['period_flood']>earliest_flood_treated)
        contamination_series.loc[control_untreated] = 1
        return contamination_series
    return contamination_series

def get_potential_contamination(df_subset):
    """
    observations of a specific unit and transaction date with different Treat and Post values in the stacked DID dataframe
    Args:
        df_subset (pd.DataFrame): df of a property ID i.e. property unit of a specific sale date
    
    """
    
    if (df_subset['treat'].nunique() > 1) & (df_subset['post'].nunique() > 1):
        return pd.Series(1, index=df_subset.index)
    else:
        return pd.Series(0, index=df_subset.index)

def add_historical_flooding(df_property, df_flooding_buffer_small,
                            df_flooding_buffer_big,offset_years=2,
                                       drop_duplicate_column = ["Project Name","Address","Sale_Date"]):
    """
    merge with empirical historical flooding data by intersection of locations ONLY
    Args:
        df_property (gpd.GeoDataFrame): df describing property transaction info and property attributes
        df_flooding_buffer_small (pd.GeoDataFrame): df describing flooding_buffer of a smaller radius (treatment group)
        df_flooding_buffer_big (pd.GeoDataFrame): df describing flooding_buffer of a bigger radius (control group)
        offset_years (int): buffer time before and after the earliest and latest transaction date
    Returns:
        pd.DataFrame: that adds columns describing whether residential area is within a flood prone area for that year
    """
    # df_copy = copy.deepcopy(df_property)
    # merge residential with flood data by location ONLY
    treatment_property = df_property.sjoin(df_flooding_buffer_small, how="left").drop(columns=['index_right'])
    control_property = df_property.sjoin(df_flooding_buffer_big, how="left").drop(columns=['index_right'])
    # merge treatment and control properties together
    # how="right" basically means keeping observations for big buffer df only, it will drop all merge=="left_only"
    # both means locations are located within the small and large buffer radius - treatment group
    # right_only means locations are located outside the small buffer but within the large buffer radius - control group
    treatment_control_df = treatment_property.merge(control_property, how="right",indicator=True) 
    # drop properties outside big buffer
    treatment_control_df = treatment_control_df.dropna(subset=["Flood_ID"])
    # create treatment column
    treatment_control_df["treat"] = (treatment_control_df["_merge"] == "both").astype(int)
    treatment_control_df['Sale_Date_corrected'] = get_year_month_date(treatment_control_df['Sale_Date'])
    treatment_control_df['Flood_Date_corrected'] = get_year_month_date(treatment_control_df['Flood_Date'])
    # get objective timeline - period_t, which is an absolute timeline (not relative to any event)
    start_date = treatment_control_df['Sale_Date_corrected'].min() - pd.DateOffset(years=2) 
    end_date = treatment_control_df['Sale_Date_corrected'].max() + pd.DateOffset(years=2) 
    unique_dates = Data.get_unique_dates(start_date=start_date, end_date=end_date+pd.DateOffset(months=1)) # add 1 month offset to be inclusive of end_date
    # map the absolute time line to the sale and flood dates
    treatment_control_df = treatment_control_df.merge(unique_dates.rename(columns={'unique_dates':'Sale_Date_corrected'}),
            how="left").rename(columns={'period_t':'period_sale'})
    treatment_control_df = treatment_control_df.merge(unique_dates.rename(columns={'unique_dates':'Flood_Date_corrected'}),
            how="left").rename(columns={'period_t':'period_flood'})
    # create period D - difference between sale date and flood date wrt to flood date
    treatment_control_df['period_D'] = treatment_control_df['period_sale'] - treatment_control_df['period_flood']
    # create POST column
    treatment_control_df['post'] = (treatment_control_df["period_sale"] > treatment_control_df["period_flood"]).astype(int)
    # identify contaminated rows
    contamination = treatment_control_df.groupby(["Property_ID"]).apply(lambda x: get_contaminated_properties(x),include_groups=False).reset_index(level=[0], name="contaminated_rows")
    potential_contamination = treatment_control_df.groupby(["Property_ID"]).apply(lambda x: get_potential_contamination(x),include_groups=False).reset_index(level=[0], name="potential_contamination")

    treatment_control_df['contaminated_rows'] = contamination['contaminated_rows']
    treatment_control_df['potential_contamination'] = potential_contamination['potential_contamination']
    
    return treatment_control_df