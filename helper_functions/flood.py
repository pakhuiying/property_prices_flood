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

def add_flood_within_months(df,sale_date_column="Sale_Date",months_after_flood=6):
    """
    Assign True/False if transaction occurs within 6 months of flood
    Args:
        df (pd.DataFrame): dataframe after merging flood_df to residential df
    Returns:
        pd.DataFrame with added columns describing True/False if transaction occurs within 6 months of flood
    """
    df_copy = copy.deepcopy(df)
    # drop duplicates which occur because multiple flood locations in the same subzone can happen during the same day
    df_copy = df_copy.drop_duplicates(subset=["Project Name","Address","Sale_Date"])
    df_copy[f"within_{months_after_flood}_months_post_flood"] = pd.Series(False,index=df.index,name=f"within_{months_after_flood}_months_post_flood")
    flood_within_months = df_copy.groupby(["SUBZONE_N"]).apply(lambda x: get_flood_within_months(x,sale_date_column=sale_date_column,months_after_flood=months_after_flood)).reset_index(level=[0])
    df_copy.loc[flood_within_months.index,f"within_{months_after_flood}_months_post_flood"] = flood_within_months[f"within_{months_after_flood}_months_post_flood"]
    return df_copy
