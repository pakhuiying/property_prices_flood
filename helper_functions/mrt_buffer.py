import numpy as np
import pandas as pd
import copy

def get_train_columns(df_subset,construction_period=6,sale_date_column="Sale_Date"):
    """
    Args:
        df_subset (pd.DataFrame): df for a specific Address and a sale date
        construction_period (int): number of months prior to opening of mrt
    Returns:
        pd.Series: that describes number of existing and upcoming train stns and their mrt lines per transaction record
    """
    stn_dict = {"existing_stn_count": 0, "upcoming_stn_count": 0, "stn_lines":"none"}
    if df_subset["opening_date"].notna().sum() > 0:
        train_date = df_subset["opening_date"]
        train_date_soon = train_date - pd.DateOffset(months=construction_period)
        train_year = df_subset["year_train"]
        sale_date = df_subset[sale_date_column]
        sale_year = df_subset["year_property"]

        mask0 = ((sale_date >= train_date_soon) & (sale_date < train_date)) | (sale_year == train_year - 1)
        stn_dict["upcoming_stn_count"] = mask0.sum()
        mask1 = sale_date >= train_date
        stn_dict["existing_stn_count"] = mask1.sum()
        valid_mrt_lines = df_subset.loc[(mask0|mask1),"mrt_line"].to_list()
        stn_dict["stn_lines"] = ','.join(sorted(set(valid_mrt_lines)))

        return pd.Series(stn_dict, index = list(stn_dict))
   
    return pd.Series(stn_dict, index = list(stn_dict))

def add_mrt_buffer(df_property, df_train, 
                    address_column = "Address", sale_date_column = "Sale_Date",
                   construction_period = 6):
    """
    Args:
        df_property (gpd.GeoDataFrame): df describing property transaction info and property attributes
        df_train (gpd.GeoDataFrame): df describing train lines, code, and opening dates and buffer of 400m radius
    Returns:
        pd.DataFrame: that adds columns describing number of existing and upcoming train stns and their mrt lines per transaction record
    """
    residential_train = df_property.sjoin(df_train,how="left",rsuffix="train",lsuffix="property")
    print("length of df after spatial joining residential and train df: ",len(residential_train))
    residential_train_columns = residential_train.groupby([address_column,sale_date_column]).apply(lambda x: get_train_columns(x,construction_period=construction_period, sale_date_column=sale_date_column)).reset_index(level=[0,1])
    print("length of train df: ",len(residential_train_columns))
    residential_train = df_property.merge(residential_train_columns,on=[address_column,sale_date_column])
    print("length of df after merging residential and train df: ",len(residential_train))
    return residential_train