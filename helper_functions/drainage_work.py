import numpy as np
import pandas as pd
import copy

def get_drainage_period(df_subset, construction_period=6):
    """ 
    Args:
        df_subset (pd.DataFrame) refers to the specific project name that have/will undergo(ne) a specific drainage work
        e.g. residential_drainage[residential_drainage["Project Name"] == "ASCENTIA SKY]
        construction_period (float): number of months that the drainage work will last for
    Returns:
        pd.Series
    """
    # make sure the df_subset is sorted by date
    df_subset = df_subset.sort_values(by="Sale_Date")
    # identify rows which coincides with drainage works
    drainage_entries = df_subset["work_categ"].notna()
    if drainage_entries.sum() > 0:
        # create drainage period series
        drainage_period = pd.Series(np.nan,index=df_subset.index,dtype="string")
        drainage_period.loc[drainage_entries] = "construction"
        # shift drainage completion down by one to indicate after completion
        after_completion = drainage_period.shift(1)
        drainage_period.loc[after_completion=="construction"] = "after"
        # get drainage_date 
        # cannot use this because it will also pick up dates that are not drainage completion dates
        # drainage_date = pd.to_datetime(df_subset["year"].astype(str) + '-' + df_subset["month"].astype(str).str.zfill(2)+ '-28')
        # to get drainage completion date from ROAD_NAME_drainage, rest is na
        drainage_completion = pd.Series(np.nan,index=df_subset.index,dtype='datetime64[ns]')
        drainage_completion_date = df_subset.loc[drainage_entries,["year","month"]]
        drainage_completion.loc[drainage_entries] = pd.to_datetime(drainage_completion_date["year"].astype(str) + '-' + drainage_completion_date["month"].astype(str).str.zfill(2)+ '-28')
        
        # backward fill drainage completion date so that NAs are filled with drainage completion date
        drainage_completion = drainage_completion.bfill()
        # get drainage construction date e.g. 6 months before completion date
        drainage_construction_start = drainage_completion - pd.DateOffset(months=construction_period)
        # get the first drainage construction start date
        first_construction_date = drainage_construction_start.values[0]

        # identify rows before construction start date
        drainage_period.loc[df_subset["Sale_Date"] < first_construction_date] = "before" 
        # assign as construction if sale date is between drainage completion and construction start
        drainage_period.loc[(df_subset["Sale_Date"]>=drainage_construction_start) & (df_subset["Sale_Date"]<drainage_completion)] = "construction"
        # assign as after if sale date is after drainage completion
        # forward fill NA with after
        drainage_period = drainage_period.ffill()
        return drainage_period
    
    else:
        # if work categ rows are all NAs, it means no drainage work has been implemented (untreated), input "never"
        return pd.Series("never",index=df_subset.index)
    
def get_work_categ(df_subset):
    """ 
    Args:
        df_subset (pd.DataFrame) refers to the specific project name that have/will undergo(ne) a specific drainage work
        e.g. residential_drainage[residential_drainage["Project Name"] == "ASCENTIA SKY]
        df after applying get_drainage_period
    Returns:
        pd.Series
    """
    # identify rows which coincides with drainage works
    drainage_entries = df_subset["work_categ"].notna()
    if drainage_entries.sum() > 0:
        # make sure the df_subset is sorted by date
        df_subset = df_subset.sort_values(by="Sale_Date")
        work_categ = df_subset['work_categ'].bfill()
        # create mask for rows with "after"
        mask = (df_subset["drainage_period"]=="after")|(df_subset["drainage_period"]=="before")
        work_categ.loc[mask] = np.nan
        work_categ = work_categ.ffill().fillna(value="none") # so as to fill the rows that are "after", and fill "before" rows with "none"
        return work_categ
    else:
        return pd.Series("none",index=df_subset.index)


def add_drainage_period(df,construction_period=6):
    """                             
    assign before, construction, after, never based on transaction_date and drainage work_date
    Returns:
        pd.DataFrame with added columns describing the stages of drainage work and drainage type
    """
    df_copy = copy.deepcopy(df)
    df_copy["drainage_period"] = pd.Series(np.nan,index=df_copy.index,dtype="string")
    # get drainage period for each project name with at least one drainage work
    drainage_period = df_copy.groupby(["Project Name"]).apply(lambda x: get_drainage_period(x,construction_period=construction_period)).reset_index(level=[0],name="drainage_period")
    df_copy.loc[drainage_period.index,"drainage_period"] = drainage_period["drainage_period"]
    # get work_categ for each project name with at least one drainage work
    work_categ = df_copy.groupby(["Project Name"]).apply(lambda x: get_work_categ(x)).reset_index(level=[0],name="work_categ")
    df_copy.loc[work_categ.index,"work_categ"] = work_categ["work_categ"]
    
    return df_copy