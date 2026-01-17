import numpy as np
import pandas as pd
import copy
import geopandas as gpd
import osmnx as ox
import re

def get_drainage_works_gdf(G, df):
    """
    Based on coordinates of drainage work (assume to be polyline), identify the nearest edge in G and create a gdf
    Args:
        G (networkx.Graph): graph representing the car network
        df (pd.DataFrame): df output from get_drainage_works_df
        **kwargs: keyword arguments for plotting in ox.plot_graph
    Returns:
        geo.DataFrame: roads/edges with road raising works
    """
    # get edges from coordinates
    lat = df["LATITUDE"]
    lon = df["LONGITUDE"]
    edges_drain = list(ox.nearest_edges(G,lon,lat))

    # get the polyline of the roads that has drainage works
    edges = ox.graph_to_gdfs(G,nodes=False,edges=True)
    edges_geometry = edges.loc[edges_drain,["geometry"]].reset_index()
    # append geometry to drainage works
    df_copy = copy.deepcopy(df)
    # reset index so the index aligns with the index of edges_geometry_df
    df_copy = df_copy.reset_index(drop=True)
    df_copy[edges_geometry.columns.to_list()] = edges_geometry
    df_copy = gpd.GeoDataFrame(df_copy,geometry=df_copy["geometry"])
    return df_copy

def get_drainage_period(df_subset, sale_date_column="Sale_Date",construction_period=6):
    """ 
    Args:
        df_subset (pd.DataFrame) refers to the specific project name that have/will undergo(ne) a specific drainage work
        e.g. residential_drainage[residential_drainage["Project Name"] == "ASCENTIA SKY]
        construction_period (float): number of months that the drainage work will last for
    Returns:
        pd.Series
    """
    # make sure the df_subset is sorted by date
    df_subset = df_subset.sort_values(by=sale_date_column)
    # identify rows which coincides with drainage works
    drainage_entries = df_subset["work_categories"].notna()
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
        drainage_period.loc[df_subset[sale_date_column] < first_construction_date] = "before" 
        # assign as construction if sale date is between drainage completion and construction start
        drainage_period.loc[(df_subset[sale_date_column]>=drainage_construction_start) & (df_subset[sale_date_column]<drainage_completion)] = "construction"
        # assign as after if sale date is after drainage completion
        # forward fill NA with after
        drainage_period = drainage_period.ffill()
        drainage_period.name = "drainage_period"
        return drainage_period
    
    else:
        # if work categ rows are all NAs, it means no drainage work has been implemented (untreated), input "never"
        return pd.Series("never",index=df_subset.index, name="drainage_period")
    
def get_work_categories(df_subset, sale_date_column="Sale_Date"):
    """ 
    Args:
        df_subset (pd.DataFrame) refers to the specific project name that have/will undergo(ne) a specific drainage work
        e.g. residential_drainage[residential_drainage["Project Name"] == "ASCENTIA SKY]
        df after applying get_drainage_period
    Returns:
        pd.Series
    """
    # identify rows which coincides with drainage works
    drainage_entries = df_subset["work_categories"].notna()
    if drainage_entries.sum() > 0:
        # make sure the df_subset is sorted by date
        df_subset = df_subset.sort_values(by=sale_date_column)
        work_categories = df_subset['work_categories'].bfill()
        # create mask for rows with "after"
        mask = (df_subset["drainage_period"]=="after")|(df_subset["drainage_period"]=="before")
        work_categories.loc[mask] = np.nan
        work_categories = work_categories.ffill().fillna(value="none") # so as to fill the rows that are "after", and fill "before" rows with "none"
        return work_categories
    else:
        return pd.Series("none",index=df_subset.index)


def add_drainage_period(df,construction_period=6,groupby_column=["Project Name"]):
    """                             
    assign before, construction, after, never based on transaction_date and drainage work_date
    Args:
        df (pd.DataFrame): dataframe after merging drainage period to residential df
        groupby_column (list of str): column to split the df by to examine for each specific location e.g. subzone, or building, or project name
    Returns:
        pd.DataFrame with added columns describing the stages of drainage work and drainage type
    """
    df_copy = copy.deepcopy(df)
    df_copy["drainage_period"] = pd.Series(np.nan,index=df_copy.index,dtype="string")
    # get drainage period for each project name with at least one drainage work
    # drainage_period = df_copy.groupby(groupby_column).apply(lambda x: get_drainage_period(x,construction_period=construction_period)).reset_index(level=[0],name="drainage_period")
    drainage_period = df_copy.groupby(groupby_column).apply(lambda x: get_drainage_period(x,construction_period=construction_period)).reset_index(level=[0])
    df_copy.loc[drainage_period.index,"drainage_period"] = drainage_period["drainage_period"]
    # # get work_categories for each project name with at least one drainage work
    # work_categories = df_copy.groupby(groupby_column).apply(lambda x: get_work_categories(x)).reset_index(level=[0],name="work_categories")
    # df_copy.loc[work_categories.index,"work_categories"] = work_categories["work_categories"]
    work_categories = df_copy.groupby(groupby_column).apply(lambda x: x['work_categories'].ffill().bfill().fillna("none")).reset_index(level=[0])
    df_copy.loc[work_categories.index,"work_categories"] = work_categories["work_categories"]
    return df_copy

def get_drainage_density_df(drain_shp,planningArea_shp,planningArea_column="PLN_AREA_N", 
                         plot = True,ax=None,save_fp=None):
    """
    calculate drainage density (km/km2) for each planning area
    Args:
        drain_shp: shapefile to the drainage network
        planningArea (gpd.GeoDataFrame): 
    """
    # ensure same crs
    drain_shp = drain_shp.to_crs(planningArea_shp.crs)
    # convert crs to cea projection for estimation in km
    drain_shp = drain_shp.to_crs({'proj':'cea'})
    planningArea_shp = planningArea_shp.to_crs({'proj':'cea'})
    drainage_length_km = planningArea_shp['geometry'].apply(lambda x: gpd.clip(drain_shp,x)['geometry'].length.sum()/1000)
    Area_km2 = planningArea_shp["geometry"].area/1e6
    drainage_density = drainage_length_km/Area_km2
    drainage_df = pd.DataFrame({planningArea_column: planningArea_shp[planningArea_column],"drainage_length_km": drainage_length_km, 
                                "area_km2":Area_km2, "drainage_density (km/km2)": drainage_density})
    
    if plot:
        planningArea_copy = copy.deepcopy(planningArea_shp)
        planningArea_copy["drainage_length_km"] = drainage_length_km
        planningArea_copy["Area_km2"] = Area_km2
        planningArea_copy["drainage_density (km/km2)"] = drainage_density
        planningArea_copy.plot(column="drainage_density (km/km2)",cmap='OrRd',ec="k",
                               ax=ax,
                               legend=True,
                               legend_kwds={"label": r"Drainage density ($km/km^2$)", 
                                            "orientation": "horizontal"},
                               missing_kwds={
                                    "color": "lightgrey",
                                    "edgecolor": "black",
                                    "hatch": "///",
                                    "label": "Missing data"}
                                    )
    
    if save_fp is not None:
        drainage_df.to_csv(save_fp,index=False)

    return drainage_df

def get_drainage_density(df_subset,drain_df,sale_date_column = "Sale_Date",noData=0):
    """ For each subzone, fill down the same drainage density
    Args:
        df_subset (pd.DataFrame) refers to the specific subzone df_subset
        
    Returns:
        pd.Series
    """
    drainage_density_columns = ["year_drainage_density","drainage_length_km","area_km2","drainage_density (km/km2)"]
    # filter drain_df to match the subzone
    drain_df_subset = drain_df[drain_df["SUBZONE_N"]==df_subset["SUBZONE_N"].values[0]].sort_values(by="year_drainage_density").reset_index(drop=True)
    if len(drain_df_subset) > 0: # means there is drain in the SUBZONE_N
        drain_year = drain_df_subset["year_drainage_density"]
        
        df_subset = df_subset.sort_values(by=sale_date_column)
        df_subset[drainage_density_columns] = df_subset[drainage_density_columns].ffill()#.fillna(noData)
        # check if there's na
        mask = df_subset[drainage_density_columns].isna()
        if mask.all(axis=1).any():
            # obtain the years where there is NA
            year_NA = df_subset.loc[mask.all(axis=1),sale_date_column].dt.year
            # extract the drain_df where drain years are lesser than minimum transaction year
            min_year = year_NA.min()
            year_mask = drain_year < min_year
            drain_NA = drain_df_subset.loc[year_mask,drainage_density_columns]
            # use idxmax to find the latest drain year that is smaller than transaction year
            df_subset.loc[mask.all(axis=1),drainage_density_columns] = drain_NA.iloc[-1].values
            return df_subset
        else:
            return df_subset
    else: # means no drain in SUBZONE_N
        df_subset[drainage_density_columns] = df_subset[drainage_density_columns].fillna(noData)
        return df_subset

        
def add_drainage_density(df,drain_df,sale_date_column = "Sale_Date",noData=0,groupby_column=["SUBZONE_N"]):
    """                             
    add drainage density based on drainage density of specific year and to that subzone
    # TODO: create a buffer of 200 m around drainage df to see if residential areas living near to drains are more/less prone to flooding, since canal overflowing often can lead to higher flood risk
    Args:
        df (pd.DataFrame): dataframe after merging drainage density to residential df
        drain_df (pd.DataFrame): dataframe on drainage density (output from get_drainage_density_df)
        noData (float): for subzones where there are no drainage length data before 2008, assign missing data as noData (default=0)
        groupby_column (list of str): column to split the df by to examine for each specific location e.g. subzone, or building, or project name
    Returns:
        pd.DataFrame with added columns describing the stages of drainage work and drainage type
    """
    df_copy = copy.deepcopy(df)
    drainage_density_columns = ["year_drainage_density","drainage_length_km","area_km2","drainage_density (km/km2)"]
    # drop True because "SUBZONE_N" already exists
    df_copy[drainage_density_columns] = df_copy.groupby(groupby_column).apply(lambda i: get_drainage_density(i,drain_df,sale_date_column = sale_date_column,noData=noData)).reset_index(drop=True)[drainage_density_columns]

    return df_copy

