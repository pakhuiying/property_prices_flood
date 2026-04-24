import helper_functions.OneMapAPI as OneMapAPI
import pandas as pd
import os
import numpy as np

# import filepath
# flooding_hotspots = pd.read_csv(r"C:\Users\hypak\OneDrive - Singapore Management University\Documents\Data\Climate Impacts in Singapore\Flooding\flooding_hotspots_2011_2024.csv")
# pub compiled flooding hotspot
FLOODING_HOTSPOTS_FP = r"C:\Users\hypak\OneDrive - Singapore Management University\Documents\Data\Climate Impacts in Singapore\Flooding\flooding_hotspots_2011_2025.csv"
FLOOD_PRONE_FP = r"C:\Users\hypak\OneDrive - Singapore Management University\Documents\Data\Climate Impacts in Singapore\Flooding\flood_prone_2011_2025.csv"
# historical empirical flooding
HISTORICAL_FLOODING_FP = r"Data\SG_pluvial_floods_2013to2025.csv"

# process flooding hotspot data to get hotspot analysis
headers = OneMapAPI.generate_OneMap_headers()
def get_flooding_hotspots(flooding_hotspots_fp=FLOODING_HOTSPOTS_FP,
                          headers=headers,save_fp=None):
    """ Returns the matched location given a flooded location, using the OneMap API
    Note: Same function should work for flood prone fp too
    Args:
        flooding_hotspots_fp (str): filepath to df where each column corresponds to flooding locations in that particular year
        save_fp (str): file to save csv
    Returns:
        pd.DataFrame: long format of flooding hotspot
    """
    flooding_hotspots = pd.read_csv(flooding_hotspots_fp)
    hotspots = []
    for col in flooding_hotspots.columns.to_list():
        locations = flooding_hotspots[col].dropna()
        year = col.split('-')[1]
        year = year.strip()
        for l in locations:
            results = OneMapAPI.get_coordinates_from_location(l,headers=headers)
            # response_found, searchVal, latitude, longitude = flood_utils.get_coordinates_from_location(l)
            # get first result
            try:
                result = results[0]

                hotspots.append({'year':year,
                                'flooded_location':l,
                                'responses_found':len(results), 
                                'matched_location':result['SEARCHVAL'], 
                                'latitude':result['LATITUDE'], 
                                'longitude':result['LONGITUDE']})
            except:
                print(l)
    df = pd.DataFrame(hotspots)
    df = df.sort_values(by=['flooded_location'])
    if save_fp is not None and os.path.exists(os.path.dirname(save_fp)):
        df.to_csv(save_fp,index=False)
    
    return df.dropna(subset=['flooded_location'])

# get_flooding_hotspots(flooding_hotspots, headers,
#                       save_fp = r"Data\flooding_hotspots_2011_2025.csv")

# process SG_pluvial_floods_2013to2025.csv to flood_events_2013_2025.csv
def get_flood_df(historical_floods_fp = HISTORICAL_FLOODING_FP,
                 headers=headers,save_fp=None):
    """ Returns the matched location given a flooded location, using the OneMap API
    Args:
        historical_floods_fp (str): filepath to df where each column corresponds to flooding locations (actual flood empirical data)
        headers (dict): OneMap headers
        save_fp (str): file to save csv
    Returns:
        pd.DataFrame: df of flooding location with their detailed locations
    """
    historical_floods_df = pd.read_csv(historical_floods_fp)

    def get_location(df, headers):
        locations = []
        for row_ix, row in df.iterrows():
            try:
                results = OneMapAPI.get_coordinates_from_location(row['flooded_location'],headers)
                result = results[0]
                index = list(result)
                locations.append(result)
            except:
                locations.append({i: np.nan for i in index})
        return pd.DataFrame(locations)
    
    historical_floods_df['flooded_location'] = historical_floods_df["Location_Road"].apply(lambda x: [i.strip() for i in x.split(',')])
    historical_floods_df = historical_floods_df.explode(column=['flooded_location'])
    historical_floods_df_locations = get_location(historical_floods_df, headers)

    historical_floods_df = historical_floods_df.reset_index(drop=True)
    flood_df = pd.concat([historical_floods_df,historical_floods_df_locations],axis=1)

    if save_fp is not None and os.path.exists(os.path.dirname(save_fp)):
        flood_df.to_csv(save_fp,index=False)
    
    return flood_df