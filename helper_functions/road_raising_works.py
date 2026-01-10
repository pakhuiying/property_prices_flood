import re
import numpy as np
import pandas as pd
import osmnx as ox
import networkx as nx
import copy
import os
import geopandas as gpd
import helper_functions.drainage_work as DrainageWork

def get_road_raising_works(G, df):
    """
    Filter to get road raising df
    Args:
        G (networkx.Graph): graph representing the car network
        df (pd.DataFrame): df output from get_drainage_works_df
        **kwargs: keyword arguments for plotting in ox.plot_graph
    Returns:
        geo.DataFrame: roads/edges with road raising works
    """
    # filter all drainage works to only obtain the road raising works
    road_raising_df = df[df['work_categories'].apply(lambda x: bool(re.match(".*Road Raising.*",x)))]
    return DrainageWork.get_drainage_works_gdf(G, road_raising_df)

def BFS_on_road_raising_works(G, u, v, reverse = True, depth_limit = 2):
    """                    
    Get neighbouring roads surrounding road raising roads
    
    :param G: (networkx.Graph) representing the car network
    :param u: (int) node of one side of an edge
    :param u: node of another side of an edge
    :param reverse: (bool) If True traverse a directed graph in the reverse direction
    :param depth_limit: (float) Specify the maximum search depth
    
    Returns:
        gpd.Geometry
    """
    # get edges_df
    def get_edges_df(G):
        edges_df = ox.graph_to_gdfs(G, nodes=False, edges=True)
        edges_df = edges_df.reset_index(level="key")
        edges = set(edges_df.index)
        return edges_df, edges
    edges_df, edges = get_edges_df(G) # edges_df (pd.DataFrame), edges (unique set of edges_df's index - a tuple of (u,v))
    # get edges using BFS
    edges_BFS_u = list(nx.bfs_edges(G,source=u, reverse=reverse, depth_limit=depth_limit))
    edges_BFS_v = list(nx.bfs_edges(G,source=v, reverse=reverse, depth_limit=depth_limit))
    edges_BFS = set(edges_BFS_u + edges_BFS_v)
    # ensure that edges in edges_BFS are in edges_df's index
    edges_BFS = edges_df.loc[list(edges_BFS & edges)].union_all()
    return edges_BFS

# iterate this over every tuple of (u,v), and add to a geometry list and append back to the original road_raising_df
def get_road_raising_works_df(G, df, reverse = False, depth_limit = 2,
                              plot = True, ax=None):
    """
    Get road raising edges and the downstream edges determined by the depth_limit
    Args:
        G (networkx.Graph): graph representing the car network
        df (pd.DataFrame): df output from get_road_raising_works
        reverse (bool): If True traverse a directed graph in the reverse direction
        depth_limit (float): Specify the maximum search depth
    Returns:
        geo.DataFrame: roads/edges with road raising works
    """
    # store 
    bfs_edges_geometry = {"u":[],"v":[],"geometry":[]}
    for _, row in df.iterrows():
        # get edges nodes
        u = row["u"]
        v = row["v"]
        edges_BFS = BFS_on_road_raising_works(G, u, v, reverse = reverse, depth_limit = depth_limit)
        # store in dict
        bfs_edges_geometry["u"].append(u)
        bfs_edges_geometry["v"].append(v)
        bfs_edges_geometry["geometry"].append(edges_BFS)

    # convert to geodataframe
    road_raising_works_df = gpd.GeoDataFrame(bfs_edges_geometry, crs="EPSG:4326")
    
    # append to the original df by u,v columns
    road_raising_works_df = road_raising_works_df.merge(df.drop(columns=['geometry']),how="right")

    if plot:
        ax = road_raising_works_df.plot(ax=ax,color="green")
        df.plot(ax=ax, color="red")
    
    return road_raising_works_df