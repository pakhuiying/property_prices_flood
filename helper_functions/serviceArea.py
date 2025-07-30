import geopandas as gpd
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import networkx as nx
import osmnx as ox
import os
from shapely.geometry import LineString
from shapely.geometry import Point
from shapely.geometry import Polygon
import shapely.ops as so
import pandas as pd
import numpy as np

# add a buffer of 400m
def add_buffer(gdf,buffer_dist=400, crs="EPSG:4326",plot=True):
    """ 
    returns a GeoSeries (polygon geometry) of the buffered area
    Args:
        gdf (Geo.DataFrame)
        buffer_dist (float): distance in meters
        plot (bool): If True, plot geoseries of the buffered area
    Returns:
        gpd.GeoSeries
    """
    gdf_buffer = gdf.to_crs({'proj':'cea'})
    gdf_buffer = gdf_buffer['geometry'].buffer(buffer_dist)
    gdf_buffer = gdf_buffer.to_crs(crs)
    if plot:
        gdf_buffer.plot(fc="None", ec="k",alpha=0.7)

    return gdf_buffer

class GetServiceArea:
    def __init__(self, G, df,radius=400, crs="EPSG:4326"):
        """ 
        Args:
            G (MultiDiGraph): graph of walking or driving network
            df (gpd): gpd of spatial data
            crs (str): see gpd documentation for crs options
            radius (float): distancce radius in metres
        """
        self.G = G
        self.crs = crs
        self.df = df
        self.radius=radius

    def get_nearest_nodes(self,latitude_column, longitude_column):
        """ 
        Args:
            GeoDataFrame that has a geometry column to extract coordinates
        Returns:
            np.ndarray: nodes ID that may be in graph G
        """
        # x, y = lon, lat
        nearest_nodes = ox.distance.nearest_nodes(self.G, self.df[longitude_column], self.df[latitude_column])
        return nearest_nodes

    def plot_nodes(self, nodes_list, node_colour='#0d0887',edge_colour="#999999",node_size=15, ax=None):
        """ 
        Args:
            bus_c (str): hex colour to colour bus nodes
            train_c (str): hex colour to colour train nodes
            node_size (float): size of nodes in G for plotting
            plot (bool): If True, plot the nearest bus stop and train exit nodes
        Returns:
            tuple of np.ndarray: bus stops and train exits nearest nodes in graph G
        """
        if ax is None:
            fig, ax = plt.subplots(1,1)
        # color the nodes according to isochrone then plot the street network
        node_colors = {}
        for node in nodes_list:
            node_colors[node] = node_colour
        nc = [node_colors[node] if node in node_colors else "none" for node in self.G.nodes()]
        ns = [node_size if node in node_colors else 0 for node in self.G.nodes()]
        fig, ax = ox.plot_graph(
            self.G,
            ax=ax,
            node_color=nc,
            node_size=ns,
            node_alpha=0.8,
            edge_linewidth=0.2,
            edge_color=edge_colour,
            show=False,
            close=False
        )
        return 


    def get_serviceArea_nodes(self, nodes_list, node_colour='#0d0887',edge_colour="#999999",node_size=15, ax=None, plot=True):
        """ 
        Args:
            bus_radius (float): walking distance in metres from the nearest bus stops
            train_radius (float): walking distance in metres from the nearest train stops
            bus_c (str): hex colour to colour bus nodes
            train_c (str): hex colour to colour train nodes
            node_size (float): size of nodes in G for plotting
            plot (bool): plot graph and bus and train nodes
        Returns:
            list: identifies all the nodes that are within the bus_radius and train_radius using the service area method
        """
        # color the nodes according to bus/train exits and color them separately
        node_colors = {}
        # identify all nodes within radius
        for node in nodes_list:
            try:
                subgraph_bus = nx.ego_graph(self.G, node, radius=self.radius, distance="length")
                for n in subgraph_bus.nodes():
                    node_colors[n] = node_colour
            except Exception as e:
                pass
                # print(f'{node}: {e}')
        
        if plot:
            # colours of nodes
            nc = [node_colors[node] if node in node_colors else "none" for node in self.G.nodes()]
            # size of nodes
            ns = [node_size if node in node_colors else 0 for node in self.G.nodes()]
            fig, ax = ox.plot_graph(
                self.G,
                node_color=nc,
                node_size=ns,
                node_alpha=0.8,
                edge_linewidth=0.2,
                edge_color=edge_colour,
            )
        return list(node_colors)
    
    def make_convex_hull(self,subgraph):
        """ 
        Args:
            subgraph (MultiGraph): Graph
        Returns:
            Polygon that covers all the nodes in the subgraph, which means points and line strings are excluded and ignored
        """
        node_points = [Point((data["x"], data["y"])) for node, data in subgraph.nodes(data=True)]
        if len(node_points) <= 2:
            raise ValueError(f"Number of nodes less than 2: {len(node_points)}")
        bounding_poly = gpd.GeoSeries(node_points).unary_union.convex_hull
        return bounding_poly

    def get_serviceArea_polygons(self, nodes_list):
        """ 
        Args:
            bus_radius (float): walking distance in metres from the nearest bus stops
            train_radius (float): walking distance in metres from the nearest train stops
        Returns:
            gpd.GeoDataFrame: polygon collection in a gdf, where each polygon represents the service area around each point
        """

        # store all the polygons
        isochrone_polys = []

        # identify all nodes within 200m of the bus stops
        for node in nodes_list:
            try:
                subgraph_bus = nx.ego_graph(self.G, node, radius=self.radius, distance="length")
                bus_poly = self.make_convex_hull(subgraph_bus)
                isochrone_polys.append(bus_poly)
                # break
            except Exception as e:
                pass
                # print(f'{node}: {e}')

        return gpd.GeoDataFrame(geometry=isochrone_polys, crs=self.crs)