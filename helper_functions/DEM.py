from osgeo import gdal
import numpy as np

SG_DEM = r"C:\Users\hypak\OneDrive - Singapore Management University\Documents\Data\FABDEM_SG_30m\FABDEM_SG_30m_Clipped.tif"
SG_DEM_raster = gdal.Open(SG_DEM)
# print("Projection is {}".format(SG_DEM_raster.GetProjection()))
# get geotransform
geotransform_SG_DEM_raster = SG_DEM_raster.GetGeoTransform()

# band = SG_DEM_raster.GetRasterBand(1)
# # print("Band Type={}".format(gdal.GetDataTypeName(band.DataType)))

# min_ = band.GetMinimum()
# max_ = band.GetMaximum()
# if not min_ or not max_:
#     (min_,max_) = band.ComputeRasterMinMax(True)
# print("Min={:.3f}, Max={:.3f}".format(min_,max_))

# read as array
SG_DEM_raster_arr = np.array(SG_DEM_raster.GetRasterBand(1).ReadAsArray())

def get_coords(dx,dy, geotransform):
    """" get coordinate with any array index
    Args:
        dx (int): column pixel from the origin (upper left corner)
        dy (int): row pixel from the origin (upper left corner)
    """
    # origin
    px = geotransform[0]
    py = geotransform[3]
    # pixel size
    rx = geotransform[1]
    ry = geotransform[5]
    x = dx*rx + px
    y = dy*ry + py
    return y,x

def get_DEM_value(lon,lat,geotransform=geotransform_SG_DEM_raster):
    """
    get DEM value in meters, given coordinates in lon and lat
    Returns:
        dx (int): column pixel from the origin (upper left corner)
        dy (int): row pixel from the origin (upper left corner)
    """
    # origin
    px = geotransform[0]
    py = geotransform[3]
    # pixel size
    rx = geotransform[1]
    ry = geotransform[5]
    dx = (lon -px)/rx
    dy = (lat - py)/ry
    return round(dx), round(dy)

# bukit timah elevation validation (163.63m)
# dx, dy = get_DEM_value(103.7763750,1.3546806,geotransform_SG_DEM_raster)
# SG_DEM_raster_arr[dy, dx]