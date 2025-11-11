from API_KEY import get_OneMap_token
import requests

def generate_OneMap_headers():
    """ generates new one map token """
    onemapKey = get_OneMap_token()
    headers = {"Authorization": onemapKey}
    return headers

def get_coordinates_from_location(location,headers):
    """returns number of results found, search value, and coordinates given a supplied location 
    Args:
        location (str): a location in singapore
    Returns:
        tuple: strings corresponding to number of results found, search value, lat, and lon
    """
    
    url = f"https://www.onemap.gov.sg/api/common/elastic/search?searchVal={location}&returnGeom=Y&getAddrDetails=Y&pageNum=1"
        
    response = requests.request("GET", url, headers=headers)
    response = response.json()
    response_first_result = response['results'] # get first item in the list
    return response_first_result