from API_KEY import locationIqApiKey
import requests

def get_coordinates_from_location(location,API_key=locationIqApiKey):
    """returns number of results found, search value, and coordinates given a supplied location 
    Args:
        location (str): a location in singapore
    Returns:
        tuple: strings corresponding to number of results found, search value, lat, and lon
    """
    
    url = "https://us1.locationiq.com/v1/search"

    data = {
        'key': API_key,
        'q': location,
        'format': 'json'
    }

    response = requests.get(url, params=data)
    searches = response.json()
    return searches