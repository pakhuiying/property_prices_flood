
import helper_functions.OneMapAPI as OneMapAPI
import pandas as pd
import numpy as np
import re
import os

def postal_to_coord(row,column_names,headers):
    try:
        searchVal = OneMapAPI.get_coordinates_from_location(row,headers)
        searchVal = searchVal[0]
    except:
        searchVal = {c:np.nan for c in column_names}
    return pd.Series(searchVal,index=list(searchVal))

def extract_road_names(text):
    
    pattern = r"^(.*?)\s*(?:\bat\b|[-–—@])\s*(.*)$"

    match = re.search(pattern, text, re.IGNORECASE)
    if match:
        work_types = match.group(1).strip()
        location = match.group(2).strip()
    else:
        pattern = r"^(.*?)\s*\b(?:in|to|along|of|for)\s*(.*)$"

        match = re.search(pattern, text, re.IGNORECASE)
        if match:
            work_types = match.group(1).strip()
            location = match.group(2).strip()
        else:
            work_types = None
            location = text
    
    return pd.Series({"work_types":work_types,"location":location},index=["work_types","location"])


# Find multiple roads and split them if applicable

def extract_multiple_road_names(text):

    # Remove extra whitespace
    text = text.strip()

    def split_separator(s):
        # Otherwise, split by common separators (/, &, to, ,)
        pattern = r'\s*(?:/|&|,|\bto\b)\s*'
        parts = re.split(pattern, s, flags=re.IGNORECASE)
        return [p.strip() for p in parts if p.strip()]
    
    # 1. Handle parentheses first — extract before and inside ()
    bracket_match = re.match(r'^(.*?)\s*\(([^)]+)\)\s*$', text)
    if bracket_match:
        before = bracket_match.group(1).strip()
        inside = bracket_match.group(2).strip()
        
        return [before] + split_separator(inside)

    return split_separator(text)

def recategorise_drainage_works(text):
    text = text.strip()
    # Check if the entire string matches "drainage work(s)"
    if re.match(r"^Drainage\s+Work(.*)$",text, flags=re.IGNORECASE):
        cat = "Drainage Work"
    elif re.match(r"Road\s+Raising(.*)$",text, flags=re.IGNORECASE):
        cat = "Road Raising"
    elif re.match(r"Improvement\s+to\s+(.*)Outlet\s+Drain(.*)$",text, flags=re.IGNORECASE):
        cat = "Improvement to Outlet Drain"
    elif re.match(r"Improvement\s+(to|of)(.*)Drain(.*)$",text, flags=re.IGNORECASE):
        cat = "Improvement to Drains"
    elif re.match(r"Improvement\s+to\s+(.*)(Sungei|River|Canal|Culvert)(.*)$",text, flags=re.IGNORECASE):
        cat = "Improvement to River/Canal/Culvert"
    elif re.match(r"Construction\s+of\s+(.*)(Detention\s+Tank|Canal)(.*)$",text, flags=re.IGNORECASE):
        cat = "Construction of Detention Tanks/Canal"
    else:
        cat = text
    return cat