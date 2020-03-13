## cansim 0.3.5
### Minor changes
- Exclude all vignettes and example code from compilation as this may cause CRAN check errors when StatCan servers are down or otherwise temporarily unavailable


## cansim 0.3.4

### Minor changes
- Expand get_cansim_table_notes functionality
- Add functionality to access the new cube list API

## cansim 0.3.3

### Minor changes
- Fix time zone problem when parsing and formatting times for the StatCan API

## cansim 0.3.2

### Minor changes
- Adjust package for changes in StatCan API with different metadata format

## cansim 0.3.1

### Major changes
- Fixes issues arising from StatCan changing their API row limit

### Minor changes
- Optimize vector retrieval by REF_DATE

## cansim 0.3.0

### Major changes
- Fixes issues arising from StatCan changing their API
- Member Names come concatenated with Classification Code by default, this could break existing code. 

### Minor changes
- Adds option to change fields to factors
- Adds option to strip Classification Codes from fields
- Exposes timeout limit to deal with slow connections and large tables

## cansim 0.2.3

### Minor changes
- More robust cansim table download
- Cleanup documentation

## cansim 0.2.2

### Major changes
- French metadata for retrieved tables now implemented
- Reference article vignettes now built for site only

## cansim 0.2.1

### Minor changes

- License file made more clear

## cansim 0.2.0

### Major changes
- Initial release
  
