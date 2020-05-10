# URAsimulatedDQ
1. Slide deck of entire framework
2. URA simulated strategy
3. Datasets
  a. resale-flat-prices-based-on-registration-date-from-jan-2017-onwards.csv - raw data obtained from https://data.gov.sg/dataset/resale-flat-prices?resource_id=42ff9cfe-abe5-4b54-beda-c88f9bb438ee
  b. hdb_complete_60k.csv - data from (a) but keeping only selected columns of interest
  c. hdb_complete.csv - complete dataset from (b), only reflecting data from Ang Mo Kio and Bedok for demonstration of code
  d. hdb_missing.csv - dataset from (c) with NAs introduced such that amount of complete observations is ~60% (similar to resi)
  e. hdb_missing_DQframework.csv - 10 rows to show example of DAMA UK DQ framework
