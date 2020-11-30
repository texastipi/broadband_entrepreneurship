# make sure to install these packages before running:
# pip install pandas
# pip install sodapy

import pandas as pd
from sodapy import Socrata

# Unauthenticated client only works with public data sets. Note 'None'
# in place of application token, and no username or password:
# client = Socrata("opendata.fcc.gov", None)

# Example authenticated client (needed for non-public datasets):
client = Socrata(opendata.fcc.gov,
                  MyAppToken,
                  userame="user@example.com",
                  password="AFakePassword")

# First 2000 results, returned as JSON from API / converted to Python list of
# dictionaries by sodapy.
results = client.get("whue-6pnt", limit=2000)

# Convert to pandas DataFrame
results_df = pd.DataFrame.from_records(results)
