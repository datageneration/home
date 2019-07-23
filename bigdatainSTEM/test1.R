install.packages("reticulate")
library(reticulate)
py_install("matplotlib")
# Scatterplot
hpi16= pd.read_excel("https://github.com/kho777/data-visualization/blob/master/data/hpi2016.xlsx?raw=true",sheet_name='Sheet1', index_col=None, na_values=['NA'])
plt.scatter("Region","Footprint", data=hpi16)
plt.xticks(rotation='vertical')
plt.xticks(rotation=45)

py_install("pandas")
py_install("numpy")

```{python}
# Import packages

import pandas as pd

hpi16= pd.read_excel("https://github.com/kho777/data-visualization/blob/master/data/hpi2016.xlsx?raw=true",sheet_name='Sheet1', index_col=None, na_values=['NA'])

# Exploring data

#List first 5 records
hpi16.head()

```
