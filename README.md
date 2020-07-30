## Overview

- `convertDataByOrg()` converts the data based on a user provided organization sheet.
- `convertDataByName()` converts the data based on specific type markers such as "_num" and "_char" in the column names.
- `groupData()` organized and filters the data based off the user provided ogranization sheet. It grabs all data of the specified group, splits it by category, sorts, it and then recombines to return a data frame. The final data frame only contains data from the specified group, each column contains data from one sub group, and a new column called catigory exists by which the data can eaisly be sorted.
- `makeGraphs()` creates a list of graphs based on the organionational model of group, subgroup, and category. Specifically, it creates one graph for every subgroup, dividing the data by category. 
- `makePage()` takes in a list of graphs (and optinaly tables/other items) and places them all on one page, then saves it.

### What is the format of the Organization sheet?
Its a data frame with at minimum, 5 columns: `name`, `group`, `subgroup`, `category`, and `type`. `name` contains the column names of your data. Essentialy, this sheet links each column in your data with how it should be grouped/organized as well as its type.
For example:
name | group | subgroup | category | type
------------ | -------------| -------------| -------------| -------------
Q1 | G1 | Null | c1 | numeric
Q2 | G2 | Alt | c2 | numeric

#### Can I have some subgroups in one group and 

### How does the data grouping work?
![img](https://github.com/Eforberger/preview/blob/master/misc/grouping_overview.png)

### What can I customize?

### Why might this be useful?

## Installation
``` r
# install.packages("devtools")
devtools::install_github("eforberger/preview")
```
## Help
If you encounter a clear bug, please file an issue with a minimal
reproducible example to forbe140@umn.edu.

## Details



