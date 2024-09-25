

## 0. setting up libraries and functions

library(data.table)

source("functions/fn_convert_geographies_to_latest.R")
source("functions/fn_aggregate_geographies_2.R")


## 1. reading in the la level pupil data

pupil_data <- fread("data/processed_data/pupil_numbers/pupil_numbers_1112_to_2223.csv")


## 2. getting rid of other la identifiers (because it will mess up the aggregation later. We need only one column that uniquely identifies local authorities)

pupil_data <- pupil_data[, -c("old_la_code", "la_name")]


## 3. fitting all LAs in the dataset to 2021 LA boundaries (in some of the older datasets, e.g. 2011/2012, the local authority codes are older ones, before various mergings and splitting and renamings. This section matches all of those to 2021 codes)

  ### 3.1. creating the lookup
old_las <- c("E06000028",
             "E06000029",
             "E06000048",
             "E06000061",
             "E06000062",
             "E08000020",
             "E10000002",
             "E10000009",
             "E10000021")


new_las <- c("E06000058",
             "E06000058",
             "E06000057",
             "E10000021",
             "E10000021",
             "E08000037",
             "E06000060",
             "E06000059",
             "E10000021")


new_old_la_lookup <- data.table(
  old_las = old_las,
  new_las = new_las
)


  ### 3.2. aggregating to new LAs
pupil_data <- convert_geographies_to_latest(
  data = pupil_data,
  lookup = new_old_la_lookup,
  geog_from_data = "new_la_code",
  geog_from_lookup = "old_las", 
  geog_to_lookup = "new_las",
  count_names = c("full_time", "part_time", "headcount", "fte")
)



## 4. fitting the local authorities to ITL2 regions

lookup <- fread("lookups/la_itl_lookup.csv")


  ### 4.1. for each local education authority that is coded as a county rather than a local authority (because in the source datasets, the geographies are bit weird and it's a mix of local authorities and some counties), we need to change it to a code that will have a match in the la-itl lookup. 
  ### What we will do is change it to the code corresponding to any of its constituent local authorities. It doesn't matter which one - they will all map onto the correct itl. 

la_county_lookup <- fread("lookups/la_county_lookup.csv")

counties_to_replace <- unique(pupil_data$new_las)[!(unique(pupil_data$new_las) %in% unique(lookup$lad21cd))] # extracting counties in the pupil dataset, which won't have any match in the la-itl lookup

counties_to_replace <- counties_to_replace[counties_to_replace != "E10000021"] # removing Northamptonshire. This one has to be treated differently. 

    #### looping through each county code in the dataset, and changing that county code to one of (any of) its constituent local authorities
for(i in counties_to_replace){
  
  la_to_replace <- la_county_lookup[cty21cd == i, lad21cd][1] # taking the  first local authority listed beside the county to replace, to replace that county. 
  
  pupil_data[new_las == i, new_las := la_to_replace]
  
}

    #### for Northampton, we need to just change it manually to one of its constituent local authorities. Because E10000021 was split into E06000061 and E06000062 (Northamptonshire was split into North and West). The only solution I can think of code them both as Northampton before it was split, either with a new code or the pre-split code.

pupil_data[new_las == "E10000021", new_las := "E06000061"]


  ### 4.2. joining and aggregating

pupil_data <- aggregate_geographies_2(
  data = pupil_data, 
  lookup = lookup,
  geog_from_data = "new_las",
  geog_from_lookup = "lad21cd",
  geog_to_lookup = "itl221cd",
  count_names = c("full_time", "part_time", "headcount",  "fte")
)


  ## 5. saving the final file
fwrite(x = pupil_data,
       file = "data/processed_data/pupil_numbers/itl_pupil_numbers_1112_to_2223.csv")


### https://l-hodge.github.io/ukgeog/articles/boundary-changes.html
### the page above tracks boundary changes in LAs, and it accounts for every troublemaking LA in the dataset. Very useful. 

