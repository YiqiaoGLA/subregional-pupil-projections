

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

    #### for Northampton, we need to just change it manually to one of its constituent local authorities. Why? I can't remember - make some better notes on this.

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



##### THIS IS where the main file ends. Everything below is rough code, testing code, and notes I took throughout the process. Look through, see what's worth keeping, and save it in a nicer format.


### CHECKING TO SEE IF I CAN SAMPLE A RANDOM LA TO STAND IN FOR WHOLE COUNTY

not_in_look <- unique(pupil_data$new_las)[!(unique(pupil_data$new_las) %in% unique(lookup$lad21cd))]

pupil_data[new_las %in% not_in_look,]

la_county_lookup <- unique(la_county_lookup)

not_in_look %in% unique(la_county_lookup$cty21cd) # all there except for Northamptonshire. That one I will need to do manually. 

lookup_for_missing <- la_county_lookup[cty21cd %in% not_in_look, ]

## what do I need...counties missing, corresponding las, and itl regions together, so that I can see if they align. If they align, I can sample any random la within a county as a stand in for that county, and join that to the ITL. If they don't align....I have no solution yet. 
## how do I check "aligning"? All las within a county must be within the same ITL. 
## what would be a misalignment? A county is composed of some las that fall within one ITL, and some that fall within another ITL. In the table it would mean that, for the multiple entries of country codes, some fall under one ITL and some fall under another. 
## I have thought of a way to do with with just code and tables. Might as well, for completeness, as well as checking the table manually 

setkey(lookup, "lad21cd")
setkey(lookup_for_missing, "lad21cd")

check_for_align <- lookup[lookup_for_missing]

to_keep <- c("lad21nm", "cty21nm", "itl221nm")

check_for_align_a <- check_for_align[, ..to_keep] # check by eye manually


  ## checking by code below

matched_county_itl <- unique(check_for_align_a[, c("cty21nm", "itl221nm")])

sum(table(matched_county_itl$cty21nm) != 1) # fantastic...no more than 1 entry per county in a unique county-itl matched table. Means there are no counties split over more than 1 ITL, which in turn means that we can select any random LA within a county to act as the stand in for that entire county, and then use that in the final LAE-ITL lookup to aggregate the pupil data.
## the only thing this doesn't solve is Northamptonshire, but that should easily be sorted by a manual entry. 


### END OF COUNTY-LA CHECKING THING


### some notes below on the difficulties I had with matching. Later, scan through them and save anything important. 

### https://l-hodge.github.io/ukgeog/articles/boundary-changes.html
### the page above tracks boundary changes in LA, and  think it accounts for every single troublemaking LA in my dataset! Which is great.
### I can just map the new codes onto the old codes and aggregate everything. 
### do remember, the new ones won't have a full 720 either. 
### there is an issue with Christchurch that seems to be unfixable, given that we don't have E07 codes. Where was Christchurch pre the 2019 change?
### pretty sure that Christchurch in Dorset, and then after 2019 Dorset loses the population of Poole and the new Bournemouth, Poole, and Christchurch gains it. And we don't have low enough geographies for each year to aggregate up. So this is just a discontinuity. These two new UTLAs won't be consistent over the period. 
### but....it doesn't matter because they're being aggregated up into the same ITL anyway. Great!

### oh no! A UTLA was split. How annoying. E10000021 was split into E06000061 and E06000062 (Northamptonshire was split into North and West). The only solution I can think of code them all as Northampton before it was split, either with a new code or the pre-split code.



