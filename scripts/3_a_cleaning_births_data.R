## the births file is already nearly ready to use, as downloaded. This is a product put together by Marta. 
## At the moment I just go to the page and download it - an outstanding to-do is to download it straight from the page.


## 0. setting up libraries and functions

library(data.table)


## 1. reading in data

births <- fread("data/raw_data/births/actual_and_predicted_births.csv")


## 2. cleaning the dataset, for our particular requirements

  ### 2.1. narrowing the dataset down only to ITL2 areas

itl_condition <- grep("TL", births$gss_code, ignore.case = TRUE)

births <- births[itl_condition, ]


  ### 2.2. narrowing down only to up to mid-year estimates, for the best alignment with the cutoff date for school year intake date

months <- tstrsplit(x = births$date, split = "-", fixed = TRUE)[2] # extracting month from the date
months <- unlist(months)

midyear_cond <- months == "07" # saving  the logical condition for midyear dates in a separate vector (doing it this way is better for readability...worse for memory...which should I choose?)

births <- births[midyear_cond, ]


  ### 2.3. removing columns we don't need

to_remove <- c("geography", "sex", # because all entries for geography are now ITL221, and all entries for sex are persons
               "interval_lower", "interval_upper") # if we only have intervals for one or two time periods, there's no point having them. Also, honestly, because births are just an input into another model, there is basically nothing we can do with the uncertainty on births alone anyway

  ### also, as a note, it doesn't really matter if the entry is predicted or actual, but we might as well keep it for now because it's not completely useless information

births <- births[, -..to_remove]


## 3. writing the dataset

fwrite(x = births,
       file = "data/processed_data/births/itl_births_92_to_23.csv") ## TO REVISIT - need to automate this 

