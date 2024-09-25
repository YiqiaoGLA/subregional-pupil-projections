### this is a file containing a lot of exploratory data analysis. It is very messy and is not intended to be updated with new data.
### I have kept it here because there some useful plots and notes, but for anyone reading through the repo to get a sense of how the data processing and modelling works, it's not recommended to look through this file. 


## 0. libraries and functions

library(data.table)

source("functions/fn_aggregate_over_columns.R")

## 1. reading in datasets

pupils <- fread("data/processed_data/pupil_numbers/itl_pupil_numbers_1112_to_2223.csv")

births <- fread("data/processed_data/births/itl_births_92_to_23.csv")


#### more stuff (to be reorganised later)


## creating the lagged date variable for births. By isolating year out from births and adding 4. (we also need to isolate a 4-digit year out from pupil numbers)

births[, year := (tstrsplit(date, "-", fixed = TRUE)[1])]
births[, year := as.numeric(year)]

births <- births[!(year <= 2006) ,] # getting rid of anything before 2006. 2006 is the first year of useful data we might have

births[, year_lag_4 := year + 4]
births[year_lag_4 > 2022 | year_lag_4 < 2011, year_lag_4 := NA] # setting any greater than 2022 or lesser than 2011 as NA - we can't use these, and for the lagged variable we want the years to align exactly with reception years

births[, year_lag_5 := year + 5]
births[year_lag_5 > 2022 |year_lag_5 < 2011, year_lag_5 := NA]


## isolating out year for reception - it's just the first four digits
pupils[, year := as.numeric(substr(time_period, 1, 4))]


## creating various different datasets, with different aggregations and selections, that we'll want to use (might be useful to  do this in a separate file and then save them? Or is there any point - will we be using them for anything else?). 
## I will calculate ratios as needed, before the plots. One reason is because ratios aren't foundational to this piece of work and are only worth calculating to illustrate a specific point. Another is that there will be then be too many complicated permutations of possible datasets and matchings for which to calculate ratios. Something has to give, and it will have to be the ratios. 

lond_itls <- c("TLI3", "TLI4", "TLI5", "TLI6", "TLI7")


  ### london itl-level datasets
pupils_lond_itl <- pupils[itl221cd %in% lond_itls, ]

births_lond_itl <- births[gss_code %in% lond_itls, ]


  ### aggregated london datasets
pupils_lond_agg <- aggregate_over_columns(
  data = pupils[itl221cd %in% lond_itls, ],
  columns_to_remove = c("itl221cd"),
  columns_to_sum = c("full_time", "part_time", "headcount", "fte")
)

births_lond_agg <- aggregate_over_columns(
  data = births[gss_code %in% lond_itls, ], 
  columns_to_remove = c("gss_code", "gss_name"),
  columns_to_sum = c("annual_births")
)


  ### aggregated across UK datasets
pupils_uk_agg <- aggregate_over_columns(
  data = pupils, 
  columns_to_remove = c("itl221cd"),
  columns_to_sum = c("full_time", "part_time", "headcount", "fte")
)

births_uk_agg <- aggregate_over_columns(
  data = births, 
  columns_to_remove = c("gss_code", "gss_name"),
  columns_to_sum = c("annual_births")
)


## plots!

  ### UK level and London level, simple line charts of births and reception. No lagging or indexing
{
plot(x = births_uk_agg$year, y = births_uk_agg$annual_births, type = "l", col = "blue", lwd = 3, 
     ylim = c(500000, 700000), 
     main = "UK wide, no lag")
lines(x = pupils_uk_agg[nc_year == "reception", ]$year, y = pupils_uk_agg[nc_year == "reception", ]$headcount,
      col = "red", lwd = 3)


plot(x = births_lond_agg$year, y = births_lond_agg$annual_births, type = "l", col = "blue", lwd = 3, 
     ylim = c(90000, 135000), 
     main = "London wide, no lag")
lines(x = pupils_lond_agg[nc_year == "reception", ]$year, y = pupils_lond_agg[nc_year == "reception", ]$headcount,
      col = "red", lwd = 3)
}

  ### same as above, but lagging, both 4 and 5 year
{
  
  

png(file = "plots/adhoc_plots/reception_births.png",
    height = 8, width = 10, units = "in", res = 600)

  plot(x = births_uk_agg$year_lag_4, y = births_uk_agg$annual_births, type = "l", col = "blue", lwd = 3, 
     ylim = c(600000, 700000), 
     main = "Comparison of UK-wide reception with births 4 years previously", las = 1, bty = "n", xlab = "Year", ylab = "")
lines(x = pupils_uk_agg[nc_year == "reception", ]$year, y = pupils_uk_agg[nc_year == "reception", ]$headcount,
      col = "red", lwd = 3)
legend("topright", legend = c("Reception headcount", "Births 4 years previously"), col = c("red", "blue"), lwd = c(3,3))

dev.off()


plot(x = births_lond_agg$year_lag_4, y = births_lond_agg$annual_births, type = "l", col = "blue", lwd = 3, 
     ylim = c(90000, 135000), 
     main = "London wide, 4-year lag")
lines(x = pupils_lond_agg[nc_year == "reception", ]$year, y = pupils_lond_agg[nc_year == "reception", ]$headcount,
      col = "red", lwd = 3)
}

  ### same as above, 4-year lag, but indexed
{
plot(x = births_uk_agg$year_lag_4, y = births_uk_agg$annual_births/643325.0, type = "l", col = "blue", lwd = 3, 
     ylim = c(0.98, 1.09), 
     main = "UK wide, 4-year lag, indexed")
lines(x = pupils_uk_agg[nc_year == "reception", ]$year, y = pupils_uk_agg[nc_year == "reception", ]$headcount/608197,
      col = "red", lwd = 3)


plot(x = births_lond_agg$year_lag_4, y = births_lond_agg$annual_births/123215.0, type = "l", col = "blue", lwd = 3, 
     ylim = c(0.95, 1.10), 
     main = "London wide, 4-year lag, indexed")
lines(x = pupils_lond_agg[nc_year == "reception", ]$year, y = pupils_lond_agg[nc_year == "reception", ]$headcount/97781,
      col = "red", lwd = 3)

## very interesting charts above! Change in births and reception is very tightly linked in the UK as a whole over time. But trends diverge in London after approx 2015 - reception falls faster than births! This will be significant for modelling...the relationship between them changes over time. 

}

  ### plot of ratio of births:pupils (or, pupils:births would be better), lagged
{
births_lond_agg[1:12, annual_births]
pupils_lond_agg[nc_year == "reception", headcount]

ratio_pupil_births_lond <- pupils_lond_agg[nc_year == "reception", headcount]/births_lond_agg[1:12, annual_births]

ratio_pupil_births_uk <- pupils_uk_agg[nc_year == "reception", headcount]/births_uk_agg[1:12, annual_births]

plot(x = 2011:2022, y = ratio_pupil_births_lond, col = "purple", lwd = 3, type = "l",
     ylim = c(0.75, 0.95),
     main = "Ratio of reception to births (lagged)\n London (purple) and UK total (orange)")
lines(x = 2011:2022, y = ratio_pupil_births_uk, col = "orange", lwd = 3)
}

  ### lines disaggregated by London ITLs, each ITL has the same colour but the alpha is different (or one is bold, one is not....etc)
  ### or ratios could express this better. If using numbers not indexed then the relationship between the change of both series isn't as clear. If using index then all lines will be too close.
  ### ratios will show if the rate of divergence between the time series is spread equally across all London ITLS. 
{
births_lond_1122 <- births_lond_itl[year_lag_4 < 2023 & year_lag_4 > 2010, ]
reception_lond_1122 <- pupils_lond_itl[nc_year == "reception", ]

setkey(births_lond_1122, "year_lag_4", "gss_code")
setkey(reception_lond_1122, "year", "itl221cd")

births_reception_lond_1122 <- births_lond_1122[reception_lond_1122]

births_reception_lond_1122$ratio <- births_reception_lond_1122$headcount/births_reception_lond_1122$annual_births

plot(x = 1, y = 1, type = "n",
     ylab = "", xlab = "",
     ylim = c(0.5, 0.97), xlim = c(2011, 2022))

cols <- c("black", "red", "darkgreen", "blue", "orange")
itl_names <- c("Inner London - West", "Inner London - East", 
               "Outer London - East and North East", "Outer London - South", "Outer London - West and North West")

col_ind <- 1

for(i in lond_itls){
  
  lines(x = 2011:2022, y = births_reception_lond_1122[gss_code == i, ]$ratio,
        lwd = 3, col = cols[col_ind])
  
  col_ind <- col_ind + 1
  
}

legend("topright", legend = itl_names, fill = cols)
title(main = "Ratio of reception:births with 4 year lag, London ITLs", cex.main = 1.5)

}

  ### similar plot to above, but with all lines for all ITLs plotted, London ITLs in one colour, all other ITLs in another
{
    #### data stuff before plotting
births_1122 <- births[year_lag_4 > 2010 & year_lag_4 < 2023, ]

reception_1122 <-  pupils[nc_year == "reception"]

setkey(births_1122, "gss_code", "year_lag_4")
setkey(reception_1122, "itl221cd", "year")

births_reception_1122 <- births_1122[reception_1122]

births_reception_1122$ratio <- births_reception_1122$headcount/births_reception_1122$annual_births

all_itls <- unique(births_reception_1122$gss_code)

non_lond_itls <- all_itls[!(all_itls %in% lond_itls)]


    #### setting up the plot frame
plot(x = 1, y = 1, type = "n",
     ylab = "", xlab = "",
     ylim = c(0.5, 1.08), xlim = c(2011, 2022),
     las = 1, bty = "n")

    #### setting the colours
non_lond_col <- rgb(204, 204, 204, 100, maxColorValue = 255) # grey

lond_cols <- c(
  rgb(109, 167, 222, 255, maxColorValue = 255), # blue
  rgb(158, 0, 89, 255, maxColorValue = 255), # dk pink (clearly purple?)
  rgb(94, 161, 93, 255, maxColorValue = 255), # green
  rgb(238, 38, 109, 255, maxColorValue = 255), # ldn pink
  rgb(235, 134, 30 , 255, maxColorValue = 255) # orange
)

itl_lond_names <- c("Inner London - West", "Inner London - East", 
               "Outer London - East and North East", "Outer London - South", "Outer London - West and North West")

    #### non london lines
for(i in non_lond_itls){
  
  lines(x = 2011:2022, y = births_reception_1122[gss_code == i, ]$ratio,
        lwd = 3, col = non_lond_col)
  
}

    #### london lines
col_ind <- 1

for(i in lond_itls){
  
  lines(x = 2011:2022, y = births_reception_1122[gss_code == i, ]$ratio,
        lwd = 3, col = lond_cols[col_ind])
  
  col_ind <- col_ind + 1
  
}

    #### adding the legend, and any other elements
legend(x = 2011, y = 0.68, legend = c("Rest of UK ITLs", itl_lond_names), 
       fill = c(non_lond_col, lond_cols),
       bty = "n", cex = 0.825)
}

  ### plot of ratios (lagged) over years, for all ITLs, using jitters
{
births_1122 <- births[year_lag_4 > 2010 & year_lag_4 < 2023, ] # keeping only 2011 to 2022, for the lagged years
reception_1122 <-  pupils[nc_year == "reception"]


setkey(births_1122, "gss_code", "year_lag_4")
setkey(reception_1122, "itl221cd", "year")

births_reception_1122 <- births_1122[reception_1122]

births_reception_1122$ratio <- births_reception_1122$headcount/births_reception_1122$annual_births

uk_lond_col <- c("orange", "purple")

inlond_cond <- births_reception_1122$gss_code %in% lond_itls

col_index <- inlond_cond + 1

plot_cols <- uk_lond_col[col_index]

plot(x = jitter(births_reception_1122$year_lag_4), y = births_reception_1122$ratio, 
     pch = 20, col = plot_cols,
     main = "Reception:births ratio by year, at ITL2-level\nLondon(purple) and non-London (orange) ITLs")
}


## to check - reception isn't the statutory starting point. Year 1 is. Is it right, then, to be using reception? (in a sense, yes it is, no matter what the answer is, because that is what Boroughs want)



# next large plotting series - seeing how progression through years vary. One of the main questions that I'll need to answer is what the trend of the ratio is over time. 
# and I think there should be some particular attention to year 7. Is it good enough to use survival from years reception to 6? Or should there be information from population and births? Hardly births...

# we need lots of little sub time series...for reception in e.g. 2011, we need year 1 in 2012, year 2 in 2013....etc
# easiest to just do this for one at a time, so do a uk wide one and a then london wide one. Probably no need to go down to ITL level for now
# yes, I should get on to the modelling of reception, but I do want to have my own understanding of progression through school before I do that. It won't take too long to explore this. 

unique(pupils_uk_agg$nc_year)

## uk wide, lines of progression through school, as index of reception

ordered_nc_year <- c("reception", "year_group_1", "year_group_2", "year_group_3", "year_group_4",
                     "year_group_5", "year_group_6", "year_group_7", "year_group_8", "year_group_9",
                     "year_group_10", "year_group_11", "year_group_12", "year_group_13", "year_group_14")

pupils_uk_agg[, nc_year_date := paste(nc_year, year, sep = "_")]


  ### do for one year, 2011. Then convert into a for loop - everything below this. But need to plot an empty frame first

  ### up to 2015 - last year we can see the drop from year 6 to 7
plot(x = 1, y = 1, type = "n", bty = "n", xlab = "", ylab = "", las = 1,
     xlim = c(2011, 2022), ylim = c(0.98, 1.02))

cols <- c("lightgray", "pink", "lightgreen", "skyblue", "yellow")

col_ind <- 1

for(i in 2011:2015){

  year_seq <- i:2022

  nc_year_seq <- ordered_nc_year[1:length(year_seq)]

  nc_year_date_seq <- paste(nc_year_seq, year_seq, sep = "_")

  series_to_plot <- pupils_uk_agg[nc_year_date %in% nc_year_date_seq, headcount]

  series_to_plot_index <- series_to_plot/series_to_plot[1]

  lines(x = year_seq, y = series_to_plot_index, lwd = 3, 
      col = cols[col_ind])

  col_ind <- col_ind + 1

}


## NEW 22072024 - SHOWING TRACKING THROUGH YEAR

plot(x = 1, y = 1, type = "n", bty = "n", xlab = "", ylab = "", las = 1,
     xlim = c(2011, 2022), ylim = c(595000, 620000))

cols <- c("lightgray", "pink", "lightgreen", "skyblue", "yellow")

col_ind <- 1
i <- 2011

for(i in 2011){
  
  year_seq <- i:2022
  
  nc_year_seq <- ordered_nc_year[1:length(year_seq)]
  
  nc_year_date_seq <- paste(nc_year_seq, year_seq, sep = "_")
  
  series_to_plot <- pupils_uk_agg[nc_year_date %in% nc_year_date_seq, headcount]
  
  series_to_plot_index <- series_to_plot/series_to_plot[1]
  
  lines(x = year_seq, y = series_to_plot, lwd = 3, 
        col = "blue")
  
  col_ind <- col_ind + 1
  
}


png(file = "plots/adhoc_plots/cohort_progression.png",
    height = 8, width = 10, units = "in", res = 600)

plot(x = 1, y = 1, type = "n", bty = "n", xlab = "", ylab = "", las = 1,
     xlim = c(2011, 2022), ylim = c(595000, 620000))

year_seq <- 2011:2022

nc_year_seq <- ordered_nc_year[1:length(year_seq)]

nc_year_date_seq <- paste(nc_year_seq, year_seq, sep = "_")

series_to_plot <- pupils_uk_agg[nc_year_date %in% nc_year_date_seq, headcount]

lines(x = year_seq, y = series_to_plot, lwd = 3, 
      col = "blue")

title(main = "Cohort progression from Reception-2011 to Year Eleven-2022", cex.main = 1.5)

dev.off()


## END OF TRACKING




  ### 2016 to 2022. Data on progression through primary school only

plot(x = 1, y = 1, type = "n", bty = "n", xlab = "", ylab = "", las = 1,
     xlim = c(2016, 2022), ylim = c(0.98, 1.02))

cols <- c("lightgray", "pink", "lightgreen", "skyblue", "yellow", "orange")

col_ind <- 1

for(i in 2016:2021){
  
  year_seq <- i:2022
  
  nc_year_seq <- ordered_nc_year[1:length(year_seq)]
  
  nc_year_date_seq <- paste(nc_year_seq, year_seq, sep = "_")
  
  series_to_plot <- pupils_uk_agg[nc_year_date %in% nc_year_date_seq, headcount]
  
  series_to_plot_index <- series_to_plot/series_to_plot[1]
  
  lines(x = year_seq, y = series_to_plot_index, lwd = 3, 
        col = cols[col_ind])
  
  col_ind <- col_ind + 1
  
}


    #### takeaways? (1) large drop from primary to secondary, even when all geographies are taken into account. 
    #### (2) there is consistently a large increase in headcount from reception to year 1, and then it stabilises at year 1. So yes, it does look like there is a significant number of families who choose to only enter their children into school at the statutorily-required time, not before. 
    #### (3) serious covid distortions! And these are the last few periods in the time series. So when modelling progression through school, it might be good to weight these recent year less...or at the very least to choose a method that doesn't give more recent periods more weight than older ones.
    #### (4) does not look like the ratio of year 1:reception is consistent throughout time or varies randomly. Looks like there is a trend. So we shouldn't apply a consistent ratio over time. Could either model this ratio separately. Or, we could model the relationship from births to year 1 with a 5 year lag, separately? And then even measure progression based on year 5 rather than reception.



## london wide, lines of progression through school, as index of reception

ordered_nc_year <- c("reception", "year_group_1", "year_group_2", "year_group_3", "year_group_4",
                     "year_group_5", "year_group_6", "year_group_7", "year_group_8", "year_group_9",
                     "year_group_10", "year_group_11", "year_group_12", "year_group_13", "year_group_14")

pupils_lond_agg[, nc_year_date := paste(nc_year, year, sep = "_")]


### do for one year, 2011. Then convert into a for loop - everything below this. But need to plot an empty frame first

### up to 2015 - last year we can see the drop from year 6 to 7
plot(x = 1, y = 1, type = "n", bty = "n", xlab = "", ylab = "", las = 1,
     xlim = c(2011, 2022), ylim = c(0.875, 1.02))

cols <- c("lightgray", "pink", "lightgreen", "skyblue", "yellow")

col_ind <- 1

for(i in 2011:2015){
  
  year_seq <- i:2022
  
  nc_year_seq <- ordered_nc_year[1:length(year_seq)]
  
  nc_year_date_seq <- paste(nc_year_seq, year_seq, sep = "_")
  
  series_to_plot <- pupils_lond_agg[nc_year_date %in% nc_year_date_seq, headcount]
  
  series_to_plot_index <- series_to_plot/series_to_plot[1]
  
  lines(x = year_seq, y = series_to_plot_index, lwd = 3, 
        col = cols[col_ind])
  
  col_ind <- col_ind + 1
  
}


## SHOWING THE TRACKING THROUGH A COHORT

plot(x = 1, y = 1, type = "n", bty = "n", xlab = "", ylab = "", las = 1,
     xlim = c(2011, 2022), ylim = c(0.875, 1.02))

cols <- c("lightgray", "pink", "lightgreen", "skyblue", "yellow")

col_ind <- 1

for(i in 2011){
  
  year_seq <- i:2022
  
  nc_year_seq <- ordered_nc_year[1:length(year_seq)]
  
  nc_year_date_seq <- paste(nc_year_seq, year_seq, sep = "_")
  
  series_to_plot <- pupils_lond_agg[nc_year_date %in% nc_year_date_seq, headcount]
  
  series_to_plot_index <- series_to_plot/series_to_plot[1]
  
  lines(x = year_seq, y = series_to_plot_index, lwd = 3, 
        col = cols[col_ind])
  
  col_ind <- col_ind + 1
  
}



## END OF THE TRACKING




### 2016 to 2022. Data on progression through primary school only

plot(x = 1, y = 1, type = "n", bty = "n", xlab = "", ylab = "", las = 1,
     xlim = c(2016, 2022), ylim = c(0.925, 1.02))

cols <- c("lightgray", "pink", "lightgreen", "skyblue", "yellow", "orange")

col_ind <- 1

for(i in 2016:2021){
  
  year_seq <- i:2022
  
  nc_year_seq <- ordered_nc_year[1:length(year_seq)]
  
  nc_year_date_seq <- paste(nc_year_seq, year_seq, sep = "_")
  
  series_to_plot <- pupils_lond_agg[nc_year_date %in% nc_year_date_seq, headcount]
  
  series_to_plot_index <- series_to_plot/series_to_plot[1]
  
  lines(x = year_seq, y = series_to_plot_index, lwd = 3, 
        col = cols[col_ind])
  
  col_ind <- col_ind + 1
  
}

    #### takeaways from the London ones, in comparison to rest of UK. 
    #### (1) much, much more pronounced attrition throughout all years, including 6 to 7, in London than compared to UK as a whole. 
    #### (2) still a notable increase from reception to year 1. But looks more muted (would need more specific analysis to check), partly because the y axis axis is greater to accommodate the larger drops in later years, but also probably because some families will have moved out of London between reception and year 1.
    #### (3) more covid distortions, of course, particularly in the early years of school it seems. 
    #### (4) but even before covid, the trend from reception to year 1 had reversed! It went from more children in year 1 than reception, to fewer children in year 1 than reception. A very, very, very interesting trend.
    #### (4, continued). So yes, the transition from reception to year 1 needs special attention and more careful modelling. 
    #### (5) also, yes, modelling needs to be geography specific.



