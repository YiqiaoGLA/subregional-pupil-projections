## this file will makes plots of all results for years 2-11, for each geography. 
## it creates one png with results for all geographies in one grid.
## this is, likely, a one-time script. Meaning there is no/little need to automate it, or to clean it up fully. 


## 0. libraries and functions
library(data.table)

## 1. reading in the datasets

  ### 1.1. reading in projections, adding the ITL names (WHY did I not do this in the original files for creating these datasets)
projections <- fread("output_projections/initial_tenyear/year2_year11_projections.csv")

itl_code_name_lookup <- fread("lookups/itl_code_name_lookup.csv")

setkey(itl_code_name_lookup, "itl22cd")
setkey(projections, "itl22cd")

projections <- itl_code_name_lookup[projections]

  ### 1.2. reading in projections, adding in the ITL names
pupils <- fread("data/processed_data/pupil_numbers/itl_pupil_numbers_1112_to_2223.csv")

pupils[, year := as.numeric(substr(time_period, 1, 4))] ## creating the year variable

colnames(pupils)[colnames(pupils) == "itl221cd"] <- "itl22cd"

setkey(pupils, "itl22cd")

pupils <- itl_code_name_lookup[pupils]


## 2. looping through the years, making the plots

  ### 2.1. setting the vector to loop through, the geographies that will be looped through for each nc_year group, and the plot colours
nc_year_vec <- c("year_group_2", "year_group_3", "year_group_4", "year_group_5", "year_group_6", "year_group_7", "year_group_8", "year_group_9", "year_group_10", "year_group_11")

unique_geogs <- unique(projections[, itl22cd])

real_line_col <- rgb(0, 0, 255, alpha = 255, maxColorValue = 255)

projection_line_col <- rgb(255, 0, 0, alpha = 255, maxColorValue = 255)


  ### 2.2 making and saving the plots - one for each nc year, and each of these will be a grid with all of the geographies in
for(nc_year_select in nc_year_vec){
  
    #### narrowing the datasets down to the nc year we want, aligning the columns of the two datasets
  real_data_nc <- pupils[nc_year == nc_year_select, c("year", "itl22cd", "itl22nm", "headcount")] ## selecting reception, narrowing down to the columns we want
  
  projections_nc <- projections[nc_year == nc_year_select, c("year", "itl22cd", "itl22nm", "pupil_number", "lower_pi", "upper_pi")]
  
  colnames(real_data_nc)[4] <- "mean_projection"
  real_data_nc[, lower_pi := NA]
  real_data_nc[, upper_pi := NA]
  
  colnames(projections_nc)[4] <- "mean_projection"
  
  projections_nc <- rbind(real_data_nc, projections_nc)
  
  projections_nc <- projections_nc[order(year), ]
  
  
    #### starting the png output file, setting the plot parameters
  
  filename <- paste0("plots/all_years_ten_year_forecast/", nc_year_select, ".png")
  
  png(file = filename,
      height = 7, width = 14, units = "in", res = 600)
  
  par(mfrow = c(4, 9))
  par(mar = c(2, 3.25, 1, 0.2))
  
  geog <- "TLE3"
  for(geog in unique_geogs){
    
    #### extracting the name of the itl
    geog_name <- projections_nc[itl22cd == geog, unique(itl22nm)]
    
    #### getting y axis limits
    ylim_max <- max(projections_nc[itl22cd == geog, 
                                c("mean_projection", "upper_pi", "lower_pi")],
                    na.rm = TRUE)
    
    ylim_min <- min(projections_nc[itl22cd == geog, 
                                c("mean_projection", "upper_pi", "lower_pi")],
                    na.rm = TRUE)
    
    ### setting the polygon colour
    polygon_colour <- rgb(255, 0, 0, alpha = 100, maxColorValue = 255)
    
    
    #### making the plot
    plot(x = 1, y = 1, type = "n", bty = "n", las = 1, xlab = "", ylab = "",
         xlim = c(2011, 2032), ylim = c(ylim_min, ylim_max))
    
    lines(x = 2011:2022, y = projections_nc[itl22cd == geog & year %in% 2011:2022, mean_projection],
          col = "blue", lwd = 3)
    
    polygon(x = c(2023:2032, 2032:2023), y = c(projections_nc[itl22cd == geog & year %in% 2023:2032, upper_pi],
                                               rev(projections_nc[itl22cd == geog & year %in% 2023:2032, lower_pi])),
            col = polygon_colour, border = NA)

    lines(x = 2023: 2032, y = projections_nc[itl22cd == geog & year %in% 2023:2032, mean_projection],
          col = "red", lwd = 3)
    
    title(main = geog_name, cex.main = 0.65)
    
  }
  
  #### adding the legend
  plot(x = 1, y = 1, type = "n", bty = "n", las = 1, xlab = "", ylab = "",
       xlim = c(1,1), ylim = c(1, 1), xaxt = "n", yaxt = "n")
  
  legend("topright", legend = c("Headcount", "Projected headcount"),
         col = c(real_line_col, projection_line_col),
         cex = 0.75)
  
  dev.off()
  
  par(mfrow = c(1, 1))
  
  
}



