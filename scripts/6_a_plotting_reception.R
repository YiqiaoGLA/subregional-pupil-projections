## this file will makes plots of all results for reception, for each geography. 
## it creates (1) a standalone png for each geography, and (2) one png with results for all geographies in one grid.
## this is, likely, a one-time script. Meaning there is no/little need to automate it, or to clean it up fully. 


## 0. libraries and functions
library(data.table)

## 1. reading in the dataset
projections <- fread("output_projections/initial_tenyear/reception_projections_2023_2032_ratio_ets.csv")

### 1.1. getting projections and real data into the same dataset
pupils <- fread("data/processed_data/pupil_numbers/itl_pupil_numbers_1112_to_2223.csv")

pupils[, year := as.numeric(substr(time_period, 1, 4))] ## creating the year variable

real_data <- pupils[nc_year == "reception", c("year", "itl221cd", "headcount")] ## selecting reception, narrowing down to the columns we want

colnames(real_data)[2:3] <- c("itl22cd", "mean_projection")

real_data[, upper_pi := NA]
real_data[, lower_pi := NA]

projections <- rbind(real_data, projections)


  ### 1.2. adding on itl names to the projections. TO DO - this should be done in the file that creates the projections
itl_code_name_lookup <- fread("lookups/itl_code_name_lookup.csv")

setkey(itl_code_name_lookup, "itl22cd")
setkey(projections, "itl22cd")

projections <- itl_code_name_lookup[projections]


## 2. making the plots, saving each plot (by ITL) separately in a folder

  ### creating the vector of geographies
unique_geogs <- unique(projections[, itl22cd])


  ### looping through geographies, making and saving the plots
for(geog in unique_geogs){

  geog_name <- projections[itl22cd == geog, unique(itl22nm)]

    #### getting y axis limits
  ylim_max <- max(projections[itl22cd == geog, 
                              c("mean_projection", "upper_pi", "lower_pi")],
                  na.rm = TRUE)

  ylim_min <- min(projections[itl22cd == geog, 
                              c("mean_projection", "upper_pi", "lower_pi")],
                  na.rm = TRUE)

    #### setting plot colours
  polygon_colour <- rgb(255, 0, 0, alpha = 100, maxColorValue = 255)

  real_line_col <- rgb(0, 0, 255, alpha = 255, maxColorValue = 255)

  projection_line_col <- rgb(255, 0, 0, alpha = 255, maxColorValue = 255)

    #### making the plot
  png(file = paste0("plots/reception_tenyear/", geog_name, ".png"),
      height = 7, width = 12, units = "in", res = 600)

  plot(x = 1, y = 1, type = "n", bty = "n", las = 1, xlab = "", ylab = "",
       xlim = c(2011, 2032), ylim = c(ylim_min, ylim_max))

  lines(x = 2011:2022, y = projections[itl22cd == geog & year %in% 2011:2022, mean_projection],
        col = "blue", lwd = 3)
  polygon(x = c(2023:2032, 2032:2023), y = c(projections[itl22cd == geog & year %in% 2023:2032, upper_pi],
                                             rev(projections[itl22cd == geog & year %in% 2023:2032, lower_pi])),
          col = polygon_colour, border = NA)
  lines(x = 2023: 2032, y = projections[itl22cd == geog & year %in% 2023:2032, mean_projection],
        col = "red", lwd = 3)

  title(main = geog_name)

      #### adding the legend
  legend("topright", legend = c("Reception headcount", "Projected reception headcount", "95% prediction intervals"),
         col = c(real_line_col, projection_line_col, NA),
        lwd = c(3, 3, NA), fill = c(NA, NA, polygon_colour), border = NA)

  dev.off()

}



## 3. making the plots, getting them all into one large grid

  ### 3.1. creating the vector of geographies
unique_geogs <- unique(projections[, itl22cd])

  ### 3.2. setting the plot colours
polygon_colour <- rgb(255, 0, 0, alpha = 100, maxColorValue = 255)

real_line_col <- rgb(0, 0, 255, alpha = 255, maxColorValue = 255)

projection_line_col <- rgb(255, 0, 0, alpha = 255, maxColorValue = 255)


  ### 3.3. plotting them all in a loop

png(file = "plots/reception_tenyear.png",
    height = 7, width = 14, units = "in", res = 600)

par(mfrow = c(4, 9))
par(mar = c(2, 3.25, 1, 0.2))

geog <- unique_geogs[1]

for(geog in unique_geogs){
  
  #### extracting the name of the itl
  geog_name <- projections[itl22cd == geog, unique(itl22nm)]
  
  #### getting y axis limits
  ylim_max <- max(projections[itl22cd == geog, 
                              c("mean_projection", "upper_pi", "lower_pi")],
                  na.rm = TRUE)
  
  ylim_min <- min(projections[itl22cd == geog, 
                              c("mean_projection", "upper_pi", "lower_pi")],
                  na.rm = TRUE)
  
  #### making the plot
  plot(x = 1, y = 1, type = "n", bty = "n", las = 1, xlab = "", ylab = "",
       xlim = c(2011, 2032), ylim = c(ylim_min, ylim_max))
  
  lines(x = 2011:2022, y = projections[itl22cd == geog & year %in% 2011:2022, mean_projection],
        col = "blue", lwd = 3)
  polygon(x = c(2023:2032, 2032:2023), y = c(projections[itl22cd == geog & year %in% 2023:2032, upper_pi],
                                             rev(projections[itl22cd == geog & year %in% 2023:2032, lower_pi])),
          col = polygon_colour, border = NA)
  lines(x = 2023: 2032, y = projections[itl22cd == geog & year %in% 2023:2032, mean_projection],
        col = "red", lwd = 3)
  
  title(main = geog_name, cex.main = 0.65)
  
  
}

  #### adding the legend
plot(x = 1, y = 1, type = "n", bty = "n", las = 1, xlab = "", ylab = "",
     xlim = c(1,1), ylim = c(1, 1), xaxt = "n", yaxt = "n")

legend("topright", legend = c("Reception \nheadcount", "Projected reception\n headcount", "95% prediction\n intervals"),
       col = c(real_line_col, projection_line_col, NA),
       lwd = c(3, 3, NA), fill = c(NA, NA, polygon_colour), border = NA,
       cex = 0.75)


dev.off()

par(mfrow = c(1, 1))


