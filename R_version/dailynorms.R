## ----------------------
## Script name: dailynorms.R
## 
## Purpose of script: to temporally downscale monthly averages to daily 
## averages. It attemps to reproduce the process conducted using Dan Upper's
## Perl (dailynorms.pl) and GRASS-GIS (produced by the Perl code) using    
## the same smoothing matrix. A new smoothing matrix may be produced by editing
## Dan's Octave code (dailynorms.m) or by some other method.                            

## Author: Brittany S. Barker

## Problem: We are given monthly averages of some weather parameter such as
## temperature and need to create daily averages of that parameter. This script
## is designed to use raster data as inputs but it can be modified to work 
## with other input data types.
## 
## Inputs and outputs: The inputs are 12 PRISM raster files which each contain 
## monthly averages for a given time frame (e.g., 1961-1990) for a climate 
## variable of interest. The expected outputs are 366 raster files each containing 
## smoothed average values which are calculated using a smoothing matrix. The
## average values of daily outputs for each month should be the same as the
## monthly inputs. The code which defines the names of the output will need to 
## be modified slightly if another source (non-PRISM) data are used.

## NOTE: what follows assumes that the value we have is an average over the days 
## of the month. While this works for temperature, it may not be what you actually 
## have for precipitation. Monthly average precipitation is likely to be the total 
## precipitation in that month (at least for PRISM data), larger than the daily 
## average by a factor of about 30. Therefore, for precipitation, we will divide
## the daily downscaled outputs by the number of days in that month, as a crude
## estimate.
#
## Last update: 4-22-2022
## ----------------------
##  
#!/usr/bin/Rscript
#.libPaths("/usr/lib64/R/library/")
library(raster)
library(dplyr)
library(stringr)

# Specify directories (working dir, data dir, output dir)
setwd("/home/brittany/scripts/PRISM_downscale/R_version")
monthly_data_dir <- "/data/PRISM/1990_monthly_30yr/"
out_dir <- "./test_output/"

# Climate variables in data
vars <- c("tmin", "tmax", "ppt", "tdmean", "tmean")

# Smoothing matrix. The matrix file contains the coefficients for the
# smoothing matrix, which puts (0.25, 0.5, 0.25) in each row with the 0.5 
# on the diagonal, wrapped around for the top and bottom rows. The first two 
# columns are month (col 1) and day (col 2):
# - month_numbers = first column of raw matrix
# - day_numbers = second column of raw matrix
# The rest of the columns (3-14) are the smoothing matrix by which a climate
# raster will be multiplied, corresponding to 12 months (12 cols) and all 
# days of year (366 rows). If you want a different smoothing matrix, edit and 
# run the octave code and put its output in the DATA section. 
m_raw <- matrix(read.table("dailynorms_mat.txt", sep = ""), 
              ncol = 14, nrow = 366)
m_raw <- matrix(unlist(m_raw), ncol = 14, nrow = 366) # 12 months, 366 days
m_df <- data.frame(m_raw)
names(m_df) <- c("mon", "day", "jan", "feb", "mar", "apr", "may", "jun", 
                 "jul", "aug", "sep", "oct", "nov", "dec")

# Variable with which to create daily normals
# Remember that PRISM precipitation data are in millimeter units.
for (var in vars) {
  
  # Input monthly data (rasters) for a climate variable are put into a raster stack
  # Need to edit file type if it's not ".bil"
  monthly_mean_fls <- list.files(path = monthly_data_dir, 
                    pattern = glob2rx(paste0("*", var,"*.bil$")), full.names = TRUE)
  monthly_means_stk <- stack(monthly_mean_fls)  
  
  # Create a raster stack containing a layer for each day of the year (366 days).
  # The raster values for each day contain the average value for the corresponding
  # month. Thus, for any given cell in the raster stack, there will only be 12 
  # unique values for the entire year.
  
  # Empty raster stack to populate (will have 366 layers after loop completes)
  all_daily_means_stk <- stack()
    
  # Loop through each month (layer in "monthly_means_stk") and add n layers to the
  # "all_daily_means_stk" corresponding the number of days in that month.
  for (lyr in 1:nlayers(monthly_means_stk)) {
    
    # Get the number of days for each month, and tack a 0 onto single-digit names
    # so that all days have 2 digits (e.g., "..., 08, 09, 10, 11...").
    # The number of layers added to the "all_days_stk" corresponds to this value.
    days_in_month <- as.numeric(m_df %>% 
      filter(mon == lyr) %>%
      tally())
    
    # Add n layers to the "all_days_stk" containing monthly mean values
    #n <- nrow(month_days) # number of copies, corresponding to days of month
    
    # Precipitation data are average monthly totals, so we need to divide each
    # layer by the number of days for that month in order to estimate the daily total.
    if (var == "ppt") {
      
      # If February, divide by 28.25 instead of 29 to account for non-leap years
      # in the averaging (only 1 in every 4 years is a leap year)
      if (lyr == 2) {
        days_in_month <- 28.25
      }
      
      monthly_means_stk[[lyr]] <- monthly_means_stk[[lyr]]/days_in_month
    }
    
    # Get stack layer corresponding to month of interest
    monthly_means_lyr <- monthly_means_stk[[lyr]]
    
    # Copy this layer n times (number of days in month)
    daily_means_stk <- stack(replicate(days_in_month, monthly_means_lyr))
    
    # Add these layers to the output raster stack
    all_daily_means_stk <- addLayer(all_daily_means_stk, daily_means_stk)
    
    # Create vector of new names to name output raster files in the very last step
    # This code will likely need to be modified if another source (non-PRISM) of
    # data are used because the input file names will have a different format. 
    
    # "Base names" - the prefix of all files plus the month number
    base_names <- str_split_fixed(names(all_daily_means_stk), "[.]", 2)[,1] 
    
    # Extract day of month and pad single-digit day numbers with a "0" 
    day <- str_split_fixed(names(all_daily_means_stk), "[.]", 2)[,2]
    day_pad <- str_pad(day, max(nchar(day)), side="left", pad="0")
    
    # New file names combine base name and day of month
    new_names <- paste0(base_names, day_pad)
  
  }
  
  # Loop over the rows of the smoothing matrix (day of year). For each day, the 
  # corresponding row in the matrix is extracted, resulting in 12 smoothing values 
  # for each month ("day_smooths", corresponding to columns 3:14 in the smoothing
  # matrix). Then, loop over each smoothing value (columns of the matrix row) and 
  # multiply this "smooth_val" to the raster layer for the corresponding month in
  # the "monthly_means_stk." 
  
  # For example, if i = 1 then m_df[i,3:14] = 
  # mon day jan   feb   mar   apr    may    jun    ....
  # 1   1   0.67 -0.18  0.48  -0.13  0.003  -0.007 ....
  # For each month (j = 1:12), multiply "day_smooths[j]" to "monthly_means_stk[[j]]"
  # If j = 1 (January), then
  # monthly_means_stk[[1]] * smooth_val[day_smooths[1]]
  
  # The output value of each multiplication event (1:12) is added to a raster 
  # layer for the given day of the year named "daily_smoothed_lyr" (this layer 
  # initially has all 0 values). After each of the 12 output values have been 
  # added to this layer, it is saved as a raster with the appropriate month and 
  # day in the file name. 
  for (i in 1:366) {
    #print(paste("i =", i))
    
    # Extract matrix row for day of year
    day_smooths <- m_df[i,3:14]
    
    # Create a raster with all 0 values (copy first layer from 
    # "all_daily_means_stk" so that it has the correct dimensions).
    ncell <- ncell(all_daily_means_stk[[1]])
    daily_smoothed_lyr <- setValues(all_daily_means_stk[[1]], unlist(rep(0, ncell)))
    
    # Loop over the columns of the matrix row (i.e., each month), and multiply the
    # corresponding smoothing value to the monthly averages for that month, found
    # in "monthly_means_stk".
    for (j in 1:12) {
      
      #print(paste("j =", j))
      
      # M(i,j) * monthly average for month j
      smooth_val <- as.numeric(day_smooths[j])
      mon_avgs_smooth <- monthly_means_stk[[j]] * smooth_val
      daily_smoothed_lyr <- daily_smoothed_lyr + mon_avgs_smooth 
      
      # The smoothing may result in some small negative precip values, so these
      # should be changed to 0. To do: find better way to correct for this?
      if (var == "ppt") {
        daily_smoothed_lyr[daily_smoothed_lyr < 0] <- 0
      }
      
    }
    
    # File name for the output raster, using the "new_names" vector created above
    filename <- paste0(new_names[i], ".bil")
  
    # Save results as a raster file (BIL format)
    writeRaster(daily_smoothed_lyr, file = paste0(out_dir, filename),
                format = "BIL",  overwrite = TRUE)
      
    # Print progress
    print(paste0("Saved ", filename))
    
  }
}