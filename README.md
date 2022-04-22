# Downscale monthly climate data to a daily resolution

The repository contains R and Perl scripts to temporally downscale averages of monthly climate data to a daily resolution.

## Problem
We are given monthly averages of some weather parameter such as temperature and need to create daily averages of that parameter. The scripts are designed to use raster data as inputs but can be modified to work with other input data types.

## Code  
The code is written in both Perl (`/Perl_version`) and R (`/R_version`). The two versions of the program should produce nearly identical outputs.

## Inputs 
 
**Input 1:** 12 raster files that contain monthly averages for a given time frame (e.g., 1961-1990) for a climate variable of interest. 
The code which defines the names of the output will need to be modified slightly if another source (non-PRISM) data are used.  

**Input 2:** A matrix file that contains the coefficients for the smoothing matrix (input #2), which puts (0.25, 0.5, 0.25) in each row with the 0.5 on the diagonal, wrapped around for the top and bottom rows.   
The first two columns are month (col 1) and day (col 2):    
- month_numbers = first column of raw matrix  
- day_numbers = second column of raw matrix
  
The rest of the columns (3-14) are the smoothing matrix by which a climate raster will be multiplied, corresponding to 12 months (12 cols) and all days of year (366 rows). If you want a different smoothing matrix, edit and run the octave code found in `/Perl_version`.

## Outputs
The outputs are 366 raster files each containing smoothed average values which are calculated using a smoothing matrix. The average values of daily outputs for each month should be the same as the monthly inputs.  

## Required programs and packages
The Perl version of the code requires the use of GRASS-GIS (see README file in `/Perl_version`).

The R version of the code requires:  
`raster`  
`dplyr`  
`stringr`  

## History
4/22/2022: Uploaded R code and put formerly deposited Perl code in its own folder