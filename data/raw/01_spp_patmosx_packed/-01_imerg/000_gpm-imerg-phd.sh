#!/bin/bash

# Define the folder where urls.txt files are stored
urls_folder="/home/ahuerta/repos/covs-grid-sa/gpm_urls"

# Define the folder where you want to save the downloaded data
download_folder="/mnt/climstor2/vol01_ecmwf/download/gpm_imerg/raw"

# Create the download folder if it doesn't exist
mkdir -p $download_folder

# Loop through each year and run wget for each urls.txt
for year_file in $urls_folder/urls_*.txt; do
  # Extract the year from the filename
  year=$(basename $year_file | cut -d'_' -f2 | cut -d'.' -f1)
  
  # Create a folder for that year in the download folder
  mkdir -p $download_folder/$year
  
  # Run wget to download files listed in the urls.txt file
  wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies -c -i $year_file -P $download_folder/$year/
  
  echo "Completed download for year $year"
done