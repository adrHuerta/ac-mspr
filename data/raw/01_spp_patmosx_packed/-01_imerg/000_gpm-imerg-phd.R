rm(list = ls())

main_tp = "/mnt/climstor2/vol01_ecmwf/download/gpm_imerg/raw"

# Define the base URL
base_url <- "https://gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGDE.07/"

dir.create("/home/ahuerta/repos/covs-grid-sa/gpm_urls", showWarnings = FALSE)


start_year <- 1998
end_year <- 1998

# Loop through each year
for (year in start_year:end_year) {
  
  # Generate all dates for the year
  start_date <- as.Date(paste0(year, "-01-01"))
  end_date <- as.Date(paste0(year, "-12-31"))
  dates <- seq(start_date, end_date, by = "day")
  
  # Generate URLs for each date
  urls <- sapply(dates, function(date) {
    year <- format(date, "%Y")
    month <- format(date, "%m")
    day <- format(date, "%d")
    paste0(
      base_url, year, "/", month, "/3B-DAY-E.MS.MRG.3IMERG.",
      year, month, day, "-S000000-E235959.V07B.nc4"
    )
  })
  
  # Save the URLs to a text file in the respective folder
  file_name <- file.path("/home/ahuerta/repos/covs-grid-sa/gpm_urls", paste0("urls_", year, ".txt"))
  writeLines(urls, file_name)
  
}