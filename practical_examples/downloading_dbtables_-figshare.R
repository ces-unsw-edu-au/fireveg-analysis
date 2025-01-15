# AIM: Download from the summary tables of the Fire Database 
# from the figshare https://figshare.com/articles/dataset/Fire_Ecology_Traits_for_Plants_Database_exports/24125088

# 0. Load the packages ---
library(readxl)
library(httr)

# 1. Download the zip files ----
# URL of the zip file
url <- "https://figshare.com/ndownloader/articles/24125088/versions/2"

# Destination path to save the file
destfile <- "practical_examples/data.zip"

# Download the file without SSL verification
GET(url, write_disk(destfile, overwrite = TRUE), config = config(ssl_verifypeer = FALSE))

# 2. Unzip the file ----
unzip(destfile, exdir = "practical_examples/input") # Create the folder input with the unzipped data

# What did the unzipped file say to the compressed file? "Wow, you've really let yourself go!"

# THE END