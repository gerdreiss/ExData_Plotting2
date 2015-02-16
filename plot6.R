########################################################################################################################
## plot6.R creates a plot answering the Question 6 from the Course Project 2 of the course Exploratory Data Analysis
##
## Author: Gerd Reiss
##
## Compare emissions from motor vehicle sources in Baltimore City (fips == "24510")
## with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
## Which city has seen greater changes over time in motor vehicle emissions?
##
########################################################################################################################

## import dplyr library for grouping and summarising emission data
if (("dplyr" %in% (installed.packages())) == F) {
        install.packages("dplyr")
}
library(dplyr)
## import ggplot2 library for plotting
if (("ggplot2" %in% (installed.packages())) == F) {
        install.packages("ggplot2")
}
library(ggplot2)

## the name of the dataset file
nei_data_file <- "summarySCC_PM25.rds"
scc_data_file <- "Source_Classification_Code.rds"

## check OS
is_windows <- .Platform$OS.type == "windows"

## get the data if not already present in the working directory
if (!file.exists(nei_data_file)) {
        download.file(url = "https://d396qusza40orc.cloudfront.net/exdata/data/NEI_data.zip", 
                      destfile = "NEI_data.zip", 
                      method = ifelse(is_windows, "auto", "curl"))
        unzip(zipfile = dest_file)
}

## read the data
NEI <- readRDS(nei_data_file)
SCC <- readRDS(scc_data_file)

## get the SCC values for all vehicles from EI.Sector column
vehicles <- 
        ## wrap data frame into dplyr's tbl type to allow selecting and filtering
        tbl_df(SCC) %>% 
        ## select SCC and EI.Sector columns
        select(SCC, EI.Sector) %>% 
        ## filter by IE.Sector values containing "Vehicle"
        filter(grepl("Vehicle", EI.Sector))
## ensure that we have unique values, and convert values to characters
vehicle_sources <- as.character(unique(vehicles$SCC))

emissions_from_vehicles <- 
        ## wrap data frame into dplyr's tbl type to allow grouping and summarising
        tbl_df(NEI) %>%
        ## filter the resulting table by SCC values for vehicle related sources in Baltimore and Los Angeles County
        filter(fips %in% c("24510", "06037") & SCC %in% vehicle_sources) %>%
        ## select the columns we're interested in
        select(fips, Emissions, year) %>%
        ## group by year
        group_by(fips, year) %>%
        ## calculate total emissions for each year
        summarise(total_emissions = sum(Emissions)/1000)

## plot the emissions
par(mar = c(5,5,4,2))
png('plot6.png', width = 480, height = 480, units = "px")
ggplot(emissions_from_vehicles, aes(year, total_emissions, color = fips)) +
        geom_line(stat = "summary", fun.y = "sum", lwd = 2) +
        ggtitle("Total Vehicle Emissions in Baltimore and Los Angeles County") +
        labs(x = "Year", y = "Emissions in kilotons")

## write the plot into an PNG file
dev.off()
