########################################################################################################################
## plot1.R creates a plot answering the Question 3 from the Course Project 2 of the course Exploratory Data Analysis
##
## Author: Gerd Reiss
##
## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
## which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
## Which have seen increases in emissions from 1999–2008? 
## Use the ggplot2 plotting system to make a plot answer this question.
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
data_file <- "summarySCC_PM25.rds"

## check OS
is_windows <- .Platform$OS.type == "windows"

## get the data if not already present in the working directory
if (!file.exists(data_file)) {
        download.file(url = "https://d396qusza40orc.cloudfront.net/exdata/data/NEI_data.zip", 
                      destfile = "NEI_data.zip", 
                      method = ifelse(is_windows, "auto", "curl"))
        unzip(zipfile = dest_file)
}


## get total emissions by year and source for Baltimore
total_emissions_by_type_and_year <- 
        ## read data as shown in the project desciption
        readRDS(data_file) %>%
        ## wrap data frame into dplyr's tbl type to allow grouping and summarising
        tbl_df() %>%
        ## filter for Baltimore
        filter(fips == "24510") %>%
        ## group by year and emission source type
        group_by(year, type) %>%
        ## summarise emission data by year
        summarise(total_emissions = sum(Emissions))

## plot the emissions
par(mar = c(5,5,4,2))
png('plot3.png', width = 480, height = 480, units = "px")
ggplot(total_emissions_by_type_and_year, aes(year, total_emissions, color = type)) +
        geom_line(stat = "summary", fun.y = "sum", lwd = 2) +
        ggtitle("Total Emissions by Type in Baltimore") +
        labs(x = "Year", y = "Emissions in tons")

## write the plot into an PNG file
dev.off()
