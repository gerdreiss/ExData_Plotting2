########################################################################################################################
## plot1.R creates a plot answering the Question 2 from the Course Project 2 of the course Exploratory Data Analysis
##
## Author: Gerd Reiss
##
## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
## Use the base plotting system to make a plot answering this question.
##
########################################################################################################################

## import dplyr library for grouping and summarising emission data
if (("dplyr" %in% (installed.packages())) == F) {
        install.packages("dplyr")
}
library(dplyr)

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

## get total emissions by year for Baltimore
total_emissions_by_year <- 
        ## read data as shown in the project desciption
        readRDS(data_file) %>%
        ## wrap data frame into dplyr's tbl type to allow grouping and summarising
        tbl_df() %>%
        ## filter for Baltimore
        filter(fips == "24510") %>% 
        ## and group emission data by year
        group_by(year) %>% 
        ## summarise emission data by year converting from tons to kilotons
        summarise(total_emissions = sum(Emissions)/1000) %>%
        ## transform to a matrix for the barplot
        as.matrix() %>% t()


## plot the emissions
par(mar = c(5,5,4,2)) ## extra margin to accommodate tick labs
barplot(total_emissions_by_year[2, ], main = "Total Emissions by Year in Baltimore",  
        xlab = "Year", ylab = "Emissions in kilotons", ylim = c(0, max(total_emissions_by_year[2, ])), las = 1, 
        names.arg = total_emissions_by_year[1,])

## write the plot into an PNG file
dev.copy(png,'plot2.png')
dev.off()

