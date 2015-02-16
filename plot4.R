########################################################################################################################
## plot4.R creates a plot answering the Question 4 from the Course Project 2 of the course Exploratory Data Analysis
##
## Author: Gerd Reiss
##
## Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
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

## get the SCC values for all coal sources from EI.Sector column and combustion source from SCC.Level.One column
coal_combustion <- 
        ## wrap data frame into dplyr's tbl type to allow selecting and filtering
        tbl_df(SCC) %>% 
        ## select SCC, EI.Sector and SCC.Level.One columns
        select(SCC, EI.Sector, SCC.Level.One) %>% 
        ## Filter by EI.Sector column values containing "Coal@ and SCC.Level.One values containing "Combustion"
        filter(grepl("Coal", EI.Sector) & grepl("Combustion", SCC.Level.One))
## ensure that we have unique values, and convert values to characters
coal_combustion_sources <- as.character(unique(coal_combustion$SCC))

## use SCC values to retrieve coal related emissions from the emission data
emissions_from_coal_combustion <- 
        ## wrap data frame into dplyr's tbl type to allow grouping and summarising
        tbl_df(NEI) %>%
        ## filter the resulting table by SCC values for coal combustion related sources
        filter(SCC %in% coal_combustion_sources) %>%
        ## select the columns we're interested in
        select(Emissions, year) %>%
        ## group by year
        group_by(year) %>%
        ## calculate total emissions for each year
        summarise(total_emissions = sum(Emissions)/1000000)


## plot the total emissions
par(mar = c(5,5,4,2))
png('plot4.png', width = 480, height = 480, units = "px")
ggplot(emissions_from_coal_combustion, aes(year, total_emissions)) +
        geom_bar(colour = "black", fill = "lightgray", stat = "identity") +
        ggtitle("Total Emissions from coal combustion-related sources in megatons") +
        labs(x = "Year", y = "Emissions in megatons") + 
        theme_bw() +
        scale_x_continuous(breaks = emissions_from_coal_combustion$year, labels = as.character(emissions_from_coal_combustion$year))

## write the plot into an PNG file
dev.off()
