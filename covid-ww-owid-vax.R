# covid-ww-owid.R
#################################################
# Description:
# Script parses cvs file and provides a graph 
# with selectable yearly trend lines for 
# comparison, also includes analysis options at 
# bottom for predicting a particular day or 
# searching by cases.

todays_date <- Sys.Date()

#Preventing scientific notation in graphs
options(scipen=999)

#################################################
# Load libraries. If library X is not installed
# you can install it with this command at the R prompt:
# install.packages('X')
library(data.table)
library(ggplot2)
library(bbplot)
library(readr)

df <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv", 
               col_types = cols(daily_vaccinations = col_skip(), 
                                daily_vaccinations_per_million = col_skip(), 
                                daily_vaccinations_raw = col_skip(), 
                                date = col_date(format = "%Y-%m-%d"), 
                                total_vaccinations = col_skip(), 
                                iso_code = col_skip(), people_fully_vaccinated = col_skip(), 
                                people_fully_vaccinated_per_hundred = col_skip(), 
                                people_vaccinated = col_integer(), people_vaccinated_per_hundred = col_skip(), 
                                total_vaccinations_per_hundred = col_skip()))

# Drop first column
# df <- df[ -c(1) ] 

# Change column order to date, country vaccinations
df <- df[ c(2,1,3) ]

View(df)

# Add column names
colnames(df)[1] <- "date"
colnames(df)[2] <- "country"
colnames(df)[3] <- "vaccinations"

# Set datatypes
df$date <- as.Date(df$date, format = '%Y-%m-%d')
df$country <- as.character(df$country)
df$vaccinations <- as.numeric(as.character(df$vaccinations)) 
df$vaccinations <- as.numeric(df$vaccinations) 
df[is.na(df)] <- 0

# Limits dates
# df <- filter(df, date >= "2020-07-01")

# Limits to cumulative world
df <- df[df$country == "India", ]

View(df)

# Drop country column
df <- df[ -c(2) ] 

df_proj <- NULL 
df_proj$date <- as.Date('2022-10-01')
df_proj$vaccinations <- as.numeric('1000000000') 
df <- rbind(df, df_proj)

plot <- ggplot() + 
  geom_line(data = df, aes(x = date, y = vaccinations)) +
  #geom_vline(xintercept=start_date, linetype="F1", colour="black") +
  geom_text(aes(x=todays_date-4, label="Today", y=300000000, angle=90), size=8) +
  geom_vline(xintercept=as.numeric(as.Date(todays_date)), linetype=4, colour="black") +
  #geom_vline(xintercept=as.numeric(as.Date("2021-08-30")), linetype=4, colour="red") +
  #geom_text(aes(x=as.Date("2021-08-27"), y=2000000000, label="August 30, 2021", angle=90), size=8) +
  #geom_hline(yintercept=22000, color="red") +
  #geom_hline(yintercept=18000, color="yellow") +
  #geom_hline(yintercept=15000, color="green") +
  bbc_style() +
  labs(title=paste0("India Vaccinations, ", todays_date)) +
  scale_x_date()

plot

finalise_plot(plot_name = plot,
              source = "Source: OWID",
              save_filepath = paste0("./output/covid-ww-owid-vaccinations-",
                                     todays_date, ".png"),
              width_pixels = 1500,
              height_pixels =500,
              logo_image_path = paste0("./branding/logo.png"))

