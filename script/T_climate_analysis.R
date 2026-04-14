# ==============================================================================
# Project: Trend of Annual Mean Temperature in Ecuador, period 2010-2024
# Author: Alexandra Lituma
# ==============================================================================

# -------------------------
# Load libraries
# -------------------------

library (tidyverse)
library(lubridate)
library(dplyr)

# -------------------------
# Import data
# -------------------------


TMAX <- read.csv("~/DATA ANALYST JR/PROYECTOS AMBIENTALES/T_ECUADOR/data/maxClimateEngine.csv",
                 skip = 1, 
                 sep = ";",
                 header = FALSE)

colnames(TMAX) <- c( "DATE", "T_MAXIMUM") 

TMAX$T_MAXIMUM<- as.numeric(gsub(",", ".", TMAX$T_MAXIMUM))



TMIN <- read.csv2("~/DATA ANALYST JR/PROYECTOS AMBIENTALES/T_ECUADOR/data/minClimateEngine.csv", 
                  skip = 1,
                  header = FALSE)

colnames(TMIN) <- c( "DATE", "T_MINIMA") 

TMIN$T_MINIMA<- as.numeric(gsub(",", ".", TMIN$T_MINIMA))


# -------------------------
# Data cleaning
# -------------------------

TMAX <- TMAX %>%
        mutate(DATE = as.Date(DATE)) %>%
        mutate(YEAR = year(DATE))



TMIN <- TMIN %>%
        mutate(DATE = as.Date(DATE)) %>%
        mutate(YEAR = year(DATE))


# -------------------------
# Analysis
# -------------------------

DATA <- TMAX %>%
        left_join (TMIN, by= c("DATE", "YEAR"))

DATA <- DATA %>%
        mutate(T_MEDIA = (T_MAXIMUM + T_MINIMA) / 2) %>%
group_by(YEAR)%>%
        summarise(
                T_MEDIA_YEAR= mean(T_MEDIA, na.rm=TRUE),
                TMAX_ABSOLUTA= max(T_MAXIMUM, na.rm= TRUE),
                T_MIN_ABSOLUTA= min(T_MINIMA, na.rm= TRUE)
                )

# ------------------------------
# Change calculation (2010-2024)
# ------------------------------


model <- lm(T_MEDIA_YEAR ~ YEAR, data = DATA)

slope <- coef(model)["YEAR"]

total_change <- slope * (max(DATA$YEAR) - min(DATA$YEAR))

print(paste(" The total change in celcius grades is:", round(total_change, 2),
            "°C"))


# -------------------------
# Visualization
# -------------------------

ggplot(DATA, aes(YEAR,T_MEDIA_YEAR)) +
        geom_line(color="red") +
        geom_point( color="darkgrey", size = 3 ) +
        geom_smooth( method = "lm", se = FALSE, color = "darkblue", linetype = "dashed") +
        annotate("text", x = 2015, y = max(DATA$T_MEDIA_YEAR), 
                 label = paste("Total Change:", round(total_change, 2), "°C"),
                 color = "darkblue", fontface = "bold")+
        labs(title = "Trend of Annual Mean Temperature in Ecuador, period 2010-2024",
        x = "Year",
        y = "Mean Temperature (°C)") +
                theme_minimal()

