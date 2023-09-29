library(data.table)
library(tidyverse)
library(dplyr)


library(fixest)
library(lubridate)


library(ggiplot)
#
options(repos = "https://cran.r-project.org")
chooseCRANmirror(ind=58)


#We defined a function to load and clean the data
#We have done this to avoid repeatation of codes.

load_and_clean <- function(city_name) {
  city <- fread(paste0(city_name, ".csv"))
  city <- select(city, "Navn", "Fra" ,"Felt","< 5,6m",">= 5,6m")
  city <- city[Felt %like% "Totalt" , ]
  city <- city %>% filter(!grepl('Totalt i retning', Felt))
  city$Fra <- as.Date.character(city$Fra)
  city$Navn <- as.character(city$Navn)
  city_complete <- city %>%
    complete(Navn, Fra = seq.Date(as.Date("2013-01-01"), as.Date("2023-02-01"), by = "month")) %>%
    arrange(Navn)
  return(city_complete)
}

# Load and preprocess data for Bergen, Oslo, and Trondheim
bergen_complete <- load_and_clean("Bergen")
oslo_complete <- load_and_clean("Oslo")
trondheim_complete <- load_and_clean("Trondheim")

head(bergen_complete)

dummy_maker <- function(city_data) {
  city_data$e_crisis <- as.integer(city_data$Fra >= "2021-01-01")
  setDT(city_data)
  city_data[, e_treat := max(e_crisis, na.rm = TRUE), by = .(Navn)]
  city_data$Fra <- as.Date(city_data$Fra)
  city_data[, begin_crisis := min(Fra[e_crisis == 1], na.rm = TRUE), by = .(Navn)]
  city_data[, time_to_crisis := ifelse(e_treat == 1, as.numeric(Fra - begin_crisis) / days_in_month(Fra), 0)]
  city_data[, time_to_crisis := round(time_to_crisis)]
  city_data$numroad <- match(city_data$Navn, unique(city_data$Navn))
  names(city_data)[names(city_data) == "< 5,6m"] <- "Private"
  names(city_data)[names(city_data) == ">= 5,6m"] <- "NonPrivate"
  return(city_data)
}

# Preprocess data for Bergen, Oslo, and Trondheim
bergen_complete <- dummy_maker(bergen_complete)
oslo_complete <- dummy_maker(oslo_complete)
trondheim_complete <- dummy_maker(trondheim_complete)


head(bergen_complete)
library(readxl)
electric<-read_xlsx("Electricity-prices.xlsx")

#The Data acquired has Column names in Norwegian.
#We convert the the data according to our needs

# Define a function to merge and process data for a city
merge_process_data <- function(city_data, city_name, col_index) {
  colnames(electric)[col_index] <- paste(city_name, "price", sep = "_")
  electric$Fra <- as.Date(electric$Fra)
  merged_data <- merge(city_data, electric[, c("Fra", paste(city_name, "price", sep = "_"))], by = "Fra")
  merged_data$Private <- as.integer(merged_data$Private)
  merged_data$NonPrivate <- as.integer(merged_data$NonPrivate)
  return(merged_data)
}

# Process data for Bergen, Oslo, and Trondheim
bergen_complete <- merge_process_data(bergen_complete, "Bergen", 7) #col7 in electric
oslo_complete <- merge_process_data(oslo_complete, "Oslo", 5)       #col5 in electric
trondheim_complete <- merge_process_data(trondheim_complete, "Trondheim", 6) #col6 in electric

head(bergen_complete)
electricity_deviation <- function(city_data, price_col, date_range_start, date_range_end) {
  # Calculate the mean price within the specified date range
  mean_price <- mean(city_data[[price_col]][as.Date(city_data$Fra) >= date_range_start & as.Date(city_data$Fra) < date_range_end], na.rm = TRUE)
  
  # Convert the price column to numeric
  city_data[[price_col]] <- as.numeric(city_data[[price_col]])
  
  # Calculate and add deviation
  city_data$Dev_Price <- city_data[[price_col]] - mean_price
  
  return(city_data)
}

# Calculate and add deviation for Bergen
bergen_complete <- electricity_deviation(bergen_complete, "Bergen_price", "2013-01-01", "2021-02-01")

# Calculate and add deviation for Oslo
oslo_complete <- electricity_deviation(oslo_complete, "Oslo_price", "2013-01-01", "2021-02-01")

# Calculate and add deviation for Trondheim
trondheim_complete <- electricity_deviation(trondheim_complete, "Trondheim_price", "2013-01-01", "2021-02-01")



bergen_complete$city <- "bergen"
oslo_complete$city <- "oslo"
trondheim_complete$city <- "trondheim"

all_cities <- bind_rows(bergen_complete, oslo_complete, trondheim_complete) %>%
  select(Fra, Navn, Private, NonPrivate, e_crisis, e_treat, 
         time_to_crisis, numroad, Dev_Price, city)

setDT(all_cities)

# Arrange data frame by the Fra column
all_cities <- all_cities %>% arrange(Fra)

head(all_cities)
library(ggplot2)



# Define common colors and labels
city_colors <- c("bergen" = "blue", "oslo" = "green", "trondheim" = "red")
city_labels <- c("bergen" = "Bergen", "oslo" = "Oslo", "trondheim" = "Trondheim")


ggplot(electric, aes(x = Fra)) +
  geom_line(aes(y = `Sørøst-Norge - Oslo (NO1)`, color = "oslo")) +
  geom_line(aes(y = `Midt-Norge - Trondheim (NO3)`, color = "trondheim")) +
  geom_line(aes(y = `Vest-Norge - Bergen (NO5)`, color = "bergen")) +
  geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), linetype = "dashed", color = "black") +
  labs(x = "", y = "Electricity price", color = "City") +
  scale_color_manual(values = city_colors) +  # Use the common color scale
  theme(legend.position = "top",
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold")) +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16),
        axis.text = element_text(size = 16))





# Plot deviation of electricity prices for the three cities
ggplot(all_cities, aes(x = Fra, y = Dev_Price, color = city)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), linetype="dashed") +
  labs(x = "", y = "Deviation above pre-January 2021 Mean") +
  scale_color_manual(values = city_colors) +  # Use the common color scale
  scale_fill_manual(values = city_colors) +   # Add scale_fill for consistency
  theme_bw() +
  theme(legend.position = "top",  # Move the legend to the top
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 16),  
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16))
library(ggiplot)
library(lmtest)
library(sandwich)

oslo_complete$Private<-as.integer(oslo_complete$Private)
trondheim_complete$Private<-as.integer(trondheim_complete$Private)

#bergen
model_bergen <- feols((Private) ~ Bergen_price + e_crisis + factor(numroad) + i(time_to_crisis, ref = -1) +  e_crisis * time_to_crisis, cluster = ~numroad, data = bergen_complete[time_to_crisis %in% c(-40:27)])

model_bergen %>%
  ggiplot(
    ref.line = -1,
    geom_style = "ribbon"
  ) +
  labs(
    x = "Time to event",
    title = "Event study",
    subtitle = "Effect of Surge in Electricity Prices on Private Car Usage in Bergen"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


#oslo


model_oslo <- feols((Private) ~ Oslo_price + e_crisis + factor(numroad) + i(time_to_crisis, ref = -1) +  e_crisis * time_to_crisis, cluster = ~numroad, data = oslo_complete[time_to_crisis %in% c(-40:27)])


model_oslo %>%
  ggiplot(
    ref.line = -1,
    geom_style = "ribbon"
  ) +
  labs(
    x = "Time to event",
    title = "Event study",
    subtitle = "Effect of Surge in Electricity Prices on Private Car Usage in Oslo"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )



#trondheim


model_trondheim <- feols((Private) ~ Trondheim_price + e_crisis + factor(numroad) + i(time_to_crisis, ref = -1) +  e_crisis * time_to_crisis, cluster = ~numroad, data = trondheim_complete[time_to_crisis %in% c(-40:27)])

model_trondheim %>%
  ggiplot(
    ref.line = -1,
    geom_style = "ribbon"
  ) +
  labs(
    x = "Time to event",
    title = "Event study",
    subtitle = "Effect of Surge in Electricity Prices on Private Car Usage in Trondheim"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

all_cities_subset <- subset(all_cities, time_to_crisis %in% c(-40:27))

model_fe<-feols(Private ~ Dev_Price * time_to_crisis+i(time_to_crisis, ref = -1)
                | numroad + city, 
                data = all_cities_subset)


model_fe %>% 
  ggiplot(
    ref.line = -1,
    geom_style = "ribbon"
  ) +
  labs(
    x = "Time to crisis",
    title = "Event study",
    subtitle = "Effect of Surge in Electricity Prices on Private Car Usage across cities"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
summary(model_fe)
