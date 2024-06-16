setwd('D:\\R Studio')
# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries

#Reading CSV file in R
data=read.csv('NSSO68.csv')

#Filtering for Uttar Pradesh UP
df=data%>%
  filter(state_1 == 'UP')

#Displaying dataset info
cat('Dataset Information:\n')
print(names(df))
print(head(df))
print(dim(df))

#Finding missing values
missing_info=colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)
#no missing values found throughout

#Subsetting the data
upnew=df%>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

#Checking missing values in the subset
cat('Missing values in Subset:\n')
print(colSums(is.na(upnew)))
#No missing values found throughout


#Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  upnew <- remove_outliers(upnew, col)
}

#Summarizing the consumption
upnew$total_consumption <- rowSums(upnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

#Summarize and display top and bottom consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- upnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top 3 Consuming Districts:\n")
print(head(district_summary, 3))
cat("Bottom 3 Consuming Districts:\n")
print(tail(district_summary, 3))

cat("Region Consumption Summary:\n")
print(region_summary)

#Renaming the districts and sectors
district_mapping <- c("15" = "Agra", "11" = "Bulandshahar", "12" = "Aligarh")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

upnew$District <- as.character(upnew$District)
upnew$Sector <- as.character(upnew$Sector)
upnew$District <- ifelse(upnew$District %in% names(district_mapping), district_mapping[upnew$District], upnew$District)
upnew$Sector <- ifelse(upnew$Sector %in% names(sector_mapping), sector_mapping[upnew$Sector], upnew$Sector)

#Testing for differences in consumption in mean consumption between urbann and rural
rural=upnew%>%
  filter(Sector == 'RURAL')%>%
  select(total_consumption)
urban=upnew%>%
  filter(Sector == 'URBAN')%>%
  select(total_consumption)
mean_rural=mean(rural$total_consumption)
mean_urban=mean(urban$total_consumption)

#Performing z-test(because more than 30 entries in the data)
z_test_result = z.test(rural, urban, alternative = "two.sided", mu = 0, sigma.x = 2.56, sigma.y = 2.34, conf.level = 0.95)
print(z_test_result)

#Generation output based on p-value
if (z_test_result$p.value < 0.05) {
  cat(glue::glue("P value is < 0.05 i.e. {round(z_test_result$p.value,5)}, Therefore we reject the null hypothesis.\n"))
  cat(glue::glue("There is a difference between mean consumptions of urban and rural.\n"))
  cat(glue::glue("The mean consumption in Rural areas is {mean_rural} and in Urban areas its {mean_urban}\n"))
} else {
  cat(glue::glue("P value is >= 0.05 i.e. {round(z_test_result$p.value,5)}, Therefore we fail to reject the null hypothesis.\n"))
  cat(glue::glue("There is no significant difference between mean consumptions of urban and rural.\n"))
  cat(glue::glue("The mean consumption in Rural area is {mean_rural} and in Urban area its {mean_urban}\n"))
}
