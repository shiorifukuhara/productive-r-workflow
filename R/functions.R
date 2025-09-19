install.packages("tidyverse")

library(readxl)
data <- read_excel("C:/Users/shior/OneDrive/Desktop/productive-r-workflow/input/data.xlsx", na = "NA")

summary(data)

calc_mean_bill <- function(island_name) {
  filtered_data <- subset(na.omit(data), species == "Adelie" & island == island_name)
  mean_bill_length <- mean(filtered_data$bill_length_mm)
  return(round(mean_bill_length, 2))
}

