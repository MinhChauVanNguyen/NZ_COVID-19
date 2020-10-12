library(readxl)
library(highcharter)
library(dplyr)

fake_dt <- read_excel("Analyst_Lab_Data 2.xls")
true_dt <- read_excel("Population_Data 2.xls")
names(true_dt)[2] <- "SA22018_V1_00"

# excluding those that replied "Unknown" for Ethnicity, n = 1656 observations
dd <- merge(fake_dt, true_dt, by = c("SA22018_V1_00", "Ethnicity"))

new <- dd[dd$TA2018_V1_00_NAME == "Far North District",]


data <- new[names(new) %in% c("Ethnicity", "SA22018_V1_00_NAME", "Number_of_Tests", "SA2_average_NZDep2018", "Census_2019_Population_Estimate")]
names(data)[c(2,5)] <- c("NZDep", "Population")

#data <- data[data$SA22018_V1_00_NAME %in% c("Tangonge", "Kaitaia East", "Kaitaia West"),]
data$Ethnicity <- factor(data$Ethnicity)
levels(data$Ethnicity) <- c("Asian", "Māori", "Other", "Pacific")
data$Ethnicity <- factor(data$Ethnicity, levels = c("Māori", "Pacific", "Asian", "Other"))

data <- with(data, data[order(Ethnicity, SA22018_V1_00_NAME, Number_of_Tests, NZDep, Population),])

data$SA22018_V1_00_NAME <- factor(data$SA22018_V1_00_NAME)

hc <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_yAxis(title = list(text = "Number of tests per day", style = list(fontWeight = 'bold'))) %>%
  hc_xAxis(categories = levels(data$Ethnicity), title = list(text = "Priotised Ethnicity group", style = list(fontWeight = 'bold'))) %>%
  hc_plotOptions(column = list(
    dataLabels = list(enabled = FALSE),
    stacking = "normal",
    borderColor = "#5ab1ef",
    borderWidth = 2,
    enableMouseTracking = TRUE))

for(lvl in levels(data$SA22018_V1_00_NAME)){
  da <- data[data$SA22018_V1_00_NAME == lvl, ][,4]
  hc <- hc_add_series(hc, data = da, name = lvl)
}

hc <- hc %>% hc_chart(inverted = TRUE) %>%
  hc_tooltip(formatter= JS("function () { 
  return '<b>Area: </b>' + this.series.name  + 
  ' <br /> <b>Number of tests:</b> ' + this.point.y + ' tests' + 
  '<br /> <b>Population:</b> ' + this.point.Population +
  '<br /> <b>NZ Deprivation Index:</b> ' + this.point.NZDep;}"))

hc
