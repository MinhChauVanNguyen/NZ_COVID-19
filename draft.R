library(highcharter)
library(plyr)
library(readxl)
library(dplyr)

fake_dt <- read_excel("Analyst_Lab_Data 2.xls")
true_dt <- read_excel("Population_Data 2.xls")
names(true_dt)[2] <- "SA22018_V1_00"

# excluding those that replied "Unknown" for Ethnicity (n = 1656 observations)
dd <- merge(fake_dt, true_dt, by = c("SA22018_V1_00", "Ethnicity"))

new <- dd[dd$TA2018_V1_00_NAME == "Auckland",]
data <- new[names(new) %in% c("Ethnicity", "SA22018_V1_00_NAME", "Number_of_Tests", "SA2_average_NZDep2018", "Census_2019_Population_Estimate")]

names(data)[c(2:5)] <- c("NZDep", "Area", "y", "Population")

data$Ethnicity <- factor(data$Ethnicity)
data$NZDep <- as.character(data$NZDep)
data$Population <- as.character(round(data$Population))

levels(data$Ethnicity) <- c("Asian", "Māori", "Other", "Pacific")
data$Ethnicity <- factor(data$Ethnicity, levels = c("Māori", "Pacific", "Asian", "Other"))

data <- with(data, data[order(Ethnicity, Area, y, NZDep, Population),])

data$Area <- factor(data$Area)

hc <- highchart() %>%
  hc_chart(type = "column", inverted = TRUE) %>%
  hc_yAxis(title = list(text = "Number of tests per day", style = list(fontWeight = 'bold'))) %>%
  hc_xAxis(categories = levels(data$Ethnicity), title = list(text = "Priotised Ethnicity group", style = list(fontWeight = 'bold'))) %>%
  hc_plotOptions(column = list(
    stacking = "normal",
    borderColor = "#5ab1ef",
    borderWidth = 2,
    enableMouseTracking = TRUE))

for(i in 1:length(levels(data$Area))){
  lvl <- levels(data$Area)[i]
  da <- list_parse(data %>% filter(Area==lvl) %>% select(y, pop = Population, name = NZDep))
  if(i <= 10){
    hc <- hc_add_series(hc, data = da, name = lvl, visible = TRUE)
  }else{
    hc <- hc_add_series(hc, data = da, name = lvl, visible = FALSE)
  }
}

hc <- hc %>% 
  hc_tooltip(formatter= JS("function () {
  return this.x +
  '<br /> <b>Area: </b>' + this.series.name  +
  '<br /> <b>Number of tests:</b> ' + this.point.y + ' tests' +
  '<br /> <b>Population:</b> ' + this.point.pop + ' persons' +
  '<br /> <b>NZ Deprivation Index:</b> ' + this.point.name;}"))

hc