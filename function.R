data$SA22018_V1_00_NAME <- factor(data$SA22018_V1_00_NAME)

hc <- highchart() %>%
  hc_chart(type = "column", height = 350, width = 400) %>%
  hc_yAxis(title = list(text = "Number of tests", style = list(fontWeight = 'bold'))) %>%
  hc_xAxis(categories = levels(data$Ethnicity), title = list(text = "Priotised Ethnicity group", style = list(fontWeight = 'bold'))) %>%
  hc_plotOptions(column = list(
    dataLabels = list(enabled = FALSE),
    stacking = "normal",
    borderColor = "#5ab1ef",
    borderWidth = 2,
    enableMouseTracking = TRUE)
  ) 

hc_series(list(name="Tangoge", data = data[data$SA22018_V1_00_NAME == "Tangonge", ][,4], color = "#AFEEEE"),
          list(name="Kaitaia East", data = data[data$SA22018_V1_00_NAME == "Kaitaia East", ][,4], color = "#ff83c1"),
          list(name="Kaitaia West", data = data[data$SA22018_V1_00_NAME == "Kaitaia West", ][,4], color = "#7CFC00"))

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

