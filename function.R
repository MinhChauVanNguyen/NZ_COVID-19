
x <- c(95, 86.5, 80.8, 80.4, 80.3, 78.4, 74.2, 73.5, 71, 69.2, 68.6, 65.5)
y <- c(95, 102.9, 91.5, 102.5, 86.1, 70.1, 68.5, 83.1, 93.2, 57.6, 20, 126.4)
z <- c(13.8, 14.7, 15.8, 12, 11.8, 16.6, 14.5, 10, 24.7, 10.4, 16, 35.3)
name <- c("BE", "BE", "BE", "BE", "SE", "SE", "SE", "SE", "UK", "UK", "UK", "UK")
country <- c("Belgium", "Belgium", "Belgium", "Belgium",
             "Sweden", "Sweden", "Sweden", "Sweden",
             "United Kingdom", "United Kingdom", "United Kingdom", "United Kingdom")
ethnicity <- c("Maori", "Pacific", "Asian", "Other", "Maori", "Pacific", "Asian", "Other",
               "Maori", "Pacific", "Asian", "Other")


data <- data.frame(x, y, z, name, country, ethnicity)

highchart() %>%
  hc_chart(type = "bubble", plotBorderWidth = 1) %>%
  hc_add_series(data = list_parse(data[data$ethnicity == "Maori", ]), color = "#AFEEEE", name = "Maori") %>%
  hc_add_series(data = list_parse(data[data$ethnicity == "Pacific", ]), color = "#ff83c1", name = "Pacific", visible = FALSE) %>%
  hc_add_series(data = list_parse(data[data$ethnicity == "Asian", ]), color = "#7CFC00", name = "Asian", visible = FALSE) %>%
  hc_add_series(data = list_parse(data[data$ethnicity == "Other", ]),  name = "Other", visible = FALSE) %>%
  hc_xAxis(title = list(text = 'Daily fat intake'), labels = list(format = '{value} gr'), gridLineWith = 1) %>%
  hc_yAxis(startOnTick = FALSE, endOnTick = FALSE, title = list(text = 'Daily sugar intake'),
           labels = list(format = '{value} gr'), maxPadding = 0.2, gridLineWidth = 1) %>%
  hc_plotOptions(
    series = list(
      dataLabels = list(
        enabled = TRUE, format = '{point.name}'))) %>%
  hc_tooltip(
    useHTML = TRUE, 
    headerFormat = '<table>',
    pointFormat = "<tr><th colspan='2'><h3>{point.country}</h3></th></tr> 
       <tr><th>Fat intake:</th><td>{point.x}g</td></tr> 
       <tr><th>Sugar intake:</th><td>{point.y}g</td></tr> 
       <tr><th>Obesity (adults):</th><td>{point.z}%</td></tr>",
    footerFormat ='</table>',
    followPointer = TRUE)


