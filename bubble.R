new <- dd[dd$TA2018_V1_00_NAME == "Napier City",]
data <- new[names(new) %in% c("Ethnicity", "SA22018_V1_00", "SA22018_V1_00_NAME", "Number_of_Tests", "SA2_average_NZDep2018", "Census_2019_Population_Estimate")]

# x - population, y - number of tests, z - NZDep, name - SA2
names(data) <- c("name", "ethnicity", "NZDep", "area", "y", "Population")

data$ethnicity <- factor(data$ethnicity)
levels(data$ethnicity) <- c("Asian", "Māori", "Other", "Pacific")
data$ethnicity <- factor(data$ethnicity, levels = c("Māori", "Pacific", "Asian", "Other"))

data$Population <- round(data$Population)

highchart() %>%
  hc_chart(type = "bubble", plotBorderWidth = 1) %>%
  hc_add_series(data = list_parse(df[df$ethnicity == "Māori", ]), color = "#AFEEEE", name = "Māori") %>%
  hc_add_series(data = list_parse(df[df$ethnicity == "Pacific", ]), color = "#ff83c1", name = "Pacific", visible = FALSE) %>%
  hc_add_series(data = list_parse(df[df$ethnicity == "Asian", ]), color = "#7CFC00", name = "Asian", visible = FALSE) %>%
  hc_add_series(data = list_parse(df[df$ethnicity == "Other", ]),  name = "Other", visible = FALSE) %>%
  hc_xAxis(title = list(text = 'Population estimate'), gridLineWith = 1) %>%
  hc_yAxis(startOnTick = FALSE, endOnTick = FALSE, title = list(text = 'Number of tests'),
           maxPadding = 0.2, gridLineWidth = 1) %>%
  hc_plotOptions(
    series = list(
      dataLabels = list(
        enabled = TRUE, format = '{point.name}',
        style = list(
          fontSize = '9px'
        )))) %>%
  hc_tooltip(
    useHTML = TRUE, headerFormat = '<table>', footerFormat = '</table>', pointFormat = "<tr><th colspan=2><h3>{point.area}</h3></th></tr> 
       <tr><th>Population estimate:</th><td>{point.x} persons</td></tr> 
       <tr><th>Number of tests:</th><td>{point.y} tests</td></tr> 
       <tr><th>NZ Dep:</th><td>{point.z}</td></tr>",
    followPointer = TRUE) %>%
  hc_legend(
    layout = 'horizontal',
    align =  'center',
    verticalAlign ='top',
    floating = TRUE
  ) 



