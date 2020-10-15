library(formattable)
library(highcharter)

fake_dt <- read_excel("Analyst_Lab_Data 2.xls")
true_dt <- read_excel("Population_Data 2.xls")
names(true_dt)[2] <- "SA22018_V1_00"

# excluding those that replied "Unknown" for Ethnicity (n = 1656 observations)
full_dt <- merge(fake_dt, true_dt, by = c("SA22018_V1_00", "Ethnicity"))

full_dt$TA2018_V1_00_NAME <- factor(full_dt$TA2018_V1_00_NAME)

big_list <- list()

test_rate <- function(highly_deprived_areas = TRUE){
  for(i in 1:length(levels(dd$TA2018_V1_00_NAME))){
    lvl <- levels(full_dt$TA2018_V1_00_NAME)[i]
    data <- full_dt[full_dt$TA2018_V1_00_NAME == lvl,]
    if(highly_deprived_areas == TRUE){
      subset_data <- data[which(data$Number_of_Tests < data$Census_2019_Population_Estimate & data$SA2_average_NZDep2018 >= 5),]
    }else{
      subset_data <- data[which(data$Number_of_Tests < data$Census_2019_Population_Estimate & data$SA2_average_NZDep2018 < 5),]
    }
    subset_data <- subset_data[,!(names(subset_data) %in% c("SA22018_V1_00", "SA2_average_NZDep2018_score"))]
    big_list[[i]] <- subset_data
  }
  
  data <- do.call(rbind.data.frame, big_list)
  names(data) <- c("Ethnicity", "NZDep", "Area", "District", "Tests", "Population")
  data <- data %>% mutate(Rate = (Tests/Population)*100)
  data <- data[which(data$Rate < 20),] 
  #data$Rate <- percent(data$Rate, 1)
  data$District <- factor(data$District)
  
  return(data)
}

highly_deprived <- test_rate()
low_deprived <- test_rate(FALSE)

# regions that both in low_deprived data and high_deprived data
levels(highly_deprived$District)[which(levels(highly_deprived$District) %in% levels(low_deprived$District))]

# regions that are in high_deprived data and not in low_deprived data
setdiff(levels(highly_deprived$District), levels(low_deprived$District))

# regions that are in low_deprived data and not in high_deprived data
setdiff(levels(highly_deprived$District), levels(highly_deprived$District))


df <- rbind(highly_deprived, low_deprived)

df <- df %>% group_by(NZDep) %>%
  mutate(Ave_rate = round(mean(Rate), 2)) %>%
  select(NZDep, Ave_rate)

df <- unique(df[,c(1,2)])

df$NZDep <- factor(df$NZDep)

df %>%
  hchart(type = "column", borderColor = "lightskyblue",
         borderWidth = 2,
         hcaes(x = levels(NZDep) , y = Ave_rate, 
               color = c("#cce7e8", "#69bdd2", "#44bcd8", "#195e83", "#3b4994", 
                         "#dfb0d6", "#a6add3", "#ba95c3", "#a164ab", "#4B0082"))) %>%
  hc_xAxis(title = list(text = "NZ Deprivation Index"),
           labels = list(style = list(fontWeight = 'bold'))) %>%
  hc_yAxis(labels = list(formatter = JS("function() {
        return this.value +'%';}")),
           title = list(text = "Average testing rate")) %>%
  hc_tooltip(useHTML = TRUE, borderWidth = 2,
  formatter= JS("function () {
  return '<span style =\"color:' + this.point.name.color  + ';\"> \u25CF ' + '<b>NZ Deprivation Index:</b> ' +  '</span>' + this.point.name  +
  '<br />' + '\u25CF Average testing rate: ' + this.y +'%'}")) %>%
  hc_title(text = "<b>Low average COVID-19 testing rates (below 20 tests per 100 persons) by NZ deprivation index</b>")

