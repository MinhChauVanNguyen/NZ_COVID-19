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
      subset_data <- data[which(data$Number_of_Tests < data$Census_2019_Population_Estimate & data$SA2_average_NZDep2018 > 5),]
    }else{
      subset_data <- data[which(data$Number_of_Tests < data$Census_2019_Population_Estimate & data$SA2_average_NZDep2018 < 5),]
    }
    subset_data <- subset_data[,!(names(subset_data) %in% c("SA22018_V1_00", "SA2_average_NZDep2018_score"))]
    big_list[[i]] <- subset_data
  }
  
  data <- do.call(rbind.data.frame, big_list)
  names(data) <- c("Ethnicity", "NZDep", "Area", "District", "Tests", "Population")
  data <- data %>% mutate(Rate = Tests/Population)
  data <- data[which(data$Rate < 0.20),] 
  data$Rate <- percent(data$Rate, 1)
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
