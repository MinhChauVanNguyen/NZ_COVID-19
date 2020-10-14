fake_dt <- read_excel("Analyst_Lab_Data 2.xls")
true_dt <- read_excel("Population_Data 2.xls")
names(true_dt)[2] <- "SA22018_V1_00"

# excluding those that replied "Unknown" for Ethnicity (n = 1656 observations)
dd <- merge(fake_dt, true_dt, by = c("SA22018_V1_00", "Ethnicity"))

dd$TA2018_V1_00_NAME <- factor(dd$TA2018_V1_00_NAME)

big_list <- list()

for(i in 1:length(levels(dd$TA2018_V1_00_NAME))){
  lvl <- levels(dd$TA2018_V1_00_NAME)[i]
  data <- dd[dd$TA2018_V1_00_NAME == lvl,]
  subset_data <- data[which(data$Number_of_Tests < data$Census_2019_Population_Estimate & data$Number_of_Tests < 400 & data$SA2_average_NZDep2018 > 5),]
  subset_data <- subset_data[,!(names(subset_data) %in% c("SA22018_V1_00", "SA2_average_NZDep2018_score"))]
  big_list[[i]] <- subset_data
}

big_list

new_data <- do.call(rbind.data.frame, big_list)
View(new_data)
names(new_data) <- c("Ethnicity", "NZDep", "Area", "Region", "Tests", "Population")
new_data$Population <- round(new_data$Population)
more_data <- new_data %>% mutate(Rate = (Tests/Population)*100)
more_data <- new_data %>% group_by(Region, Ethnicity) %>%
  summarize_at(c("Tests", "Population"), sum)

View(more_data)
