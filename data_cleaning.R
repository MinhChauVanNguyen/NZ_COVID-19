library(openxlsx)

MAIN_PATH <- paste0(getwd(), "/")


# make sure all the xlsx files are closed before running the below command
file.list <- list.files(path = paste0(MAIN_PATH, "data"),
                        pattern='*.xlsx')

# remove otago_NZDEP.xlsx
file.list <- file.list[-17]

dat <- lapply(file.list, function(i){
  
  x <- read.xlsx(i, sheet = 2, startRow = 3,
                 cols = c(2, 3, 5, 11),
                 sep.names = "_")
  
  names(x) <- c("SA22018_Code", "SA22018_Name", "TA2018_Name", "Region")
  
  x
})

regional_data <- do.call("rbind.data.frame", dat)

# new zealand deprivation index by SA22018 AREA data
NZ_Dep <- read.xlsx("otago_NZDEP.xlsx")

NZ_Dep <- NZ_Dep[,-4]

names(NZ_Dep) <- c("SA22018_Code", "SA22018_Name", "SA22018_NZDep", "SA22018_Total_Pop")

# combine the data 
full_data <- merge(regional_data, NZ_Dep, by = c("SA22018_Code", "SA22018_Name"))
full_data <- unique(full_data[ , 1:6])   # remove duplicates, n = 2078

# remove data where total population equals to 0
full_data <- full_data[!(full_data$SA22018_Total_Pop == 0), ]

# add Ethnicity column
full_data <- full_data[rep(seq_len(nrow(full_data)), each = 4), ]

full_data$Ethnicity <- rep(c("Māori", "Pacific", "Asian", "Other"), times = 2078)

full_data$SA22018_Code <- factor(full_data$SA22018_Code)

# generate random number of people for each ethnicity group
full_data$SA22018_Code <- factor(full_data$SA22018_Code)
total_area_pop <- c()

for(i in 1:length(levels(full_data$SA22018_Code))){
  lvl <- levels(full_data$SA22018_Code)[i]
  total_area_pop[i] <- full_data$SA22018_Total_Pop[full_data$SA22018_Code == lvl][1]
}

# algorithms
set.seed(3)

big_list <- list()

for(i in 1:length(total_area_pop)){
  required_sum <- total_area_pop[i]
  rnd_num <- runif(4, min = 0, max = required_sum)
  rnd_sum <- sum(rnd_num)
  big_list[[i]] <- round(c((rnd_num[1]*required_sum)/rnd_sum, 
                     (rnd_num[2]*required_sum)/rnd_sum, 
                     (rnd_num[3]*required_sum)/rnd_sum,
                     (rnd_num[4]*required_sum)/rnd_sum))
}

full_data$Population_by_Ethnicity <- unlist(big_list)

set.seed(3)

# add Number_of_cases columns
for(i in 1:length(full_data$Population_by_Ethnicity)){
  if(full_data$Population_by_Ethnicity[i] > 10){
    full_data$Number_of_cases[i] <- round(runif(1, min = 0, max = 10))
  }else{
    full_data$Number_of_cases[i] <- round(runif(1, min = 0, max = 1))
  }
}

full_data <- replace(full_data, is.na(full_data), 0)


full_data$Region[full_data$Region == "Manawatu-Wanganui Region"] <- "Manawatū-Whanganui Region"

full_data$Ethnicity <- factor(full_data$Ethnicity, levels = c("Māori", "Pacific", "Asian", "Other"))

full_data$TA2018_Name <- factor(full_data$TA2018_Name)

full_data$SA22018_NZDep[full_data$SA22018_NZDep == 0] <- "NA"

#setwd(MAIN_PATH)


