library(wbstats)
library(dplyr)


euclidean_distance <- function(x,y)
{
  return(sum((x-y)^2))
}

find_closest_country <- function(country,country_year,variable_list,valid_countries)
{
  out <- tryCatch(
    {
  country_year_data = wb_data(indicator=variable_list,country=country,start_date = country_year,end_date = country_year)
  country_year_data <- country_year_data %>% na.exclude()
  country_year_data <- country_year_data %>% group_by(country) %>% select(variable_list)
  if (nrow(country_year_data) != 1)
  {
    return(data.frame())
  }
  
  # get most rest of world variables
  world_current_data <- wb_data(indicator=variable_list,country=valid_countries,mrv=1)
  # here we remove na values
  # basically we have downloaded current most recent values, and combine
  world_current_data<-world_current_data %>% group_by(country) %>% select(variable_list) %>% 
    replace(is.na(.),0) %>% summarise_all(sum) %>% na_if(0) %>% na.exclude()
  #remove original country from current year
  world_current_data <- world_current_data %>% filter(country!=!!country)
  #append country year data to world data
  world_data <- rbind(country_year_data,world_current_data)
  
  # find closest match
  #first normalize
  min_max_norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
  new_variable_list = c()
  for (i in 1:length(variable_list))
  {
    variable <- variable_list[i]
    new_variable <- paste0(variable,".new")
    new_variable_list <- append(new_variable_list,new_variable)
    world_data[,new_variable] <- lapply(world_data[,variable],min_max_norm)
    #world_data[,new_variable] <- scale(world_data[,variable])
  }
  
  distance_list <- apply(world_data[2:nrow(world_data),variable_list],1,euclidean_distance,world_data[1,variable_list])
  
  
  # we add 1, because first row is original country
  index <- which.min(distance_list) + 1
  
  closest_country_data <- world_data[c(1,index),]
  closest_country_data <- closest_country_data %>% select(c("country",variable_list))
  return(closest_country_data)
    },
  error=function(cond)
  {
    return(data.frame())
  }
  )
}








