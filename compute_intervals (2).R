library(tidyverse)
champions_data <- read_csv("data/champions_data.csv")

#compute average of the stats without removing leicester
champions_data_1 <- champions_data %>%
  summarize(avg_pts = mean(points),
            avg_shots_on_goal = mean(avg_shots_on_target),
            avg_pass_accuracy = mean(avg_pass_rate),
            avg_possession = mean(avg_possession))
champions_data_mean <- champions_data_1

#convert into a proper tibble
predict_with_lei <- tibble(stats = c("avg_pts", "avg_shots_on_goal", "avg_pass_accuracy", "avg_possession"),
                           avg = c(as.double(unlist(champions_data_mean["avg_pts"])),
                                   as.double(unlist(champions_data_mean["avg_shots_on_goal"])),
                                   as.double(unlist(champions_data_mean["avg_pass_accuracy"])),
                                   as.double(unlist(champions_data_mean["avg_possession"]))))
predict_with_lei

#after removing leicester, because they are so abnormal, compute the same value again
champions_data_without_Lei <- champions_data %>%
  filter(! team == "Leicester")


#compute the average of stats without leicester
champions_data_2 <- champions_data_without_Lei %>%
  summarize(avg_pts = mean(points),
            avg_shots_on_goal = mean(avg_shots_on_target),
            avg_pass_accuracy = mean(avg_pass_rate),
            avg_possession = mean(avg_possession))
champions_data_mean_witout_lei <- champions_data_2

#convert into a proper tibble
predict_without_lei <- tibble(stats = c("avg_pts", "avg_shots_on_goal", "avg_pass_accuracy", "avg_possession"),
                              avg = c(as.double(unlist(champions_data_mean_witout_lei["avg_pts"])),
                                      as.double(unlist(champions_data_mean_witout_lei["avg_shots_on_goal"])),
                                      as.double(unlist(champions_data_mean_witout_lei["avg_pass_accuracy"])),
                                      as.double(unlist(champions_data_mean_witout_lei["avg_possession"]))))


#compute the standard deviation of stats with leicester
champions_data_sd <- champions_data %>%
  summarize(sd_pts = sd(points),
            sd_shots_on_goal = sd(avg_shots_on_target),
            sd_pass_accuracy = sd(avg_pass_rate),
            sd_possession = sd(avg_possession))
champions_data_sd

#add the sd to the previous tibble
predict_with_lei <- predict_with_lei %>%
  mutate(sd = c(as.double(unlist(champions_data_sd["sd_pts"])),
                as.double(unlist(champions_data_sd["sd_shots_on_goal"])),
                as.double(unlist(champions_data_sd["sd_pass_accuracy"])),
                as.double(unlist(champions_data_sd["sd_possession"]))))


#compute the standard deviation without leicester
champions_data_sd_without_lei <- champions_data_without_Lei %>%
  summarize(sd_pts = sd(points),
            sd_shots_on_goal = sd(avg_shots_on_target),
            sd_pass_accuracy = sd(avg_pass_rate),
            sd_possession = sd(avg_possession))
champions_data_sd_without_lei

#add the sd to the previous tibble
predict_without_lei <- predict_without_lei %>%
  mutate(sd = c(as.double(unlist(champions_data_sd_without_lei["sd_pts"])),
                as.double(unlist(champions_data_sd_without_lei["sd_shots_on_goal"])),
                as.double(unlist(champions_data_sd_without_lei["sd_pass_accuracy"])),
                as.double(unlist(champions_data_sd_without_lei["sd_possession"]))))




#function to predict threshold & next season's performance
t_interval <- function(mu, sd, n, CL){
  error <- qt(CL, df=n-1)*sd/sqrt(n)
  left <- mu - error
  right <- mu + error
  result <- tibble(left_end = left, right_end = right)
  return(result)
}

t_interval(5, 2, 20, 0.975)

norm_interval <- function(mu, sd, n, CL){
  error <- qnorm(CL)*sd/sqrt(n)
  left <- mu - error
  right <- mu + error
  result <- tibble(left_end = left, right_end = right)
  return(result)
}

trial <- norm_interval(5, 2, 20, 0.975)


#estimate without lei
interval_pts_7 <- t_interval(87.14286, 3.976119, 7, 0.95)
interval_shots_7 <- t_interval(6.071429, 0.7977222, 7, 0.95)
interval_pass_7 <- t_interval(83.78571, 1.75445, 7, 0.95)
interval_possession_7 <- t_interval(55.25714, 1.167415, 7, 0.95)
intervals_7 <- rbind(interval_pts_7,
                     interval_shots_7,
                     interval_pass_7,
                     interval_possession_7)
intervals_7 <- mutate(intervals_7, stats = c("avg_pts", "avg_shots_on_goal", "avg_pass_accuracy", "avg_possession"))


#estimate with lei
interval_pts_8 <- t_interval(86.375, 4.274091, 8, 0.95)
interval_shots_8 <- t_interval(5.901316, 0.8814527, 8, 0.95)
interval_pass_8 <- t_interval(82.125, 4.970125, 8, 0.95)
interval_possession_8 <- t_interval(53.9375, 3.885849, 8, 0.95)
intervals_8 <- rbind(interval_pts_8,
                     interval_shots_8,
                     interval_pass_8,
                     interval_possession_8)
intervals_8 <- mutate(intervals_8, stats = c("avg_pts", "avg_shots_on_goal", "avg_pass_accuracy", "avg_possession"))


#combine the "interval" data and "avg-sd" data together
all_prediction_with_lei <- merge(intervals_8, predict_with_lei, by = "stats")
all_prediction_with_lei <- all_prediction_with_lei[c(1, 4, 5, 2, 3)]
colnames(all_prediction_with_lei)[colnames(all_prediction_with_lei) == "avg"] <- "center(mean)"

all_prediction_without_lei <- merge(intervals_7, predict_without_lei, by = "stats")
#all_prediction_without_lei <- all_prediction_without_lei %>%
  #select(-avg.x, -avg.y)
all_prediction_without_lei <- all_prediction_without_lei[c(1, 4, 5, 2, 3)]
colnames(all_prediction_without_lei)[colnames(all_prediction_without_lei) == "avg"] <- "center(mean)"

write_csv(all_prediction_with_lei, "data/threshold_with_lei.csv")
write_csv(all_prediction_without_lei, "data/threshold_without_lei.csv")



#standardize the data point to calculate the area under curve
standardize <- function(x, mu, sd){
  z = (x - mu) / sd
  return(z)
}
