library(tidyverse)
library(modelr)
#predict_points <- read_csv("data/overall_data1.csv")

predict_points <- read_csv("data/predict_points.csv")

predict_points <- predict_points %>%
  filter(team %in% c("Arsenal", "Manchester City", "Manchester United", "Liverpool", "Chelsea", "Tottenham")) %>%
  arrange(season, desc(points))


Arsenal <- predict_points %>%
  filter(team =="Arsenal")
fit_arsenal <- lm(points ~ season, data = Arsenal)
fit_arsenal[["coefficients"]]
plot_arsenal <- ggplot(Arsenal)
plot_arsenal <- plot_arsenal + geom_point(aes(x = season, y = points))
plot_arsenal <- plot_arsenal + geom_abline(intercept = -789.2500000, slope = 0.4285714, color = "red")
plot_arsenal <- plot_arsenal + labs(title = "ARS")
plot_arsenal
#data doesn't have a clear pattern
ARS_predict_points <- Arsenal %>%
  summarize(predicted_points = mean(points),
            predicted_shots_on_goal = mean(ontarget_scoring_att),
            predicted_pass_accuracy = mean(Pass),
            predicted_possession = mean(Possession))
ARS_predict_points <- ARS_predict_points %>%
  mutate(team = "ARS")
ARS_predict_points


Manchester_city <- predict_points %>%
  filter(team == "Manchester City")
fit_MC <- lm(points ~ season, data = Manchester_city)
fit_MC[["coefficients"]]
plot_MC <- ggplot(Manchester_city)
plot_MC <- plot_MC + geom_point(aes(x = season, y = points))
plot_MC <- plot_MC + geom_abline(intercept = -642.0000000, slope = 0.3571429, color = "red")
plot_MC <- plot_MC + labs(title = "MC")
plot_MC
#data doesn't have a clear pattern except for two champion season
MC_predict_points <- Manchester_city %>%
  summarize(predicted_points = mean(points),
            predicted_shots_on_goal = mean(ontarget_scoring_att),
            predicted_pass_accuracy = mean(Pass),
            predicted_possession = mean(Possession))
MC_predict_points <- MC_predict_points %>%
  mutate(team = "MC")
MC_predict_points


Manchester_united <- predict_points %>%
  filter(team == "Manchester United")
fit_MU <- lm(points ~ season, data = Manchester_united)
fit_MU[["coefficients"]]
plot_MU <- ggplot(Manchester_united)
plot_MU <- plot_MU + geom_point(aes(x = season, y = points))
plot_MU <- plot_MU + geom_abline(intercept = 6401.500000, slope = -3.142857, color = "red")
plot_MU <- plot_MU + labs(title = "MU")
plot_MU
#MU became trash after Sir Fergusson left
MU_predict_points <- Manchester_united %>%
  summarize(predicted_points = mean(points),
            predicted_shots_on_goal = mean(ontarget_scoring_att),
            predicted_pass_accuracy = mean(Pass),
            predicted_possession = mean(Possession))
MU_predict_points <- MU_predict_points %>%
  mutate(team = "MU")
MU_predict_points


Liverpool <- predict_points %>%
  filter(team == "Liverpool")
fit_LIV <- lm(points ~ season, data = Liverpool)
fit_LIV[["coefficients"]]
plot_LIV <- ggplot(Liverpool)
plot_LIV <- plot_LIV + geom_point(aes(x = season, y = points))
plot_LIV <- plot_LIV + geom_abline(intercept = -3625.083333, slope = 1.833333, color = "red")
plot_LIV <- plot_LIV + labs(title = "LIV")
plot_LIV
#Liverpool is probably getting better
LIV_predict_points <- Liverpool %>%
  summarize(predicted_points = mean(points),
            predicted_shots_on_goal = mean(ontarget_scoring_att),
            predicted_pass_accuracy = mean(Pass),
            predicted_possession = mean(Possession))
LIV_predict_points <- LIV_predict_points %>%
  mutate(team = "LIV")
LIV_predict_points


Tottenham <- predict_points %>%
  filter(team == "Tottenham")
fit_TOT <- lm(points ~ season, data = Tottenham)
fit_TOT[["coefficients"]]
plot_TOT <- ggplot(Tottenham)
plot_TOT <- plot_TOT + geom_point(aes(x = season, y = points))
plot_TOT <- plot_TOT + geom_abline(intercept = -3140.166667, slope = 1.595238 , color = "red")
plot_TOT <- plot_TOT + labs(title = "TOT")
plot_TOT
#Tottenham is definitely getting better
TOT_predict_points <- Tottenham %>%
  summarize(predicted_points = mean(points),
            predicted_shots_on_goal = mean(ontarget_scoring_att),
            predicted_pass_accuracy = mean(Pass),
            predicted_possession = mean(Possession))
TOT_predict_points <- TOT_predict_points %>%
  mutate(team = "TOT")
TOT_predict_points


Chelsea <- predict_points %>%
  filter(team == "Chelsea")
fit_CHE <- lm(points ~ season, data = Tottenham)
fit_CHE[["coefficients"]]
plot_CHE <- ggplot(Chelsea)
plot_CHE <- plot_CHE + geom_point(aes(x = season, y = points))
plot_CHE <- plot_CHE + geom_abline(intercept = -3140.166667, slope = 1.595238 , color = "red")
plot_CHE <- plot_CHE + labs(title = "CHE")
plot_CHE
#Chelsea seems to get better
CHE_predict_points <- Chelsea %>%
  summarize(predicted_points = mean(points),
            predicted_shots_on_goal = mean(ontarget_scoring_att),
            predicted_pass_accuracy = mean(Pass),
            predicted_possession = mean(Possession))
CHE_predict_points <- CHE_predict_points %>%
  mutate(team = "CHE")
CHE_predict_points


all_predictive_stats <- rbind(ARS_predict_points,
                              MC_predict_points,
                              MU_predict_points,
                              TOT_predict_points,
                              LIV_predict_points,
                              CHE_predict_points)
all_predictive_stats <- all_predictive_stats[c(5, 1, 2, 3, 4)]

write_csv(all_predictive_stats, "data/all_predictive_stats.csv")
