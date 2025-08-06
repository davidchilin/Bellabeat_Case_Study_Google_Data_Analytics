monthSec <- 33*31*24*60*60
monthMin <- 33*31*24*60
monthHour <- 33*31*24
monthDay <- 33*31

heartrateData <- read.csv(here(data_dir,"heartrate_seconds_merged.csv"))

pie_data <- data.frame(
  variable = c("Weight",
               " Heartrate",
               " Sleep",
               "Calories/Steps",
               "Daily\nActivity/Intensity",
               "Daily\nSteps"
               ),

  pctUsers = round(1/33 * 100 * c(
                n_distinct(weightLogInfo$Id),
                n_distinct(heartrateData$Id),
                n_distinct(sleepDay$Id),
                n_distinct(dailyCalories$Id),
                n_distinct(dailyActivity$Id),
                n_distinct(dailySteps$Id)
                ), 1),

  pctObs = round(100 * c(
                nrow(weightLogInfo)/ monthDay,
                nrow(heartrateData)/monthSec,
                nrow(sleepDay)/monthDay,
                nrow(dailyCalories)/monthDay,
                nrow(dailyActivity)/monthDay,
                (nrow(dailySteps)-78)/ monthDay # 78 Daily Steps of Zero
                ),1)
  )

a <- ggplot(pie_data,
            aes(
              x = variable,
              y = pctUsers,
              fill = factor(variable)
            )) +
  labs(
    title = "Feature Usage",
    subtitle = "by % User"
  ) +
  geom_col(width = 1, color = "white") +
  coord_polar() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

b <- ggplot(pie_data,
            aes(
              x = variable,
              y = pctObs,
              fill = factor(variable)
            )) +
  labs(
    subtitle = "by % Observations"
  ) +
  geom_col(width = 1, color = "white") +
    coord_polar() +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(face = "bold"),
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 12)
    )