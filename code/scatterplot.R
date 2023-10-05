attach(USA_Women_2022_2023)
library(readxl)
USA_WOMEN_2022_2023 <- read_excel("Downloads/USA WOMEN 2022-2023.xlsx")
View(USA_WOMEN_2022_2023)
attach(USA_WOMEN_2022_2023)
df <- USA_WOMEN_2022_2023
aggregate(df$Score, by=list(df$LastName, df$Apparatus), function(x) c(mean = mean(x), sd = sd(x)))
AGTab <- aggregate(df$Score, by=list(df$LastName, df$Apparatus), function(x) c(mean = mean(x), sd = sd(x)))
str(AGTab)
df <- df %>%
mutate(LastName = tolower(LastName))
result <- df %>%
group_by(LastName) %>%
summarize(Combined_Score = mean(Score))
print(result)
df <- df %>%
mutate(LastName = tolower(LastName))
result <- df %>%
group_by(LastName, Apparatus) %>%
summarize(Combined_Score = mean(Score))
apparatus_to_plot <- "BB"
df_filtered <- df %>%
filter(Apparatus == apparatus_to_plot)
df_combined <- df_filtered %>%
group_by(LastName) %>%
summarize(
Combined_Name = unique(LastName),
Mean_Score = mean(Score),
SD_Score = sd(Score))
ggplot(df_combined, aes(x = Mean_Score, y = SD_Score, label = Combined_Name)) +
geom_point() +
geom_text(aes(label = Combined_Name), hjust = 0, vjust = 0) +
labs(title = paste("SD vs. Mean for Combined Names (Apparatus", apparatus_to_plot, ")"),
x = "Mean Score",
y = "Standard Deviation Score") +
theme_minimal()
ggplot(df_combined, aes(x = SD_Score, y = Mean_Score, label = Combined_Name)) +
geom_point() +
geom_text(aes(label = Combined_Name), hjust = 0, vjust = 0) +
labs(title = paste("SD vs. Mean for Combined Names (Apparatus", apparatus_to_plot, ")"),
x = "Mean Score",
y = "Standard Deviation") +
theme_minimal()
ggplot(df_combined, aes(y = Mean_Score, x = SD_Score, label = Combined_Name)) +
geom_point() +
geom_text(aes(label = Combined_Name), hjust = 0, vjust = 0) +
labs(title = paste("SD vs. Mean for Combined Names (Apparatus", apparatus_to_plot, ")"),
x = "Standard Deviation",
y = "Mean") +
theme_minimal()
df <- df %>%
mutate(LastName = tolower(LastName))
result <- df %>%
group_by(LastName, Apparatus) %>%
summarize(Combined_Name = unique(LastName),
Mean_Score = mean(Score),
SD_Score = sd(Score))
