library(readxl)
df <- read_excel("pots5ldata.xlsx")
str(df)
model <- lm(Moisture_meter_readings ~ RWC, data = df)
summary(model)
df$RWC_percent <- df$RWC * 100
pred_points <- data.frame(
  RWC = c(0.60, 0.30)
)
pred_points$Moisture_meter_readings <- predict(model, pred_points)
pred_points
pred_points$RWC_percent <- pred_points$RWC * 100

m <- coef(model)[2]
c <- coef(model)[1]
r2 <- summary(model)$r.squared
eq_label <- paste0(
  "y = ", round(m, 2), "x + ", round(c, 2),
  "\nR² = ", round(r2, 2)
)

eq_label

# Understanding the Relationship
library(ggplot2)
ggplot(df, aes(x = RWC_percent, y = Moisture_meter_readings)) +
  geom_point(
    size = 3,
    color = "#2C7BB6",
    alpha = 0.8
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#D7191C",
    linewidth = 1
  ) +
  labs(
    title = "Relationship between Relative Water Content and Soil Moisture Meter Reading",
    x = "Relative Water Content (%)",
    y = "Soil Moisture Meter Reading (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )


#Predicting corresponding readings

ggplot(data = df, aes(x = RWC_percent, y = Moisture_meter_readings)) +
  
  # Data points
  geom_point(size = 3, color = "#2C7BB6", alpha = 0.8) +
  
  # Regression line + CI
  geom_smooth(method = "lm", se = TRUE,
              color = "#D7191C", linewidth = 1) +
  
  # Vertical lines at 30% and 60%
  geom_vline(xintercept = c(30, 60),
             linetype = "dashed",
             color = "grey40") +
  
  # Predicted points
  geom_point(data = pred_points,
             aes(x = RWC_percent, y = Moisture_meter_readings),
             size = 4,
             shape = 21,
             fill = "yellow",
             color = "black") +
  
  # Labels for predicted points
  geom_text(data = pred_points,
            aes(x = RWC_percent, y = Moisture_meter_readings,
                label = round(Moisture_meter_readings, 1)),
            vjust = -1.2,
            size = 4.5) +
  
  # Equation + R²
  annotate(
    "text",
    x = min(df$RWC_percent) + 0.02,
    y = max(df$Moisture_meter_readings),
    label = eq_label,
    hjust = 0,
    size = 5,
    fontface = "bold"
  ) +
  
  labs(
    title = "Calibration of Soil Moisture Meter Using Relative Water Content",
    x = "Relative Water Content (%)",
    y = "Soil Moisture Meter Reading (%)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )
ggsave(
  "RWC_Moisture_calibration5Lpots.png",
  width = 8,
  height = 6,
  dpi = 300
)



