# Step 1: Load required libraries
library(datasets)  # Contains the airquality dataset
library(lattice)   # For lattice-based visualizations

# Step 2: Load dataset and preview
airquality <- datasets::airquality
head(airquality)  # Preview first few rows

# Step 3: Handling missing values
airquality <- na.omit(airquality)

# Step 4: Base Plotting System

## Histogram: Distribution of Ozone levels
hist(airquality$Ozone, main = "Ozone Distribution", xlab = "Ozone (ppb)", col = "lightblue", border = "black")

## Boxplot: Ozone levels by Month
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)", col = "lightgreen")

## Scatterplot: Wind vs Ozone
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in NYC", xlab = "Wind (mph)", ylab = "Ozone (ppb)", col = "blue", pch = 16))

## Pie Chart: Ozone values for first 5 observations
pie(airquality$Ozone[1:5], labels = airquality$Ozone[1:5], main = "Ozone Levels Pie Chart", col = rainbow(5))

# Step 5: Base Graphics Parameters
par(mfrow = c(1, 2))  # Set multiple plots (1 row, 2 columns)
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
})

# Step 6: Base Plot with Regression Line
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in NYC", pch = 20))
model <- lm(Ozone ~ Wind, airquality)  # Fit linear model
abline(model, lwd = 2, col = "red")  # Add regression line

# Step 7: Highlight May observations
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind", type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue", pch = 17))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red", pch = 8))
legend("topright", pch = c(17, 8), col = c("blue", "red"), legend = c("May", "Other Months"))

# Step 8: Add vertical line at median wind speed
abline(v = median(airquality$Wind), lty = 2, lwd = 2)

# Step 9: Lattice Graphics

## Histogram
histogram(~ Ozone, data = airquality, main = "Ozone Distribution (Lattice)", col = "lightblue")

## Scatterplot with regression line
xyplot(Wind ~ Ozone, data = airquality, type = c("p", "r"), main = "Ozone vs Wind", xlab = "Ozone", ylab = "Wind")

## Multivariate Scatterplot (by Month)
airquality <- transform(airquality, Month = factor(Month))
xyplot(Wind ~ Ozone | Month, data = airquality, layout = c(5,1), type = c("p", "r"), groups = Month,
       main = "Ozone vs Wind by Month", xlab = "Ozone", ylab = "Wind")

## Boxplot in Lattice
bwplot(Ozone ~ Month, data = airquality, main = "Ozone Levels by Month", xlab = "Month", ylab = "Ozone", col = "orange")

# Step 10: Exercises

?