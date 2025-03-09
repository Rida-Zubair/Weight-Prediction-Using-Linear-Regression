
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////// Load the data/////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

data <- read.csv("D:/academia/university/semester3/prob abd stat/prob_weight/processed_weight_final1.csv")

#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////// step 1/////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

cat("Step 1: ","\n")

# Function to calculate mode
get_mode <- function(x) {
  unique_x <- unique(x)
  freq <- tabulate(match(x, unique_x))
  unique_x[which.max(freq)]
}

# Explicit calculations for each variable
for (col in colnames(data)) {
  cat("Variable:", col, "\n")
  
  cat("Mean:", mean(data[[col]], na.rm = TRUE), "\n")
  cat("Median:", median(data[[col]], na.rm = TRUE), "\n")
  cat("Mode:", get_mode(data[[col]]), "\n")
  cat("1st Quartile:", quantile(data[[col]], 0.25, na.rm = TRUE), "\n")
  cat("3rd Quartile:", quantile(data[[col]], 0.75, na.rm = TRUE), "\n")
  cat("Minimum:", min(data[[col]], na.rm = TRUE), "\n")
  cat("Maximum:", max(data[[col]], na.rm = TRUE), "\n")
  cat("Range:", range(data[[col]], na.rm = TRUE), "\n\n")
}
#the mean median mode can be represented this way too
#summary(data)


#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////// step 1/////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# Remove rows with non-finite values
install.packages("reshape2")  # Install the package
library(reshape2)             # Load the package

clean_data <- data[complete.cases(data), ]

# Proceed with melting and plotting
melted_data <- melt(clean_data)
# the box and wisker plot of weight and height as the valuse were much higher for these two
library(reshape2)

# Select Height and Weight columns
height_weight_data <- data[, c("Height_cm", "Weight")]

# Melt the data for ggplot
melted_hw <- melt(height_weight_data, variable.name = "Measurement", value.name = "Value")

# Create combined boxplot
ggplot(melted_hw, aes(x = Measurement, y = Value, fill = Measurement)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(
    title = "Boxplot for Height and Weight",
    x = "Measurement",
    y = "Value"
  ) +
  scale_fill_manual(values = c("Height_cm" = "skyblue", "Weight" = "pink")) +
  theme_minimal()
melted_other <- melt(other_variables)



# the box and whisker for other all variables

# Load necessary library
library(reshape2)
library(ggplot2)

# Remove Height and Weight to focus on other variables
other_variables <- data[, !(colnames(data) %in% c("Height_cm", "Weight"))]

# Melt the data for ggplot
melted_other <- melt(other_variables, variable.name = "Variable", value.name = "Value")

# Create a boxplot for the other variables
ggplot(melted_other, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  theme_minimal() +
  labs(
    title = "Box and Whisker Plots for Other Variables",
    x = "Variables",
    y = "Values"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )



# all in one  graphs
#install.packages("GGally")

# Convert Gender to a factor
data$Gender <- as.factor(data$Gender)

# Load the GGally library
library(GGally)

# Pairwise scatterplots for all variables
ggpairs(
  data,
  title = "Scatter Plot Grid",
  aes(color = Gender)  # Using Gender as a categorical variable
)

# scatter only
# Load ggplot2
library(ggplot2)

# Select numeric columns excluding Weight
numeric_vars <- data[, sapply(data, is.numeric)]
numeric_vars <- numeric_vars[, !colnames(numeric_vars) %in% c("Weight")]

# Loop through each numeric variable to plot against Weight
for (var in colnames(numeric_vars)) {
  p <- ggplot(data, aes_string(x = "Weight", y = var)) +
    geom_point(alpha = 0.7, color = "blue") +
    labs(
      title = paste("Scatter Plot of Weight vs", var),
      x = "Weight",
      y = var
    ) +
    theme_minimal()
  print(p)
}

# step 5 regrassion model
# Fit a Multiple Linear Regression Model
mlr_model <- lm(Weight ~ Gender + Processed.Food + Medical.History.Genetics +  Height_cm, data = data)

# Summary of the regression model
summary(mlr_model)

# Interpret the Coefficient of Determination
cat("Coefficient of Determination (R^2):", summary(mlr_model)$r.squared, "\n")


# coffe of determination
# Fit the Multiple Linear Regression Model
mlr_model <- lm(Weight ~ Gender + Processed.Food + Medical.History.Genetics +  Height_cm, data = data)

# View the summary of the model
summary(mlr_model)

# Extract R² (Coefficient of Determination)
r_squared <- summary(mlr_model)$r.squared
cat("Coefficient of Determination (R²):", r_squared, "\n")
