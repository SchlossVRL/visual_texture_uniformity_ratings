#clear environment 
rm(list=ls())

#libraries
library(tidyverse)
library(skimr)
library(broom)
library(psych)
library(ggimage)
library(grid) 
library(png)

#load data
data <- read.csv("uniformity_ratings_clean.csv")

# Rating Differences -----
rating_differences <- data %>% 
  group_by(subject_id, image_path) %>% 
  summarize(
    rating1 = response[1],
    rating2 = response[2],
    rating_diff = rating1 - rating2, .groups = "drop") %>% 
  mutate(rating_diff = abs(rating_diff))

print(rating_differences)

average_differences <- rating_differences %>% 
  group_by(image_path) %>% 
  summarize(avg_rating_diff = mean(rating_diff, na.rm = TRUE), .groups = "drop")
 

print(average_differences)

full_image_set <- average_differences %>%
  select(image_path)
  
write.csv(full_image_set, "full_image_set.csv", row.names = FALSE)
# Participant correlations -----

# Calculate the within-subject correlations for each participant
correlations <- rating_differences %>%
  group_by(subject_id) %>%
  summarize(correlation = cor(rating1, rating2, use = "complete.obs"), .groups = "drop")

print(correlations)

# Average Ratings ------
## Error and Confidence Interval
# Calculate confidence intervals for each image_path
ci_data <- data %>%
  group_by(image_path) %>%
  summarise(
    mean_rating = mean(response, na.rm = TRUE),
    ci_lower = mean(response, na.rm = TRUE) - qt(0.975, df = n() - 1) * sd(response, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean(response, na.rm = TRUE) + qt(0.975, df = n() - 1) * sd(response, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"  # Drop grouping after summarize
  )

# View the confidence intervals
print(ci_data)

ci_data <- ci_data %>% 
  arrange(desc(mean_rating))

#error bars
# Calculate mean and standard error for each image_path
error_data <- data %>%
  group_by(image_path) %>%
  summarise(
    mean_rating = mean(response, na.rm = TRUE),
    se = sd(response, na.rm = TRUE) / sqrt(n()),  # Standard error of the mean
    error_lower = mean_rating - se,               # Lower bound for error bars
    error_upper = mean_rating + se,               # Upper bound for error bars
    .groups = "drop"  # Drop grouping after summarize
  )

# View the error bars
print(error_data)

# Arrange data by mean rating in descending order
error_data <- error_data %>% 
  arrange(desc(mean_rating))


# Select subset
# Create a new data frame with ci_lower above 0
#subset_images_data <- ci_data %>%
  #filter(ci_lower > 0)
subset_images_data <- ci_data %>%
  filter(mean_rating > 0)

#bars above 0

ratings <- data %>%
  group_by(image_path) %>%
  summarise(
    mean_rating = mean(response, na.rm = TRUE),
    .groups = "drop"  # Drop grouping after summarize
  )

subset_images_data_bars <- ratings %>%
  filter(mean_rating > 0)

export_subset <- subset_images_data %>% 
  mutate(image_path = str_remove(image_path, "images/")) %>% 
  select(image_path) 

write.csv(export_subset, "uniformity_ratings_subset.csv", row.names = FALSE)

#prep for new triplets study
export_subset_paths <- subset_images_data_bars %>% 
  mutate(image_path = str_remove(image_path, "images/"),
         image_path = paste0("materials/imgs/", image_path)) %>% 
  select(image_path) 

write.csv(export_subset_paths, "uniformity_ratings_subset_paths.csv", row.names = FALSE)

#Plotting ----------------------------------------------------------------------
# Plotting Uniformity Ratings
ggplot(ci_data, aes(x = reorder(image_path, -mean_rating), y = mean_rating)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "darkblue") +
  labs(
    title = "Average Uniformity Ratings by Image",
    x = "Image",
    y = "Average Uniformity Rating"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text labels
    axis.ticks.x = element_blank()   # Remove x-axis ticks
  )

#check beans
ggplot(ci_data, aes(x = reorder(image_path, -mean_rating), y = mean_rating)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "darkblue") +
  labs(
    title = "Average Uniformity Ratings by Image",
    x = "Image",
    y = "Average Uniformity Rating"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plotting Subset Uniformity Ratings
# Create a new column for the position of the images
subset_images_data <- subset_images_data %>% 
  mutate(
    position_zero = 0
  )
subset_images_data$image_position <- subset_images_data$position_zero - 15
ggplot(subset_images_data, aes(x = reorder(image_path, -mean_rating), y = mean_rating)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "darkblue") +
  geom_image(aes(image = image_path, y = image_position), size = 0.1, by = "width") +
  labs(
    title = "Average Uniformity Ratings by Image",
    x = "Image",
    y = "Average Uniformity Rating"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text labels
    axis.ticks.x = element_blank()   # Remove x-axis ticks
  )

#full ratings dist with images
ci_data <- ci_data %>% 
  mutate(
    position_zero = 0
  )
ci_data$image_position <- ci_data$position_zero - 200
ggplot(ci_data, aes(x = reorder(image_path, -mean_rating), y = mean_rating)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "darkblue") +
  geom_image(aes(image = image_path, y = image_position), size = 0.1, by = "width") +
  labs(
    title = "Average Uniformity Ratings by Image",
    x = "Image",
    y = "Average Uniformity Rating"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text labels
    axis.ticks.x = element_blank()   # Remove x-axis ticks
  )

#full ratings dist with images - error bars
error_data <- error_data %>% 
  mutate(
    position_zero = 0
  )
error_data$image_position <- error_data$position_zero - 200
ggplot(error_data, aes(x = reorder(image_path, -mean_rating), y = mean_rating)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = error_lower, ymax = error_upper), width = 0.2, color = "darkblue") +
  geom_image(aes(image = image_path, y = image_position), size = 0.1, by = "width") +
  labs(
    title = "Average Uniformity Ratings by Image",
    x = "Image",
    y = "Average Uniformity Rating"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text labels
    axis.ticks.x = element_blank()   # Remove x-axis ticks
  )


# Plotting average differences
ggplot(average_differences, aes(x = reorder(image_path, -avg_rating_diff), y = avg_rating_diff)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average Rating Differences by Image",
    x = "Image Path",
    y = "Average Rating Difference"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Image visualizations -----
# Create a function to display images with correct aspect ratio
display_images <- function(image_paths) {
  # Create a grid layout with one column and as many rows as images
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(length(image_paths), 1)))
  
  # Initialize a new plot
  plot.new()
  
  for (i in seq_along(image_paths)) {
    img <- readPNG(image_paths[i])  # Change to readJPEG if images are JPEG
    
    # Get dimensions of the image
    img_height <- dim(img)[1]  # Number of rows (height)
    img_width <- dim(img)[2]    # Number of columns (width)
    
    # Calculate the aspect ratio
    aspect_ratio <- img_height / img_width
    
    # Determine the height for display
    plot_height <- 1 / length(image_paths)  # Set each image to take up equal vertical space
    plot_width <- plot_height / aspect_ratio  # Width based on aspect ratio
    
    # Set coordinates for rasterImage
    xleft <- (1 - plot_width) / 2  # Center the image horizontally
    xright <- xleft + plot_width
    ybottom <- 1 - (i / length(image_paths)) + plot_height  # Set vertical position
    ytop <- ybottom + plot_height  # Equal height for each image
    
    # Display the image
    rasterImage(img, xleft = xleft, ybottom = ybottom, xright = xright, ytop = ytop)
  }
}

# Call the function with the ordered image paths
display_images(subset_images_data$image_path)


#cronbach's alpha