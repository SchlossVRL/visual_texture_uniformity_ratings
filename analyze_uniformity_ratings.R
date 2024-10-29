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
  summarize(avg_rating_diff = mean(rating_diff, na.rm = TRUE), .groups = "drop") %>% 
 

print(average_differences)

# Participant correlations -----

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

# Select subset
# Create a new data frame with ci_lower above 0
subset_images_data <- ci_data %>%
  filter(ci_lower > 0)

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
display_images(ordered_data$image_path)


#cronbach's alpha