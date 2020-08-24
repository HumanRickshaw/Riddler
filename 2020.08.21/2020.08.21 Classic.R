sides <- NULL
lengths <- NULL
min_weights <- NULL
post_weights <- NULL
areas <- NULL


#Calculates the minimum weight of a post for side n before incrementing n.
get_min_weight <- function(n) {
  t_1 <- tan(pi / n)
  t_2 <- tan(pi / (n+1))
  a <- n * (n + 1) * (n * t_2 - (n+1) * t_1)
  b <- -2 * n * (n + 1) * (t_2 - t_1)
  c <- ((n+1) * t_2 - n * t_1)
  d <- b ** 2 - 4 * a * c
  (-1 * b + sqrt(d)) / (2 * a)
}

#Increases n by 1 if weight drops below minimum weight.
increment_n <- function(n, post_weight, min_weight) {
  if (post_weight <= min_weight) {
    n + 1
  } else {
    n
  }
}

n <- 3
for (post_weight in seq(0.3333, 0, -.0001)) {
  
  min_weight <- get_min_weight(n)
  n <- increment_n(n, post_weight, min_weight)
  
  #Takes the number of sides and post weight, returns polygon side length so
  #that total weight is 1 kilogram).
  length <- (1 - n * post_weight) / n
  
  #Takes the number of sides and returns the angle between the apothem and radius.
  angle <- pi / n
  
  #Takes the side length and angle and return the apothem length.
  apothem <- length / (2 * tan(angle))
  
  #Takes the side length and angle and returns the radius length. 
  radius <- length / (2 * sin(angle))

  #Takes the apothem, number of sides, and side length and returns the area.           
  area <- apothem * n * length / 2
  
  sides <- c(sides, n)
  lengths <- c(lengths, length)
  min_weights <- c(min_weights, min_weight)
  post_weights <- c(post_weights, post_weight)
  areas <- c(areas, area)
}

min_weights <- c(min_weights, get_min_weight(n))

polygons <- data.frame("Sides" = sides,
                       "Sides2" = ifelse(sides < 9, sides, "More than 8"),
                       "Lengths" = lengths,
                       "Weights" = post_weights,
                       "Areas" = areas)

answer <- data.frame("Sides" = unique(sides),
                     "Min_Weights" = unique(min_weights))


g <- ggplot(polygons, aes(x = Lengths, y = Areas, color = as.factor(Sides2)))
g <- g + geom_point(size = 3)
#Title
g <- g + ggtitle("Maximum Hamster Pen Area vs Length and Number of Sides.")
#X-axis
g <- g + scale_x_continuous("Length of Side")
#Y-axis.
g <- g + scale_y_continuous(name = "Area of Hamster Pen")
#Viridis Color Scheme.
g <- g + scale_color_viridis("Number of Sides", discrete = TRUE)
#Modify labels and text.
g <- g + theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
               axis.text.x = element_text(size = 12),
               axis.title.x = element_text(size = 14, face = "bold"),
               axis.text.y = element_text(size = 12),
               axis.title.y = element_text(size = 14, face = "bold"),
               legend.text = element_text(size = 12),
               legend.title = element_text(size = 14, face = "bold"))