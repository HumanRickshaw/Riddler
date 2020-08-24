library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)

classicum = data.frame("Division_prob" = seq(0, 1, .001))
classicum <- classicum %>%
  mutate(Extinction = ifelse(Division_prob < 0.5, 1, (1 / Division_prob) - 1),
         Everlasting = ifelse(Division_prob < 0.5, 0, 2 - (1 / Division_prob)))

classicum <- classicum %>%
  gather(Result, Probability, Extinction:Everlasting)

g <- ggplot(data = classicum, aes(x = Division_prob, y = Probability))
g <- g + geom_line(aes(color = Result), size = 3)
g <- g + labs(title = "Colony End Behavior of Riddlerium classicum",
              x = "Probability of Division (R. classicum)",
              y = "Probability of End Behavior (R. classicum)")
g <- g + scale_color_manual(values = c(viridis(7)[6], viridis(7)[3]))

g <- g + geom_point(x = 0.8, y = 0.75, color = viridis(7)[1], size = 6)
g <- g + annotate(geom = "text", label = "(0.8, 0.75)",
                  x = 0.8, y = 0.7, color = viridis(7)[1], size = 5)
g <- g + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
               axis.text.x = element_text(size = 12),
               axis.title.x = element_text(size = 14, face = "bold"),
               axis.text.y = element_text(hjust = 1, size = 12, angle = 35),
               axis.title.y = element_text(size = 14, face = "bold"),
               legend.text = element_text(size = 12),
               legend.key = element_blank(),
               legend.title = element_text(hjust = 0.5, size = 14, face = "bold"))
g