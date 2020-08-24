library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)

ruler = data.frame("Splits" = seq(0, 10, 1))
ruler <- ruler %>%
  mutate(Middle = (2^(Splits+1) - 1) / ((Splits+1) * 2^Splits))

g <- ggplot(data = ruler, aes(x = Splits, y = Middle))
g <- g + geom_point(size = 5, color = viridis(7)[6])
g <- g + labs(title = "Fraction of Ruler Length By Number of Splits",
              x = "Number of Splits",
              y = "Fraction of Ruler Length")
g <- g + theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
               axis.text.x = element_text(size = 12),
               axis.title.x = element_text(size = 14, face = "bold"),
               axis.text.y = element_text(hjust = 1, size = 12, angle = 35),
               axis.title.y = element_text(size = 14, face = "bold"))
g
