library(tidyverse)
install.packages("tsibble")

(mauna <- tsibble::as_tsibble(co2) %>%
    rename(month = index, conc = value))

ggplot(mauna) +
  aes(x=month, y=conc) +
geom_line() + 
  tsibble::scale_x_yearmonth()
#scatterplot
ggplot(palmerpenguins::penguins) +
  aes(x=body_mass_g, y=bill_depth_mm)+
  geom_point()+
  geom_smooth()

#map color to species
ggplot(palmerpenguins::penguins) +
  aes(x=body_mass_g, y=bill_depth_mm)+
  geom_point(aes(color=species))+
  geom_smooth()

#map color to global aes
ggplot(palmerpenguins::penguins) +
  aes(x=body_mass_g, y=bill_depth_mm, color=species)+
  geom_point()+
  geom_smooth()

#assign to p
p<-ggplot(mauna) +
  aes(x=month, y=conc) +
  geom_line() + 
  tsibble::scale_x_yearmonth()

p +
  geom_smooth(color='darkgreen')


