
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## HW1 Data Exploration

In this assignment, I'll be exploring the Palmer Penguins data set!

```{r penguins}
HW1data<-palmerpenguins::penguins
View(HW1data)
```

## Viewing Data Structure

```{r penguins structure}
str(HW1data)
nrow(HW1data)
ncol(HW1data)
plot(HW1data$species) 
mean(HW1data$bill_length_mm, na.rm=TRUE)
```
In this data set, we have 344 rows of data with 8 columns representing penguin species, the island they are from, their bill length and depth, their flipper length, body mass, sex, and the year of I assume data collection.

Plotting the species variable shows that the Adelie species is the most common, followed by the Gentoo, and lastly the Chinstrap. The average length of all penguins' bills is 43.92 mm!


