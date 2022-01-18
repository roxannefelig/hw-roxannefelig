time <- c(6,9,8,5,7.5,6.4,6,2.5,5,4,7.5,8,8)
mean(time)
sd(time)
length(time)
range(time)
time<6
time < 6
time[4]
time[1]

#produces true false vector for where values are greater or equal to 7
time >=7
#shows which position are greater or equal to 7
which(time>=7)
#creating a subset produces the values that are greater than 7 in the order 
#they appear
time[time>=7]

times2<- c(time, NA)
times2[is.na(times2)] #produces subset of values that are NA
times2[!is.na(times2)] #produces subset of values that are NOT NA

#modify vector so all times less than 5 become NA
time[time<5]<-NA
time

mtcars
head(mtcars)
#What’s the first column name in the mtcars dataset?
colnames(mtcars)
#Which column number is named "wt"?
which(colnames(mtcars)=="wt")

mtcars$cyl
mtcars[["cyl"]]
#Extract the vector of mpg values. What’s the mean mpg of all cars in the dataset?
mean(mtcars$mpg)
mpg<-mtcars[["mpg"]]
mean(mpg)
