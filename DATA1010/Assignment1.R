#----------------------------------------------
# DATA 101
# Homework 1: YOUR NAME HERE
#----------------------------------------------

# Please save this script as "YOUR LAST NAME_HW1.R" and upload the script to Canvas. 
# You should also upload a word document containing your write up and graphs.
# Please type your code into the sections outlined below. 

#----------------------------------------------
# Question 1

#general setup
install.packages("tidyverse")
library(tidyverse)

#inspecting the data set and variables first, to get oriented

mpg
mpg$cyl
mpg$hwy
glimpse(mpg)
summary(mpg$cyl)
summary(mpg$hwy)

#exploring different kinds of plots to get a sense of
#which might be best for visualizing these variables

?geom_point
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = hwy))

?geom_smooth
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = cyl, y = hwy))

?geom_bar
ggplot(data = mpg) + 
  geom_bar(mapping = aes(x = cyl, y = hwy))
ggplot(data = mpg) + 
  geom_bar(mapping = aes(x = cyl))

?geom_col
ggplot(data = mpg) + 
  geom_col(mapping = aes(x = cyl, y = hwy))

?geom_boxplot
ggplot(data = mpg) + 
  geom_boxplot(mapping = aes(x = cyl, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = cyl, y = hwy))

#the geom I think is best

ggplot(data = mpg) +
  geom_boxplot(mapping= aes(x = cyl, y = hwy, group = cyl, color = as.factor(cyl)))


#----------------------------------------------
# Question 2

#original code in the problem
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() + 
  coord_fixed()

#code without coord_fixed()
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline()

#code without geom_abline()
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  coord_fixed()

?coord_fixed
?geom_abline



#----------------------------------------------
# Question 3

?mpg
glimpse(mpg)
glimpse(mpg$drv)
summary(mpg$drv)
view(mpg$drv)

ggplot(data = mpg, mapping = aes(x = drv, y = hwy)) +
  geom_point()

view(mpg$displ)

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv))
