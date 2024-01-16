library(tidyverse)
#Load the yrbss_samp dataset from openintro package
yrbss_samp<- openintro::yrbss_samp

#Make a histogram for the height variable
ggplot(data=yrbss_samp)+
  geom_histogram(
    aes(x=height),
    fill="yellow")

#Make a density plot to see the distribution of the weight variable
ggplot(data=yrbss_samp)+
  geom_density(
    aes(x=weight))

#What can you infer from the density plot of age?
ggplot(data=yrbss_samp)+
  geom_density(
    aes(x=age))

#Using the yrbss_samp data, plot a bar chart to see the distribution of males and females
ggplot(data=yrbss_samp)+
  geom_bar(
    aes(x=gender))

