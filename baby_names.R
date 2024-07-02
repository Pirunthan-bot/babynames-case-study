library(tidyverse)
library(ggplot2)
library(babynames)

babynames_summarized <- babynames %>% 
    #selecting only required columns avoiding sex
    select(year,name,n) %>%
    #grouping by year and name
    group_by(year,name) %>%
    #summarising n as number to get total number
    summarize(number = sum(n))


#filtering for babynames in 1990
filtered_babynames <- babynames %>%
    #Filter for the year 1990
    filter(year == 1990) %>%
    # Sort the number column in descending order
    arrange(desc(n))    

filtered_babynames

#Most common babyname for each sex for each year
common_babynames <- babynames %>%
    group_by(year,sex) %>%
        slice_max(n, n=1) 

common_babynames

#Selecting only for Steven, Thomas, and Matthew and visualising as line chart
selected_names <- babynames_summarized %>% 
    #filtering on specific names
    filter(name %in% c("Steven", "Thomas", "Matthew")) 

    #plotting the results on a line chart 
    ggplot(selected_names, aes(x = year, y= number, color = name )) + geom_line()

# Calculate the fraction of people born each year with the same name
fraction_names <- babynames_summarized %>% 
        # Calculate the fraction of people born each year with the same name
        group_by(year) %>%
        mutate(year_total =  sum(number)) %>%
        ungroup() %>%
        mutate(fraction = number/year_total) %>%
            # Find the year each name is most common
            group_by(name) %>%
            slice_max(fraction, n=1)
        
fraction_names

#Adding the total and maximum fro each name
max_tot_babynames <- babynames_summarized %>% 
    group_by(name) %>%
    mutate(name_total = sum(number), name_max = max(number)) %>%
  # Ungroup the table 
    ungroup() %>%
  # Add the fraction_max column containing the number by the name maximum 
    mutate(fraction_max = number/name_max)

names_filtered <- max_tot_babynames %>%
  # Filter for the names Steven, Thomas, and Matthew
  filter(name %in% c("Steven", "Thomas", "Matthew")) 

# Visualize the names in names_filtered over time
ggplot(names_filtered, aes(x = year, y= fraction_max, color = name )) + geom_line()



babynames_fraction <- babynames %>%  group_by(year)%>%  mutate(year_total =sum(n))%>%  ungroup()%>%  mutate(fraction = n / year_total)

#Using ratios to describe the frequency of a name
lag_ratio_babynames<- babynames_fraction %>%
  # Arrange the data in order of name, then year 
  arrange(name, year) %>%
  # Group the data by name
  group_by(name) %>%
  # Add a ratio column that contains the ratio of fraction between each year 
  mutate(ratio = fraction / lag(fraction))


#Biggest Jump in a name
babynames_ratios_filtered <- babynames_fraction %>%
                     arrange(name, year) %>%
                     group_by(name) %>%
                     mutate(ratio = fraction / lag(fraction)) %>%
                     filter(fraction >= 0.00001)

biggest_jump <- babynames_ratios_filtered %>%
  # Extract the largest ratio from each name 
  slice_max(ratio, n=1) %>%
  # Sort the ratio column in descending order 
  arrange(desc(ratio)) %>%
  # Filter for fractions greater than or equal to 0.001
  filter(fraction >= 0.001)
