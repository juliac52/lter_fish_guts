#taking a look at the gut contents 
library("readxl")
library("tidyverse")
library("dplyr")
library("reshape2")
library("stringr")
library("janitor")
library("ggplot2")
library("ggpattern")
library("writexl")
library("here")
#import data
gut_contents <- read_excel("Data/SBC368_FISH_Gut_Contents_20200416.xlsx")
food_items <- read_excel("Data/SBC368_FISH_Gut_Contents_20200416.xlsx", sheet = 2)
fish_data <- read_excel("Data/SBC_LTER_FishGut_Size_Weight_Measurements_2017-2019.xlsx")

#clean fish_data and merge fish species, columns 2:9, by ID
fish_data_sub <- fish_data %>%
  clean_names() %>%
  rename("fish_species" = "species", "season_collected" = "season") %>%
  select(2:9)

#clean data
gut_contents_cln <- gut_contents %>% 
  clean_names() %>%
  rename("volume_ml" = "volume_m_l") %>%
  separate(col = date_collected,
           into = c("year","month","day"),
           sep = "-",
           convert = TRUE)
#merge 
gut_contents_mrg <- merge(gut_contents_cln, fish_data_sub, by = c('sample_number','season_collected','site'))

#***
#need to sort out the "present" taxa in the species column, can't be numeric
#maybe make another table with presence/absence data, can't really compare to counts data
pres_abs_gut <- gut_contents_mrg %>%
  filter(count == "present")
count_gut <- gut_contents_mrg %>%
  filter(count != "present")

#find frequency (%) of prey by fish species, season, and site 
#add column with % of items in each gut
count_gut$count <- as.numeric(count_gut$count)
count_gut <- count_gut %>% 
  group_by(sample_number) %>%  
  mutate(percent_gut_items = count/sum(count))

#figure this out 
count_gut <- count_gut %>%
  group_by(fish_species, species) %>%
  mutate(proportion_gut_items = mean(percent_gut_items))
  

#try plotting food in the x-axis and fish in the y? 
barp_count_gut <- ggplot(count_gut, aes(x = species, y = percent_gut_items)) +
  geom_bar(stat = 'identity')
#change angle of x axis text 
barp_count_gut + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#
ggplot(data = count_gut, aes( x = factor(species), y = proportion_gut_items, fill = fish_species)) +    
  geom_bar(position = "dodge2", stat = "identity") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
  





#manipulate to look at number of taxa present per gut 
gut_wider_count <- count_gut %>%
  pivot_wider(names_from = species, #making species into columns, ideally only have one row per sample # 
              values_from = count)


#test of mean proportions 
oxca <- c(3,4,3)
seat <- c(5,5,0)
pacl <- c(2,6,2)
prey <- c("sp1","sp2","sp3")

df <- data.frame(oxca,seat,pacl,prey)




  
