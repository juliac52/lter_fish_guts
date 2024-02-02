library("readxl")
library("tidyverse")
library("stringr")
library("janitor")
library("ggplot2")
library("ggpattern")
library("writexl")
library("here")
library("ggpubr")

#data exploration 
fish_data <- read_excel(here("Data/SBC_LTER_FishGut_Size_Weight_Measurements_2017-2019.xlsx"))
gut_contents <- read_excel(here("Data/SBC368_FISH_Gut_Contents_20200416.xlsx"))
sia_log <- read_excel(here("Data/FishGutROA_IsotopeProcessingLog.xlsx"))
#find differences between fish IDs and guts analyzed 
no_gut_IDs <- fish_data[!fish_data$"SAMPLE #" %in% gut_contents$"SAMPLE #",]
#list of samples without gut analysis 
no_gut_IDs$`SAMPLE #`
#list of samples without SIA tissue taken 
no_si_coll <- fish_data[!fish_data$'ISOTOPE SAMPLE?' %in% 'Y',]
no_si_coll <- no_si_coll[!no_si_coll$'ISOTOPE SAMPLE?' %in% 'y',]
no_si_coll$'SAMPLE #'

#compare SIA log to fish_data and gut_data
no_SIA_prep <- fish_data[!fish_data$'SAMPLE #' %in% sia_log$'Sample #' ,]
no_SIA_prep$`SAMPLE #`
no_SIA_prep_guts <- gut_IDs[!gut_IDs$'gut_IDs' %in% sia_log$'Sample #' ,]

#split date up into year month day and clean up column headings
fish_data_cln <-
  fish_data %>%               
  separate(col = DATE,
    into = c("year","month","day"),
    sep = "-",
    convert = TRUE) %>% clean_names()
#
gut_contents_cln <-
  gut_contents %>%               
  separate(col = DATE_COLLECTED,
           into = c("year","month","day"),
           sep = "-",
           convert = TRUE) %>% clean_names()
#make column headings consistent & ready for merge 
fish_data_cln <- fish_data_cln %>%
  rename("fish_notes" = "notes") %>%
  rename("sia_collected" = "isotope_sample") %>%
  rename("fish_species" = "species")
#
gut_contents_cln <- gut_contents_cln %>% 
  rename("season" = "season_collected") %>%
  rename("gut_cont_id" = "species") %>%
  rename("count_in_gut" = "count") %>%
  rename("gut_vol_ml" = "volume_m_l") %>%
  rename("gut_notes" = "notes")
#
sia_log_cln <- sia_log %>% clean_names() %>%
  rename("fish_species" = "species") %>%
  rename("sia_notes_pl_stored_in_plastic_6mo" = "notes_pl_stored_in_plastic_for_6_mo") %>%
  drop_na(sample_number)#remove rows without data data

#add columns containing 1s to fish and sia dataframes for inventory plotting purposes 
fish_data_cln$fish_count <- 1 
sia_log_cln$sia_sample_count <- 1

#merge all three dataframes by year, season, site, and sample_number  
fish_merge <- merge(gut_contents_cln,fish_data_cln,
                    by = c("sample_number","site","season","day","month","year"),
                    all = TRUE)
fish_merge <- merge(fish_merge,sia_log_cln,
                    by = c("sample_number","fish_species","site","season","year"),
                    all = TRUE) 
#convert sample_number to numeric, but some rows were converted to NA due to characters included in string
fish_merge$sample_number <- as.numeric(fish_merge$sample_number)
#
#might just need to merge fish_data and sia_log, don't need repeats introduced by gut_log
fish_sia_merge <- merge(fish_data_cln,sia_log_cln,
                    by = c("sample_number","fish_species","site","season","year"),
                    all = TRUE) %>%
  drop_na(month)#remove rows without date data
fish_sia_merge$sample_number <- as.numeric(fish_sia_merge$sample_number)

#replace nas in "dried_by" and "ground_by"
fish_sia_merge$dried_by <- fish_sia_merge$dried_by %>% replace_na('none')
fish_sia_merge$ground_by <- fish_sia_merge$ground_by %>% replace_na('none')

#include column with sia sample id and drop NA rows 
fish_sia_merge$sia_fish <- paste(fish_sia_merge$fish_species, fish_sia_merge$sia_sample_count, sep = c('-'))
fish_sia_merge$sia_fish <- gsub('1','prepped',fish_sia_merge$sia_fish)
fish_sia_merge$sia_fish <- gsub('NA','not_prepped',fish_sia_merge$sia_fish)


#only include rows that haven't been dried and ground
fish_no_sia <- fish_sia_merge %>% 
  filter(dried_by=='none') %>%
  filter(ground_by == 'none') 
#reality check: how many samples have been dried and ground (551 have not):
948-551 #adds up 

#play around with plotting data
info_plot <- ggplot(fish_sia_merge %>% count(site, sia_fish) %>%    # Group by region and species, then count number in each group
         mutate(pct=n/sum(n),               # Calculate percent within each region
                ypos = cumsum(n) - 0.5*n),  # Calculate label positions
       aes(site, n, fill=sia_fish)) +
  geom_bar(stat="identity", position = "dodge") 

info_plot + 
  scale_fill_manual(values=c("yellow1","yellow3","olivedrab1","olivedrab4","lightpink1","lightpink3"),
                    name = "Samples dried and ground?", labels = c("O. californica - needs prep","O. californica - done",
                                                                   "P. calthratus - needs prep","P. calthratus - done",
                                                                   "S. atrovirens - needs prep","S. atrovirens - done")) +
  ylab("number") + ggtitle("SIA sample prep status")

####individual species plots 
info_plot_oxca <- ggplot(fish_sia_merge %>% 
                           filter(fish_species == "OXCA") %>%
                           count(year, sia_fish) %>%    # Group by region and species, then count number in each group
                           mutate(pct=n/sum(n),               # Calculate percent within each region
                                  ypos = cumsum(n) - 0.5*n),  # Calculate label positions
                         aes(year, n, fill=sia_fish)) +
  geom_bar(stat="identity", position = "dodge") 

info_plot_oxca + 
  scale_fill_manual(values=c("yellow1","yellow3"),
                    name = "Samples dried and ground?", labels = c("Needs prep","Done")) +
  ylab("number") + ggtitle("O. californica SIA sample prep status")
#

info_plot_pacl <- ggplot(fish_sia_merge %>% 
                           filter(fish_species == "PACL") %>%
                           count(year, sia_fish) %>%    # Group by region and species, then count number in each group
                           mutate(pct=n/sum(n),               # Calculate percent within each region
                                  ypos = cumsum(n) - 0.5*n),  # Calculate label positions
                         aes(year, n, fill=sia_fish)) +
  geom_bar(stat="identity", position = "dodge") 

info_plot_pacl + 
  scale_fill_manual(values=c("olivedrab1","olivedrab4"),
                    name = "Samples dried and ground?", labels = c("Needs prep","Done")) +
  ylab("number") + ggtitle("P. calthratus SIA sample prep status")
#

info_plot_seat <- ggplot(fish_sia_merge %>% 
                           filter(fish_species == "SEAT") %>%
                           count(year, sia_fish) %>%    # Group by region and species, then count number in each group
                           mutate(pct=n/sum(n),               # Calculate percent within each region
                                  ypos = cumsum(n) - 0.5*n),  # Calculate label positions
                         aes(year, n, fill=sia_fish)) +
  geom_bar(stat="identity", position = "dodge") 

info_plot_oxca + 
  scale_fill_manual(values=c("lightpink1","lightpink3"),
                    name = "Samples dried and ground?", labels = c("Needs prep","Done")) +
  ylab("number") + ggtitle("S. atrovirens SIA sample prep status")
#

#remove unnecessary columns from lists to be exported to xl 
fish_no_sia_xl <- fish_no_sia %>%
  select(!17:23) %>%
  rename(mus_collected = sia_collected)

fish_sia_prepped_xl <- fish_sia_merge %>%
  filter(sia_fish %in% c("OXCA-prepped","SEAT-prepped","PACL-prepped")) %>%
  select(!c(17,20,22:23)) %>%
  rename(mus_collected = sia_collected)

#write list of samples that need sia prep 
write.csv(fish_no_sia, "/Users/julia/Documents/SBC-LTER/Fish Gut Contents ROA/not_sia_prepped.csv", row.names=FALSE)
write_xlsx(fish_no_sia_xl, "/Users/julia/Documents/SBC-LTER/Fish Gut Contents ROA/fish_gut_contents_not_sia_prepped.xlsx")
write_xlsx(fish_sia_prepped_xl, "/Users/julia/Documents/SBC-LTER/Fish Gut Contents ROA/fish_gut_contents_sia_prepped.xlsx")

#breakdown of:
# total % of each species ready 
# total numbers of each species overall 
oxca <- fish_sia_prepped_xl%>%
  filter(fish_species == "OXCA") #129
seat <- fish_sia_prepped_xl%>%
  filter(fish_species == "SEAT") #131
pacl <- fish_sia_prepped_xl%>%
  filter(fish_species == "PACL") #137

oxca_notdone <- fish_no_sia_xl%>%
  filter(fish_species == "OXCA") #231
seat_notdone <- fish_no_sia_xl%>%
  filter(fish_species == "SEAT") #161
pacl_notdone <- fish_no_sia_xl%>%
  filter(fish_species == "PACL") #159

129+231 == 360 #oxca tot
(129/360)*100 == 35.83333
131+161 == 292 #seat tot
(131/292)*100 == 44.86301
137+159 == 296 #pacl tot
(137/296)*100 = 46.28378

# total numbers collected for each year
# total % ready for each year
f2017 <- fish_sia_prepped_xl%>%
  filter(year == 2017) #0
f2018 <- fish_sia_prepped_xl%>%
  filter(year == 2018) #231
f2019 <- fish_sia_prepped_xl%>%
  filter(year == 2019) #166

notdone2017 <- fish_no_sia_xl%>%
  filter(year == 2017) #181
notdone2018 <- fish_no_sia_xl%>%
  filter(year == 2018) #140
notdone2019 <- fish_no_sia_xl%>%
  filter(year == 2019) #230

#2018
(231/(231+140))* 100 == 62.26415 
#2019
(166/(166+230))*100 == 41.91919

# total numbers collected from each site
# total % ready from each site 
ivee <- fish_sia_prepped_xl%>%
  filter(site == "IVEE") #95
aque <- fish_sia_prepped_xl%>%
  filter(site == "AQUE") #102
mohk <- fish_sia_prepped_xl%>%
  filter(site == "MOHK") #100
napl <- fish_sia_prepped_xl%>%
  filter(site == "NAPL") #100

ivee_notdone <- fish_no_sia_xl%>%
  filter(site == "IVEE") #73
aque_notdone <- fish_no_sia_xl%>%
  filter(site == "AQUE") #93
mohk_notdone <- fish_no_sia_xl%>%
  filter(site == "MOHK") #195
napl_notdone <- fish_no_sia_xl%>%
  filter(site == "NAPL") #190

#ivee
(95/(95+73))*100 == 56.54762
#aque
(102/(102+93))*100 == 52.30769
#mohk
(100/(100+195))*100 == 33.89831
#napl
(100/(100+190))*100 == 34.48276

#could do more detailed breakdown or could just pick which samples to run given the information - 
#   how many mus samples to run for exploration? 

#add a column to fish_sia_merge$sia_prepped <- "yes" or "no" 
fish_sia_merge$sia_prepped <- paste(fish_sia_merge$sia_sample_count)
fish_sia_merge$sia_prepped <- gsub('1','yes',fish_sia_merge$sia_prepped)
fish_sia_merge$sia_prepped <- gsub('NA','no',fish_sia_merge$sia_prepped)

#make table of breakdowns of each category 
fish_inventory_sia <- fish_sia_merge %>%
  count(year, fish_species, season, site, sia_prepped)

fish_inventory <- fish_sia_merge %>%
  count(year, fish_species, season, site)

fish_by_year <- fish_sia_merge %>%
  count(year, fish_species, sia_prepped)

fish_by_site <- fish_sia_merge %>%
  count(site, fish_species, sia_prepped)

fish_by_seaon <- fish_sia_merge %>%
  count(season, fish_species, sia_prepped)

#write these to excel 
write_xlsx(fish_inventory_sia, "/Users/julia/Documents/SBC-LTER/Fish Gut Contents ROA/fish_inventory_sia_status.xlsx")
write_xlsx(fish_inventory, "/Users/julia/Documents/SBC-LTER/Fish Gut Contents ROA/fish_inventory.xlsx")
write_xlsx(fish_by_site, "/Users/julia/Documents/SBC-LTER/Fish Gut Contents ROA/fish_by_site.xlsx")
write_xlsx(fish_by_year, "/Users/julia/Documents/SBC-LTER/Fish Gut Contents ROA/fish_by_year.xlsx")
write_xlsx(fish_by_seaon, "/Users/julia/Documents/SBC-LTER/Fish Gut Contents ROA/fish_by_season.xlsx")

#
#histogram of size by season 
#all years 
level_order <- c('SPRING', 'SUMMER', 'FALL', 'WINTER') 
level_order_2 <- c('NAPL','IVEE','MOHK','AQUE')

#SEAT 
seat_season_2017_2019 <- ggplot(fish_sia_merge %>%
         filter(fish_species == 'SEAT'), 
       aes(x = standard_length_mm, fill = factor(season, level = level_order), 
           color = factor(season, level = level_order))) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5)+
  scale_color_manual(values= c("olivedrab3", "indianred", "#E69F00", "#56B4E9")) +
  scale_fill_manual(values= c("olivedrab3", "indianred", "#E69F00", "#56B4E9")) +
  ggtitle("Kelp rockfish size by Season") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw()

#OXCA
oxca_season_2017_2019 <- ggplot(fish_sia_merge %>%
         filter(fish_species == 'OXCA'), 
       aes(x = standard_length_mm, fill = factor(season, level = level_order), 
           color = factor(season, level = level_order))) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5)+
  scale_color_manual(values= c("olivedrab3", "indianred", "#E69F00", "#56B4E9")) +
  scale_fill_manual(values= c("olivedrab3", "indianred", "#E69F00", "#56B4E9")) +
  ggtitle("Señorita fish size by Season") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw()

#PACL
pacl_season_2017_2019 <- ggplot(fish_sia_merge %>%
         filter(fish_species == 'PACL'), 
       aes(x = standard_length_mm, fill = factor(season, level = level_order), 
           color = factor(season, level = level_order))) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5)+
  scale_color_manual(values= c("olivedrab3", "indianred", "#E69F00", "#56B4E9")) +
  scale_fill_manual(values= c("olivedrab3", "indianred", "#E69F00", "#56B4E9")) +
  ggtitle("Kelp bass by Season") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw()

#excluding 2017
#SEAT
seat_season_2018_2019 <- ggplot(fish_sia_merge %>%
         filter(fish_species == 'SEAT', year != 2017), 
       aes(x = standard_length_mm, fill = factor(season, level = level_order), 
           color = factor(season, level = level_order))) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5)+
  scale_color_manual(values= c("olivedrab3", "indianred", "#E69F00", "#56B4E9")) +
  scale_fill_manual(values= c("olivedrab3", "indianred", "#E69F00", "#56B4E9")) +
  ggtitle("Kelp rockfish size by Season 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw()

#OXCA
oxca_season_2018_2019 <- ggplot(fish_sia_merge %>%
         filter(fish_species == 'OXCA', year != 2017), 
       aes(x = standard_length_mm, fill = factor(season, level = level_order), 
           color = factor(season, level = level_order))) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5)+
  scale_color_manual(values= c("olivedrab3", "indianred", "#E69F00", "#56B4E9")) +
  scale_fill_manual(values= c("olivedrab3", "indianred", "#E69F00", "#56B4E9")) +
  ggtitle("Señorita fish size by Season 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw()

#PACL
pacl_season_2018_2019 <- ggplot(fish_sia_merge %>%
         filter(fish_species == 'PACL', year != 2017), 
       aes(x = standard_length_mm, fill = factor(season, level = level_order), 
           color = factor(season, level = level_order))) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5)+
  scale_color_manual(values= c("olivedrab3", "indianred", "#E69F00", "#56B4E9")) +
  scale_fill_manual(values= c("olivedrab3", "indianred", "#E69F00", "#56B4E9")) +
  ggtitle("Kelp bass by Season 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw()

#split up seasons into separate plots 
#SEAT spring
seat_spring <- ggplot(fish_sia_merge %>%
         filter(fish_species == 'SEAT', season == 'SPRING', year != 2017), 
       aes(x = standard_length_mm)) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5, col = "olivedrab3", fill = "olivedrab3") +
  ggtitle("Kelp rockfish size Spring 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw() +
  xlim(100,325) +
  ylim(0,11)

#SEAT summer
seat_summer <- ggplot(fish_sia_merge %>%
         filter(fish_species == 'SEAT', season == 'SUMMER', year != 2017, sia_prepped == 'yes'), 
       aes(x = standard_length_mm)) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5, col = "indianred", fill = "indianred") +
  ggtitle("Kelp rockfish size Summer 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw() +
  xlim(100,325) +
  ylim(0,11)

#SEAT fall
seat_fall <- ggplot(fish_sia_merge %>%
         filter(fish_species == 'SEAT', season == 'FALL', year != 2017), 
       aes(x = standard_length_mm)) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5, col = "#E69F00", fill = "#E69F00") +
  ggtitle("Kelp rockfish size Fall 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw()+
  xlim(100,325) +
  ylim(0,11)

#SEAT winter
seat_winter <- ggplot(fish_sia_merge %>%
         filter(fish_species == 'SEAT', season == 'WINTER', year != 2017), 
       aes(x = standard_length_mm)) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5, col = "#56B4E9", fill = "#56B4E9") +
  ggtitle("Kelp rockfish size Winter 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw()+
  xlim(100,325) +
  ylim(0,11)

#OXCA spring
oxca_spring <- ggplot(fish_sia_merge %>%
         filter(fish_species == 'OXCA', season == 'SPRING', year != 2017), 
       aes(x = standard_length_mm)) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5, col = "olivedrab3", fill = "olivedrab3") +
  ggtitle("Señorita fish size Spring 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw()+
  xlim(25,225) +
  ylim(0,32)

#OXCA summer
oxca_summer <- ggplot(fish_sia_merge %>%
         filter(fish_species == 'OXCA', season == 'SUMMER', year != 2017), 
       aes(x = standard_length_mm)) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5, col = "indianred", fill = "indianred") +
  ggtitle("Señorita fish size Summer 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw()+
  xlim(25,225) +
  ylim(0,32)

#OXCA fall
oxca_fall <- ggplot(fish_sia_merge %>%
         filter(fish_species == 'OXCA', season == 'FALL', year != 2017), 
       aes(x = standard_length_mm)) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5, col = "#E69F00", fill = "#E69F00") +
  ggtitle("Señorita fish size Fall 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw()+
  xlim(25,225) +
  ylim(0,32)

#OXCA winter
oxca_winter <- ggplot(fish_sia_merge %>%
         filter(fish_species == 'OXCA', season == 'WINTER', year != 2017), 
       aes(x = standard_length_mm)) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5, col = "#56B4E9", fill = "#56B4E9") +
  ggtitle("Señorita fish size Winter 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw()+
  xlim(25,225) +
  ylim(0,32)

#PACL spring
pacl_spring <- ggplot(fish_sia_merge %>%
                        filter(fish_species == 'PACL', season == 'SPRING', year != 2017), 
                      aes(x = standard_length_mm)) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5, col = "olivedrab3", fill = "olivedrab3") +
  ggtitle("Kelp bass size Spring 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw()+
  xlim(0,500) +
  ylim(0,9)

#PACL summer
pacl_summer <- ggplot(fish_sia_merge %>%
                        filter(fish_species == 'PACL', season == 'SUMMER', year != 2017), 
                      aes(x = standard_length_mm)) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5, col = "indianred", fill = "indianred") +
  ggtitle("Kelp bass size Summer 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw()+
  xlim(0,500) +
  ylim(0,9)

#PACL fall
pacl_fall <- ggplot(fish_sia_merge %>%
                      filter(fish_species == 'PACL', season == 'FALL', year != 2017), 
                    aes(x = standard_length_mm)) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5, col = "#E69F00", fill = "#E69F00") +
  ggtitle("Kelp bass size Fall 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw()+
  xlim(0,500) +
  ylim(0,9)

#PACL winter
pacl_winter <- ggplot(fish_sia_merge %>%
                        filter(fish_species == 'PACL', season == 'WINTER', year != 2017), 
                      aes(x = standard_length_mm)) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5, col = "#56B4E9", fill = "#56B4E9") +
  ggtitle("Kelp bass size Winter 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw()+
  xlim(75,500) +
  ylim(0,9)

#combine into 4 panel figures 
kelp_rockfish_size_season <- ggarrange(seat_spring, seat_summer, seat_fall, seat_winter,
                    ncol = 2, nrow = 2)

kelp_bass_size_season <- ggarrange(pacl_spring, pacl_summer, pacl_fall, pacl_winter,
                                       ncol = 2, nrow = 2)

senorita_size_season <- ggarrange(oxca_spring, oxca_summer, oxca_fall, oxca_winter,
                                       ncol = 2, nrow = 2)
#by site
#SEAT 
seat_site_2018_2019 <- ggplot(fish_sia_merge %>%
         filter(fish_species == 'SEAT', year != 2017), 
       aes(x = standard_length_mm, fill = factor(site, level = level_order_2), 
           color = factor(site, level = level_order_2))) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5)+
  scale_color_manual(values= c("blue", "lightblue", "yellow4", "yellow")) +
  scale_fill_manual(values= c("blue", "lightblue", "yellow4", "yellow")) +
  ggtitle("Kelp rockfish size by Site 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Site") +
  labs(fill = "Site") +
  theme_bw()

#OXCA
oxca_site_2018_2019 <- ggplot(fish_sia_merge %>%
         filter(fish_species == 'OXCA', year != 2017), 
       aes(x = standard_length_mm, fill = factor(site, level = level_order_2), 
           color = factor(site, level = level_order_2))) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5)+
  scale_color_manual(values= c("blue", "lightblue", "yellow4", "yellow")) +
  scale_fill_manual(values= c("blue", "lightblue", "yellow4", "yellow")) +
  ggtitle("Señorita fish size by Site 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Site") +
  labs(fill = "Site") +
  theme_bw()

#PACL
pacl_site_2018_2019 <- ggplot(fish_sia_merge %>%
         filter(fish_species == 'PACL', year != 2017, site =="NAPL"), 
       aes(x = standard_length_mm, fill = factor(site, level = level_order_2), 
           color = factor(site, level = level_order_2))) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5)+
  scale_color_manual(values= c("blue", "lightblue", "yellow4", "yellow")) +
  scale_fill_manual(values= c("blue", "lightblue", "yellow4", "yellow")) +
  ggtitle("Kelp bass by Site 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Site") +
  labs(fill = "Site") +
  theme_bw()
#individual site plots 
#SEAT 
seat_site_2018_2019 <- ggplot(fish_sia_merge %>%
                                filter(fish_species == 'SEAT', year != 2017), 
                              aes(x = standard_length_mm, fill = factor(site, level = level_order_2), 
                                  color = factor(site, level = level_order_2))) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5)+
  scale_color_manual(values= c("blue", "lightblue", "yellow4", "yellow")) +
  scale_fill_manual(values= c("blue", "lightblue", "yellow4", "yellow")) +
  ggtitle("Kelp rockfish size by Site 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Site") +
  labs(fill = "Site") +
  theme_bw()

#OXCA
oxca_site_2018_2019 <- ggplot(fish_sia_merge %>%
                                filter(fish_species == 'OXCA', year != 2017), 
                              aes(x = standard_length_mm, fill = factor(site, level = level_order_2), 
                                  color = factor(site, level = level_order_2))) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5)+
  scale_color_manual(values= c("blue", "lightblue", "yellow4", "yellow")) +
  scale_fill_manual(values= c("blue", "lightblue", "yellow4", "yellow")) +
  ggtitle("Señorita fish size by Site 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Site") +
  labs(fill = "Site") +
  theme_bw()

#PACL
pacl_site_2018_2019 <- ggplot(fish_sia_merge %>%
                                filter(fish_species == 'PACL', year != 2017), 
                              aes(x = standard_length_mm, fill = factor(site, level = level_order_2), 
                                  color = factor(site, level = level_order_2))) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5)+
  scale_color_manual(values= c("blue", "lightblue", "yellow4", "yellow")) +
  scale_fill_manual(values= c("blue", "lightblue", "yellow4", "yellow")) +
  ggtitle("Kelp bass by Site 2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Site") +
  labs(fill = "Site") +
  theme_bw()

#save some of the plots using ggsave


ggsave(here("outputs/plots","SEAT_size_by_season_2018-2019.jpg"), seat_season_2018_2019, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","OXCA_size_by_season_2018-2019.jpg"), oxca_season_2018_2019, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","PACL_size_by_season_2018-2019.jpg"), pacl_season_2018_2019, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","SEAT_size_by_site_2018-2019.jpg"), seat_site_2018_2019, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","OXCA_size_by_site_2018-2019.jpg"), oxca_site_2018_2019, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","PACL_size_by_site_2018-2019.jpg"), pacl_site_2018_2019, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","SEAT_size_spring_2018-2019.jpg"), seat_spring, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","SEAT_size_summer_2018-2019.jpg"), seat_summer, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","SEAT_size_fall_2018-2019.jpg"), seat_fall, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","SEAT_size_winter_2018-2019.jpg"), seat_winter, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","OXCA_size_spring_2018-2019.jpg"), oxca_spring, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","OXCA_size_summer_2018-2019.jpg"), oxca_summer, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","OXCA_size_fall_2018-2019.jpg"), oxca_fall, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","OXCA_size_winter_2018-2019.jpg"), oxca_winter, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","PACL_size_spring_2018-2019.jpg"), pacl_spring, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","PACL_size_summer_2018-2019.jpg"), pacl_summer, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","PACL_size_fall_2018-2019.jpg"), pacl_fall, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","PACL_size_winter_2018-2019.jpg"), pacl_winter, 
       width = 6, height = 3, units = "in", dpi = 600)

#4 panel plots 
ggsave(here("outputs/plots","kelp_rockfish_size_season_4_panel.jpg"), kelp_rockfish_size_season, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","kelp_bass_size_season_4_panel.jpg"), kelp_bass_size_season, 
       width = 6, height = 3, units = "in", dpi = 600)

ggsave(here("outputs/plots","senorita_size_season_4_panel.jpg"), senorita_size_season, 
       width = 6, height = 3, units = "in", dpi = 600)



#
fish_info_plot <- ggplot(fish_data_cln %>% 
                           filter(fish_species == "SEAT") %>%
                           count(year, site) %>%    # Group by region and species, then count number in each group
                         aes(year, n, fill=sia_fish)) +
  geom_bar(stat="identity", position = "dodge") 







#################################list comprehension 
#subset data based on species, site, season, and year - try a different approach, this is dumb 
#ugh this is a mess there has GOT to be a better way to do this, look into making functions tomorrow? Myabe loops? idk 
#use unique() and n_distinct()
#PACL fish_data
#PACL SPRING
fish_pacl_spring_mohk_2017 <- fish_data %>%
  filter(year==2017, SEASON=='SPRING', SITE=='MOHK', SPECIES=='PACL')

fish_pacl_spring_mohk_2018 <- fish_data %>%
  filter(year==2018, SEASON=='SPRING', SITE=='MOHK', SPECIES=='PACL')

fish_pacl_spring_mohk_2019 <- fish_data %>%
  filter(year==2019, SEASON=='SPRING', SITE=='MOHK', SPECIES=='PACL')

fish_pacl_spring_napl_2017 <- fish_data %>%
  filter(year==2017, SEASON=='SPRING', SITE=='NAPL', SPECIES=='PACL')

fish_pacl_spring_napl_2018 <- fish_data %>%
  filter(year==2018, SEASON=='SPRING', SITE=='NAPL', SPECIES=='PACL')

fish_pacl_spring_napl_2019 <- fish_data %>%
  filter(year==2019, SEASON=='SPRING', SITE=='NAPL', SPECIES=='PACL')

fish_pacl_spring_ivee_2017 <- fish_data %>%
  filter(year==2017, SEASON=='SPRING', SITE=='IVEE', SPECIES=='PACL')

fish_pacl_spring_ivee_2018 <- fish_data %>%
  filter(year==2018, SEASON=='SPRING', SITE=='IVEE', SPECIES=='PACL')

fish_pacl_spring_ivee_2019 <- fish_data %>%
  filter(year==2019, SEASON=='SPRING', SITE=='IVEE', SPECIES=='PACL')

fish_pacl_spring_aque_2017 <- fish_data %>%
  filter(year==2017, SEASON=='SPRING', SITE=='AQUE', SPECIES=='PACL')

fish_pacl_spring_aque_2018 <- fish_data %>%
  filter(year==2018, SEASON=='SPRING', SITE=='AQUE', SPECIES=='PACL')

fish_pacl_spring_aque_2019 <- fish_data %>%
  filter(year==2019, SEASON=='SPRING', SITE=='AQUE', SPECIES=='PACL')

#PACL SUMMER
fish_pacl_WINTER_mohk_2017 <- fish_data %>%
  filter(year==2017, SEASON=='WINTER', SITE=='MOHK', SPECIES=='PACL')

fish_pacl_WINTER_mohk_2018 <- fish_data %>%
  filter(year==2018, SEASON=='WINTER', SITE=='MOHK', SPECIES=='PACL')

fish_pacl_WINTER_mohk_2019 <- fish_data %>%
  filter(year==2019, SEASON=='WINTER', SITE=='MOHK', SPECIES=='PACL')

fish_pacl_WINTER_napl_2017 <- fish_data %>%
  filter(year==2017, SEASON=='WINTER', SITE=='NAPL', SPECIES=='PACL')

fish_pacl_WINTER_napl_2018 <- fish_data %>%
  filter(year==2018, SEASON=='WINTER', SITE=='NAPL', SPECIES=='PACL')

fish_pacl_WINTER_napl_2019 <- fish_data %>%
  filter(year==2019, SEASON=='WINTER', SITE=='NAPL', SPECIES=='PACL')

fish_pacl_WINTER_ivee_2017 <- fish_data %>%
  filter(year==2017, SEASON=='WINTER', SITE=='IVEE', SPECIES=='PACL')

fish_pacl_WINTER_ivee_2018 <- fish_data %>%
  filter(year==2018, SEASON=='WINTER', SITE=='IVEE', SPECIES=='PACL')

fish_pacl_WINTER_ivee_2019 <- fish_data %>%
  filter(year==2019, SEASON=='WINTER', SITE=='IVEE', SPECIES=='PACL')

fish_pacl_WINTER_aque_2017 <- fish_data %>%
  filter(year==2017, SEASON=='WINTER', SITE=='AQUE', SPECIES=='PACL')

fish_pacl_WINTER_aque_2018 <- fish_data %>%
  filter(year==2018, SEASON=='WINTER', SITE=='AQUE', SPECIES=='PACL')

fish_pacl_WINTER_aque_2019 <- fish_data %>%
  filter(year==2019, SEASON=='WINTER', SITE=='AQUE', SPECIES=='PACL')

#PACL FALL
fish_pacl_FALL_mohk_2017 <- fish_data %>%
  filter(year==2017, SEASON=='FALL', SITE=='MOHK', SPECIES=='PACL')

fish_pacl_FALL_mohk_2018 <- fish_data %>%
  filter(year==2018, SEASON=='FALL', SITE=='MOHK', SPECIES=='PACL')

fish_pacl_FALL_mohk_2019 <- fish_data %>%
  filter(year==2019, SEASON=='FALL', SITE=='MOHK', SPECIES=='PACL')

fish_pacl_FALL_napl_2017 <- fish_data %>%
  filter(year==2017, SEASON=='FALL', SITE=='NAPL', SPECIES=='PACL')

fish_pacl_FALL_napl_2018 <- fish_data %>%
  filter(year==2018, SEASON=='FALL', SITE=='NAPL', SPECIES=='PACL')

fish_pacl_FALL_napl_2019 <- fish_data %>%
  filter(year==2019, SEASON=='FALL', SITE=='NAPL', SPECIES=='PACL')

fish_pacl_FALL_ivee_2017 <- fish_data %>%
  filter(year==2017, SEASON=='FALL', SITE=='IVEE', SPECIES=='PACL')

fish_pacl_FALL_ivee_2018 <- fish_data %>%
  filter(year==2018, SEASON=='FALL', SITE=='IVEE', SPECIES=='PACL')

fish_pacl_FALL_ivee_2019 <- fish_data %>%
  filter(year==2019, SEASON=='FALL', SITE=='IVEE', SPECIES=='PACL')

fish_pacl_FALL_aque_2017 <- fish_data %>%
  filter(year==2017, SEASON=='FALL', SITE=='AQUE', SPECIES=='PACL')

fish_pacl_FALL_aque_2018 <- fish_data %>%
  filter(year==2018, SEASON=='FALL', SITE=='AQUE', SPECIES=='PACL')

fish_pacl_FALL_aque_2019 <- fish_data %>%
  filter(year==2019, SEASON=='FALL', SITE=='AQUE', SPECIES=='PACL')


#PACL WINTER
fish_pacl_WINTER_mohk_2017 <- fish_data %>%
  filter(year==2017, SEASON=='WINTER', SITE=='MOHK', SPECIES=='PACL')

fish_pacl_WINTER_mohk_2018 <- fish_data %>%
  filter(year==2018, SEASON=='WINTER', SITE=='MOHK', SPECIES=='PACL')

fish_pacl_WINTER_mohk_2019 <- fish_data %>%
  filter(year==2019, SEASON=='WINTER', SITE=='MOHK', SPECIES=='PACL')

fish_pacl_WINTER_napl_2017 <- fish_data %>%
  filter(year==2017, SEASON=='WINTER', SITE=='NAPL', SPECIES=='PACL')

fish_pacl_WINTER_napl_2018 <- fish_data %>%
  filter(year==2018, SEASON=='WINTER', SITE=='NAPL', SPECIES=='PACL')

fish_pacl_WINTER_napl_2019 <- fish_data %>%
  filter(year==2019, SEASON=='WINTER', SITE=='NAPL', SPECIES=='PACL')

fish_pacl_WINTER_ivee_2017 <- fish_data %>%
  filter(year==2017, SEASON=='WINTER', SITE=='IVEE', SPECIES=='PACL')

fish_pacl_WINTER_ivee_2018 <- fish_data %>%
  filter(year==2018, SEASON=='WINTER', SITE=='IVEE', SPECIES=='PACL')

fish_pacl_WINTER_ivee_2019 <- fish_data %>%
  filter(year==2019, SEASON=='WINTER', SITE=='IVEE', SPECIES=='PACL')

fish_pacl_WINTER_aque_2017 <- fish_data %>%
  filter(year==2017, SEASON=='WINTER', SITE=='AQUE', SPECIES=='PACL')

fish_pacl_WINTER_aque_2018 <- fish_data %>%
  filter(year==2018, SEASON=='WINTER', SITE=='AQUE', SPECIES=='PACL')

fish_pacl_WINTER_aque_2019 <- fish_data %>%
  filter(year==2019, SEASON=='WINTER', SITE=='AQUE', SPECIES=='PACL')
###############################


#PACL gut_contents 
#PACL gut SPRING 
gut_pacl_spring_mohk_2017 <- gut_contents_merge %>%
  filter(year==2017, SEASON=='SPRING', SITE=='MOHK', SPECIES=='PACL')

gut_pacl_spring_mohk_2018 <- gut_contents_merge %>%
  filter(year==2018, SEASON=='SPRING', SITE=='MOHK', SPECIES=='PACL')

gut_pacl_spring_mohk_2019 <- gut_contents_merge %>%
  filter(year==2019, SEASON=='SPRING', SITE=='MOHK', SPECIES=='PACL')

gut_pacl_spring_napl_2017 <- gut_contents_merge %>%
  filter(year==2017, SEASON=='SPRING', SITE=='NAPL', SPECIES=='PACL')

gut_pacl_spring_napl_2018 <- gut_contents_merge %>%
  filter(year==2018, SEASON=='SPRING', SITE=='NAPL', SPECIES=='PACL')

gut_pacl_spring_napl_2019 <- gut_contents_merge %>%
  filter(year==2019, SEASON=='SPRING', SITE=='NAPL', SPECIES=='PACL')

gut_pacl_spring_ivee_2017 <- gut_contents_merge %>%
  filter(year==2017, SEASON=='SPRING', SITE=='IVEE', SPECIES=='PACL')

gut_pacl_spring_ivee_2018 <- gut_contents_merge %>%
  filter(year==2018, SEASON=='SPRING', SITE=='IVEE', SPECIES=='PACL')

gut_pacl_spring_ivee_2019 <- gut_contents_merge %>%
  filter(year==2019, SEASON=='SPRING', SITE=='IVEE', SPECIES=='PACL')

gut_pacl_spring_aque_2017 <- gut_contents_merge %>%
  filter(year==2017, SEASON=='SPRING', SITE=='AQUE', SPECIES=='PACL')

gut_pacl_spring_aque_2018 <- gut_contents_merge %>%
  filter(year==2018, SEASON=='SPRING', SITE=='AQUE', SPECIES=='PACL')

gut_pacl_spring_aque_2019 <- gut_contents_merge %>%
  filter(year==2019, SEASON=='SPRING', SITE=='AQUE', SPECIES=='PACL')

#PACL SUMMER
gut_pacl_SUMMER_mohk_2017 <- gut_contents_merge %>%
  filter(year==2017, SEASON=='SUMMER', SITE=='MOHK', SPECIES=='PACL')

gut_pacl_SUMMER_mohk_2018 <- gut_contents_merge %>%
  filter(year==2018, SEASON=='SUMMER', SITE=='MOHK', SPECIES=='PACL')

gut_pacl_SUMMER_mohk_2019 <- gut_contents_merge %>%
  filter(year==2019, SEASON=='SUMMER', SITE=='MOHK', SPECIES=='PACL')

gut_pacl_SUMMER_napl_2017 <- gut_contents_merge %>%
  filter(year==2017, SEASON=='SUMMER', SITE=='NAPL', SPECIES=='PACL')

gut_pacl_SUMMER_napl_2018 <- gut_contents_merge %>%
  filter(year==2018, SEASON=='SUMMER', SITE=='NAPL', SPECIES=='PACL')

gut_pacl_SUMMER_napl_2019 <- gut_contents_merge %>%
  filter(year==2019, SEASON=='SUMMER', SITE=='NAPL', SPECIES=='PACL')

gut_pacl_SUMMER_ivee_2017 <- gut_contents_merge %>%
  filter(year==2017, SEASON=='SUMMER', SITE=='IVEE', SPECIES=='PACL')

gut_pacl_SUMMER_ivee_2018 <- gut_contents_merge %>%
  filter(year==2018, SEASON=='SUMMER', SITE=='IVEE', SPECIES=='PACL')

gut_pacl_SUMMER_ivee_2019 <- gut_contents_merge %>%
  filter(year==2019, SEASON=='SUMMER', SITE=='IVEE', SPECIES=='PACL')

gut_pacl_SUMMER_aque_2017 <- gut_contents_merge %>%
  filter(year==2017, SEASON=='SUMMER', SITE=='AQUE', SPECIES=='PACL')

gut_pacl_SUMMER_aque_2018 <- gut_contents_merge %>%
  filter(year==2018, SEASON=='SUMMER', SITE=='AQUE', SPECIES=='PACL')

gut_pacl_SUMMER_aque_2019 <- gut_contents_merge %>%
  filter(year==2019, SEASON=='SUMMER', SITE=='AQUE', SPECIES=='PACL')

#PACL FALL
gut_pacl_FALL_mohk_2017 <- gut_contents_merge %>%
  filter(year==2017, SEASON=='FALL', SITE=='MOHK', SPECIES=='PACL')

gut_pacl_FALL_mohk_2018 <- gut_contents_merge %>%
  filter(year==2018, SEASON=='FALL', SITE=='MOHK', SPECIES=='PACL')

gut_pacl_FALL_mohk_2019 <- gut_contents_merge %>%
  filter(year==2019, SEASON=='FALL', SITE=='MOHK', SPECIES=='PACL')

gut_pacl_FALL_napl_2017 <- gut_contents_merge %>%
  filter(year==2017, SEASON=='FALL', SITE=='NAPL', SPECIES=='PACL')

gut_pacl_FALL_napl_2018 <- gut_contents_merge %>%
  filter(year==2018, SEASON=='FALL', SITE=='NAPL', SPECIES=='PACL')

gut_pacl_FALL_napl_2019 <- gut_contents_merge %>%
  filter(year==2019, SEASON=='FALL', SITE=='NAPL', SPECIES=='PACL')

gut_pacl_FALL_ivee_2017 <- gut_contents_merge %>%
  filter(year==2017, SEASON=='FALL', SITE=='IVEE', SPECIES=='PACL')

gut_pacl_FALL_ivee_2018 <- gut_contents_merge %>%
  filter(year==2018, SEASON=='FALL', SITE=='IVEE', SPECIES=='PACL')

gut_pacl_FALL_ivee_2019 <- gut_contents_merge %>%
  filter(year==2019, SEASON=='FALL', SITE=='IVEE', SPECIES=='PACL')

gut_pacl_FALL_aque_2017 <- gut_contents_merge %>%
  filter(year==2017, SEASON=='FALL', SITE=='AQUE', SPECIES=='PACL')

gut_pacl_FALL_aque_2018 <- gut_contents_merge %>%
  filter(year==2018, SEASON=='FALL', SITE=='AQUE', SPECIES=='PACL')

gut_pacl_FALL_aque_2019 <- gut_contents_merge %>%
  filter(year==2019, SEASON=='FALL', SITE=='AQUE', SPECIES=='PACL')


#PACL WINTER
gut_pacl_WINTER_mohk_2017 <- gut_contents_merge %>%
  filter(year==2017, SEASON=='WINTER', SITE=='MOHK', SPECIES=='PACL')

gut_pacl_WINTER_mohk_2018 <- gut_contents_merge %>%
  filter(year==2018, SEASON=='WINTER', SITE=='MOHK', SPECIES=='PACL')

gut_pacl_WINTER_mohk_2019 <- gut_contents_merge %>%
  filter(year==2019, SEASON=='WINTER', SITE=='MOHK', SPECIES=='PACL')

gut_pacl_WINTER_napl_2017 <- gut_contents_merge %>%
  filter(year==2017, SEASON=='WINTER', SITE=='NAPL', SPECIES=='PACL')

gut_pacl_WINTER_napl_2018 <- gut_contents_merge %>%
  filter(year==2018, SEASON=='WINTER', SITE=='NAPL', SPECIES=='PACL')

gut_pacl_WINTER_napl_2019 <- gut_contents_merge %>%
  filter(year==2019, SEASON=='WINTER', SITE=='NAPL', SPECIES=='PACL')

gut_pacl_WINTER_ivee_2017 <- gut_contents_merge %>%
  filter(year==2017, SEASON=='WINTER', SITE=='IVEE', SPECIES=='PACL')

gut_pacl_WINTER_ivee_2018 <- gut_contents_merge %>%
  filter(year==2018, SEASON=='WINTER', SITE=='IVEE', SPECIES=='PACL')

gut_pacl_WINTER_ivee_2019 <- gut_contents_merge %>%
  filter(year==2019, SEASON=='WINTER', SITE=='IVEE', SPECIES=='PACL')

gut_pacl_WINTER_aque_2017 <- gut_contents_merge %>%
  filter(year==2017, SEASON=='WINTER', SITE=='AQUE', SPECIES=='PACL')

gut_pacl_WINTER_aque_2018 <- gut_contents_merge %>%
  filter(year==2018, SEASON=='WINTER', SITE=='AQUE', SPECIES=='PACL')

gut_pacl_WINTER_aque_2019 <- gut_contents_merge %>%
  filter(year==2019, SEASON=='WINTER', SITE=='AQUE', SPECIES=='PACL')





#SEAT
fish_seat_spring_mohk_2017 <- fish_data %>%
  filter(year==2017, SEASON=='SPRING', SITE=='MOHK', SPECIES=='SEAT')

fish_seat_spring_mohk_2018 <- fish_data %>%
  filter(year==2018, SEASON=='SPRING', SITE=='MOHK', SPECIES=='SEAT')

fish_seat_spring_mohk_2019 <- fish_data %>%
  filter(year==2019, SEASON=='SPRING', SITE=='MOHK', SPECIES=='SEAT')

fish_seat_spring_napl_2017 <- fish_data %>%
  filter(year==2017, SEASON=='SPRING', SITE=='NAPL', SPECIES=='PACL')

fish_seat_spring_napl_2018 <- fish_data %>%
  filter(year==2018, SEASON=='SPRING', SITE=='NAPL', SPECIES=='PACL')

fish_seat_spring_napl_2019 <- fish_data %>%
  filter(year==2019, SEASON=='SPRING', SITE=='NAPL', SPECIES=='PACL')

fish_seat_spring_ivee_2017 <- fish_data %>%
  filter(year==2017, SEASON=='SPRING', SITE=='IVEE', SPECIES=='PACL')

fish_seat_spring_ivee_2018 <- fish_data %>%
  filter(year==2018, SEASON=='SPRING', SITE=='IVEE', SPECIES=='PACL')

fish_seat_spring_ivee_2019 <- fish_data %>%
  filter(year==2019, SEASON=='SPRING', SITE=='IVEE', SPECIES=='PACL')

fish_seat_spring_aque_2017 <- fish_data %>%
  filter(year==2017, SEASON=='SPRING', SITE=='AQUE', SPECIES=='PACL')

fish_seat_spring_aque_2018 <- fish_data %>%
  filter(year==2018, SEASON=='SPRING', SITE=='AQUE', SPECIES=='PACL')

fish_seat_spring_aque_2019 <- fish_data %>%
  filter(year==2019, SEASON=='SPRING', SITE=='AQUE', SPECIES=='PACL')

#OXCA
fish_oxca_spring_mohk_2017 <- fish_data %>%
  filter(year==2017, SEASON=='SPRING', SITE=='MOHK', SPECIES=='OXCA')

fish_oxca_spring_mohk_2018 <- fish_data %>%
  filter(year==2018, SEASON=='SPRING', SITE=='MOHK', SPECIES=='OXCA')

fish_oxca_spring_mohk_2019 <- fish_data %>%
  filter(year==2019, SEASON=='SPRING', SITE=='MOHK', SPECIES=='OXCA')



#look at sia_log next, compare sample IDs to fish data and gut contents list 
#make a table of what species, sites, and years we have for fish id, gut id, and sia processing

#plot sandbox vvv
seat_size <- ggplot(fish_sia_merge %>%
                        filter(fish_species == 'PACL'), 
                      aes(x = standard_length_mm)) +
  geom_histogram(position="identity", binwidth = 10, alpha=0.5, col = "indianred", fill = "indianred") +
  ggtitle("Kelp rockfish size  2018-2019") +
  xlab("Standard Length mm") +
  ylab("Count") +
  labs(color = "Season") +
  labs(fill = "Season") +
  theme_bw() 
