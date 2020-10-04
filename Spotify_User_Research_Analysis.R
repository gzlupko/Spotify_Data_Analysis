# Top 50 Spotify Songs in 2019 



getwd()
setwd("/Users/gianzlupko/Desktop") 

# load packages 
# data manipulation
library(readr)
library(wesanderson) 
library(tidyverse) 
library(broom) 
library(gridExtra) 

# statistical analysis 
library(corrplot) 
library(Hmisc)




top50 <- read_csv("top50.csv")
View(top50) 

 top_genre <- top50 %>% 
  group_by(Genre) %>%
  count(Genre) %>%
   arrange(desc(n)) 

 
 

 
top_10_genre <- top_genre[1:6, ]
   

top_10_genre %>%
  ggplot(aes(x = Genre, y = n, fill = Genre)) + geom_bar(stat = "identity") + 
  ylab("Genre Count") + theme(legend.position = "none") + 
  theme(text = element_text(size = 12)) + 
  scale_fill_manual(values = wes_palette("Zissou1", 6, "continuous")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Genres by Count in Top 50 Playlist")



# density plots 



# what was most danceable? 

good_dancing <- top50 %>% 
  filter(Genre %in% c("latin", "dance pop", "pop")) 
         
ggplot(good_dancing) + 
  geom_density(aes(x = Danceability, fill = Genre), alpha = 0.3) + 
  scale_fill_manual(values = wes_palette("GrandBudapest1")) 




# what songs were fastest? 

# first pull from Genres with same or similar counts 
genre_count <- top50 %>%
  group_by(Genre) %>%
  count(Genre) %>%
  arrange(desc(n))

View(genre_count) 

top50 %>% 
  filter(Genre %in% c("dfw rap", "electropop", "reggaeton")) %>%
  ggplot() + 
  geom_density(aes(x = Beats.Per.Minute, fill = Genre), alpha = 0.3) +
  scale_fill_manual(values = wes_palette("Moonrise3")) 


# the majority of dance pops songs fell in 100 bpm range with a few songs in uber 
# fast territory 

top50 %>% 
  filter(Genre %in% c("dance pop")) %>%
  ggplot() + 
  geom_density(aes(x = Beats.Per.Minute, fill = Genre), alpha = 0.5) + 
  scale_fill_manual(values = wes_palette("Moonrise3")) 




# how about bpm for all 50 songs ?


top50 <- top50 %>%
  mutate(length_compr = ifelse(Length. > mean(Length.), "High", "Low")) %>%
  mutate(energy_level = ifelse(Energy > mean(Energy), "High", "Low")) %>%
  mutate(mood_compr = ifelse(Valence. > mean(Valence.), "High", "Low"))


top50 %>% 
  ggplot() + 
  geom_density(aes(x = Beats.Per.Minute, color = mood_compr), alpha = 0.2) +  
  labs(color = "Valence") 
  

top50 %>% 
  ggplot() + geom_density(aes(x = Beats.Per.Minute, color = length_compr), 
                          alpha = 0.2) + labs(color = "Song Length")

 top50 %>% 
  ggplot() + geom_density(aes(x = Beats.Per.Minute, color = energy_level), 
                          alpha = 0.2) + labs(color = "Energy Level") 

ggplot(data = top50) + 
  geom_density(aes(x = Beats.Per.Minute, alpha = 0.2)) + 
  theme(legend.position = "none") + geom_label( 
    label = "Lil Tecca - Ransom", 
    x = 176, y = 0.003, label.padding = unit(0.07, "lines"),
    label.size = 0.02,
    color = "black",
    fill="#69b3a2") + geom_label( 
      label = "Chainsmokers - Takeaway", 
      x = 100, y = 0.011, label.padding = unit(0.07, "lines"),
      label.size = 0.2,
      color = "black",
      fill="#69b3a2")


# beats per minute for all songs with select labels 
ggplot(data = top50) + 
  geom_density(aes(x = Beats.Per.Minute, alpha = 0.2)) + 
  theme(legend.position = "none") + 
  annotate(geom = "text", x = 95, y = 0.011, label = "Chainsmokers", 
           color = "dodgerblue2", size = 4) + 
  annotate(geom = "text", x = 95, y = 0.010, label = "Takeaway", 
           color = "dodgerblue2", size = 4) + 
  annotate(geom = "text", x = 178, y = 0.003, label = "Lil Tecca", 
           color = "dodgerblue2", size = 4) + 
  annotate(geom = "text", x = 178, y = 0.002, label = "Ransom", 
           color = "dodgerblue2", size = 4) + theme(text = element_text(size = 8)) 



ggplot(data = top50) + 
  geom_density(aes(x = Beats.Per.Minute, alpha = 0.2)) + 
  theme(legend.position = "none") + 
  annotate(geom = "text", x = 93, y = 0.011, label = "Chainsmokers", 
           color = "blue") + 
  annotate(geom = "text", x = 93, y = 0.010, label = "Takeaway", 
           color = "blue") + 
  annotate(geom = "text", x = 178, y = 0.003, label = "Lil Tecca", 
           color = "blue") + 
  annotate(geom = "text", x = 178, y = 0.002, label = "Ransom", 
           color = "blue")


ggplot(data = top50) + 
  geom_density(aes(x = Beats.Per.Minute, alpha = 0.2)) + 
  theme(legend.position = "none") + 
  geom_label_repel(aes(label = Track.Name$Takeaway(top50), 
                       color = 'black', size = 3.5)) 



# correlations in data set


song_quant <- top50[ ,5:14]
rel <- cor(song_quant) 
rel

# simplify correlation matrix with corrplot 

corrplot(rel, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45) 


# calculate significant correlations 

rel2 <- rcorr(as.matrix(rel)) 
rel2

# extract correlation coefficients 
rel2$r

# p-values 
rel2$P

# leave out insignificant correlations 

corrplot(rel, type="upper", order="hclust", 
         p.mat = rel2$P, sig.level = 0.05, insig = "blank")


# valence is a measure of positive mood. The higher valence, the more cheerful 
# visualize negative relationship between valence and popularity 
# less cheerful songs were more popular in the top 50 songs 

lm(Valence. ~ Popularity, data = top50) %>%
  summary()

ggplot(data = top50, aes(x = Valence., y = Popularity)) + geom_point() + 
  geom_smooth(method = 'lm') 


# Energy by Genre. Spotify tags song energy - the higher the value the more energy. 

 Genre <- top50 %>%
  group_by(Genre) %>%
  select(Genre, Energy) %>%
  summarize(mean_energy = mean(Energy)) 

 ggplot(data = Genre, aes(Genre, mean_energy)) + 
   geom_col(color = "red", fill = "orange", alpha = 0.2) + 
   coord_flip() + ggtitle("Energy by Genre") + xlab("Avg. Energy") + 
   theme(text = element_text(size = 12))


# Beats Per Minute by Genre 

top50$beats_per_minute <- top50$Beats.Per.Minute

bpm <- top50 %>%
   group_by(Genre) %>%
   select(Genre, beats_per_minute) %>%
   summarize(mean_bpm = mean(beats_per_minute)) 

ggplot(data = bpm, aes(x = reorder(Genre, -mean_bpm), y = mean_bpm, fill = mean_bpm)) +  
  geom_col() + coord_flip() + 
  scale_fill_gradient(low = "sky blue", high = "blue") + 
  theme(legend.position = "none") + ggtitle("Beats Per Minute by Genre") + 
  ylab("Beats Per Minute") + xlab("Genre") + theme(text = element_text(size = 12))


 

ggplot(data = top50, aes(Genre, Energy)) + 
  geom_boxplot(color = "red", fill = "orange", alpha = 0.2) + 
  coord_flip() + ggtitle("Energy by Genre") 



# multiple regression with valence and energy predicting popularity and 
# indicating relationship






# split energy into binary 'high' and 'low'
top50 <- top50 %>%
  mutate(energy_level = ifelse(Energy > mean(Energy), "High", "Low"))

# create multiple linear model
#use condition as categorial model
mod <- lm(Popularity ~ Valence. + energy_level, data = top50)

#create augmented model to gain fitted values
augmented_mod <- mod %>%
  augment()


#create scatter plot
#color code by condition category
data_space <- ggplot(augmented_mod, aes(x= Valence., y = Popularity, color = energy_level)) + geom_point()

#add geom_line with fitted values





# multiple regressions (x3) below and then will stack together 

# split length into binary 'high' and 'low' compared with mean 
top50 <- top50 %>%
  mutate(length_compr = ifelse(Length. > mean(Length.), "High", "Low"))
# create multiple linear model
#use condition as categorial model
mod1 <- lm(Popularity ~ beats_per_minute + length_compr, data = top50)
#create augmented model to gain fitted values
augmented_mod1 <- mod1 %>%
  augment()
#create scatter plot
#color code by condition category
data_space1 <- ggplot(augmented_mod1, aes(x= beats_per_minute, y = Popularity, color = length_compr)) + geom_point()
#add geom_line with fitted values



# split length into binary 'high' and 'low' compared with mean 
top50 <- top50 %>%
  mutate(mood_compr = ifelse(Valence. > mean(Valence.), "High", "Low"))
# create multiple linear model
#use condition as categorial model
mod2 <- lm(Popularity ~ beats_per_minute + mood_compr, data = top50)
#create augmented model to gain fitted values
augmented_mod2 <- mod2 %>%
  augment()
#create scatter plot
#color code by condition category
data_space2 <- ggplot(augmented_mod2, aes(x= beats_per_minute, y = Popularity, color = mood_compr)) + geom_point()
#add geom_line with fitted values



# create multiple linear model
#use condition as categorial model
mod3 <- lm(Popularity ~ Danceability + energy_level, data = top50)
#create augmented model to gain fitted values
augmented_mod3 <- mod3 %>%
  augment()
#create scatter plot
#color code by condition category
data_space3 <- ggplot(augmented_mod3, aes(x= Danceability, y = Popularity, color = energy_level)) + geom_point()
#add geom_line with fitted values



reg <- data_space + geom_line(aes(y= .fitted)) + labs(color = 'Energy Level')
reg1 <- data_space1 + geom_line(aes(y= .fitted)) + labs(color = 'Song Length')
reg2 <- data_space2 + geom_line(aes(y= .fitted)) + labs(color = 'Song Mood')
reg3 <- data_space3 + geom_line(aes(y= .fitted)) + labs(color = 'Energy Level')

reg
reg1
reg2
reg3 


View(top50)

