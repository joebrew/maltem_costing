library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggthemes)
options(scipen = '999')

df <- read_excel('Analysis.xlsx', sheet = '2')
df <- df[,1:8]
names(df)[8] <- 'SOCIAL_MOB'
df <- data.frame(df)
df$Year <- as.numeric(as.character(df$Year))
df <- df %>% filter(!is.na(Year))
df <- gather(df, key, value, ENTO:SOCIAL_MOB)
df$value[is.na(df$value)] <- 0
df <- df %>% arrange(Year)

# grouped_by_category <- 
#   df %>%
#   group_by(category = key) %>%
#   summarise(value = sum(value))

cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(df$key)))
df$key <- gsub('_', ' ', df$key)
ggplot(data = df, 
       aes(x = Year,
           y = value,
           group = key,
           fill = key)) +
  geom_bar(stat = 'identity', position = 'stack') +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_fivethirtyeight() +
  scale_fill_manual(name = '',
                    values = cols) +
  ggtitle('Costs per year')
ggsave(filename = 'static.png')

years <- sort(unique(df$Year))
for (i in 1:length(years)){
  this_year <- years[i]
  sub_data <- df %>%
    mutate(value = ifelse(Year > this_year,
                          0,
                          value))
  g <- ggplot(data = sub_data, 
         aes(x = Year,
             y = value,
             group = key,
             fill = key)) +
    geom_bar(stat = 'identity', position = 'stack') +
    theme(axis.text.x = element_text(angle = 90)) +
    theme_fivethirtyeight() +
    scale_fill_manual(name = '',
                      values = cols) +
    ggtitle(this_year,
            paste0('Costs per year'))
  print(g)
  ggsave(filename = paste0('gif/',this_year, '.png'))
}