#library
library(csodata)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(scales)

options(scipen=10000)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#data pull from CSO using data source key
HPA02 <- cso_get_data('HPA02') # households vs non households
HPA12 <- cso_get_data('HPA12') # non households split by entity type

#reshape HPA02 so the years convert to rows under 1 column
#and the values of the years convert into another column
HPA02_shaped <- HPA02 %>% 
  gather(Year, Volume, 7:ncol(HPA02))

#reshape HPA12 so the years convert to rows under 1 column
#and the values of the years convert into another column
HPA12_shaped <- HPA12 %>% 
  gather(Year, Volume, 6:ncol(HPA12))

#exclude unneeded data from HPA02
HPA02_filtered <- HPA02_shaped %>%
  filter(Type.of.Buyer %in% c('Household Buyer - All', 'Non-Household Buyer') &
         Type.of.Sale == 'All Sale Types' &
         Stamp.Duty.Event == 'Executions'&
         Statistic == 'Volume of Sales')

#exclude unneeded data from HPA12
HPA12_filtered <- HPA12_shaped %>%
  filter(Stamp.Duty.Event == 'Executions'&
           Statistic == 'Volume of Sales')


#new buys % change - institutional buyers
inst_increase_pct <- HPA02_filtered %>%
  filter(Type.of.Buyer == 'Non-Household Buyer') %>%
  group_by(Dwelling.Status, Year) %>%
  arrange(Year) %>%
  summarise(VOL = sum(Volume)) %>%
  mutate(CUM_VOL = cumsum(VOL),
         YR_PCT_CHNG = round((VOL - lag(VOL))/lag(VOL), 2)) %>%
 mutate(YR_PCT_CHNG = ifelse(is.na(YR_PCT_CHNG), 0, YR_PCT_CHNG))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#bar chart bar colours and fill levels
bar_cols <- c('#7CBFFA', '#000958')
line_cols <- c('#400033', '#900373', '#D626B2')
area_cols <- c('#11F131', '#585F59')
fill_levels <- c('Non-Household Buyer', 'Household Buyer - All')

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#bar chart - purchases, household vs institutional buyers through time
ggplot(data = HPA02_filtered, aes(x = Year, y = Volume, fill = factor(Type.of.Buyer, levels = fill_levels))) +
  geom_bar(stat = 'identity', alpha = 0.6) +
  ggtitle('Home Purchases by Institution Vs Household Buyer (2010-2021)') +
  scale_fill_manual(values = bar_cols) +
  guides(fill = guide_legend(title = 'Buyer Type')) +
  facet_grid(cols = vars(Dwelling.Status), scales = 'free') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))

#line chart -  purchases, institutional volume annual change
ggplot(data = inst_increase_pct, aes(x = Year, y = YR_PCT_CHNG, group = Dwelling.Status)) +
  geom_line(aes(color = Dwelling.Status), size = 0.8, arrow = arrow(type = 'closed'), alpha = 0.6) +
  scale_y_continuous(labels = percent) +
  geom_point(aes(color = Dwelling.Status), size = 2) + 
  scale_fill_manual(values = line_cols) +
  scale_color_manual(values = line_cols) +
  guides(fill = guide_legend(title = 'Dewlling Status'), color = 'none') +
  ggtitle('Residential Home Purchases by Institutions Year on Year Change (2020-2021)') +
  ylab('Year on Year % Change') + 
  geom_label(label = paste(round(inst_increase_pct$YR_PCT_CHNG * 100, 2), '%'),
             color = 'white',
             aes(fill = Dwelling.Status)) +
  facet_grid(rows = vars(Dwelling.Status), scales = 'free') +
  theme(plot.title = element_text(hjust = 0.5))

#area chart - institutional sales an buys by participant and sector through time
HPA12_filtered %>%
  ggplot(aes(x = Year, y = Volume, group = Participant)) +
  geom_area(aes(color = Participant, fill = Participant), alpha = 0.6) +
  scale_fill_manual(values = area_cols) +
  scale_color_manual(values = area_cols) +
  facet_grid(cols = vars(Type.of.Dwelling), rows = vars(NACE.Section), scales = 'free') +
  ggtitle('Institution Residential Home Transactions by Participant and Sector (2020-2021)') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))

HPA12_filtered %>%
  filter(Participant == 'Purchases') %>%
  ggplot(aes(x = Year, y = Volume, fill = NACE.Section)) +
  geom_bar(stat = 'identity')

#heatmap of new purchases from institutions by county and year
HPA02_filtered %>%
  filter(County != 'All Counties' &
           Dwelling.Status == 'New' &
           Type.of.Buyer == 'Non-Household Buyer') %>%
ggplot(aes(x = Year, y = County, fill = Volume)) +
  ggtitle('Institution Residential New Home Purchases By Year and County (2020-2021)') +
  geom_tile() +
  geom_text(label = new_purchases_county$Volume, color = 'white')

#heatmap of all purchases from institutions by county and year
HPA02_filtered %>%
  filter(County != 'All Counties' &
           Dwelling.Status == 'All Dwelling Statuses' &
           Type.of.Buyer == 'Non-Household Buyer') %>%
ggplot(all_purchases_county, aes(x = Year, y = County, fill = Volume)) +
  ggtitle('Institution Residential All Home Purchases By Year and County (2020-2021)') +
  geom_tile() +
  geom_text(label = all_purchases_county$Volume, color = 'white')

#heatmap of all purchases from institutions by county and year
HPA02_filtered %>%
  filter(County != 'All Counties' &
           Dwelling.Status == 'Existing' &
           Type.of.Buyer == 'Non-Household Buyer') %>%
ggplot(existing_purchases_county, aes(x = Year, y = County, fill = Volume)) +
  ggtitle('Institution Residential Existing Home Purchases By Year and County (2020-2021)') +
  geom_tile() +
  geom_text(label = existing_purchases_county$Volume, color = 'white')


