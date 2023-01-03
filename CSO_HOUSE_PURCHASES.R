#library
library(csodata)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(scales)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#data pull from CSO using data source key
HPA02 <- cso_get_data('HPA02')

#reshape HPA02 so the years convert to rows under 1 column
#and the values of the years convert into another column
HPA02_shaped <- HPA02 %>% 
  gather(Year, Volume, 7:ncol(HPA02))

HPA02_filtered <- HPA02_shaped %>%
  filter(Type.of.Buyer %in% c('Household Buyer - All', 'Non-Household Buyer') &
         Type.of.Sale == 'All Sale Types' &
         Stamp.Duty.Event == 'Executions'&
         Statistic == 'Volume of Sales')

#new buys data - all counties
new_purchases <- HPA02_filtered %>%
  filter(County == 'All Counties' &
         Dwelling.Status == 'New')

#new buys data - by county
new_purchases_county <- HPA02_filtered %>%
  filter(County != 'All Counties' &
         Dwelling.Status == 'New' &
         Type.of.Buyer == 'Non-Household Buyer')

#all buys data - all counties
all_purchases <- HPA02_filtered %>%
  filter(County == 'All Counties' &
         Dwelling.Status == 'All Dwelling Statuses')

#all buys data - by county
all_purchases_county <- HPA02_filtered %>%
  filter(County != 'All Counties' &
        Dwelling.Status == 'All Dwelling Statuses' &
        Type.of.Buyer == 'Non-Household Buyer')

#secondary buys data - all counties
existing_purchases <- HPA02_filtered %>%
  filter(County == 'All Counties' &
         Dwelling.Status == 'Existing')

#secondary buys - by county
existing_purchases_county <- HPA02_filtered %>%
  filter(County != 'All Counties' &
         Dwelling.Status == 'Existing' &
         Type.of.Buyer == 'Non-Household Buyer')

#new buys % change - institutional buyers
new_pur_inst_increase_pct <- new_purchases %>%
  filter(Type.of.Buyer == 'Non-Household Buyer') %>%
  group_by(Year) %>%
  arrange(Year) %>%
  summarise(VOL = sum(Volume)) %>%
  mutate(CUM_VOL = cumsum(VOL),
         YR_PCT_CHNG = round((VOL - lag(VOL))/lag(VOL), 2)) %>%
 mutate(YR_PCT_CHNG = ifelse(is.na(YR_PCT_CHNG), 0, YR_PCT_CHNG))

#all buys % change - institutional buyers
all_pur_inst_increase_pct <- all_purchases %>%
  filter(Type.of.Buyer == 'Non-Household Buyer') %>%
  group_by(Year) %>%
  arrange(Year) %>%
  summarise(VOL = sum(Volume)) %>%
  mutate(CUM_VOL = cumsum(VOL),
         YR_PCT_CHNG = round((VOL - lag(VOL))/lag(VOL), 2)) %>%
  mutate(YR_PCT_CHNG = ifelse(is.na(YR_PCT_CHNG), 0, YR_PCT_CHNG))

#second hand buys % change - institutional buyers
existing_pur_inst_increase_pct <- existing_purchases %>%
  filter(Type.of.Buyer == 'Non-Household Buyer') %>%
  group_by(Year) %>%
  arrange(Year) %>%
  summarise(VOL = sum(Volume)) %>%
  mutate(CUM_VOL = cumsum(VOL),
         YR_PCT_CHNG = round((VOL - lag(VOL))/lag(VOL), 2)) %>%
  mutate(YR_PCT_CHNG = ifelse(is.na(YR_PCT_CHNG), 0, YR_PCT_CHNG))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#bar chart bar colours and fill levels
bar_cols <- c('#7CBFFA', '#000958')
fill_levels <- c('Non-Household Buyer', 'Household Buyer - All')

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#bar chart - new purchases, household vs institutional buyers through time
ggplot(data = new_purchases, aes(x = Year, y = Volume, fill = factor(Type.of.Buyer, levels = fill_levels))) +
  geom_bar(stat = 'identity') +
  ggtitle('Residential New Home Purchases by Institution Vs Household Buyer (2010-2021)') +
  scale_fill_manual(values = bar_cols) +
  guides(fill = guide_legend(title = 'Buyer Type')) +
  theme(plot.title = element_text(hjust = 0.5))

#line chart - new purchases, institutional volume annual change
ggplot(data = new_pur_inst_increase_pct, aes(x = Year, y = YR_PCT_CHNG, group = 1)) +
  geom_line(color = 'blue', size = 0.8, arrow = arrow(type = 'closed')) +
  scale_y_continuous(labels = percent) +
  geom_point(color = 'blue', size = 2) + 
  ggtitle('Residential New Home Purchases by Institutions Year on Year Change (2020-2021)') +
  ylab('Year on Year % Change') + 
  geom_label(label = paste(round(new_pur_inst_increase_pct$YR_PCT_CHNG * 100, 2), '%'),
             color = 'white',
             fill = 'blue') +
  theme(plot.title = element_text(hjust = 0.5))

#heatmap of new purchases from institutions by county and year
ggplot(new_purchases_county, aes(x = Year, y = County, fill = Volume)) +
  ggtitle('Institution Residential New Home Purchases By Year and County (2020-2021)') +
  geom_tile() +
  geom_text(label = new_purchases_county$Volume, color = 'white')

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#bar chart - all purchases, household vs institutional buyers through time
ggplot(data = all_purchases, aes(x = Year, y = Volume, fill = factor(Type.of.Buyer, levels = fill_levels))) +
  geom_bar(stat = 'identity') +
  ggtitle('Residential All Home Purchases by Institution Vs Household Buyer (2010-2021)') +
  scale_fill_manual(values = bar_cols) +
  guides(fill = guide_legend(title = 'Buyer Type')) +
  theme(plot.title = element_text(hjust = 0.5))

#line chart - new purchases, institutional volume annual change
ggplot(data = all_pur_inst_increase_pct, aes(x = Year, y = YR_PCT_CHNG, group = 1)) +
  geom_line(color = 'blue', size = 0.8, arrow = arrow(type = 'closed')) +
  scale_y_continuous(labels = percent) +
  geom_point(color = 'blue', size = 2) + 
  ggtitle('Residential All Home Purchases by Institutions Year on Year Change (2020-2021)') +
  ylab('Year on Year % Change') + 
  geom_label(label = paste(round(all_pur_inst_increase_pct$YR_PCT_CHNG * 100, 2), '%'),
             color = 'white',
             fill = 'blue') + 
  theme(plot.title = element_text(hjust = 0.5))

#heatmap of all purchases from institutions by county and year
ggplot(all_purchases_county, aes(x = Year, y = County, fill = Volume)) +
  ggtitle('Institution Residential All Home Purchases By Year and County (2020-2021)') +
  geom_tile() +
  geom_text(label = all_purchases_county$Volume, color = 'white')

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#bar chart - existing purchases, household vs institutional buyers through time
ggplot(data = existing_purchases, aes(x = Year, y = Volume, fill = factor(Type.of.Buyer, levels = fill_levels))) +
  geom_bar(stat = 'identity') +
  ggtitle('Residential Existing Home Purchases by Institution Vs Household Buyer (2010-2021)') +
  scale_fill_manual(values = bar_cols) +
  guides(fill = guide_legend(title = 'Buyer Type')) +
  theme(plot.title = element_text(hjust = 0.5))

#line chart - existing purchases, institutional volume annual change
ggplot(data = existing_pur_inst_increase_pct, aes(x = Year, y = YR_PCT_CHNG, group = 1)) +
  geom_line(color = 'blue', size = 0.8, arrow = arrow(type = 'closed')) +
  scale_y_continuous(labels = percent) +
  geom_point(color = 'blue', size = 2) + 
  ggtitle('Residential Existing Home Purchases by Institutions Year on Year Change (2020-2021)') +
  ylab('Year on Year % Change') + 
  geom_label(label = paste(round(existing_pur_inst_increase_pct$YR_PCT_CHNG * 100, 2), '%'),
             color = 'white',
             fill = 'blue') +
  theme(plot.title = element_text(hjust = 0.5))

#heatmap of all purchases from institutions by county and year
ggplot(existing_purchases_county, aes(x = Year, y = County, fill = Volume)) +
  ggtitle('Institution Residential Existing Home Purchases By Year and County (2020-2021)') +
  geom_tile() +
  geom_text(label = existing_purchases_county$Volume, color = 'white')
