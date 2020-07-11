library(tidyverse)
library(gganimate)
library(magick)
library(gifski)
library(scales)
library(janitor)
library(extrafont)

#importing font
font_import(pattern = "Andale Mono")

#read in data
carsales <- read.csv("carsales.csv", header = T)

#recoding fixes from webscrape
carsales$brand<- recode_factor(as.factor(carsales$brand), "Land"="Land Rover", "Range"="Range Rover")

#filter data for years of interests
filtered_sales <- carsales %>% 
  filter(year>1995)

#generate colors for figures
colors <- sample(colors(distinct = T), size = 62)
names(colors) <- levels(factor(carsales$brand))
colors["Kia"] <- "darkblue"
colors["Honda"] <- "palegreen3"
colors["Dodge"] <- "palevioletred4"
colors["Hyundai"] <- "tan3"
colors["GMC"] <- "purple"
colors["Buick"] <- "orange"
colors["Volkswagen"] <- "plum3"
colors["Jeep"] <- "steelblue3"
colors["Ford"] <- "darksalmon"
colors["Toyota"] <- "plum"
colors["Nissan"] <- "thistle4"
colors["Subaru"] <- "darkred"
colors["Pontiac"] <- "gray47"
my_scale <- scale_fill_manual(name = "brand", values = colors)

#total sales by brand
filtered_sales %>% 
  group_by(brand) %>% 
  summarise(sum=sum(sales)) %>% 
  arrange(-sum) %>% 
  head(n=10) %>% 
  ggplot(aes(x=reorder(brand,sum), y=sum/1000000))+
  geom_bar(stat = "identity", aes(fill=brand))+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "none")+
  my_scale+
  ylab("Sales in millions of Units Sold")+
  xlab("Car Brand")+
  ggtitle("Total Car Sales by Brand Since 1996")+
  labs(caption = "Data Source:CarSalesBase.com")+
  theme(text = element_text(family = "Andale Mono", size=12) ,
        plot.title = element_text(hjust = .5, size = 15),
        plot.subtitle = element_text(hjust = .5, size = 20),
        legend.title = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(size=8, color = "grey45"))

#count difference by brand
filtered_sales %>% 
  group_by(brand) %>% 
  mutate(minyear=min(year),
         maxyear=max(year)) %>% 
  ungroup() %>% 
  group_by(brand) %>% 
  summarise(diff = sales[year==2019]-sales[year==minyear]) %>% 
  arrange(desc(diff)) %>% 
  ggplot(aes(x = reorder(brand, diff) , y = diff, fill=brand))+
  geom_bar(stat = "identity")+
  my_scale +
  theme_minimal()+
  theme(legend.position = "none")+
  coord_flip()+
  ylab("Sales in 2019 - Sales in 1996")+
  xlab("Car Brands")+
  ggtitle("Car Sales Difference of First and Last Year")+
  labs(caption = "Data Source:CarSalesBase.com")+
  theme(text = element_text(family = "Andale Mono", size=12) ,
        plot.title = element_text(hjust = .5, size = 14),
        plot.subtitle = element_text(hjust = .5, size = 20),
        legend.title = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(size=8, color = "grey45"))+
  scale_y_continuous(labels = comma, breaks = c(-1000000 ,-500000, 0, 500000, 1000000), 
                     limits = c(-1200000, 1200000))

#percent difference by brand
filtered_sales %>% 
  group_by(brand) %>% 
  mutate(minyear=min(year),
         maxyear=max(year)) %>% 
  ungroup() %>% 
  group_by(brand) %>% 
  summarise(percentdiff = sales[year==2019]/sales[year==minyear]) %>% 
  filter(percentdiff!=1&is.finite(percentdiff)==T) %>% 
  mutate(percentdiff = (percentdiff)-1) %>% 
  ggplot(aes(x = reorder(brand, percentdiff) , y = percentdiff, fill=brand))+
  geom_bar(stat = "identity")+
  my_scale +
  theme_minimal()+
  theme(legend.position = "none")+
  coord_flip()+
  ylab("Difference Between 1996 and 2019(%)")+
  xlab("Car Brands")+
  ggtitle("Percent Difference Between 1996 and 2019")+
  labs(caption = "Data Source:CarSalesBase.com")+
  geom_hline(yintercept = 0, color = "gray67")+
  theme(text = element_text(family = "Andale Mono", size=12) ,
        plot.title = element_text(hjust = .5, size = 14),
        plot.subtitle = element_text(hjust = .5, size = 20),
        legend.title = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(size=8, color = "grey45"))+
  scale_y_continuous(breaks=c(-1, 0, 5, 10, 15), 
                     labels = scales::percent)

#total yearly sales over time
filtered_sales %>% 
  group_by(year) %>% 
  summarise(
    sum=sum(sales)
  ) %>% 
  ggplot(aes(year, sum/1000000))+
  geom_point()+
  geom_line()+
  ylim(0,25)+
  theme_minimal()+
  ylab("Car sales in millions of units sold")+
  xlab("Year")+
  ggtitle("Total US Car Sales Since 1996")+
  labs(caption = "Data Source:CarSalesBase.com")+
  theme(text = element_text(family = "Andale Mono", size=12) ,
        plot.title = element_text(hjust = .5, size = 15),
        plot.subtitle = element_text(hjust = .5, size = 20),
        legend.title = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(size=8, color = "grey45"))

#total yearly sd in sales over time
filtered_sales %>% 
  group_by(year) %>% 
  summarise(
    sd=sd(sales, na.rm = T)
  ) %>% 
  ggplot(aes(year, sd/100000))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  ylab("SD of car sales in millions of units sold")+
  xlab("Year")+
  ggtitle("SD of Car Sales Past 25 Years")+
  labs(caption = "Data Source:CarSalesBase.com")+
  theme(text = element_text(family = "Andale Mono", size=12) ,
        plot.title = element_text(hjust = .5, size = 15),
        plot.subtitle = element_text(hjust = .5, size = 20),
        legend.title = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(size=8, color = "grey45"))

#Car Brands Over Time
filtered_sales %>% 
  ggplot(aes(x=year, y=sales/1000000))+
  geom_point(aes(color=brand))+
  geom_line(aes(color=brand))+
  geom_label(data = filtered_sales %>% filter(year==2019),
             aes(label = brand,x= year +.5, y= sales/1000000 ,color=brand), label.size = .1,
             family = "Andale Mono")+
  theme_minimal()+
  theme(legend.position = "none")+
  xlim(1995,2020)+
  scale_color_manual(values = colors)+
  ylab("Sales in millions of units")+
  xlab("Year")+
  ggtitle("US Car Sales Over Time by Brand")+
  labs(caption = "Data Source:CarSalesBase.com")+
  theme(text = element_text(family = "Andale Mono", size=12) ,
        plot.title = element_text(hjust = .5, size = 15),
        plot.subtitle = element_text(hjust = .5, size = 20),
        legend.title = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(size=8, color = "grey45"))

#Race Bar Chart
testdf <- carsales %>% 
  group_by(year) %>% 
  mutate(rank = rank(-sales),
         Value_rel = sales/sales[rank==1],
         Value_lbl = paste0(" ", round(sales/1000),"K")) %>% 
  group_by(brand) %>% 
  filter(rank <=10, year>1995) %>% 
  ungroup()

anim = ggplot(testdf, aes(rank, group = brand, 
                                fill = brand)) +
  geom_tile(aes(y = sales/2,
                height = sales,
                width = 0.9)) +
  geom_text(aes(y = 0, label = paste(brand, " ")), vjust = 0.2, hjust = 1, size=8,
            family = "Andale Mono") +
  geom_text(aes(y=sales, label = Value_lbl, hjust=0), size=6, 
            family = "Andale Mono") +
  coord_flip(clip = "off", expand = TRUE) +
  scale_x_reverse() +
  theme_minimal()+
  guides(color = FALSE, fill = FALSE) +
  theme(text = element_text(family = "Andale Mono", size=12),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=30, hjust=0.5, face="bold", colour="black"),
        plot.caption =element_text(size=14, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))+
  scale_y_continuous(labels = scales::comma, limits = c(0, 3500000)) +
  my_scale + 
  transition_states(year, transition_length = 30, state_length = 30) +
  ease_aes("sine-in-out")+
  labs(title = 'US Car Sales Per Year of Top 10 Brands : {closest_state}',  
       caption  = "US Car Sales | Data Source: CarSalesBase.com")

animate(anim, 400, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("carsales.gif"))

