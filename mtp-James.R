# Midterm Project
# Jingxiang Li (James)
# BUAN 5210
# Oct 26, 2016

# ======================================================= #
# clean environment and load libraries
rm(list = ls(all = TRUE))

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

# load data
oj <- read.csv("6_MTP_Data.csv")
View(oj)  #view data frame

# ======================================================= #
###           I. Univariate non-graphical EDA           ###

# Structure of data
str(oj)  

# Review summary statistics
summary(oj)
# about 23.4% sales have in store promotion

### Categorical data

# count and percentage of Advertisment
(count_ad <- as.data.frame(count(oj, ad)) %>% 
  mutate(percent_coun = n/sum(n)))
# 6.8% big promo, 6.2% medium/small, 87% no promo

# total package sales by brand
(brand_sales <- oj %>% 
  group_by(brand) %>% 
  summarise(sales = sum(units)) %>% 
  arrange(desc(sales)))
# Sales No.1: POST HONEY BUNCHES OF OATS
# Sales No.2: KELLOGGS FROSTED FLAKES
# Sales No.10: POST SHREDDED WHEAT
# Sales No.11: KELLOGGS SMART START

### interval and continuous data 

# summary of data
summary(oj[c(1,5,7,8)])
# units not normal heavily negative skew, mean >> median
# volume and price: mean close to median

# ======================================================= #
###           II. Univariate graphical EDA              ###

# Categorical variables
par(mfrow = c(2,2))
barplot(table(oj$brand), main = "Observations by brand")
barplot(table(oj$flavor),main = "Observations by flavor" )
barplot(table(oj$grain),main = "Observations by grain" )
barplot(table(oj$ad), main = "Observations by advertisment")
# most common flavor is regular, almost tripled other flavors
# most common grain is corn
# followed by whole grain wheat
# most observation brands: KELLOGGS FROSTED FLAKES, POST HONEY BUNCHES OF OATS
# POST HONEY BUNCHES OF OATS has more package sold, 
# KELLOGGS FROSTED FLAKES has more transactions

# Continuous variables
par(mfrow = c(2,2))
hist(oj$units, breaks = 20, main = "Dist of units per sale")
hist(oj$volume, breaks = 20, main = "Dist of volumes")
hist(oj$price, breaks = 20, main = "Dist of price")

par(mfrow = c(2,2))
hist(oj$units, breaks = 40, main = "Dist of units per sale")
hist(oj$volume, breaks = 40, main = "Dist of volumes")
hist(oj$price, breaks = 40, main = "Dist of price")
## units per sale: decreasing by units
## dist of price is negative skewed, most common price range is 2.5 - 5

# ======================================================= #
###                 III. Multivariate EDA               ###


# how many distinct products in this data (81)
# and how many packages of each product sold
dist_products <- oj %>% 
  group_by(brand, flavor, grain, volume) %>% 
  summarise(sales = sum(units)) %>% 
  arrange(desc(sales))#81 combinations
# KELLOGGS FROSTED FLAKES-regular-corn-1.44 is the best seller

# ======================================================= #
# does promotion increase brand's units per sale?
# ======================================================= #
oj2 <- oj 
oj2$promo <- as.factor(oj2$promo)
  
promo_oj <- oj2 %>% 
  group_by(brand, promo) %>%
  summarise(med_sales = median(units))

ggplot(promo_oj, aes(reorder(brand,med_sales), med_sales, color = promo)) +
  geom_point(stat = "identity") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=12,face="bold")) +
  scale_color_discrete(labels=c("no", "Yes")) +
  ylab("") +
  xlab("") +
  ggtitle("Promotion improves brand's \nmedian units per sale") +
  coord_flip()

# ==========================================================#
# 
# Comparing the advertisements between best and worst 
# selling brands
#
# ==========================================================#
# From brand_sales (most units sold)
# Sales No.1: POST HONEY BUNCHES OF OATS
# Sales No.2: KELLOGGS FROSTED FLAKES
# Sales No.10: POST SHREDDED WHEAT
# Sales No.11: KELLOGGS SMART START

adv_oj <- oj2 %>% 
  filter(brand %in% c("POST HONEY BUNCHES OF OATS",
                    "KELLOGGS FROSTED FLAKES",
                    "POST SHREDDED WHEAT",
                    "KELLOGGS SMART START")) %>% 
  group_by(brand, ad) %>% 
  summarise(purchase_coun = n())

ggplot(adv_oj, aes(purchase_coun, reorder(brand,purchase_coun), color = ad)) +
  geom_point(stat = "identity") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(ad ~., scales = "free_x", space="fixed")

ggplot(adv_oj, aes(reorder(brand,purchase_coun), purchase_coun, fill = ad)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(ad ~., scales = "free", space="free") +
  theme(axis.text=element_text(size=7),
        axis.title=element_text(size=12,face="bold")) +
  xlab("") +
  ggtitle("Advertisement more effective for \nnon-popular brands")
  
# ==========================================================#
# 
# Comparing the two of best selling brands
#
# ==========================================================#

best2 <- oj2 %>% 
  filter(brand %in% c("POST HONEY BUNCHES OF OATS",
                        "KELLOGGS FROSTED FLAKES")) %>% 
  group_by(brand, promo, ad) %>% 
  summarise(med_sales = median(units))

ggplot(best2, aes(x = ad, y = med_sales, shape = brand, color = promo)) +
  geom_point(size = 4) +
  theme(legend.position = "bottom") +
  ggtitle("Promotion and advertisement not always help with
          \n median unit per sale") +
  ylab("median units per sale") +
  theme(legend.text=element_text(size = 7))



