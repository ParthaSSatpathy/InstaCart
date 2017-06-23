
#-------------------------Read the files------------------------------------#
#library(readr)
orders <- read.csv("C:/Users/parth/Desktop/DTM/Project/orders.csv",header = T)
aisles <- read.csv("C:/Users/parth/Desktop/DTM/Project/aisles.csv",header = T)
departments <- read.csv("C:/Users/parth/Desktop/DTM/Project/departments.csv",header = T)
order.prior <- read.csv("C:/Users/parth/Desktop/DTM/Project/order_products__prior.csv",header = T)
order.train <- read.csv("C:/Users/parth/Desktop/DTM/Project/order_products__train.csv",header = T)
products <- read.csv("C:/Users/parth/Desktop/DTM/Project/products.csv",header = T)

head(orders)
str(orders)

head(aisles)
str(aisles)

head(departments)
str(departments)

head(order.prior)
str(order.prior)

head(order.train)
str(order.train)

head(products)
str(products)

#-------------------------Restructure the data frames----------------------#
orders$order_dow <- factor(orders$order_dow,
                           levels = c(0,1,2,3,4,5,6),
                           labels = c('Sat','Sun','Mon','Tue','Wed','Thu','Fri'))
orders$order_hour_of_day <- factor(orders$order_hour_of_day)

#-------------------------Visualize Order Details--------------------------#
# Orders are diided into prior, train and test- Let's check how

# Add the required libraries
library(ggplot2)
library(dplyr)

# A small function to convert the axis to Millions
formatter1000 <- function(x){ 
  x / 1000
}

# Show count of orders divided into prior, train, and test
ggplot(orders,
       aes(eval_set)) + 
  geom_bar(aes(fill=eval_set)) +
  geom_text(stat='count',aes(label=..count..),vjust=-1,fontface = "bold") +
  ylab("Count in Thousand") +
  ggtitle('Orders in Different sets')

# tmp <- orders %>%
#   group_by(eval_set,user_id) %>%
#   summarise(count=n()) %>%
#   summarise(n.count=n()) %>%
# ggplot(tmp,aes(x=eval_set,y=n.count)) +
#   geom_bar(stat = 'identity',aes(fill=n.count))
# rm(tmp)  

# Show No of users in prior, train and test  
orders %>%
  group_by(eval_set,user_id) %>%
  summarise(count=n()) %>%
  summarise(n.count=round(n()/1000)) %>%
  ggplot(aes(x=eval_set,y=n.count)) +
    geom_bar(stat = 'identity',aes(fill=eval_set)) +
    geom_text(stat='identity',aes(label=n.count),vjust=-1,fontface = "bold") +
    ylab('Count of User Ids in Thousands') +
    ggtitle('No of Users per different sets of Data')

# Show number of orders per day of week
orders %>%
  group_by(order_dow) %>%
  summarise(NoofOrders=round(n()/1000))%>%
  ggplot(aes(x=order_dow,y=NoofOrders)) +
    geom_bar(stat = 'identity',aes(fill=order_dow)) +
    geom_text(stat = 'identity',aes(label=NoofOrders),vjust=-1,fontface = "bold") +
    ylab('No of Orders in Thousands') +
    ggtitle('No of Orders Per Day of Week') 
    
    #scale_y_continuous(labels = formatter1000)

# Show number of orders per hour of day
orders %>%
  group_by(order_hour_of_day) %>%
  summarise(NoofOrders=round(n()/1000))%>%
  ggplot(aes(x=order_hour_of_day,y=NoofOrders)) +
  geom_bar(stat = 'identity',aes(fill=NoofOrders,color='red')) +
  geom_text(stat = 'identity',aes(label=NoofOrders),vjust=-1,fontface = "bold") +
  ylab('No of Orders in Thousands') +
  ggtitle('No of Orders Per Hour of Day') 

# Create a heat map to show orders by both Day of week and hours of day
orders %>%
  group_by(order_dow,order_hour_of_day) %>%
  summarise(count = n()) %>%
  ggplot(aes(x=order_hour_of_day,y=order_dow))+
  geom_tile(aes(fill=count)) +
  scale_fill_gradient(low="white",high="red", space ="Lab") +
  ggtitle('No of Orders per Day and per hour')

library(RColorBrewer) #To create new Color Pallet in R
#display.brewer.all()
orders %>%
  group_by(days_since_prior_order) %>%
  summarise(count=n()/1000) %>%
  ggplot(aes(x=days_since_prior_order,y=count)) +
  geom_bar(stat = 'identity',aes(fill=count)) +
  ggtitle('Days since prior order') +
  scale_fill_gradientn(colours = brewer.pal(4,'Spectral'))
  
  


