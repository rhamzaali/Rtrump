library(ggplot2)
library(plotly)

trump_df_iphone = transform(trump_df_iphone, time_numeric = as.POSIXct(created_at, format = "%a %b %d %H:%M:%S +0000 %Y"))
trump_df_android = transform(trump_df_android, time_numeric = as.POSIXct(created_at, format = "%a %b %d %H:%M:%S +0000 %Y"))
trump_df = transform(trump_df, time_numeric = as.POSIXct(created_at, format = "%a %b %d %H:%M:%S +0000 %Y"))

ggplot(trump_df_iphone$time_numeric, aes(x = Month, y = AvgVisits)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Month", y = "Average Visits per User") +
  scale_x_date(labels = date_format("%m-%Y"))

ggplot(data=trump_df_android, aes(trump_df_android$time_numeric)) + 
  geom_histogram(aes(fill=..count..),binwidth =604800 ) 

ggplot(data=trump_df_iphone, aes(trump_df_iphone$time_numeric)) + 
  geom_histogram(aes(fill=..count..),binwidth =604800 ) 

ggplot(data=trump_df, aes(trump_df$time_numeric,fill = trump_df$source)) + 
  geom_histogram(binwidth =604800 )

ggplot(data=trump_df, aes(trump_df$time_numeric,fill = trump_df$source)) + 
  geom_histogram(binwidth =2629743 ) 


ggplotly()
