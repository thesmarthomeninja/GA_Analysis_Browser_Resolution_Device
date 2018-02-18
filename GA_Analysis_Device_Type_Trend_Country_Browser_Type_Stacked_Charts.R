library(ggplot2)
library(tidyverse)
library(googleAnalyticsR)
library(googleAuthR)
#authorize
ga_auth()


view_id <- <Insert View ID for GA Here>

channels<- google_analytics_4(view_id, 
                              date_range = c(Sys.Date() - 30, Sys.Date()),
                              metrics = c("sessions", "pageviews", "entrances", "bounces"),
                              dimensions = "channelGrouping")

channels<- google_analytics_4(view_id, 
                              date_range = c("30daysAgo", "yesterday"),
                              metrics = c("sessions", "pageviews", "entrances", "bounces"),
                              dimensions = "channelGrouping")

channels2<- google_analytics_4(view_id, 
                               date_range = c("2017-01-01","2017-12-31"),
                               metrics = c("sessions", "pageviews", "entrances", "bounces"),
                               dimensions = c("channelGrouping", "date"))




channels2 %>%
  group_by(channelGrouping) %>%
  tally()




device_cat<- google_analytics_4(view_id, 
                                date_range = c("2017-01-01","2017-12-31"),
                                metrics = c("sessions", "users"),
                                dimensions = "deviceCategory")


ggplot(data = device_cat, aes(x = deviceCategory, y = users)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_light() + ggtitle("Users by Device")


device_bydate<- google_analytics_4(view_id, 
                                   date_range = c("2017-06-01","2017-12-31"),
                                   metrics = "sessions",
                                   dimensions = c("date", "deviceCategory"))



country_info<- google_analytics_4(view_id, 
                                  date_range = c("30daysAgo", "yesterday"),
                                  metrics = "users",
                                  dimensions = "country",
                                  order = order_type("users",
                                                     sort_order=c("DESCENDING"),
                                                     orderType = c("VALUE")))




# Pie Chart with Percentages using BaseR
slices <- country_info$users
lbls <- country_info$country
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Users by Country")



ggplot(data = country_info, aes(x = country, y = users, fill = country)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_classic() + ggtitle("Users by Country")


#here i want to look at device & browser info
#i also want to limit my output
browser_info<- google_analytics_4(view_id, 
                                  date_range = c("2017-01-01","2017-12-31"),
                                  metrics = c("sessions", "users", "goalCompletionsAll"),
                                  dimensions = c("browser", "browserVersion", 
                                                 "browserSize", "screenResolution", "mobileDeviceinfo"),
                                  max = 5)



ggplot(data = browser_info, aes(x = browserSize, y = sessions)) +
  geom_bar(stat = "identity") +
  theme_classic() + ggtitle("Which browsers sizes are our visitors using?")





ggplot(data = browser_info, aes(browser)) +
  geom_bar(aes(fill= screenResolution))



dim_filter_object <- dim_filter("deviceCategory", 
                                operator = "REGEXP",
                                expressions = ".*mobile.*")


#Put filter object into a filter clause.
dim_filter_clause <- filter_clause_ga4(list(dim_filter_object),
                                       operator = "AND")

# Pull the data
google_analytics_4(viewId = view_id,
                   date_range = c("2017-01-01","2017-12-31"),
                   metrics = c("sessions","goalCompletionsAll"),
                   dimensions = "deviceCategory",
                   dim_filters = dim_filter_clause)






met_filter_object <- met_filter("goalCompletionsAll", 
                                operator = "GREATER_THAN",
                                0)


#Put filter object into a filter clause.
met_filter_clause <- filter_clause_ga4(list(met_filter_object),
                                       operator = "AND")

# Pull the data
google_analytics_4(viewId = view_id,
                   date_range = c("2017-01-01","2017-12-31"),
                   metrics = c("sessions","goalCompletionsAll"),
                   dimensions = "deviceCategory",
                   met_filters = met_filter_clause)



#Create a dimension filter object
dim_filter_object <- dim_filter("country", 
                                operator = "REGEXP",
                                expressions = ".*Sweden.*")


#Put filter object into a filter clause.
dim_filter_clause <- filter_clause_ga4(list(dim_filter_object),
                                       operator = "AND")

met_filter_object <- met_filter("sessions", 
                                operator = "GREATER_THAN",
                                1)


#Put filter object into a filter clause.
met_filter_clause <- filter_clause_ga4(list(met_filter_object),
                                       operator = "AND")

#lets use both of our filters
filtered_country_info<-google_analytics_4(view_id,  date_range = c("30daysAgo", "yesterday"),
                                          metrics = c("sessions", "users"),
                                          dimensions = "country",
                                          met_filters = met_filter_clause,
                                          dim_filters = dim_filter_clause)
## 2018-02-04 09:28:46> Downloaded [1] rows from a total of [1].
#let's take a look
filtered_country_info
##   country sessions users
## 1  Sweden        6     6
############################Segmentation###########################################

## get list of segments
my_segments <- ga_segment_list()

## just the segment items
segs <- my_segments$items

####Returning Users ####
## choose the v3 segment
segment_for_call <- "gaid::-3" #returning users

## make the v3 segment object in the v4 segment object:
seg_obj <- segment_ga4("ReturningUsers", segment_id = segment_for_call)

## make the segment call
returning_users_traffic <- google_analytics_4(view_id, 
                                              c("2017-10-31","2017-12-31"), 
                                              dimensions=c('source','medium','segment'), 
                                              segments = seg_obj, 
                                              metrics = c('sessions','bounces'))
## 2018-02-04 09:28:47> Downloaded [3] rows from a total of [3].
#view results
View(returning_users_traffic)
## Warning: running command ''/usr/bin/otool' -L '/Library/Frameworks/
## R.framework/Resources/modules/R_de.so'' had status 1
####Organic Traffic ####
## choose the v3 segment
segment_for_call <- "gaid::-5" #organic traffic

## make the v3 segment object in the v4 segment object:
seg_obj_organic <- segment_ga4("Organic Traffic", segment_id = segment_for_call)

## make the segment call
organic_users_traffic <- google_analytics_4(view_id, 
                                            c("2017-10-31","2017-12-31"), 
                                            dimensions=c("source","medium", "segment"), 
                                            segments = seg_obj_organic, 
                                            metrics = c("sessions","users", "bounces"))

View(organic_users_traffic)
