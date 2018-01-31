#Keep these install notes. They could be useful in the future.

#install.packages("devtools")
#library(devtools)
#install_github("shinyGlobe", "trestletech")

library(data.table)
library(dplyr)

wine1 <- fread(file = "winemag-data-130k-v2.csv")
geocoded_new <- fread(file = "geocoded.csv")
countriesLoc = fread(file = "countriesLoc.csv")

wine_Address = as.data.frame(wine1) %>% distinct(country,province,region_1)
wine_Address = wine_Address %>% 
      mutate(address = paste(region_1, province, country, sep = ", ", collapse =NULL))
# I now have geocoded addresses called geocoded_new, I can concatenate this to wine address.
# Then I need to add address to wine 1.
# Then I need to do an inner join on address between wine 1 and wine_address.
# Then I can choose to remove any wines that do not have lat long data.
wine_Address = cbind(wine_Address, geocoded_new) %>%
              na.omit()
wine1 = wine1 %>% mutate(address = paste(region_1, province, country, sep = ", ", collapse =NULL))
print (nrow(wine1))
wine3 = inner_join(wine1, wine_Address, by = "address") #%>% na.omit()
print (nrow(wine3))
wine3 = wine3
print (nrow(wine3))

drops <- c("country.y","province.y", "V1.y","accuracy",
           "formatted_address","address_type","status","index")
wine3 = wine3[ , !(names(wine3) %in% drops)]

# Then to create the graph, I now have all the data in one dataframe. 
# This is the most important dataframe. Save it as a csv.
write.csv(wine3, "wine.csv")
# I need to convert it to the right format (lat, long, density) before output.
wine_lat_long = wine3 %>% group_by(lat, long) %>%
                  summarise(n_wines = n()/nrow(wine3))
# So I will group_by lat long. Then I will create a column called "density".
# Save this to a new dataframe for the globe output.
# Future filters can do the same thing.

wine = fread("wine.csv")
wine = wine[,-c("V1","V1.x")]
wine = wine %>% rename(province = province.x) %>%
                rename(country  = country.x)  %>%
                rename(region_1 = region_1.x)
wine = wine[,-"region_1.y"]

wine = wine[!is.na(wine$price),]

numberNAprice = sum(is.na(wine$price))
numberNArating = sum(is.na(wine$points))

wine = wine[,-"V1"]
write.csv(wine, file = "wine4.csv")

