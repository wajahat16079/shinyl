

library(readxl)
library(tidyverse)
library(zipcodeR)

library(leaflet)
library(htmltools)


dealers = read_excel("Dealer Contacts.xlsx", sheet = "Dealer Contact Information", skip = 2)
prods = read_excel("Dealer Contacts.xlsx", sheet = "Dealer Territories", skip = 0)
names(prods)[2] = "State"
prods[prods == "N/A"] <- NA

dealers = full_join(dealers,prods, by = c("State","Dealer"))

dealers$Zip

latLong = geocode_zip(dealers$Zip)
names(latLong)[1] = "Zip"
latLong$Zip = as.numeric(latLong$Zip)

# latLong1 = latLong %>% full_join(dealers[,c("Zip","Dealer")], by = "Zip")
# 
# 
# tt =dealers %>%
#   group_by(Dealer) %>%
#   summarise(Count = n_distinct(Zip))



dealers = dealers %>% inner_join(latLong, by = "Zip")


# length(unique(dealers1$Dealer))
# length(unique(dealers$Dealer))
# length(unique(prods$Dealer))


# str(dealers)
# str(latLong)
# geocode_zip(33707)

# dealers$lat =  NA
# dealers$long =  NA

# dealers$lat[!is.na(dealers$Zip)] =  latLong$lat
# dealers$long[!is.na(dealers$Zip)]  =  latLong$lng


# df = dealers %>% left_join(prods, by = c("Dealer","State"))
df = dealers[!is.na(dealers$Zip),]

mapDf = df %>% filter(State == "NY")
mapDf = df 

Line0 = paste0("<b>State</b>: ", mapDf$State)
Line1 = paste0("<b>City</b>: ", mapDf$City)
Line2 = paste0("<b>Dealer</b>: ", mapDf$Dealer)
Line3 = paste0("<b>Name</b>: ", mapDf$Name)
Line4 = paste0("<b>Dolphin</b>: ", mapDf$Dolphin)
Line5 = paste0("<b>Rehan</b>: ", mapDf$Rehan)
Line6 = paste0("<b>Embossers</b>: ", mapDf$Embossers)
Line7 = paste0("<b>Clovers</b>: ", mapDf$Clovers)
Line8 = paste0("<b>VisionAid</b>: ", mapDf$VisionAid)

Line4 = ifelse(is.na(mapDf$Dolphin), 
               paste0("<b><font color='red'>Dolphin</font></b>: NA"), 
               paste0("<b><font color='green'>Dolphin</font></b>: ", mapDf$Dolphin))

Line5 = ifelse(is.na(mapDf$Rehan), 
               paste0("<b><font color='red'>Rehan</font></b>: NA"), 
               paste0("<b><font color='green'>Rehan</font></b>: ", mapDf$Rehan))

Line6 = ifelse(is.na(mapDf$Embossers), 
               paste0("<b><font color='red'>Embossers</font></b>: NA"), 
               paste0("<b><font color='green'>Embossers</font></b>: ", mapDf$Embossers))

Line7 = ifelse(is.na(mapDf$Clovers), 
               paste0("<b><font color='red'>Clovers</font></b>: NA"), 
               paste0("<b><font color='green'>Clovers</font></b>: ", mapDf$Clovers))

Line8 = ifelse(is.na(mapDf$VisionAid), 
               paste0("<b><font color='red'>VisionAid</font></b>: NA"), 
               paste0("<b><font color='green'>VisionAid</font></b>: ", mapDf$VisionAid))

Line9 = ifelse(is.na(mapDf$VisionAid), 
               paste0("<b><font color='red'>Notes</font></b>: NA"), 
               paste0("<b><font color='blue'>Notes</font></b>: ", mapDf$Notes))


pointLabels <- paste(sep = "<br/>",
                      Line0,
                      Line1,
                      Line2,
                      Line3,
                      Line4,
                      Line5,
                      Line6,
                      Line7,
                      Line8,
                      Line9
                      )%>%
  lapply(htmltools::HTML)


leaflet(mapDf) %>%
  addTiles() %>%  
  addMarkers(lng=~lng, lat=~lat,popup= pointLabels,label= pointLabels)%>%
  addCircleMarkers(lng=~lng, lat=~lat,popup= pointLabels,label= pointLabels, radius = 5)







url = ("https://www.dropbox.com/scl/fi/3rgw9dpk5m25jid42dfrq/Dealer-Contacts.xlsx?rlkey=bh9c3ebw9m3vrj0j5h6ot9ma4&dl=0")
#download the files first
walk2(url, "new_iris_filenames", ~drop_download(path = .x, local_path = .y))

url = "https://www.dropbox.com/scl/fi/3rgw9dpk5m25jid42dfrq/Dealer-Contacts.xlsx?rlkey=bh9c3ebw9m3vrj0j5h6ot9ma4&dl=0"
library(readxl)
library(httr)
# packageVersion("readxl")
# [1] ‘0.1.1’
getwd()
# tt = GET(url, write_disk("temp.xls"))
tt = GET(url)

df <- read_excel("temp.xls")

str(df)

download.file(url,destfile = "local.xls")
datf <- read_excel("local.xls",col_names = TRUE,col_types = "text")
readxl::read_excel(path = path.expand("local.xls"))
getwd()
