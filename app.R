
library(shiny)
library(leaflet)
library(RColorBrewer)
library(DT)
library(tidyverse)
library(readxl)
library(shinyBS)

library(zipcodeR)

library(htmltools)
library(janitor)
library(shinyWidgets)


# Function to add a small random offset to coordinates
add_random_offset <- function(coordinate) {
  offset <- runif(length(coordinate), -0.001, 0.001) # Adjust the range as needed
  return(coordinate + offset)
}

dms_to_decimal <- function(dms_str) {
  # dms_str = df$lon
  num <- parse_number(dms_str)
  
  num[grepl("S",dms_str)] = -num[grepl("S",dms_str)] 
  num[grepl("W",dms_str)] = -num[grepl("W",dms_str)] 
  
  
  
  return(num)
}

getCleanData = function(df){
  
  
  
  df = clean_names(df)
  df = df[!is.na(df$dealer),]
  
  # df$lat  = parse_number(df$lat)
  # df$lon  = parse_number(df$lon)
  names(df)[names(df) == "long"] = "lon"
  
  df$lat  = dms_to_decimal(df$lat)
  df$lon  = dms_to_decimal(df$lon)
  
  # names(df)
  
  latLong = geocode_zip(df$zip_codes)
  
  names(latLong)[1] = "zip_codes"
  names(latLong)[2] = "latitude"
  names(latLong)[3] = "longitude"
  
  latLong$zip_codes = as.numeric(latLong$zip_codes)
  
  unique(df$zip_codes)
  
  
  dealers = df %>% left_join(latLong, by = "zip_codes")
  
  dealers$lat[is.na(dealers$lat)] = dealers$latitude[is.na(dealers$lat)]
  dealers$lon[is.na(dealers$lon)] = dealers$longitude[is.na(dealers$lon)]
  
  
  zipCodeMissingLatLong = dealers$zip_codes[is.na(dealers$lat)] %>% unique()
  
  zipCodeMissingLatLong
  
  dealers = dealers[!is.na(dealers$lat),]
  
  dealers$lat = add_random_offset(dealers$lat)
  dealers$lon = add_random_offset(dealers$lon)
  
  dealers$rehan[is.na(dealers$rehan)] = "Not Defined"
  dealers$embossers[is.na(dealers$embossers)] = "Not Defined"
  dealers$clover[is.na(dealers$clover)] = "Not Defined"
  dealers$eurobraille[is.na(dealers$eurobraille)] = "Not Defined"
  
  
  dealers$store_type[is.na(dealers$store_type)] = "Not Defined"
  
  dealers$store_type_col = NA
  dealers$store_type_col[dealers$store_type == "Not Defined"] = "black"
  dealers$store_type_col[dealers$store_type == "store"] = "blue"
  dealers$store_type_col[dealers$store_type == "satellite"] = "red"
  
  dealers$store_type_radius= NA
  dealers$store_type_radius[dealers$store_type == "Not Defined"] = 5
  dealers$store_type_radius[dealers$store_type == "store"] = 7.5
  dealers$store_type_radius[dealers$store_type == "satellite"] = 10
  
  dealers
  
}
# df = read_excel("Dealer Contacts (2).xlsx", sheet = "Dealer Territories", skip = 0)
# df = read_excel("Dealer Contacts.xlsx", sheet = "Dealer Territories", skip = 0)
# df = getCleanData(df)


getwd()

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  # titlePanel("15-Minute City Metro Vancouver"),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput('myMap',  width = "100%", height = "100%"),
  
  
  # ),
  # column(width = 3,  
  absolutePanel(top = 10, left = 10,
                
            
                bsCollapse(id = "filePanel",
                           open = "Data Upload",
                           bsCollapsePanel(
                             title = "Data Upload",style = "primary",
                             wellPanel(
                              style = "width:800px; max-height: 600px; background: #FFFFFF",
                               fileInput("file1", "Choose Exel File",
                                         multiple = FALSE,
                                         accept = c(".xlsx"))
                             )
                           ),
                           bsCollapsePanel(
                             title = "Filters",style = "primary",
                             
                             pickerInput(
                               inputId = "state",
                               label = "Select States",
                               choices = list(),
                               selected = list(),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE
                               )
                             ), 
                             pickerInput(
                               inputId = "store_type",
                               label = "Select Store Type",
                               choices = list(),
                               selected = list(),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE
                               )
                             ),
                             pickerInput(
                               inputId = "product",
                               label = "Select Product",
                               choices = c("Rehan",	"Eurobraille",	"Embossers",	"Clover"),
                               selected = list(),
                               multiple = F
                             ),
                             pickerInput(
                               inputId = "status",
                               label = "Select Status",
                               choices = list(),
                               selected = list(),
                               multiple = TRUE,
                               options = list(
                                 `actions-box` = TRUE
                               )
                             )
                  
                           )
                )
  ),
  absolutePanel(top = 10, right = 10,
                
                bsCollapse(id = "tableContent",
                           open = "Table Content",
                           bsCollapsePanel(
                             title = "Selected Dealers",style = "primary",
                             uiOutput("region"),
                             wellPanel(style = "overflow-y:scroll; max-height: 600px; background: #BDBDBD",
                                       tableOutput("table"),
                                       
                             )
                           )
                ),
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  ##  Reactive table object
  RV = reactiveValues(
    df = NULL
  )
  
  observe({
    req(RV$df)
    df = RV$df
    
    updatePickerInput(session,"store_type",choices = unique(df$store_type),selected= unique(df$store_type))
    updatePickerInput(session,"state",choices = unique(df$state_1),selected= unique(df$state_1))
    
    
    if(input$product == "Rehan"){
      updatePickerInput(session,"status",choices = unique(df$rehan),selected= unique(df$rehan))
    } 
    if(input$product == "Eurobraille"){
      updatePickerInput(session,"status",choices = unique(df$eurobraille),selected= unique(df$eurobraille))
    } 
    if(input$product == "Embossers"){
      updatePickerInput(session,"status",choices = unique(df$embossers),selected= unique(df$embossers))
    } 
    if(input$product == "Clover"){
      updatePickerInput(session,"status",choices = unique(df$clover),selected= unique(df$clover))
    }
  
    
  })
  
  observe({
    req(input$file1)

    tryCatch(
      {
        
        # df = getCleanData(read_excel("Dealer Contacts.xlsx", sheet = "Dealer Territories", skip = 0))
        df = getCleanData(read_excel(input$file1$datapath, sheet = "Dealer Territories", skip = 0))
        RV$df = df

      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    
  })
  
  ##  Map
  observeEvent(c(input$state,input$product,input$store_type,input$status),{
    
    
    
    req(input$status)
    req(RV$df)
    df = RV$df
    
    mapDf = df %>%
      filter(state_1 %in% input$state) %>%
      filter(store_type %in% input$store_type) %>%
      filter(get(tolower(input$product)) %in% input$status)
    
    names(mapDf)
    
    Line00 = paste0("<b>Dealer</b>: ", mapDf$dealer)
    Line0 = paste0("<b>State</b>: ", mapDf$state_1)
    Line1 = paste0("<b>Store Type</b>: ", mapDf$store_type)
    Line2 = paste0("<b>Phone</b>: ", mapDf$phone)
    Line3 = paste0("<b>Email</b>: ", mapDf$email)
    Line4 = paste0("<b>Website</b>: ", mapDf$website)
    
    
    
    Line5 = ifelse(is.na(mapDf$rehan), 
                   paste0("<b><font color='red'>Rehan</font></b>: NA"), 
                   paste0("<b><font color='green'>Rehan</font></b>: ", mapDf$rehan))
    
    Line6 = ifelse(is.na(mapDf$embossers), 
                   paste0("<b><font color='red'>Embossers</font></b>: NA"), 
                   paste0("<b><font color='green'>Embossers</font></b>: ", mapDf$embossers))
    
    Line7 = ifelse(is.na(mapDf$clover), 
                   paste0("<b><font color='red'>Clovers</font></b>: NA"), 
                   paste0("<b><font color='green'>Clovers</font></b>: ", mapDf$clover))
    
    Line8 = ifelse(is.na(mapDf$eurobraille), 
                   paste0("<b><font color='red'>Eurobraille</font></b>: NA"), 
                   paste0("<b><font color='green'>Eurobraille</font></b>: ", mapDf$eurobraille))
    
    Line9 = ifelse(is.na(mapDf$notes), 
                   paste0("<b><font color='red'>Notes</font></b>: NA"), 
                   paste0("<b><font color='blue'>Notes</font></b>: ", mapDf$notes))
    
    
    pointLabels <- paste(sep = "<br/>",
                         Line00,
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
    

    
    output$myMap <- renderLeaflet({
      
     
      
      
      leaflet(mapDf,options = leafletOptions(zoomControl = FALSE)) %>%
        addTiles() %>%  
        addMarkers(lng=~lon, lat=~lat,popup= pointLabels,label= pointLabels)%>%
        addCircleMarkers(lng=~lon, lat=~lat,popup= pointLabels,
                         color = ~store_type_col,
                         label= pointLabels,
                         fill = F,
                         opacity = 1,
                         weight = 2,
                         radius = ~store_type_radius)
      
    }) 
    
    
    output$table = renderTable({
      mapDf = mapDf %>% select(state_1,dealer,phone,email,website,store_type,(tolower(input$product)), notes)
    })
    
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

