
library(shiny)
library(shinythemes)
library(dplyr)
library(leaflet)


# Read the file
ship_table1 <- read.csv("ships.csv")

# Remove the observations for parked ships
ship_table <- ship_table1 %>% filter(is_parked == 0)


# Function to calculate distance in meters
# https://stackoverflow.com/questions/639695/how-to-convert-latitude-or-longitude-to-meters
measure <- function(lon1,lat1,lon2,lat2) {

    R <- 6378.137                                # radius of earth in Km
    dLat <- (abs(lat2-lat1))*pi/180
    dLon <- (abs(lon2-lon1))*pi/180
    a <- sin((dLat/2))^2 + cos(lat1*pi/180)*cos(lat2*pi/180)*(sin(dLon/2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1-a))
    d <- R * c
    return (d * 1000)                            # distance in meters
}


# Define UI for application 
ui <- fluidPage(
	
	theme = shinytheme("cerulean"),
    titlePanel("Ship Information Resource"),
	sidebarLayout(
		sidebarPanel(
			h4("The time is ",
        			textOutput("currentTime", container = span)),
			br(),
			# first dropdown
			selectInput("type", label = h3("Select Vessel Type:"), 
   			 choices = unique(ship_table$ship_type), 
   			 selected = unique(ship_table$ship_type)[1]),
			
			# second dropdown
			selectInput("name", label = h3("Choose Ship:"), 
   			 choices = "KAROLI"
					),

			br(),
			br(),
			br(),
			br(),
			h5("WebApp designed by"),
			tags$a(href="https://orcid.org/0000-0001-5686-640X", "Arun Arumugaperumal"),
			br(),
			br(),
			br(),
			br()
  			   
				),
		mainPanel(
						textOutput("text", container = span),		
						leaflet::leafletOutput("map")
			
			    )#close mainPanel
				)#close sidebarLayout
			)#close fluidpage

    

# Define server 
server <- function(input, output, session) {

	 observeEvent(input$type, {
    			updateSelectInput(inputId = "name", choices = ship_table %>% 
				filter(ship_type == input$type)%>%
				summarize(names  = unique(SHIPNAME)))

				})

	 observeEvent(input$name, {

			# Subset the dataframe
				df <- ship_table[ship_table$SHIPNAME == input$name,]	
#Calculate distances
ms <- dim(df)[1] -1
mh <- vector("numeric")
for(i in 1:ms){
mh[i] <- measure(df$LON[i], df$LAT[i], df$LON[i+1], df$LAT[i+1])
mh <- c(mh, mh[i])
		    }
mh_max <- max(mh)

# Update the distance travelled in meters
			output$text <- renderText({
				paste("The distance sailed by",input$name, "is", mh_max, "meters")
						})


# Select the maximum travelled locations
mh_max_index <- which(mh == mh_max)

# Create the leaflet map
output$map <- leaflet::renderLeaflet({
leaflet() %>% 
    addTiles() %>% 
    addMarkers(lat = c(df$LAT[mh_max_index:(mh_max_index+1)]), lng = c(df$LON[mh_max_index:(mh_max_index+1)]), popup=df$DATETIME[mh_max_index:(mh_max_index+1)]) %>%
    addPolylines(lat = c(df$LAT[mh_max_index:(mh_max_index+1)]), lng = c(df$LON[mh_max_index:(mh_max_index+1)]))
})
	
				})

# Create timer that updates each second				
	output$currentTime <- renderText({
		invalidateLater(1, session)
		format(Sys.time())
  				})
}

# Run the application 
shinyApp(ui = ui, server = server)




