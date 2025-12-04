#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(XML)
library(tidyverse)
library(shinylive)
library(httpuv)

# Define UI for application that draws a histogram
ui <- fluidPage(
	
	# Application title
	titlePanel("EAD3 Validator"),
	fileInput("XML", "Upload EAD3 XML file"), 
	tableOutput("Validation")
) 

# Define server logic required to draw a histogram
server <- function(input, output) {
	validation <-  reactive({
		validate(
			need(input$XML, message = "Please upload an XML document.")
		)
		
		validation_out <- xmlSchemaValidate(schema = xmlSchemaParse("ead3_undeprecated.xsd"),
						  doc = xmlParse(input$XML$datapath))$errors %>% 
		cbind() %>% 
		as_data_frame()
		
		validation_table <- as_tibble(matrix(ncol=2, nrow=1000), name_repair="unique")
		colnames(validation_table) <- c("message", "line")
		
		for(i in 1:nrow(validation_out)){
			validation_table$message[i] <- as.character(validation_out$.[[i]][1])
			validation_table$line[i] <- as.character(validation_out$.[[i]][4])
		}
		validation_table %>% filter(is.na(message) == F)
	})
	
	output$Validation <- renderTable(validation())
	
	# ead3_xsl <- read_xml("EAD3toHTML.xsl")
	# 
	# target <- read_xml(input$XML)
	# 
	# output <- xslt::xml_xslt(doc=target, stylesheet=ead_xsl)
	# 
	# cat(as.character(output))
	# 
	# write_html(output, "html_test.html")
	
}

# Run the application 
shinyApp(ui = ui, server = server)
