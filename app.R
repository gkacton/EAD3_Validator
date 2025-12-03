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
	table = reactive({
		xmlSchemaValidate(schema = xmlSchemaParse("ead3_undeprecated.xsd"),
						  doc = xmlParse(input$XML$datapath))$errors %>% 
			as.character() %>% 
			str_remove("list\\(msg = ") %>% 
			str_remove_all("\\\\n") %>% 
			str_remove_all("\\{http://ead3.archivists.org/schema/undeprecated/\\}") %>% 
			str_remove_all('\\"') %>% 
			str_remove_all("line = ") %>% 
			strsplit(split=",") %>% 
			tibble(.name_repair = "unique") %>% 
			bind_rows() %>% 
			unnest_wider(col = ".",
						 names_sep = "x") %>% 
			rename(c("msg" = ".x1", "code" = ".x2", "dom" = ".x3", "line" = ".x4", "col" = ".x5", "lev" = ".x6", "other" = ".x7")) %>% 
			as_data_frame() %>% 
			select(msg, line) %>% 
			rename("error" = "msg")
	})
	
	output$Validation <- renderTable({table()})
	
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
