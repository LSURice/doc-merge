library(shiny)
library(readxl)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Planting Information Merger"),
    fluidRow(
        column(8,
               fileInput("porder", "Upload planting order file", multiple=F, 
			 accept=c("csv", "xls", "xlsx")),
	       fileInput("horder", "Upload harvest order file", multiple=F, 
			 accept=c("csv", "xls", "xlsx")),
               fileInput("entry", "Upload entry file", multiple=F, 
			 accept=c("csv", "xls", "xlsx")),
               fileInput("layout", "Upload layout file", multiple=F, 
			 accept=c("csv", "xls", "xlsx")),
               fileInput("info", "Upload accession info file(s)", multiple=T, 
			 accept=c("csv", "xls", "xlsx")),
               downloadButton("download")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    data <- reactive({
	   # Function to handle excel or csv file import 
	   import_file <- function(fPath){
              if(grepl(".xls", fPath)){
	         f <- read_excel(fPath)
	         return(f)
	       }
	       f <- read.csv(fPath, StringsAsFactors=F)
	       return(f)
           }

           req(input$porder)
           # Read in plant order file and add plant order number
	   plant <- import_file(input$porder$datapath) 
	   plant$Planting_order <- 1:nrow(plant)

	   # Read in harvest ofrder file and add harvest order number
	   harvest <- import_file(input$horder$datapath) 
	   harvest$Harvest_order <- 1:nrow(harvest)

	   # Merge plant and harvest order by "Plot_name"
	   new <- merge(plant, harvest[, c("Plot_Name", "Harvest_order")], by="Plot_Name", 
	                 sort=F, all.x=T) 
	   
	   # Read in entry file and merge with previous by accession name
	   ent <- import_file(input$entry$datapath)
           new <- merge(new, ent,by.x="Accession_Name",  by.y="Accession", 
	                 sort=F, all.x=T) 
	   
            # Read in layout file and merge with previous by plot name
           lay <- import_file(input$layout$datapath)
	   new <- merge(new, lay, by.x="Plot_Name", by.y="plot_name",
			sort=F, all.x=T)
           
	   # Read in one or more accession info files and merge with previous by accession name
           info <- list()
           for(f in input$info$datapath){
            info[[length(info) + +1]] <- import_file(f) 
           }  
	   info <- do.call(rbind, info)
	   new <- merge(new, info, by="accession_name",
	                sort=F, all.x=T)

	   # Sort combined data by planting order
	   new <- new[order(new$Planting_order),]

	   # Subset columns
	   new[, c("Planting_order", "Harvest_order", "Plot_Name", "Plot_Number", 
		   "accession_name", "Entry.Number", "plot_id", 
		   "is_a_control", "rep_number", "row_number", "col_number", "block_number", 
		   "location_name","trial_name", "year", "purdy_pedigree",
		   "herbicide_class", "grain_class","grain_type", 
		   "population_type")]
	  })

    output$download <- downloadHandler(
        filename = function(){
            paste0(Sys.time(),"_","formatted.csv")
        },
        content = function(file){
            write.csv(data(), file, row.names=F)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
