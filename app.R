library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(DT)
library(gplots)




annotation_df <- read.csv("Annoteation.csv", header = TRUE)

# Define UI using fluidPage
ui <- navbarPage("B10 Cucumber Toolikit",
                 
                 tabPanel("App 1",
                          titlePanel('PlnatRegMap ID converter'),
                          sidebarLayout(
                            sidebarPanel(
                              textAreaInput("gene_input", "Paste Gene IDs Here:",
                                            placeholder = "e.g.\nCucsat.PASA.G13932\nCucsat.PASA.G13934\nCucsat.PASA.G1332\n",
                                            rows = 10, width = "100%"),
                              helpText("Paste your gene list (separated by newlines, commas, or spaces)."),
                              submitButton("Submit")

                            )
                            ,
                            mainPanel(
                              textOutput("greeting"),
                              DTOutput("contents")
                            ))
                          
                          
                          
                 ),
                 
                 tabPanel("App 2",
                          titlePanel('ENTREZ ID converter'),
                          sidebarLayout(
                            sidebarPanel(
                              textAreaInput("gene_input_app2", "Paste Gene IDs Here:",
                                            placeholder = "e.g.\nCucsat.PASA.G13932\nCucsat.PASA.G13934\nCucsat.PASA.G1332\n",
                                            rows = 10, width = "100%"),
                              helpText("Paste your gene list (separated by newlines, commas, or spaces)."),
                              submitButton("Submit")
                              
                            )
                            ,
                            mainPanel(
                              textOutput("greeting_app2"),
                              DTOutput("contents_app2")
                            ))
                          
                          
                 ),
                 
                 tabPanel("App 3",
                          titlePanel('Create a Heatmap'),
                          # App title ----
                         # titlePanel("Uploading Files"),
                          
                          # Sidebar layout with input and output definitions ----
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              
                              # Input: Select a file ----
                              fileInput("file1", "Choose CSV File",
                                        multiple = FALSE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),
                              helpText("Import the CSV file containing the count matrix. The first column should contain the gene names."),
                             # actionButton("show_help", "Help"),
                              # Horizontal line ----
                             tags$hr(),
                              
                              # Input: Checkbox if file has header ----
                              checkboxInput("header", "Header", TRUE),
                              
                              # Input: Select separator ----
                              radioButtons("sep", "Separator",
                                           choices = c(Comma = ",",
                                                       Semicolon = ";",
                                                       Tab = "\t"),
                                           selected = ","),
                              
                              # Input: Select quotes ----
                              radioButtons("quote", "Quote",
                                           choices = c(None = "",
                                                       "Double Quote" = '"',
                                                       "Single Quote" = "'"),
                                           selected = '"'),
                              
                              # Horizontal line ----
                              tags$hr(),
                              
                              # Input: Select number of rows to display ----
                              radioButtons("disp", "Display",
                                           choices = c(Head = "head",
                                                       All = "all"),
                                           selected = "head"),
                            
                              
                          #  ),
                            
                            # sidebarPanel(
                              checkboxInput("cluster_rows", "Cluster Rows", value = TRUE),
                              checkboxInput("cluster_cols", "Cluster Columns", value = TRUE),
                              selectInput("dendrogram", "Dendrogram to show",
                                          choices = c("both", "row", "column", "none"),
                                          selected = "both"),
                              selectInput("Distance_metric", "Distance metric",
                                      choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                                      selected = "euclidean"),
                              selectInput("Clustering_method", "Clutering method",
                                      choices = c("ward.D", "single", "complete", "average", "ward.D2", "mcquitty", "median", "centroid"),
                                      selected = "ward.D"),
                              textInput("heatmap_title", "Enter a heatmap title"),
                              textInput("Plot_width", "Enter a plot width", value = 800),
                              textInput("Plot_height", "Enter a plot height", value = 800),
                          
                              sliderInput("X_marigin", "X marigin", min = 1, max = 200, value = 8, step = 1),
                              sliderInput("Y_marigin", "Y marigin", min = 1, max = 200, value = 8, 1),
                          
                              sliderInput("Row_font_size", "Font size of rows:", min = 0, max = 2, value = 0.75, step = 0.1),
                              sliderInput("Column_font_size", "Font size of columns:", min = 0, max = 2, value = 0.75, 0.01),
                              downloadButton("downloadPNG", "Download PNG"),
                              downloadButton("downloadPDF", "Download PDF"),
                              downloadButton("downloadTIFF", "Download TIFF"),
                              submitButton("Submit")
                             ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              
                              # Output: Data file ----
                              tableOutput("contents_app3"),
                              plotOutput("heatmapPlot")
                            ))
                          
                          
                       
                 ),
                 
                          
                          
                 
                 
                 tabPanel("App 4",
                          titlePanel('Get protein sequences'),
                          sidebarLayout(
                            sidebarPanel(
                              textAreaInput("gene_input_app4", "Paste Gene IDs Here:",
                                            placeholder = "e.g.\nCucsat.PASA.G13932\nCucsat.PASA.G13934\nCucsat.PASA.G1332\n",
                                            rows = 10, width = "100%"),
                              helpText("Paste your gene list (separated by newlines, commas, or spaces).  IMPORTANT: Please upload only gene_ids which code proteins"),
                              helpText("IMPORTANT: Please upload only gene_ids which code proteins"),
                              
                              #  downloadButton("downloadFASTA", "Download FASTA file"),
                             
                              submitButton("Submit"),
                              tags$hr(),
                              downloadButton("downloadFASTA", "Download FASTA file")
                              
                            )
                            ,
                            mainPanel(
                              
                              tableOutput("proteins_app4")
                            ))
                 ),
                 
                 tabPanel("App 5",
                          fluidPage(
                            titlePanel("Interactive PCA Plot with Group Coloring"),
                            sidebarLayout(
                              sidebarPanel(
                                fileInput("file", "Upload CSV file (samples as rows, variables as columns)",
                                          accept = c(".csv")),
                                checkboxInput("center", "Center data", TRUE),
                                checkboxInput("scale", "Scale data", TRUE),
                                uiOutput("group_select_ui"),   # UI to select grouping column
                                uiOutput("pc_select_ui")
                              ),
                              
                              mainPanel(
                                plotlyOutput("pca_plot"),
                                tableOutput("pca_summary")
                              )
                            
                            )
                          )
                 ),
                 
                 tabPanel("App 6",
                          fluidPage(
                            titlePanel("App 2"),
                            sidebarLayout(
                              sidebarPanel(
                                textInput("text", "Enter text:")
                              ),
                              mainPanel(
                                textOutput("result4")
                              )
                            )
                          )
                 ),
                 
                 tabPanel("App 7",
                          fluidPage(
                            titlePanel("App 2"),
                            sidebarLayout(
                              sidebarPanel(
                                textInput("text", "Enter text:")
                              ),
                              mainPanel(
                                textOutput("result4")
                              )
                            )
                          )
                 )
)













# Define server logic
server <- function(input, output, session) {
  
  # App 1 logic
  
  filtered_data <- reactive({
    req(input$gene_input)
    genes <- unlist(strsplit(input$gene_input, "[,\n ]+"))
    genes <- genes[genes != ""]
    print(genes)
    annotation_df_app1 <- annotation_df %>% filter(Gene.ID %in% genes)
    annotation_df_app1 <- annotation_df_app1[,c("Gene.ID", "PlantRegMap_ID")]
    return(annotation_df_app1)
  })
  
  output$contents <- renderDT({
    datatable(rownames = FALSE,
              filtered_data(),
              extensions = 'Buttons',
              options = list(
                pageLength = -1,
                dom = 'Bfrtip',         # Show buttons, filter, table
                buttons = list(
                  'copy', 'csv',              # Copy visible columns to clipboard
                  list(
                    extend = 'colvis',  # Column visibility button
                    columns = 0:(ncol(filtered_data())-1)  # Allow all columns to be toggled
                  )
                ),
                
                scrollX = TRUE, scrollY = "300px"       # Enable horizontal scrolling if needed
              ),
              
              selection = 'none'
    )
  })
  
  
  
 
  
  # App 2 logic
  filtered_data_app2 <- reactive({
    req(input$gene_input_app2)
    genes <- unlist(strsplit(input$gene_input_app2, "[,\n ]+"))
    genes <- genes[genes != ""]
    print(genes)
    annotation_df_app2 <- annotation_df %>% filter(Gene.ID %in% genes)
    annotation_df_app2 <- annotation_df_app2[,c("Gene.ID", "X9930_ENTREZ_ID")]
    return(annotation_df_app2)
  })
  
  output$contents_app2 <- renderDT({
    datatable(rownames = FALSE,
              filtered_data_app2(),
              extensions = 'Buttons',
              options = list(
                pageLength = -1,
                dom = 'Bfrtip',         # Show buttons, filter, table
                buttons = list(
                  'copy', 'csv',              # Copy visible columns to clipboard
                  list(
                    extend = 'colvis',  # Column visibility button
                    columns = 0:(ncol(filtered_data_app2())-1)  # Allow all columns to be toggled
                  )
                ),
                
                scrollX = TRUE, scrollY = "300px"       # Enable horizontal scrolling if needed
              ),
              
              selection = 'none'
    )
  })
  
  
  
  # App 3 logic
 
  observeEvent(input$show_help, {
    showModal(modalDialog(title = "Important message",
                          "This is an important message!"))
  })
  
  output$contents_app3 <- renderTable({
    req(input$file1)
    
  tryCatch(
    {
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
      
      },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  
  if(input$disp == "head") {
    return(head(df))
  }
  else {
    return(df)
  }
  })
  
   
  
    draw_heatmap <- function() {
        
        req(input$file1)
   
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        
        df_rounded <- data.frame(lapply(df, function(x) if(is.numeric(x)) round(x) else x))
        df <- df_rounded
        df_clean <- na.omit(df)
        df <- df_clean
    
        mat_data <- data.matrix(df[,2:ncol(df)])
        rnames <- df[,1]
        rownames(mat_data) <- rnames
       # mat_data <- scale(mat_data)
        # Compute distance matrix with Manhattan distance (change method as needed)
        dist_rows <- dist(mat_data, method = input$Distance_metric)
        dist_cols <- dist(t(mat_data), method = input$Distance_metric)
        
        # Perform hierarchical clustering with ward.D linkage
        hc_rows <- hclust(dist_rows, method = input$Clustering_method)
        hc_cols <- hclust(dist_cols, method = input$Clustering_method)
        
        
        #my_hclust <- function(x) hclust(x, method = input$Clustering_method)
        col_breaks = c(seq(-1,1,length=100),  # for red
                       seq(1.01,1.9,length=100),           # for yellow
                       seq(2,3,length=100))  
    
    
      my_palette <- colorRampPalette(c("red", "green", "black"))(n = 1000)  
       
     # par(mar = c(10, 2, 3, 210)) 
      heatmap.2(
      x = mat_data,
      Rowv = if (input$cluster_rows) as.dendrogram(hc_rows) else FALSE,
      Colv = if (input$cluster_cols)  as.dendrogram(hc_cols) else FALSE,
      dendrogram = input$dendrogram,
      trace = "none",
      scale="row",
      cexCol = input$Column_font_size,cexRow = input$Row_font_size,
      col = colorRampPalette(c("#28f732", "black", "#fa0000"))(n = 1000),
      margins = c(input$X_marigin, input$Y_marigin),
      main = input$heatmap_title,
      key = TRUE,
      breaks = seq(-4, 4, length.out = length(my_palette) + 1),
      #lhei = c(9, 10),lwid = c(9, 10)
      #hclustfun = my_hclust
    )
    }
    
    
    output$heatmapPlot <- renderPlot({
      draw_heatmap()
      
  })
    
   
  output$downloadPNG <- downloadHandler(
    filename = function() {
      paste0("heatmap_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, as.numeric(input$Plot_width), as.numeric(input$Plot_height))
      draw_heatmap()
      dev.off()
    }
  )
  
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste0("heatmap_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      pdf(file)
      draw_heatmap()
      dev.off()
    }
  )
  output$downloadTIFF <- downloadHandler(

    filename = function() {
      paste0("heatmap_", Sys.Date(), ".tiff")
    },
    content = function(file) {
      tiff(file, as.numeric(input$Plot_width), as.numeric(input$Plot_height))
      draw_heatmap()
      dev.off()
    }
  )
  
  
  
  

  # App 4 logic
  dss2df <- function(dss) {
    data.frame(
      names = names(dss),
      seq = as.character(dss),
      width = width(dss)
    )
  }
  
 read_protein_ids <- function(){
 #   req(input$gene_input_app4)
   
    genes <- unlist(strsplit(input$gene_input_app4, "[,\n ]+"))
    genes <- genes[genes != ""]
   # print(genes)
    return(genes)
 }

 
 #getData_app4 <- reactive({
 #  req(input$gene_input_app4)
 #  fasta <- input$funParameter
   #corrStartDate <- input$StartDate
   #corrEndDate <- input$EndDate
   
#   return(someData(corrStartDate, corrEndDate, funParameter))
 #}) 
 
proteins_seq_app4 <- function(genes, fasta){
    fasta <- readDNAStringSet("proteome_only_longest.fasta")
    #req()

    #asta <- readDNAStringSet("proteome_only_longest.fasta")
    df_sequence <- dss2df(fasta)
    #filtered_fasta <- df_sequence[df_sequence$names %in% genes]
    filtered_fasta <-  df_sequence %>% filter(names %in% genes)
    print(head(fasta))
    #print(head(fasta))
    #annotation_df_app2 <- annotation_df %>% filter(Gene.ID %in% genes)
   # annotation_df_app2 <- annotation_df_app2[,c("Gene.ID", "X9930_ENTREZ_ID")]
    return(filtered_fasta)
  }
  

  output$proteins_app4 <- renderTable({
    req(input$gene_input_app4)
    library(Biostrings)
    fasta <- readDNAStringSet("proteome_only_longest.fasta")
    protein_ids <- read_protein_ids()
    proteins_seq_app4(protein_ids, fasta)
   # 
    #filtered_fasta <- proteins_seq_app4()
   # reactive(proteins_seq_app4(protein_ids))
  #  df <- dss2df(filtered_fasta)
  #  df
    #typeof(seq(filtered_fasta))      
   # paste("App 2 says:", as.vector(seq(filtered_fasta)))
    
  })
  
  output$downloadFASTA <- downloadHandler(
    filename = function() {
      paste0("Proteins_", Sys.Date(), ".fasta")
    },
    content = function(file) {
      protein_ids <- read_protein_ids()
      fasta <- readDNAStringSet("proteome_only_longest.fasta")
      print(head(protein_ids))
      print(head(names(fasta)))
      filtered_fasta <- fasta[protein_ids]
      writeXStringSet(filtered_fasta, file,  format = "fasta")
      
     
    }
  )
  
  
  
}

# Run the app
shinyApp(ui, server)
