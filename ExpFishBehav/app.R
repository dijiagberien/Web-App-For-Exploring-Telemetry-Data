# App for analysis of acoustic telemetry data to explore fish bahaviour
# Author: Adogbeji Agberien
# Data: March 15, 2021

# App for Exploration of aquatic fish behavior

# Load libraries
library(factoextra)
library(FactoMineR)
library(lubridate)
library(tidyverse)
library(data.table)
library(leaflet)
library(leaflet.extras)
library(shinyRGL)
library(shinythemes)
library(bslib)

options(shiny.maxRequestSize = 5*1024^2)

ui <- fluidPage(
    
    # Select theme
    theme = bs_theme(bootswatch = "lux"),
    
    titlePanel(title = "Exploration of fish activity"), 
    
    # Interface design...with sidebar and main panel 
    sidebarLayout(
        
        # Add sidebar 
        sidebarPanel(
            fileInput(inputId = "detection.file", label = "Upload detection data"), 
            hr(), 
            helpText("CSV FILE ONLY!", 
                     "Please summarize data by daily detections prior to upload...it reduces the size
                     of the data set and speeds up analysis."),
            hr(), 
            helpText("Currently only able to analyze data containing the following columns: 
               \"animal ID\", \"date\", \"receiver group\",
               \"longitude\", \"latitude\", \"time spent (secs)\""),
            hr(),
            h4("UPDATES COMING IN DUE TIME!"), 
            hr(), 
            tags$li(class = "dropdown", 
                    tags$a(href = "https://github.com/dijiagberien/ExpFishBehavApp",
                           icon("github", "Source code", target = "_blank"))), 
            tags$li(class = "dropdown", 
                    tags$a(href = "https://adogbejiagberien.netlify.app/",
                           icon("globe", "Website", target = "_blank")))
        ), 
        
        # Add main panel ----
        mainPanel(tabsetPanel(
            
            # Tab design ----
            type = "tab", 
            
            # Map and basic summary tab ----
            tabPanel("Map and basic summary", 
                     hr(), 
                     helpText("Hover over icon to show pertaining info."), 
                     helpText("Toggle icon at top right of map to hide receiver labels"),
                     leafletOutput("data.map", height = 400),
                     hr(),
                     tableOutput("data.summary")), 
            
            # High level summary based on fish residency ----
            tabPanel("Fish: PCA-3D plot and high level summary",
                     tabsetPanel(
                         type = "tab",
                         tabPanel("PCA", hr(), plotOutput("fishPCA", height = 700), hr()),
                         tabPanel("Summary", DT::dataTableOutput("fish.summary"))
                     )
                     ), 
            
            # High level summary based on receiver detections ----
            tabPanel("Receivers: PCA-2D plot and high level summary", 
                     tabsetPanel(
                         type = "tab", 
                         tabPanel("PCA", hr(), plotOutput("receiverPCA", height = 700), hr()),
                         tabPanel("Summary", DT::dataTableOutput("receiver.summary"))
                     ))
            
        ))
    )
)


server <- function(input, output){
    
    # Import the detection data
    fish.detection.data <- reactive({
        fish.detection.file <- input$detection.file
        if (is.null(fish.detection.file)) {
            return()
        }
        fread(fish.detection.file$datapath)
    })
    
    # Plot map of receivers and add metadata to receiver groups
    output$data.map <- renderLeaflet({
        
        if (is.null(fish.detection.data())) {
            return()
        }
        
        receiver.data <- fish.detection.data()[
            , by = .(`receiver group`), 
            .(`longitude` = mean(longitude), 
              `latitude` = mean(latitude), 
              `total time spent` = sum(`time spent (secs)`), 
              `fish count` = uniqueN(`animal ID`), 
              `earliest detection date` = min(date), 
              `latest detection date` = max(date),
              `possible no. of days present` = difftime(max(date), min(date)),
              `no. of. days with detections` = uniqueN(date)
            )] %>% 
            .[, c("percent time spent") := (`total time spent`/sum(`total time spent`)) * 100] %>% 
            .[, !c("total time spent")]
        
        
        receiver.data$label <- paste(
            "<p>", "Receiver group: ", receiver.data$`receiver group`, "</p>", 
            "<p>", "Fish count: ", receiver.data$`fish count`, "</p>", 
            "<p>", "Percent use by tagged fish: ", round(receiver.data$`percent time spent`, 2), "</p>", 
            "<p>", "Possible no. of days present: ", receiver.data$`possible no. of days present`, "</p>",
            "<p>", "No. of days with detections: ", receiver.data$`no. of. days with detections`, "</p>", 
            "<p>", "Earliest detection date: ", receiver.data$`earliest detection date`, "</p>", 
            "<p>", "Latest detection date: ", receiver.data$`latest detection date`, "</p>")
        
        leaflet() %>% 
            addTiles() %>% 
            setView(lng = mean(receiver.data$longitude),
                    lat = mean(receiver.data$latitude), 
                    zoom = 14) %>% 
            addMarkers(lng =receiver.data$longitude,
                       lat = receiver.data$latitude, 
                       label = lapply(receiver.data$label, HTML), 
                       labelOptions = labelOptions(noHide = F),
                       group = "Detailed info.") %>%
            addCircleMarkers(lng = receiver.data$longitude, 
                             lat = receiver.data$latitude, 
                             label = receiver.data$`receiver group`,
                             labelOptions = labelOptions(noHide = T),
                             group = "Receiver groups", 
            ) %>%
            addLayersControl(overlayGroups = c("Detailed info.", "Receiver groups")) 
    })
    
    # Summary table of activity at the study location
    output$data.summary <- renderTable({
        if (is.null(fish.detection.data())) {
            return()
        }
        
        detectionBriefSummary <- fish.detection.data()[
            , by = .(`receiver group`, year(date)), 
            .(`No. of detected fish` = uniqueN(`animal ID`), 
              `No. of receiver groups` = uniqueN(`receiver group`), 
              `Total time spent` = sum(`time spent (secs)`))] %>% 
            .[order(year, -`Total time spent`)] 
        
        leastUtilizedLocations <- detectionBriefSummary %>% 
            group_by(year) %>% 
            slice(tail(row_number(), 3)) %>% 
            select(`receiver group`) %>% 
            data.table() %>% 
            .[, by = .(`year`), .(`Least utilized receiver groups` = paste(`receiver group`, collapse = ", "))]
        
        mostUtilizedLocations <- detectionBriefSummary %>% 
            group_by(year) %>% 
            slice(1:3) %>% 
            select(`receiver group`) %>% 
            data.table() %>% 
            .[, by = .(`year`), .(`Most utilized receiver groups` = paste(`receiver group`, collapse = ", "))]
        
        detectionBriefSummary <- detectionBriefSummary[
            , by = .(year), 
            .(`No. of detected fish` = sum(`No. of detected fish`), 
              `No. of receiver groups` = sum(`No. of receiver groups`))] 
        
        detectionBriefSummary <- merge(detectionBriefSummary, mostUtilizedLocations, by = "year")
        
        detectionBriefSummary <- merge(detectionBriefSummary, leastUtilizedLocations, by = "year")
        
        detectionBriefSummary
    })
    
    # Clustering detected fish by the time spent at the different receiver groups ----
    
    # Convert detection data to wide format animal ID ~ receiver group 
    fish.receiver.wide <- reactive({
        if (is.null(fish.detection.data())) {
            return()
        }
        fish.detection.data() %>% 
            dcast(`animal ID` ~ `receiver group`,
                  value.var = "time spent (secs)",
                  fun = mean, fill = 0) %>% 
            as.matrix(rownames = "animal ID") %>% t() %>% scale() %>% t() %>% data.frame()
    })
    
    # Perform hierarchical clustering 
    fish.groups <- reactive({
        if (is.null(fish.receiver.wide())) {
            return()
        }
        fish.groups.hclust <- HCPC(fish.receiver.wide(), nb.clust = 0, graph = F)$data.clust
        
        data.table(`fish cluster` = fish.groups.hclust$clust, 
                   `animal ID` = rownames(fish.groups.hclust))
        
    })
    
    # Determine fish groups and return a 3-D PCA plot of fish ~ receiver ----
    # Map not used because it may take time to render all points if data set is large
    output$fishPCA <- renderPlot({
        if (is.null(fish.receiver.wide())) {
            return()
        }
        
        fish.pca <- PCA(
            (fish.receiver.wide()), scale.unit = F, graph = F, ncp = length(fish.receiver.wide()))
        
        fviz_pca_biplot(fish.pca, geom = "point", geom.var = c("text"),
                        col.ind = fish.groups()$`fish cluster`, repel = T, 
                        col.var = "black", )


    })
    
    # Convert to side format receiver ~ month, calculate  moving. averages, and cluster receivers ----
    receiver.month.wide <- reactive({
        if (is.null(fish.detection.data())) {
            return()
        }
        
        # Convert to wide format receiver group ~ month
        receiver.month.wide <- fish.detection.data() %>% 
            dcast(`receiver group` ~ lubridate::month(date, label = T),
                  value.var = "time spent (secs)", 
                  fun = sum, 
                  fill = 0) %>% 
            as.matrix(rownames = "receiver group") %>% t() 
        
        # Calculate moving average
        ma <- function(x, n = 3){stats::filter(x, rep(1 / n, n), sides = 2, circular = T)}
        
        receiver.month.wide[1:12, ] <- ma(receiver.month.wide[1:12, ], )
        
        receiver.month.wide <- receiver.month.wide %>% scale() %>% t()
    })
    
    # Table of receiver groups and their cluster
    receiver.groups <- reactive({
        if (is.null(receiver.month.wide())) {
            return()
        }
        receiver.groups.hclust <- HCPC(data.frame(receiver.month.wide()), nb.clust = 0, graph = F)$data.clust
        
        data.table(`receiver cluster` = receiver.groups.hclust$clust, 
                   `receiver group` = rownames(receiver.groups.hclust))
    })
    
    # PCA plot of receiver group ~ month
    output$receiverPCA <- renderPlot({
        if (is.null(receiver.month.wide())) {
            return()
        }
        
        receiver.pca <- PCA(
            (receiver.month.wide()), scale.unit = F, graph = T, ncp = length(receiver.month.wide()))
        
        fviz_pca_biplot(receiver.pca, geom = "text",
                        col.ind = receiver.groups()$`receiver cluster`,
                        col.var = "black",
                        repel = T,
                        legend.title = "Receiver cluster", 
                        ggtheme = theme_classic(base_size = 12,
                                                base_family = "Times New Roman")) 
        
    })
    
    detection.table.with.groups <- reactive({
        if (is.null(fish.detection.data())) {
            return()
        }
        fish.detection.data <- merge(fish.detection.data(), fish.groups(), by = "animal ID") 
        fish.detection.data <- merge(fish.detection.data, receiver.groups(), by = "receiver group")
    })
    
    output$fish.summary <- DT::renderDataTable({
        if (is.null(detection.table.with.groups())) {
            return()
        }
        
        detection.table.with.groups()[
            , 
            by = .(`fish cluster`, `animal ID`), 
            .(`no of days present` = uniqueN(date), 
              `location count` = uniqueN(`receiver group`),
              `first detection` = as.character(min(date)), 
              `last detection` = as.character(max(date)), 
              `% days present` = round(
                  100*(uniqueN(date)/as.numeric(difftime(max(date), min(date)) + 1)), 2),
              `locations` = paste(unique(`receiver group`), collapse = ", "))] %>% 
            .[order(`animal ID`)]
    })
    
    
        output$receiver.summary <- DT::renderDataTable({
        if (is.null(detection.table.with.groups())) {
            return()
        }
        detection.table.with.groups()[, 
                                      by = .(`receiver cluster`, `receiver group`), 
                                      .(`fish count` = uniqueN(`animal ID`), 
                                        `detection count` = sum(`detection count`),
                                        `First detection` = as.character(min(date)), 
                                        `Last detection` = as.character(max(date)), 
                                        `date range` = difftime(max(date), min(date)),
                                        `Percentage use` = round(100 * (uniqueN(date)/
                                                                            as.numeric(difftime(max(date), min(date)))), 2))
        ] %>% 
            .[order(-`Percentage use`)]
    })
}

shinyApp(ui = ui, server = server)
