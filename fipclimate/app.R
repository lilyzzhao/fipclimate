library(shiny)
library(plotly)
library(tidyverse)
library(readr)


################UI

ui <- fluidPage(
  # Application title
  titlePanel("Do FIP Assessments Evaluate Climate Resilience?"),
  
  HTML("<p>Fishery Improvement Projects (FIPs) enhance fishery performance by first assessing the environmental sustainability and sometimes the social responsibility of a fishery. These assessments are used to develop workplans that address related challenges. In the context of increasing climate risk, alongside the goals of environmental sustainability and social responsibility, climate resilience — the ability to cope, adapt, and transform in the face of climate impacts — is critical to fishery performance.</p>"),
  
  HTML("<p><strong>On the tabs below, explore the effectiveness of existing assessment tools for FIPs in capturing the climate resilience of a fishery system.</strong></p>"),
  
  HTML("<p>Here we explore two primary tools — the Environmental Rapid Assessment (ERA) for basic FIPs and the Social Responsibility Assessment (SRA) — and the extent to which they consider attributes of climate resilience as defined for fishery systems in Mason et al. 2022, separately and when used in combination.</p>"),
  
  HTML("<p>This interface allows users to see how indicators that promote environmental sustainability through the ERA and social responsibility through the SRA can also help build climate resilience in a fishery. It also identifies gaps (attributes of climate resilience not considered by indicators in either assessment). Assessing and promoting these attributes may foster resilient improvement projects and enhance fishery progress under increasing uncertainty.</p>"),
  
  # Tabs for different visualizations
  tabsetPanel(
    tabPanel(title = HTML("<strong>Overview of Climate Resilience Coverage</strong>"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("attDimensionFilter", "Select Attribute Dimension",
                             choices = c("All", 
                                         "Ecological Resilience of Stock, Habitats and Ecosystem" = "Ecological",
                                         "Governance Resilience of Fishery Operations & Management" = "Governance",
                                         "Socio-economic Resilience for Fishers and Fishing Community" = "Socio-economic"))
               ),
               mainPanel(
                 plotlyOutput("coveragePlot", height = "700px")
               )
             )
    ),
    tabPanel(title = HTML("<strong>Overview of Relevant Indicators</strong>"),
             # Original UI components for Sankey plot
             # Custom CSS for the legend
             tags$head(
               tags$style(HTML("
          .custom-legend .legend-item {
            display: inline-block;
            margin-right: 10px;
            margin-bottom: 5px;
            font-size: 14px; /* Font size */
          }
          .custom-legend .legend-color {
            display: inline-block;
            width: 20px;
            height: 20px;
            margin-right: 5px;
            border: 1px solid #000; /* Optional: adds a border around the color swatch */
          }
          .custom-legend .legend-text {
            vertical-align: middle;
          }
          .legend-header {
            font-weight: bold; /* Make legend headers bold */
          }
        "))
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("groupFilter", "Select an Assessment", 
                             choices = c("All", 
                                         "Environmental Rapid Assessment (ERA)" = "ERA", 
                                         "Social Responsibility Assessment (SRA)" = "SRA")),
                 # Custom HTML legend as before
                 div(class = "custom-legend",
                     # Header for the legend
                     div(class = "legend-header", "Fishery Assessment Tools"),
                     div(class = "legend-item", 
                         span(class = "legend-color", style = "background-color: #007a7c;"), 
                         span(class = "legend-text", "ERA")),
                     div(class = "legend-item", 
                         span(class = "legend-color", style = "background-color: #4169e1;"), 
                         span(class = "legend-text", "SRA")),
                     # Second header for the legend
                     div(class = "legend-header", "Attributes of Climate Resilience"),
                     div(class = "legend-item", 
                         span(class = "legend-color", style = "background-color: #2c9f51;"), 
                         span(class = "legend-text", "Ecological Resilience of Stock, Habitats and Ecosystem")),
                     div(class = "legend-item", 
                         span(class = "legend-color", style = "background-color: #Be213c;"), 
                         span(class = "legend-text", "Governance Resilience of Fishery Operations & Management")),
                     div(class = "legend-item", 
                         span(class = "legend-color", style = "background-color: #Cd9827;"), 
                         span(class = "legend-text", "Socio-economic Resilience for Fishers and Fishing Community")),
                     style = "padding: 10px 0;"
                 )
               ),
               mainPanel(
                 # Subheaders for "Indicators" and "Attributes of Climate Resilience"
                 div(style = "display: flex; justify-content: space-between;", 
                     div(style = "align-self: flex-start; font-weight: bold;", "Relevant Fishery Indicators"),
                     div(style = "align-self: flex-end; font-weight: bold;", "Attributes of Climate Resilience")
                 ),
                 plotlyOutput("sankeyPlot", height = "900px")
               )
             )
    )
  )
)



#####################SERVER note 2nd tab stuff is here before first tab but runs the same 

# for Sankey diagram tab (2nd tab):
server <- function(input, output, session) {
  # Read data for the Sankey plot
  links_df <- read_csv("p_links.csv", show_col_types = FALSE)
  nodes_df <- read_csv("p_nodes.csv", show_col_types = FALSE)
  
  # Define colors for groups
  group_colors <- c("Ecological" = "#2c9f51",
                    "Governance" = "#Be213c",
                    "Socio-economic" = "#Cd9827",
                    "ERA" = "#007a7c",
                    "SRA" = "#4169e1")
  
  # Pre-assign colors to all nodes based on group
  nodes_df$color <- group_colors[nodes_df$group]
  # Fill any NAs with a default color
  nodes_df$color[is.na(nodes_df$color)] <- "#cccccc"
  
  # Reactive expression for filtered data for the Sankey plot
  filtered_sankey_data <- reactive({
    if (input$groupFilter != "All") {
      filtered_links <- links_df %>%
        filter(from %in% nodes_df$name[nodes_df$group == input$groupFilter] |
                 to %in% nodes_df$name[nodes_df$group == input$groupFilter])
    } else {
      filtered_links <- links_df
    }
    
    unified_node_names <- unique(c(filtered_links$from, filtered_links$to))
    plot_nodes <- tibble(name = unified_node_names) %>%
      left_join(nodes_df, by = "name") %>%
      mutate(id = row_number() - 1)
    
    plot_links <- filtered_links %>%
      mutate(source = plot_nodes$id[match(filtered_links$from, plot_nodes$name)],
             target = plot_nodes$id[match(filtered_links$to, plot_nodes$name)])
    
    list(nodes = plot_nodes, links = plot_links)
  })
  
  output$sankeyPlot <- renderPlotly({
    data <- filtered_sankey_data()
    nodes <- data$nodes
    links <- data$links
    
    # Create a named vector for mapping weight labels to colors not used right now
    color_map <- c("Weak coverage" = "#E1AF00", "Medium coverage" = "#78B7C5", "Strong coverage" = "#3B9AB2")
    
    plot_ly(
      type = "sankey",
      domain = list(x = c(0,1), y = c(0,1)),
      orientation = "h",
      node = list(
        pad = 15,
        thickness = 20,
        line = list(color = "black", width = 0.5),
        label = nodes$name,
        color = nodes$color
      ),
      link = list(
        source = links$source,
        target = links$target,
        value = links$weight
        #color = color_map[links$weight_label]  # Apply color based on weight_label
        #  custom hover text settings would go here if they worked
      )
    )
  })
  
  
  # Load the data for the Attribute Coverage plot (first tab)
  coverage_df <- read_csv("coverage_plot.csv")
  
  
  filtered_coverage_data <- reactive({
    filtered_df <- coverage_df
    
    if (input$attDimensionFilter != "All") {
      filtered_df <- filtered_df %>% filter(att_dimension == input$attDimensionFilter)
    }
    
    return(filtered_df)
  })
  
  
  # Render the Attribute Coverage plot
  output$coveragePlot <- renderPlotly({
    data <- filtered_coverage_data()
    
    # Relevel the factors as required
    data <- data %>%
      mutate(
        att_dimension = fct_relevel(att_dimension, "Ecological", "Governance", "Socio-economic"),
        assessment = fct_relevel(assessment, "ERA", "SRA", "ERA + SRA"),
        coverage_factor = factor(coverage, 
                                 levels = 0:3,
                                 labels = c("Missing", "Weak coverage", "Medium coverage", "Strong coverage"))
      )
    
    #the coverage plot
    
    p <- ggplot(data, aes(x = assessment, y = attribute, fill = coverage_factor, text = hover_tile)) +
      geom_tile(color = "white", width = 0.3, height = 1) +
      scale_fill_manual(values = c("Missing" = "#F21A00", "Weak coverage" = "#E1AF00", "Medium coverage" = "#78B7C5", "Strong coverage" = "#3B9AB2")) +
      theme_minimal() +
      labs(title = "", fill = "Coverage", x = "Assessment(s)") +  # Set y-axis label to empty
      theme(
        plot.title = element_blank(),
        axis.title.y = element_blank(),  # Remove y-axis label
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(face = "bold", color = "black", angle = 0, vjust = 0.5, hjust=0.5),
        axis.ticks.x = element_blank(),
        legend.position = "bottom"
      )
    #making it interactive
    ggplotly(p, tooltip = "text")
    
    
  })
}



# Run the application 
shinyApp(ui = ui, server = server)



