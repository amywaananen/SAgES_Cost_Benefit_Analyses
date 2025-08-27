# Source scripts, load libraries, and read data sets

library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(janitor)
library(tidyverse)

# Solar Energy
data <- read.csv('data/combined_SAM_output.csv')

# Ag Cost-Benefit: Corn ($/acre)
row_collapse <- 4
# nms <- read_excel('01_Data/SaGES MN Ag Production.xlsx', sheet = 'Replace Corn', range = cell_rows(seq_len(row_collapse)), col_names = F)
nms <- read_excel('data/SaGES MN Ag Production.xlsx', sheet = 'Replace Corn', range = cell_rows(seq_len(row_collapse)), col_names = F)
nms <- lapply(nms, na.omit)
nms <- lapply(nms, paste, collapse = "_")

Ag_corn <- read_excel('data/SaGES MN Ag Production.xlsx', sheet = 'Replace Corn', skip = row_collapse, col_names = unlist(nms)) %>% clean_names()
# Ag_corn <- read_excel('01_Data/SaGES MN Ag Production.xlsx', sheet = 'Replace Corn', skip = row_collapse, col_names = unlist(nms)) %>% clean_names()

# Ag Cost-Benefit: Soy ($/acre)
row_collapse <- 4
nms <- read_excel('data/SaGES MN Ag Production.xlsx', sheet = 'Replace Soybeans', range = cell_rows(seq_len(row_collapse)), col_names = F)
# nms <- read_excel('01_Data/SaGES MN Ag Production.xlsx', sheet = 'Replace Soybeans', range = cell_rows(seq_len(row_collapse)), col_names = F)
nms <- lapply(nms, na.omit)
nms <- lapply(nms, paste, collapse = "_")

Ag_soy <- read_excel('data/SaGES MN Ag Production.xlsx', sheet = 'Replace Soybeans', skip = row_collapse, col_names = unlist(nms)) %>% clean_names()
# Ag_soy <- read_excel('01_Data/SaGES MN Ag Production.xlsx', sheet = 'Replace Soybeans', skip = row_collapse, col_names = unlist(nms)) %>% clean_names()


# Pivot data from wide to long --------------------------------------------

Ag_corn <- Ag_corn %>% 
  pivot_longer(
    cols = starts_with(c("corn", "turfgrass", "prairie", "grazing", "switchgrass")),
    names_to = c("land_use","year_min","year_max","farm_size"),
    names_sep = "_",
    values_to = "ag_USD_per_acre"
  )

Ag_soy <- Ag_soy %>% 
  pivot_longer(
    cols = starts_with(c("corn", "turfgrass", "prairie", "grazing", "switchgrass")),
    names_to = c("land_use","year_min","year_max","farm_size"),
    names_sep = "_",
    values_to = "ag_USD_per_acre"
  )


# Expand ag tables to include all years -----------------------------------

Ag_corn_long <- Ag_corn %>%
  mutate(year_min = as.numeric(year_min), year_max = as.numeric(year_max)) %>% 
  mutate(year = map2(year_min, year_max, seq)) %>%
  mutate(prior_land_use = "corn") %>% 
  select(-c(year_min, year_max)) %>% 
  unnest(year) %>%
  ungroup %>% 
  arrange(fips, land_use, farm_size, year)

Ag_soy_long <- Ag_soy %>%
  mutate(year_min = as.numeric(year_min), year_max = as.numeric(year_max)) %>% 
  mutate(year = map2(year_min, year_max, seq)) %>%
  mutate(prior_land_use = "soy") %>% 
  select(-c(year_min, year_max)) %>% 
  unnest(year) %>%
  ungroup %>% 
  arrange(fips, land_use, farm_size, year)

Ag_long <- bind_rows(Ag_corn_long, Ag_soy_long)

nw <- c("Kittson", "Roseau", "Lake of the Woods", "Marshall", "Pennington", "Red Lake", "Polk", "Beltrami", "Mahnomen", "Clearwater", "Clay", "Becker", "Hubbard", "Norman")
central <- c("Wilkin", "Otter Tail", "Wadena", "Traverse", "Big Stone", "Grant", "Douglas", "Stevens", "Pope", "Kandiyohi", "Stearns", "Meeker", "Wright", "Todd", "Benton","Morrison", "Mille Lacs", "Swift")
ne <- c("Koochiching", "St. Louis", "Itasca", "Cass", "Aitkin", "Crow Wing", "Carlton", "Kanabec", "Pine", "Lake", "Cook")
metro <- c('Sherburne', "Isanti", "Anoka", "Hennepin", "Carver", "Scott", "Dakota", "Ramsey", "Washington", "Chisago")
sw <- c("Lac qui Parle", "Chippewa","Renville", "Yellow Medicine", "Lincoln", "Lyon", "Redwood", "Pipestone", "Murray", "Cottonwood", "Rock", "Nobles", "Jackson")
sc <- c("McLeod", "Sibley", "Nicollet", "Le Sueur", "Brown", "Watonwan", "Blue Earth", "Waseca", "Martin", "Faribault")
se <- c("Rice", "Goodhue", "Wabasha", "Steele", "Dodge", "Olmsted", "Winona", "Freeborn", "Mower", "Fillmore", "Houston")

mn_regions <- data.frame(mn_county = c(nw, central, ne, metro, sw, sc, se),region = c(rep("Northwest", times = length(nw)), rep("Central", times = length(central)), rep("Northeast", times = length(ne)), rep("Metro", times = length(metro)), rep("Southwest", times = length(sw)), rep("South Central", times = length(sc)), rep("South East", times = length(se))))

# Merge layers by county --------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Exploring SAgES Cost-Benefit Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(position = "right",
                sidebarPanel( 
                  selectInput("cambium", label = h3("Cambium 2022"),
                              choices = unique(data$Cambium)),
                  
                  selectInput("panel_type", label = h3("Panel Type"),
                              choices = list("Fixed Tilt - 45 degrees" = "fl",
                                             "Fixed Tilt - 30 degrees" = "fs",
                                             "Fixed Tilt - 20 degrees" = "f20",
                                             "1-Axis Tracking" = 't')),
                  
                  selectInput("output", label = h3("Model Output"),
                              choices = list("Energy" = "e",
                                             "Capacity" = "c",
                                             "Avoided CO2" = "co2e"),
                              selected = "e"),
                  
                  selectInput("farm_size", label = h3("Farm Size"),
                              choices = list("Small" = "small",
                                             "Medium" = "medium",
                                             "Large" = "large"),
                              selected = "medium"),
                  
                  selectInput("prior_land_use",
                              h3("Prior Land Use"),
                              choices = list("Corn" = "corn",
                                             "Soy" = "soy"),
                              selected = "corn"),
                  
                  checkboxGroupInput("land_use",
                                     h3("Agrivoltaic Land Use"),
                                     choices = list("Pollinator Habitat" = "prairie",
                                                    "Turfgrass" = 'turfgrass'),
                                     selected = "prairie")
                  
                  # Add slider for carbon price
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  plotOutput("distPlot")
                )
  )
)  



# Define server logic required to draw a plot
server <- function(input, output) {
  
  # Add reactive functions
  
  subset <- reactive({data %>% filter( 
    Cambium == input$cambium,
    panel_type == input$panel_type,
    output == input$output)})
  
  merged_subset <- reactive({Ag_long %>% 
      select(-c(latitude, longitude)) %>% 
      filter(prior_land_use %in% input$prior_land_use) %>% 
      filter(land_use %in% input$land_use) %>% 
      filter(farm_size %in% input$farm_size) %>% 
      filter(year %in% 2023:2051) %>% 
      filter(!is.na(mn_county)) %>% 
      left_join(subset(), by = c('mn_county', 'year'), relationship = "many-to-many") %>% 
      left_join(mn_regions, by = 'mn_county') %>% 
      mutate(value = case_when(land_use == "corn" ~ 0,
                               .default = value)) %>% 
      mutate(net_profit = value + ag_USD_per_acre)
    
    # Add merge with ES summarized by county
    
    
    
    # Add carbon * price to net profit
    
  })
  
  output$distPlot <- renderPlot({
    g <- ggplot(merged_subset(), 
                aes(x = year, y = net_profit, group = interaction(mn_county, land_use), color = land_use))+
      geom_line(alpha = 0.3)+
      theme_bw(base_size = 20)+
      ylim(-1000,14000)+
      ylab("$/Acre")+
      xlab("Year")
    g
  }
  
  # Add second plot for N and P runoff, pollination?
  
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
