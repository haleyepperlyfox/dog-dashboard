# load libraries
library(tidyverse)
library(tidytuesdayR)
library(shiny)
library(shinydashboard)
library(ggrepel)
library(DT)
library(here)

# load in data
trait_description <- read_csv(here("trait_description.csv"))
breed_traits <- read_csv(here("breed_traits.csv"))
breed_rank <- read_csv(here("breed_rank.csv"))

# subset to remove variables with categorical responses
trait_description <- trait_description %>% 
  filter(!c(Trait == "Coat Type" | Trait == "Coat Length"))
breed_traits <- breed_traits %>% 
  select(-c(`Coat Type`, `Coat Length`))

# build app
ui <- dashboardPage(
  dashboardHeader(title = "Dog Breeds"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Traits", tabName = "traits", icon = icon("child")),
      menuItem("Popularity", tabName = "popularity", icon = icon("bone")),
      menuItem("Compare Dogs", tabName = "compare", icon = icon("dog")),
      menuItem("Data Information", tabName = "source", icon = icon("info"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "traits",
        fluidRow(
          box(width = 3, title = "Dog Traits", 
              sliderInput("affection", "Affectionate with Family", min = 1, max = 5, step = 1, value = c(1,5)),
              sliderInput("other_dogs", "Good with Other Dogs", min = 1, max = 5, step = 1, value = c(1,5)),
              sliderInput("shedding", "Shedding Level", min = 1, max = 5, step = 1, value = c(1,5)),
              sliderInput("watchdog", "Watchdog/Protective Nature", min = 1, max = 5, step = 1, value = c(1,5)),
              sliderInput("energy", "Energy Level", min = 1, max = 5, step = 1, value = c(1,5))),
          box(width = 9, p("It can be daunting to try and find a dog that fits your lifestyle, 
                but that’s where this page come in! Some common things people consider 
                when deciding what breed of dog to get are how good they are with people 
                and other dogs, how much they shed, and how much energy they have. 
                Some people also want a dog for protection. On this page, you can slide 
                the scales on the five traits to find which breeds might be good for you. 
                For each trait, each breed is scored from 1 (lowest) to 5 (highest). 
                           Trait descriptions can be found on the Data Information tab.")),
          box(width = 9, 
              dataTableOutput(outputId = "traits_table", width="300px"))
        )
      ),
      tabItem(
        tabName = "compare",
        fluidRow(
          box(width=5, title = "Compare Select Traits Across Breeds",
              selectInput("breed_selected", "Select up to 10 breeds for best visual", choices = c(unique(breed_traits$Breed)), multiple = TRUE, selected = breed_traits[1,1]),
              selectInput("trait_1", "Select first trait", choices = colnames(breed_traits[,-1])),
              selectInput("trait_2", "Select second trait", choices = colnames(breed_traits[,-1]), selected = colnames(breed_traits[,5]))),
          box(width = 5, p("Maybe you have a few dog breeds that you are considering and you want to compare them, 
                           that’s what this page is for! Select some dog breeds and then select two traits to use to 
                           compare the different breeds. Choosing only ten breeds or fewer at a time will make the 
                           graph easier to read. For each trait, each breed is scored from 1 (lowest) to 5 (highest).
                           Trait descriptions can be found on the Data Information tab.")),
          box(width = 10, plotOutput(outputId = "compare_plot"))
        )
  ),
  tabItem(
    tabName = "popularity",
    fluidRow(
      box(width=10, title = "Breed Popularity Over Time",
          p("Some dog breeds become more or less trendy over time, although the Labrador Retriever has been number one 
            in the USA for quite some time! Select different dog breeds to see how their popularity has changed over the 
            years. Choosing only ten breeds or fewer at a time will make the graph easier to read. A lower rank means a 
            more popular dog (closer to being number one like the lab!) while a higher rank means a less popular dog. 
            That means that an increasing trend says that breed has become less popular over time, while a decreasing trend 
            says that breed has been growing in popularity."),
          selectInput("breed_selected_pop", "Select up to 10 breeds for best visual", choices = c(unique(breed_rank$Breed)), multiple = TRUE, selected = breed_rank[1,1]),
          plotOutput(outputId = "pop_plot")
      )
    )
  ),
  tabItem(
    tabName = "source",
    fluidRow(
      box(width = 12, title = "Data Information",
          tags$div(
            "These data come from the ",
            tags$a(href="https://www.akc.org/dog-breeds/", 
                   "American Kennel Club."),
            "Data used on this website include information on dog breed traits and popularity in the USA.
            See below a more thorough description for each trait.
            These data were also used as part of a ",
            tags$a(href="https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-02-01/readme.md", 
                   "#TidyTuesday"),
            "for the week of February 1, 2022. This dashboard was built by Haley Fox using Shiny and R (v.4.1.3) and 
            RStudio Server (v.2022.07.1 Build 554). "
          ),
          br(),
          datatable(trait_description)
    )
  )
  )
)
)
)



server <- function(input, output) {
  breed_traits_1 <- breed_traits %>% 
    select(Breed, `Affectionate With Family`, `Good With Other Dogs`, `Shedding Level`,
           `Watchdog/Protective Nature`, `Energy Level`) %>% 
    rename(`Watchdog/ Protective Nature` = `Watchdog/Protective Nature`)
  traits_df <- reactive({
      breed_traits_1 %>% 
      filter(`Affectionate With Family` %in% input$affection[1]:input$affection[2]) %>% 
      filter(`Good With Other Dogs` %in% input$other_dogs[1]:input$other_dogs[2]) %>% 
      filter(`Shedding Level` %in% input$shedding[1]:input$shedding[2]) %>% 
      filter(`Watchdog/ Protective Nature` %in% input$watchdog[1]:input$watchdog[2]) %>% 
      filter(`Energy Level` %in% input$energy[1]:input$energy[2])
  })
  output$traits_table <- renderDataTable({
    datatable(traits_df()) %>%  
      formatStyle(columns = "Breed", target = "cell", backgroundColor = "#F7080820") 
  })
  compare_df <- reactive({
    breed_traits %>% filter(Breed %in% input$breed_selected) %>% 
      select(Breed, input$trait_1, input$trait_2)
  })
  output$compare_plot <- renderPlot({
    ggplot(compare_df(), aes(x = compare_df()[[2]], y = compare_df()[[3]], label = Breed)) +
      geom_point(color = "darkblue", shape = 18, size = 3) +
      scale_x_continuous(limits = c(1,6), breaks = c(1, 2, 3, 4, 5)) +
      scale_y_continuous(limits = c(1,6), breaks = c(1, 2, 3, 4, 5)) +
      geom_text_repel() +
      labs(x = input$trait_1, y = input$trait_2,
           caption = "Data source: American Kennel Club") +
      theme_classic() +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 12))
  })
  breed_rank <- breed_rank %>% 
    pivot_longer(cols = starts_with("2"), names_to = "year", values_to = "rank") %>% 
    mutate(year =  gsub(" Rank", "", year)) %>% 
    mutate(label = ifelse(year == 2020, Breed, NA))
  breed_rank$year <- as.numeric(breed_rank$year)
  pop_df <- reactive({
    breed_rank %>% filter(Breed %in% input$breed_selected_pop)
  })
  output$pop_plot <- renderPlot({
    ggplot(pop_df(), aes(x = year, y = rank, color = Breed)) +
      geom_line(size = 2) +
      scale_x_continuous(limits = c(2013, 2024), breaks = c(2014, 2016, 2018, 2020)) +
      geom_label_repel(aes(label = label),
                       nudge_x = 0.5,
                       na.rm = TRUE) +
      theme_classic() +
      labs(x = "Year", y = "Rank (1 = most popular)",
           caption = "Data source: American Kennel Club") +
      theme(legend.position = "none",
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 12))
  })
}

shinyApp(ui = ui, server = server)
