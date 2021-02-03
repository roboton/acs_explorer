library(shiny)
library(tidyverse)
library(tidycensus)

census_api_key(Sys.getenv("US_CENSUS_KEY"), install = TRUE) # in .Rprofile
acs_vars <- load_variables(year = 2019, dataset = "acs5", cache = TRUE)
acs_choices <- acs_vars %>%
    filter(label != "Estimate!!Total:") %>%
    rename(value = name) %>%
    unite(name, concept, label, sep = ":") %>%
    filter(!duplicated(name)) %>%
    select(name, value) %>% deframe()

acs_results <- function(variables, geography = "county", states = NULL,
                        year = 2019, summary_var = "B01003_001") {
    query_vars <- acs_vars %>%
        filter(name %in% variables)
    if (nrow(query_vars) == 0) {
       return(NA) 
    }
    shift_geo <- TRUE
    if (geography != "county") {
      states <- geography
      geography <- "tract"
      shift_geo <- FALSE
    }
    get_acs(geography = geography,
            variables = query_vars %>% pull(name),
            state = states, shift_geo = shift_geo,
            survey = "acs5", year = 2019, cache_table = TRUE, geometry = TRUE,
            summary_var = "B01003_001" # total population
            ) %>%
        left_join(query_vars, by = c("variable" = "name")) %>%
        mutate(per_capita = if_else(str_starts(label, "Estimate!!Total"),
                                    estimate / summary_est, estimate)) %>%
        return()
}

ui <- function(request) {
    fluidPage(
        shinybusy::add_busy_bar(color = "CornflowerBlue", timeout = 800),
        titlePanel("ACS Explorer"),
        tags$a(
            href = "https://github.com/roboton/acs_explorer",
            target = "_blank", "[git]"),
        tags$a(
            href = "mailto:roberton@gmail.com",
            target = "_blank", "[contact]"),
        sidebarLayout(
            sidebarPanel(
                # data options
                selectizeInput(
                    "variables", "Variables",
                    choices = NULL,
                    multiple = TRUE,
                    options = list(placeholder = 'type to search variables')),
                selectInput("geography", "Geography:",
                            choices = list("County" = c("county"),
                                           "Tract" = state.abb)),
                bookmarkButton(),
                width = 3),
            mainPanel(
                tabsetPanel(
                    id = "plotTabs", type = "tabs",
                    tabPanel(
                        "Map", value = "map",
                        plotOutput(
                            "mapPlot",
                            width = "auto",
                            height = "750px"),
                        downloadButton("downloadData", "download data (csv)")
                    ),
                    tabPanel(
                        "Data Table", value = "dataTable",
                        DT::dataTableOutput("dataTable")
                    )
                ),
                width = 9
            )
        )
    )
}

server <- function(input, output, session) {
    output$mapPlot <- renderPlot({
        results <- acs_results(variables = input$variables,
                               geography = input$geography)
        if (all(is.na(results))) {
            ggplot() + geom_text(aes(0, 0, label = "no data")) +
                ggthemes::theme_map() %>% return()
        } else {
            results %>%
                ggplot(aes(fill = per_capita, color = per_capita)) +
                geom_sf() +
                ggthemes::theme_map() +
                theme(legend.position = "right",
                      legend.title = element_blank(),
                      strip.text.x = element_text(size = 15)) +
                facet_wrap(~ label, nrow = case_when(
                    n_distinct(results$variable) <= 2 ~ 1,
                    n_distinct(results$variable) <= 6 ~ 2,
                    TRUE ~ 3)) %>%
                return()
        }
    })
    
    output$dataTable <- DT::renderDataTable({
        results <- acs_results(variables = input$variables,
                               geography = input$geography)
        if (all(is.na(results))) {
          DT::datatable(tibble()) %>% return()
        } else {
          DT::datatable(results, filter = "top") %>% as_tibble() %>%
            select(-geometry) %>% return()
        }
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0("acs-explorer-data-", Sys.Date(), ".csv")
        },
        content = function(file) {
            acs_results(variables = input$variables,
                        geography = input$geography) %>%
                as_tibble() %>% select(-geometry) %>%
                readr::write_csv()
        }
    )
    
    # server side location selectize
    updateSelectizeInput(session, "variables", choices = acs_choices,
                         server = TRUE)
    
    # ensure we don't overwrite bookmark locations with default
    session$onRestore(function(state) {
        session$userData$restored <- TRUE
        updateSelectizeInput(session, "variables", choices = acs_choices,
                             selected = state$input$variables, server = TRUE)
    })
}

shinyApp(ui = ui, server = server, enableBookmarking = "server")