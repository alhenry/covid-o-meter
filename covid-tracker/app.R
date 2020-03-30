#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyverse, shinythemes, glue,
               lubridate, ggthemes, ggrepel, ggiraph)

# PREPARATION -------
DT_case <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
DT_death <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
DT_recov <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

walk(list(DT_case, DT_death, DT_recov),
     setnames, c("Province/State", "Country/Region"),
     c("area", "country"))
date_end <- colnames(DT_case)[ncol(DT_case)] %>% mdy
date_start <- colnames(DT_case)[5] %>% mdy
# function to filter and transpose data
transp <- function(data, value_name){
    data %>% pivot_longer(matches("^[[:digit:]]*/"), names_to = "date",
                          values_to = value_name)
} 

df_case <- transp(DT_case, "case")
df_death <- transp(DT_death, "death")
df_recov <- transp(DT_recov, "recov")

df_all <- reduce(list(df_case, df_death, df_recov),
                 full_join, by = c("area", "country", "Lat", "Long", "date")) %>% 
    mutate(country = recode(country,
                            `United Kingdom` = "UK",
                            `Korea, South` = "South Korea"),
           date = mdy(date))

countries <- df_all$country %>% unique
max_day <- as.numeric(date_end - date_start)

# top countries
n_top <- 10
top_countries <- df_all %>% 
    filter(date == date_end) %>% 
    group_by(country) %>% 
    summarise(total_case = sum(case)) %>% 
    arrange(-total_case) %>% 
    pull(country) %>% 
    .[1:n_top]

to_include <- c(
    top_countries,
    "India", "Japan", "Indonesia", "Singapore", "Taiwan", "South Korea"
) %>% unique

# UI page --------
ui <- navbarPage("Covid-19 Tracker",
                 position = "fixed-top",
                 theme = shinytheme("flatly"),
    tabPanel(
    "Case statistics",
    # Display plots
    absolutePanel(
        top = 70, left = 10, width = "70%",
        girafeOutput("case_plot"),
        girafeOutput("case_growth")
    ),
    
    # Sidebar
    fixedPanel(
        top = 70, right = 10, width = "30%",
        style = "opacity: 0.90",
        draggable = TRUE,
        selectizeInput('to_include', 'Countries to include', multiple = T,
                       choices = countries,
                       selected = to_include),
        uiOutput('to_highlight_case'),
        dateRangeInput('date', 'Range of dates to include', 
                       start = date_start, end = date_end),
        # parameters for plots
        h3("Plot Parameters"),
        
        sliderInput('case_day1', 'Minimum cases for day 1',
                    min = 1, max = 1000,
                    value = 100),
        sliderInput('day_range_case', 'Range of days to display',
                    min = 1, max = max_day, value = c(1,40), step = 1),
    )
    ),
    
    tabPanel(
        "Death statistics",
        # Display plots
        absolutePanel(
            top = 70, left = 10, width = "70%",
            girafeOutput("death_plot"),
            girafeOutput("cfr_plot"),
            girafeOutput("death_growth")
        ),
        # Sidebar
        fixedPanel(
            top = 70, right = 10, width = "30%",
            style = "opacity: 0.90",
            draggable = TRUE,
            selectizeInput('to_include', 'Countries to include', multiple = T,
                           choices = countries,
                           selected = to_include),
            uiOutput('to_highlight_death'),
            dateRangeInput('date', 'Range of dates to include', 
                           start = date_start, end = date_end),
            # parameters for plots
            h3("Plot Parameters"),
            
            sliderInput('death_day1', 'Minimum deaths for day 1',
                        min = 1, max = 100,
                        value = 10),
            sliderInput('day_range_death', 'Range of days to display',
                        min = 1, max = max_day, value = c(1,40), step = 1)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Create reactive data
    df_reactive <- reactive({
        req(input$to_include,
            input$to_highlight,
            input$day_range_case,
            input$day_range_death,
            input$case_day1,
            input$death_day1)
        
        df_all %>% 
            filter(date >= input$date[1] & date <= input$date[2]) %>% 
            filter(country %in% input$to_include) %>% 
            # aggregate by country
            group_by(country, date) %>% 
            arrange(date) %>%
            summarise_at(vars(case:recov), sum)
    })
    
    # output for to_highlight UI
    output$to_highlight_case <- output$to_highlight_death <- renderUI({
        selectizeInput('to_highlight', 'Countries to highlight', multiple = T,
                       choices = input$to_include,
                       selected = input$to_include)
    })
    
    
    # Helper function for plotting
    make_exp_break <- function(x,n) map(x, `*`, c(1, 10^seq(n))) %>% unlist %>% sort
    
    make_df_plot <- function(df, metric, max_day, n_start){
        metric <- rlang::enquo(metric)
        df %>%
            filter(!!metric >= n_start) %>% 
            mutate(day = seq_along(date)) %>% 
            filter(day <= max_day)
    }
    
    make_tooltip <- function(df, metric, n_start, text1, text2){
        metric <- rlang::enquo(metric)
        df %>% mutate(
            tooltip = glue("
                {country}
                {date}
                Day {day} after {n_start} {text1}\n
                ") %>% 
                paste0(round(!!metric, 2), " ", text2)
            )
    }
    
    make_plot <- function(df_plot, to_highlight, metric,
                          min_day, nudge_x = 0, nudge_y = 0,
                          base_col = "gray80"){
        metric <- rlang::enquo(metric)
        
        df_highlight <- df_plot %>% 
            filter(country %in% to_highlight)

        last_day <- df_plot %>% filter(day == max(day))
        
        max_day <- max(last_day$day)
        xlim <- c(min_day, max_day + 5)
        
        ggplot(df_plot, aes(x = day, y = !!metric, group = country, color = country,
                            tooltip = tooltip)) +
            scale_x_continuous(limits = xlim) +
            scale_color_tableau("Tableau 20") +
            geom_line(color = base_col, size = 0.5) +
            geom_point(color = base_col, size = 0.5) +
            geom_line(data = df_highlight, size = 0.8) +
            geom_point_interactive(data = df_highlight, size = 1) +
            theme_minimal(base_size = 10) +
            geom_text_repel(
                aes(label = country),
                segment.size = 0,
                fontface = "bold",
                hjust = 1,
                nudge_x = nudge_x,
                nudge_y = nudge_y,
                direction = "both",
                data = last_day %>% filter (country %in% to_highlight)
            ) +
            theme(legend.position = "none",
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_line(colour = "gray95"))
        
    }
    
    # Case plot
    output$case_plot <- renderGirafe({
        df <- df_reactive()
        min_day <- input$day_range_case[1]
        max_day <- input$day_range_case[2]
        n_start <- input$case_day1
        
        df_plot <- make_df_plot(df, case, max_day, n_start) %>% 
            make_tooltip(case, n_start, "cases", "cases")
        
        to_highlight <- input$to_highlight
        y_breaks <- c(100,200,500) %>% make_exp_break(3)
        
        plot <- make_plot(df_plot, to_highlight, case, min_day) +
            scale_y_continuous(trans = "log", breaks = y_breaks, labels = scales::comma)
        
        girafe(ggobj = plot)    
    })
    
    # Death plot
    output$death_plot <- renderGirafe({
        df <- df_reactive()
        min_day <- input$day_range_death[1]
        max_day <- input$day_range_death[2]
        n_start <- input$death_day1
        
        df_plot <- make_df_plot(df, death, max_day, n_start) %>% 
            make_tooltip(case, n_start, "deaths", "deaths")
        
        to_highlight <- input$to_highlight
        y_breaks <- c(10,20,50) %>% make_exp_break(3)
        
        plot <- make_plot(df_plot, to_highlight, death, min_day) +
            scale_y_continuous(trans = "log", breaks = y_breaks, labels = scales::comma)
        
        girafe(ggobj = plot)
    })
    
    # CFR plot
    output$cfr_plot <- renderGirafe({
        df <- df_reactive()
        min_day <- input$day_range_death[1]
        max_day <- input$day_range_death[2]
        n_start <- input$death_day1
        
        df_plot <- make_df_plot(df, death, max_day, n_start) %>% 
            mutate(cfr = death/case) %>% 
            make_tooltip(cfr, n_start, "deaths", "deaths")
        
        to_highlight <- input$to_highlight

        plot <- make_plot(df_plot, to_highlight, cfr, min_day)
        
        girafe(ggobj = plot)
    })
    
    # Daily Case Growth
    output$case_growth <- renderGirafe({
        df <- df_reactive()
        min_day <- input$day_range_case[1]
        max_day <- input$day_range_case[2]
        n_start <- input$case_day1
        
        df_plot <- make_df_plot(df, death, max_day, n_start) %>% 
            mutate(case_growth = case / lag(case))  %>% 
            make_tooltip(case_growth, n_start, "cases", "daily case growth")
        
        to_highlight <- input$to_highlight
        
        plot <- make_plot(df_plot, to_highlight, case_growth, min_day)
        
        girafe(ggobj = plot)
    })
    
    # Daily Death Growth
    output$death_growth <- renderGirafe({
        df <- df_reactive()
        min_day <- input$day_range_death[1]
        max_day <- input$day_range_death[2]
        n_start <- input$death_day1
        
        df_plot <- make_df_plot(df, death, max_day, n_start) %>% 
            mutate(death_growth = death / lag(death)) %>% 
            make_tooltip(death_growth, n_start, "deaths", "daily death growth")
        
        to_highlight <- input$to_highlight
        
        plot <- make_plot(df_plot, to_highlight, death_growth, min_day)
        
        girafe(ggobj = plot)
    })
    
    
}
# For testing
input <- list(
    to_include = to_include,
    to_highlight = to_include,
    max_day_case = 40,
    max_day_death = 30,
    case_day1 = 100,
    death_day1 = 10
)


# Run the application 
shinyApp(ui = ui, server = server)
