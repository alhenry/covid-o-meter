#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(data.table)
library(tidyverse)
library(shinythemes)
library(glue)
library(lubridate)
library(ggrepel)
library(ggiraph)
library(ggthemes)
library(gganimate)
library(shinycustomloader)
library(gifski)

# PREPARATION -------
DT_case <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
DT_death <- fread("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

walk(list(DT_case, DT_death),
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

df_all <- reduce(list(df_case, df_death),
                 full_join, by = c("area", "country", "Lat", "Long", "date")) %>% 
    mutate(country = recode(country,
                            `United Kingdom` = "UK",
                            `Korea, South` = "South Korea",
                            `Taiwan*` = "Taiwan"),
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
    "India", "Japan", "Indonesia", "Singapore", "Brazil", "South Korea"
) %>% unique

# to_highlight <- c("UK", "US", "China", "Italy", "Indonesia", "South Korea", "")

# Set global options for interactive
girafe_mod <- function(...){
    girafe(..., width_svg = 8, height_svg = 6,
           fonts = list(sans = "Lato"),
           options = list(
               opts_sizing(rescale = T, width = 1),
               opts_tooltip(use_fill = T, opacity = 0.95),
               opts_hover(css = "fill:salmon;stroke:red;r:3pt;"),
               opts_toolbar(position = "bottomleft")
          )
       )
    }


# ===========
# UI
# ===========
ui <- navbarPage("Covid-19 Tracker",
                 position = "fixed-top",
                 theme = shinytheme("flatly"),
    tabPanel(
    "Case statistics",
    # Display plots
    absolutePanel(
        top = 70, left = 10, bottom = 50, width = "65%",
        withLoader(girafeOutput("case_plot"), type = "html", loader = "loader4"),
        h1("\n"),
        withLoader(girafeOutput("case_growth"), type = "html", loader = "loader4"),
        h1("\n")
        ),
    
    # Sidebar
    fixedPanel(
        top = 70, right = 10, width = "30%",
        style = "opacity: 0.90",
        draggable = TRUE,
        
        h3("Data"),
        selectizeInput('to_include_case', 'Countries (max 20)', multiple = T,
                       choices = countries,
                       selected = to_include,
                       options = list(maxItems = 20)),
        dateRangeInput('date_case', 'Date range', 
                       start = date_start, end = date_end),
        
        # parameters for plots
        h3("Plot Parameters"),
        
        sliderInput('case_day1', 'Minimum cases for day 1',
                    min = 1, max = 300, value = 100),
        sliderInput('day_range_case', 'Range of days to display',
                    min = 1, max = max_day, value = c(1,40), step = 1),
        
        h3("\n"),
        actionButton("make_plot_case", "Update plots")
    )
    ),
    
    tabPanel(
        "Death statistics",
        # Display plots
        absolutePanel(
            top = 70, left = 10, bottom = 50, width = "65%",
            withLoader(girafeOutput("death_plot"), type = "html", loader = "loader4"),
            h1("\n"),
            withLoader(girafeOutput("cfr_plot"), type = "html", loader = "loader4"),
            h1("\n"),
            withLoader(girafeOutput("death_growth"), type = "html", loader = "loader4"),
            h1("\n")
        ),
        # Sidebar
        fixedPanel(
            top = 70, right = 10, width = "30%",
            style = "opacity: 0.90",
            draggable = TRUE,
            
            h3("Data"),
            selectizeInput('to_include_death', 'Countries', multiple = T,
                           choices = countries,
                           selected = to_include),
            dateRangeInput('date_death', 'Date range', 
                           start = date_start, end = date_end),
            
            # parameters for plots
            h3("Plot Parameters"),
            
            sliderInput('death_day1', 'Minimum deaths for day 1',
                        min = 1, max = 100, value = 10),
            sliderInput('day_range_death', 'Range of days to display',
                        min = 1, max = max_day, value = c(1,40), step = 1),
            
            h3("\n"),
            actionButton("make_plot_death", "Update plots")
        )
    ),
    
    tabPanel(
        "Animation",
        # Display plots
        absolutePanel(
            top = 70, left = 10, width = "65%",
            
            withLoader(imageOutput('case_animation'), type = "text",
                       loader=list(marquee("Making animation... Consider washing your hands while waiting"))),

            h1("\n"),
            withLoader(imageOutput('death_animation'), type = "text",
                       loader=list(marquee("Making animation... Consider washing your hands while waiting")))
        ),
        # Sidebar
        fixedPanel(
            top = 70, right = 10, width = "30%",
            style = "opacity: 0.90",
            draggable = TRUE,
            
            
            h3("Data"),
            selectizeInput('to_include_anim', 'Countries', multiple = T,
                           choices = countries, selected = to_include),
            dateRangeInput('date_anim', 'Date range', 
                           start = date_start, end = date_end),
            
            # Animation parameters
            h3("Plot parameters"),
            sliderInput('case_day1_anim', 'Minimum cases for day 1',
                        min = 1, max = 300, value = 100),
            sliderInput('death_day1_anim', 'Minimum deaths for day 1',
                        min = 1, max = 100, value = 10),
            sliderInput('day_range_anim', 'Range of days to display',
                        min = 1, max = max_day, value = c(1,40), step = 1),
            
            # # Action button
            h3("\n"),
            actionButton("animate", "Update animation")
            
            
        )
    )
    
)

# ===========
# SERVER
# ===========
server <- function(input, output) {
    # Helper functions -------
    exp_breaks <- function(lims, base_breaks = c(1,2,5)) {
      lower <- log10(lims[1]) %>% floor
      upper <- log10(lims[2]) %>% ceiling
      
      map(base_breaks, `*`, c(10^seq(lower, upper))) %>% unlist %>% sort
    }
    
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
                paste0(round(!!metric, 2), " ", text2),
            data_id = paste(country, day)
            )
    }
    
    make_plot <- function(df_plot, metric,
                          min_day, nudge_x = 0, nudge_y = 0,
                          base_col = "gray80"){
        metric <- rlang::enquo(metric)
        
        last_day <- df_plot %>% filter(day == max(day))
        
        max_day <- max(last_day$day)
        xlim <- c(min_day, max_day + 5)
        
        ggplot(df_plot, aes(x = day, y = !!metric, group = country, color = country)) +
            scale_x_continuous(limits = xlim) +
            scale_color_tableau("Tableau 20") +
            geom_line(size = 0.5) +
            geom_point_interactive(aes(tooltip = tooltip, data_id = data_id), size = 0.8) +
            theme_minimal(base_family = "sans", base_size =  12) +
            geom_text_repel(
                aes(label = country),
                segment.size = 0,
                fontface = "bold",
                hjust = 1,
                nudge_x = nudge_x,
                nudge_y = nudge_y,
                direction = "both",
                data = last_day
            ) +
            theme(legend.position = "none",
                  plot.title = element_text(face = "bold"),
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_line(colour = "gray95"))
        
    }
    
    # Case statistics -----
    # Create reactive data
    df_reactive_case <- eventReactive(input$make_plot_case, {
        req(input$to_include_case,
            input$date_case,
            input$day_range_case,
            input$case_day1)
        
        df_all %>%
            filter(country %in% input$to_include_case) %>%
            filter(date >= input$date_case[1] & date <= input$date_case[2]) %>% 
            # aggregate by country
            group_by(country, date) %>% 
            arrange(date) %>%
            summarise_at(vars(case), sum)
    }, ignoreNULL = F)
    
    plot_pars_case <- eventReactive(input$make_plot_case,{
      list(min_day = input$day_range_case[1],
           max_day = input$day_range_case[2],
           n_start = input$case_day1)
    }, ignoreNULL = F)
    
    # Case plot
    output$case_plot <- renderGirafe({
        df <- df_reactive_case()
        
        pars <- plot_pars_case()
        min_day <- pars$min_day
        max_day <- pars$max_day
        n_start <- pars$n_start
        
        df_plot <- make_df_plot(df, case, max_day, n_start) %>% 
            make_tooltip(case, n_start, "cases", "cases")
        
        plot <- make_plot(df_plot, case, min_day) +
            scale_y_continuous(trans = "log", breaks = exp_breaks, labels = scales::number_format(accuracy = 1)) +
            labs(title = glue("Number of cases"),
                 y = "Number of cases (log scale)",
                 x = glue("Day after first {n_start} cases"))
        
        girafe_mod(ggobj = plot)    
    })
    
    # Daily Case Growth
    output$case_growth <- renderGirafe({
        df <- df_reactive_case()
        
        pars <- plot_pars_case()
        min_day <- pars$min_day
        max_day <- pars$max_day
        n_start <- pars$n_start
        
        df_plot <- make_df_plot(df, case, max_day, n_start) %>% 
            mutate(case_growth = case / lag(case))  %>% 
            make_tooltip(case_growth, n_start, "cases", "daily case growth")
        
        # to_highlight <- input$to_highlight
        
        plot <- make_plot(df_plot, case_growth, min_day) +
            labs(title = glue("Daily case growth rate"),
                 y = "Daily case growth rate",
                 x = glue("Day after first {n_start} cases"))
        
        girafe_mod(ggobj = plot)
    })
    
    # Death statistics ------
    # Reactive data
    df_reactive_death <- eventReactive(input$make_plot_death, {
        req(input$to_include_death,
            input$date_death,
            input$day_range_death,
            input$death_day1)
        
        df_all %>% 
            filter(country %in% input$to_include_death) %>%
            filter(date >= input$date_death[1] & date <= input$date_death[2]) %>% 
            # aggregate by country
            group_by(country, date) %>% 
            arrange(date) %>%
            summarise_at(vars(case,death), sum)
    }, ignoreNULL = F)
    
    plot_pars_death <- eventReactive(input$make_plot_death,{
      list(min_day = input$day_range_death[1],
           max_day = input$day_range_death[2],
           n_start = input$death_day1)
    }, ignoreNULL = F)
    
    # Death plot
    output$death_plot <- renderGirafe({
        df <- df_reactive_death()
        
        pars <- plot_pars_death()
        min_day <- pars$min_day
        max_day <- pars$max_day
        n_start <- pars$n_start
        
        df_plot <- make_df_plot(df, death, max_day, n_start) %>% 
            make_tooltip(death, n_start, "deaths", "deaths")
        
        plot <- make_plot(df_plot, death, min_day) +
            scale_y_continuous(trans = "log", breaks = exp_breaks, labels = scales::number_format(accuracy = 1)) +
            labs(title = glue("Number of deaths"),
                 y = "Number of deaths (log scale)",
                 x = glue("Day after first {n_start} deaths"))
        
        girafe_mod(ggobj = plot)
    })
    
    # CFR plot
    output$cfr_plot <- renderGirafe({
        df <- df_reactive_death()
        
        pars <- plot_pars_death()
        min_day <- pars$min_day
        max_day <- pars$max_day
        n_start <- pars$n_start
        
        df_plot <- make_df_plot(df, death, max_day, n_start) %>% 
            mutate(cfr = death/case) %>% 
            make_tooltip(cfr, n_start, "deaths", "deaths")
        
        plot <- make_plot(df_plot, cfr, min_day) +
            labs(title = glue("Case fatality rate"),
                 y = "Case fatality rate",
                 x = glue("Day after first {n_start} cases"))
        
        girafe_mod(ggobj = plot)
    })
    
    # Daily Death Growth
    output$death_growth <- renderGirafe({
        df <- df_reactive_death()
        
        pars <- plot_pars_death()
        min_day <- pars$min_day
        max_day <- pars$max_day
        n_start <- pars$n_start
        
        df_plot <- make_df_plot(df, death, max_day, n_start) %>% 
            mutate(death_growth = death / lag(death)) %>% 
            make_tooltip(death_growth, n_start, "deaths", "daily death growth")
        

        plot <- make_plot(df_plot, death_growth, min_day) +
            labs(title = glue("Daily death growth rate"),
                 y = "Daily death growth rate",
                 x = glue("Day after first {n_start} deaths"))
        
        girafe_mod(ggobj = plot)
    })
    
    # Animation ------
    # Set global options
    options(gganimate.dev_args =
                list(width = 1280, height = 720, res = 150),
            gganimate.fps = 30,
            gganimate.nframes = 300,
            gganimate.duration = 15)
    
    # Reactive data
    df_reactive_anim <- eventReactive(input$animate,{
    # df_reactive_anim <- reactive({
      req(input$to_include_anim,
            input$date_anim,
            input$day_range_anim,
            input$case_day1_anim,
            input$death_day1_anim)
        
      df_all %>% 
            filter(country %in% input$to_include_anim) %>%
            filter(date >= input$date_anim[1] & date <= input$date_anim[2]) %>% 
            # aggregate by country
            group_by(country, date) %>% 
            arrange(date) %>%
            summarise_at(vars(case,death), sum)
    }, ignoreNULL = F)
      
        
    output$case_animation <- renderImage({
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext='.gif')

        # now make the animation
        df <- df_reactive_anim()

        min_day <- input$day_range_anim[1]
        max_day <- input$day_range_anim[2]
        n_start <- input$case_day1_anim

        df_plot <- make_df_plot(df, case, max_day, n_start)

        xlim <- c(min_day, max_day + 5)

        plot <- ggplot(df_plot, aes(x = day, y = case, group = country, color = country)) +
            scale_x_continuous(limits = xlim) +
            scale_y_continuous(trans = "log", breaks = exp_breaks, labels = scales::number_format(accuracy = 1))+
            scale_color_tableau("Tableau 20") +
            geom_line(size = 0.5) +
            geom_point(size = 0.8) +
            geom_text(aes(x = day + 0.2, label = country), hjust = 0) +
            theme_minimal(base_family = "sans", base_size =  11) +
            theme(legend.position = "none",
                  plot.title = element_text(face = "bold")) +
            labs(title = "Number of cases, day {frame_along}",
                 y = "Number of cases (log scale)",
                 x = glue("Day after first {n_start} cases")) +
            transition_reveal(day)

        anim_save("case_outfile.gif",
                  animate(plot))

        # Return a list containing the filename
        list(src = "case_outfile.gif",
             contentType = 'image/gif',
             width = getOption('gganimate.dev_args')$width/2,
             height = getOption('gganimate.dev_args')$height/2
        )}, deleteFile = TRUE)
    
    
    output$death_animation <- renderImage({
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext='.gif')
        
        # now make the animation
        df <- df_reactive_anim()
        
        min_day <- input$day_range_anim[1]
        max_day <- input$day_range_anim[2]
        n_start <- input$death_day1_anim
        
        df_plot <- make_df_plot(df, death, max_day, n_start)
        
        xlim <- c(min_day, max_day + 5)

        plot <- ggplot(df_plot, aes(x = day, y = death, group = country, color = country)) +
            scale_x_continuous(limits = xlim) +
            scale_y_continuous(trans = "log", breaks = exp_breaks, labels = scales::number_format(accuracy = 1))+
            scale_color_tableau("Tableau 20") +
            geom_line(size = 0.5) +
            geom_point(size = 0.8) +
            geom_text(aes(x = day + 0.2, label = country), hjust = 0) +
            theme_minimal(base_family = "sans", base_size =  11) +
            labs(title = "Number of deaths, day {frame_along}",
                 y = "Number of deaths (log scale)",
                 x = glue("Day after first {n_start} deaths")) +
            theme(legend.position = "none",
                  plot.title = element_text(face = "bold")) +
            transition_reveal(day)
        
        anim_save("death_outfile.gif",
                  animate(plot))
        
        # Return a list containing the filename
        list(src = "death_outfile.gif",
             contentType = 'image/gif',
             width = getOption('gganimate.dev_args')$width/2,
             height = getOption('gganimate.dev_args')$height/2
        )}, deleteFile = TRUE)
}
# For testing
input <- list(
    to_include_anim = to_include,
    date_anim = c(date_start, date_end),
    day_range_anim = c(1,40),
    day_range_anim = c(1,40),
    case_day1_anim = 100,
    death_day1_anim = 10,
    n_frame =  100,
    fps = 10,
    duration = 16
)


# Run the application 
shinyApp(ui = ui, server = server)
