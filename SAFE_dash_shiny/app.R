library(tidyverse)
library(shiny)
library(ggiraph)
library(bs4Dash)
library(fresh)
library(cicerone)

#theming
theme <- create_theme(
  bs4dash_vars(
    navbar_light_color = "#f18c3f",
    navbar_light_active_color = "#f18c3f",
    navbar_light_hover_color = "#FFF",
    card_bg = "#FFF"
  ),
  bs4dash_yiq(
    contrasted_threshold = 1,
    text_dark = "#FFF",
    text_light = "black"
  ),
  bs4dash_layout(
    main_bg = "#f9cd9e"
  ),
  bs4dash_sidebar_light(
    bg = "#a53437",
    header_color = "#f18c3f"
  ),
  bs4dash_sidebar_dark(
    bg = "#a53437",
    header_color = "#f18c3f"
  ),
  bs4dash_status(
    primary = "#3c9459",
    success = "#dc5d61",
    info = "#9b8172",
    danger = "#f18c3f",
    light = "#edaaac",
    dark = "#edaaac"
  ),
  bs4dash_color(
    gray_900 = CB::cchmc_color(4)[[1]],
    white = dht::degauss_colors(4)
  )
)

#guided tour---
guide <- Cicerone$
  new(allow_close = TRUE)$
  step(
    el = 'title_wrap',
    "Tour of the SAFE Dashboard",
    paste0("This is a guided tour of the SAFE Dashboard. You can close this now or continue with the tour. In the menu on the right side of the page (",icon("circle-info"),") is a button to restart this tutorial.")
  )$
  step(
    el = 'mealcoverage',
    "Meal Coverage Plot",
    "This panel displays the monthly meal coverage for the selected neighborhoods. Hover over a meal source to compare it across neighborhoods.",
  )$
  step(
    el = 'coversidebar',
    "Select Neighborhoods",
    "Click this button to add/remove neighborhoods from both plots",
    position = "left"
    )$
  step(
    el = 'mealgap',
    "Meal Gap Plot",
    "This panel displays the monthly meal gap for the selected neighborhoods. Hover over a line to highlight that neighborhood. Neighborhoods displayed are the same as those selected in the Meal Coverage Plot above.",
  )$
  step(
    el = "control_wrap",
    "More Information",
    "Click this button to view disclaimers or restart this tour.",
    position = "left"
  )


#get neighborhood list
neighborhood_list <- read_csv('monthly_all_sources_2_.csv') |>
  distinct(SNA_NAME)

ui <- dashboardPage(
  preloader = list(html = tagList(waiter::spin_refresh(), "Loading ..."), color = "#edaaac"),
  fullscreen = TRUE,
  freshTheme = theme,
  dark = NULL,

  use_cicerone(),

  tags$head( tags$style(type="text/css", "text {font-family: sans-serif}")),

  header = dashboardHeader(
    title = div(
      id = "title_wrap",
      dashboardBrand(
      title = "SAFE Meal Gap Dashboard",
      color = "danger")
    ),
    controlbarIcon = div(id = "control_wrap", icon("circle-info"))
  ),

  footer = dashboardFooter(
    left = "System to Achieve Food Equity (SAFE) Meal Gap Dashboard"
  ),

  controlbar = dashboardControlbar(
    id = "my_controlbar",
    controlbarMenu(
      id = "controlmenu",
      type = "tabs",
      controlbarItem(
        title = "Disclaimers:",
        column(width = 12,
               p("i). We acknowledge that ACS Census data is limited and may not be completely representative of the population in all situations."),
               actionButton("guide_btn", "Restart Guide"))
      )
    )
  ),

  sidebar = dashboardSidebar(
    id = "sidebar",
    sidebarMenu(
      id = "sidebarmenu",
      menuItem(
        text = "Meal Gap",
        tabName = "meal_gap_tab",
        icon = icon("utensils")
        )
    ),
    div(img(src = "SAFE_logo.png", width = '100%'),
        style = "position: absolute; bottom: 0; left: 0;
        border-radius: 1pt;
        border: 2px solid #FFF;")
  ),

    body = dashboardBody(

      tags$head( tags$style(type="text/css", "text {font-family: sans-serif}")),

    tabItems(
      tabItem("meal_gap_tab",
              fluidRow(
                box(
                  title = "Meal Coverage",
                  width = 12,
                  status = "primary",
                  solidHeader = T,
                  girafeOutput('mealcoverage'),
                  sidebar = boxSidebar(
                    startOpen = TRUE,
                    id = 'coversidebar',
                    width = 33,
                    selectizeInput(
                      'neighborhood',
                      choices = neighborhood_list$SNA_NAME,
                      selected = c('Avondale', 'East Price Hill', 'West Price Hill', 'Lower Price Hill'),
                      label = "Select Neighborhoods",
                      multiple = TRUE,
                      options = list(maxItems = 5)
                    )
                  )
                )),
              fluidRow(
                box(title = "Meal Gap",
                    width = 12,
                    status = "primary",
                    solidHeader = T,
                    girafeOutput('mealgap'))
              )
      )
    )
  )
)

server <- function(input,output,session){

  d <- reactive({
    dat <- read_csv('monthly_all_sources_3.csv')

    temp_d <- dat |>
      filter(SNA_NAME %in% c(input$neighborhood)) |>
      dplyr::select(neighborhood = 'SNA_NAME', month, year, contains('covered')) |>
      mutate(month = as.numeric(month)) |>
      mutate(month_abbr = month.abb[month]) |>
      distinct(neighborhood, year, month, .keep_all = T)

    temp_d$date <- as.Date(paste(temp_d$year, temp_d$month, 1, sep = "-"))

    temp_d <- temp_d |>
      pivot_longer(cols = contains('covered'), names_to = 'source', values_to = 'pct_covered') |>
      filter(source != 'charitable_percent_covered')

    temp_d$source <- factor(temp_d$source, levels = c("meal_percent_income_covered",
                                                   "meal_percent_snap_covered",
                                                   "meal_percent_cps_covered",
                                                   "meal_percent_fsb_covered",
                                                   "meal_percent_lasoupe_covered",
                                                   "meal_percent_whole_again_covered"),
                            labels = c("Income",
                                       "SNAP",
                                       "CPS",
                                       "Free Store Foodbank",
                                       "La Soupe",
                                       "Whole Again"))

    temp_d
  })

  output$mealcoverage <- renderGirafe({

    meal_cover_plot <- ggplot(d()) +
      geom_hline(yintercept = 1, linewidth = .5, alpha = .5) +
      geom_bar_interactive(position = position_stack(reverse = TRUE), stat = "identity",
                           aes(fill = source, y = pct_covered, x = date,
                               data_id = source,
                               tooltip = paste0(source, "<br>",
                                                month_abbr, " ", year, "<br>",
                                                round(pct_covered*100,1), "%"))) +
      theme_minimal() +
      ggsci::scale_fill_jama() +
      labs(x = "", y = "Meal Coverage (% Meals Covered)", fill = "Meal Source") +
      scale_y_continuous(labels = scales::percent) +
      scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
      ggeasy::easy_rotate_x_labels(angle = -60) +
      facet_wrap(~neighborhood, nrow = 1)

    girafe(ggobj = meal_cover_plot, width_svg = ifelse(length(input$neighborhood) == 5, 24, 18),
           options = list(opts_hover(css = "stroke-width:5;"),
                          opts_hover_inv(css = "opacity:0.2;"),
                          opts_sizing(rescale = TRUE, width = 1),
                          opts_selection(type = "none"),
                          opts_zoom(max = 5)))

  })

  output$mealgap <- renderGirafe({

      d_2 <- reactive({
        d() |>
        pivot_wider(names_from = 'source', values_from = 'pct_covered') |>
        rowwise() |>
        mutate(meal_gap = -1 * (1 - sum(Income, SNAP, CPS, `Free Store Foodbank`, `La Soupe`, `Whole Again`))) |>
        ungroup()
    })

    meal_gap_plot <- ggplot(d_2(), aes(x = date, y = meal_gap, col = neighborhood)) +
      geom_line_interactive(aes(data_id = neighborhood, group = neighborhood)) +
      geom_point_interactive(#position = "dodge",
        aes(data_id = neighborhood, group = neighborhood,
            tooltip = paste(neighborhood, "<br>",
                            month_abbr, " ", year, "<br>",
                            round(meal_gap,2)*100, "%"))) +
      geom_hline(yintercept = 0, linewidth = .5) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal() +
      ggsci::scale_color_aaas() +
      labs(x = "", y = "Meal Gap (% Meals Short)", col = "Neighborhood") +
      scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
      #ggeasy::easy_move_legend("top") +
      annotate("rect", xmin = as.Date("2018-12-15", "%Y-%m-%d"), xmax = as.Date("2023-04-01", "%Y-%m-%d"),
               ymin = .06, ymax = 0.0001, fill = "forestgreen", alpha = .2) +
      annotate("rect", xmin = as.Date("2018-12-15", "%Y-%m-%d"), xmax = as.Date("2023-04-01", "%Y-%m-%d"),
               ymin = -0.0001, ymax = -.40, fill = "firebrick", alpha = .2)

    girafe(ggobj = meal_gap_plot, width_svg = 18,
           options = list(opts_hover(css = "stroke-width:5;"),
                          opts_hover_inv(css = "opacity:0.1;"),
                          opts_selection(type = "none"),
                          opts_zoom(max = 5)))
  })

  guide$init()$start()

  observeEvent(input$guide_btn, {
    guide$start()
  })

}

shinyApp(ui, server)



