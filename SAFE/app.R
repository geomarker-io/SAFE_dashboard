library(tidyverse)
library(shiny)
library(ggiraph)
library(bs4Dash)
library(fresh)
library(cicerone)
library(leaflet)
library(leafpop)

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
    el = 'map_tab_wrap',
    "Map",
    "Clicking this tab will bring up a map displaying meal coverage by neighborhood."
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

  tags$head(tags$style(type="text/css", "text {font-family: sans-serif}")),

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
    left = "System to Achieve Food Equity (SAFE) Meal Gap Dashboard",
    right = glue::glue("Data updated on ", format(Sys.Date(), "%m/%d/%Y"))
  ),

  controlbar = dashboardControlbar(
    id = "my_controlbar",
    controlbarMenu(
      id = "controlmenu",
      type = "tabs",
      controlbarItem(
        title = "About SAFE",
        column(width = 12,
               p("The System to Achieve Food Equity (SAFE) is a sub-network of All Children Thrive made up of individuals and organizations committed to improving food security in Cincinnati to ensure that all children have the food that they need to grow, develop, learn, and thrive. The SAFE Network includes emergency food organizations, healthcare, education, and government institutions, data scientists and families in the Cincinnati area.
                 The monthly, neighborhood-level meal gap is estimated by harmonzing data about the population, food need, and food supply from the U.S Census Bureau’s American Community Survey, Cincinnati Public School District, non-profit meal providers, Feeding America, and USDA. "),
               actionButton('safe_btn',
                            "More Information",
                            icon = icon("link"),
                            onclick = "window.open('https://actcincy.org/safe')"),
               hr(),
               actionButton("guide_btn", "Restart Guide",
                            icon = icon('circle-question')))
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
        ),
      div(
        id = 'map_tab_wrap',
        menuItem(
        text = "Map",
        tabName = "map_tab",
        icon = icon("map")
      ))
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
      ),
      tabItem(
        'map_tab',
          box(
            title = "Map",
            width = 12,
            height = 1000,
            status = "primary",
            solidHeader = T,
            leafletOutput('map',
                          height = 900)
        )
      )
    )
  )
)

server <- function(input,output,session){

  dat <- read_csv('monthly_all_sources_05sep2023.csv')

  d <- reactive({

    temp_d <- dat |>
      filter(SNA_NAME %in% c(input$neighborhood)) |>
      dplyr::select(neighborhood = 'SNA_NAME', month, year, contains('covered')) |>
      mutate(month = as.numeric(month)) |>
      mutate(month_abbr = month.abb[month]) |>
      distinct(neighborhood, year, month, .keep_all = T)

    temp_d$date <- as.Date(paste(temp_d$year, temp_d$month, 1, sep = "-"))

    temp_d <- temp_d |>
      pivot_longer(cols = contains('covered'), names_to = 'source', values_to = 'pct_covered')

    temp_d$source <- factor(temp_d$source, levels = c("meal_percent_income_covered",
                                                   "meal_percent_snap_covered",
                                                   "meal_percent_cps_covered",
                                                   "meal_percent_fsb_covered",
                                                   "meal_percent_lasoupe_covered",
                                                   "meal_percent_whole_again_covered",
                                                   "charitable_percent_covered"),
                            labels = c("Income",
                                       "SNAP",
                                       "CPS",
                                       "Free Store Foodbank",
                                       "La Soupe",
                                       "Whole Again",
                                       "Charitable"))

    temp_d
  })

  output$mealcoverage <- renderGirafe({

    d <- d() |>
      filter(!source %in% c("Free Store Foodbank",
                            "La Soupe",
                            "Whole Again"))

    meal_cover_plot <- ggplot(d) +
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
      annotate("segment", x = max(d()$date) + lubridate::days(30), xend = max(d()$date) + lubridate::days(30),
               y = .90, yend = 1.00,
               color = "#3c9459", linewidth = 1,
               arrow = arrow(type = "closed", length = unit(.3, "cm"))) +
      coord_cartesian(clip = "off", ylim = c(0, 1)) +
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
        mutate(meal_gap = -1 * (1 - sum(Income, SNAP, CPS, Charitable))) |>
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
      annotate("segment", x = max(d()$date) + lubridate::days(30), xend = max(d()$date) + lubridate::days(30),
               y = 0, yend = .06,
               color = "#3c9459", linewidth = 1,
               arrow = arrow(type = "closed", length = unit(.3, "cm"))) +
      coord_cartesian(clip = "off", ylim = c(-.4, .06), xlim = c(as.Date("2018-12-15", "%Y-%m-%d"), max(d()$date))) + #as.Date("2023-08-01", "%Y-%m-%d")
      #ggeasy::easy_move_legend("top") +
      annotate("rect", xmin = as.Date("2018-12-15", "%Y-%m-%d"), xmax = max(d()$date) + lubridate::days(30),
               ymin = .06, ymax = 0.0001, fill = "forestgreen", alpha = .2) +
      annotate("rect", xmin = as.Date("2018-12-15", "%Y-%m-%d"), xmax = max(d()$date) + lubridate::days(30),
               ymin = -0.0001, ymax = -.40, fill = "firebrick", alpha = .2)

    girafe(ggobj = meal_gap_plot, width_svg = 18,
           options = list(opts_hover(css = "stroke-width:5;"),
                          opts_hover_inv(css = "opacity:0.1;"),
                          opts_selection(type = "none"),
                          opts_zoom(max = 5)))
  })

  output$map <- renderLeaflet({

    d_map <- dat |>
      dplyr::select(neighborhood = 'SNA_NAME', month, year, contains('covered')) |>
      mutate(month = as.numeric(month)) |>
      mutate(month_abbr = month.abb[month]) |>
      distinct(neighborhood, year, month, .keep_all = T)

    d_map$date <- as.Date(paste(d_map$year, d_map$month, 1, sep = "-"))

    d_map <- d_map |>
      group_by(neighborhood) |>
      slice_max(date) |>
      ungroup()

    d_map <- d_map |>
      pivot_longer(cols = contains('covered'), names_to = 'source', values_to = 'pct_covered')

    d_map$source <- factor(d_map$source, levels = c("meal_percent_income_covered",
                                                      "meal_percent_snap_covered",
                                                      "meal_percent_cps_covered",
                                                      "meal_percent_fsb_covered",
                                                      "meal_percent_lasoupe_covered",
                                                      "meal_percent_whole_again_covered",
                                                      "charitable_percent_covered"),
                            labels = c("Income",
                                       "SNAP",
                                       "CPS",
                                       "Free Store Foodbank",
                                       "La Soupe",
                                       "Whole Again",
                                       "Charitable"))

    d_map_wide <- d_map |>
      pivot_wider(names_from = 'source', values_from = 'pct_covered') |>
      rowwise() |>
      mutate(meal_coverage = sum(Income, SNAP, CPS, Charitable)*100) |>
      ungroup()

    d_map_final <- d_map_wide |>
      left_join(cincy::neigh_sna, by = c("neighborhood" = "neighborhood")) |>
      sf::st_as_sf() |>
      sf::st_transform(crs = 4326)

    neighborhood_plot <- function(d){
      d |>
        filter(!source %in% c("Free Store Foodbank",
                              "La Soupe",
                              "Whole Again")) |>
        ggplot() +
        geom_bar(position = position_stack(reverse = TRUE), stat = "identity",
                             aes(fill = source, y = pct_covered, x = date)) +
        geom_hline(yintercept = 1, linewidth = .8, alpha = .5) +
        theme_minimal() +
        ggsci::scale_fill_jama() +
        labs(x = "", y = "Meal Coverage (% Meals Covered)", fill = "Meal Source") +
        scale_y_continuous(labels = scales::percent) +
        scale_x_date(date_labels = "%b %Y")
    }

    d_map_plots <- d_map |>
      group_nest(neighborhood) |>
      mutate(plot = map(data, neighborhood_plot))

    d_map_final <- d_map_final |>
      left_join(d_map_plots, by = 'neighborhood')

     d_map_final <- d_map_final |>
       mutate(lab = paste(neighborhood, "<br>",
                            "Meal coverage: ", round(meal_coverage,2),"%", sep = ""))

    pal <- colorNumeric(palette = "Blues", domain = d_map_final$meal_coverage)

    map <- leaflet(d_map_final) |>
      setView(-84.55, 39.18, zoom = 11.5) |>
      addProviderTiles(provider = providers$CartoDB.Positron) |>
      addPolygons(fillColor = ~pal(meal_coverage), fillOpacity = 0.85, stroke = T,
                  label = ~lapply(d_map_final$lab, HTML),
                  weight = .5, color = "#333333",
                  group = "neighborhood",
                  popup = popupGraph(d_map_final$plot)
                  )

    map


  })

  guide$init()$start()

  observeEvent(input$guide_btn, {
    guide$start()
  })
}

shinyApp(ui, server)



