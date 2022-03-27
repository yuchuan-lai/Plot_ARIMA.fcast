library(shiny)
library(ggplot2)
library(dplyr)

fcast_plot <- function(city.ID, climate.var, select.st.yr) {
  
  city.name <- cities.list$Name[cities.list$ID %in% city.ID]
  select.var <- c(1:length(var.list))[var.list %in% climate.var]
  cliamte.var.ID <- var.ID[select.var]
  
  if (select.var <= 11) {
    my.colors <- RColorBrewer::brewer.pal(5, "Oranges")
    fill.colors <- c("#FD8D3C70", "#FDBE8570")
    col.colors <- c("#A63603", "#E6550D", "#FD8D3C")
  } else {
    my.colors <- RColorBrewer::brewer.pal(5, "Blues")
    fill.colors <- c("#6BAED6", "#BDD7E7")
    col.colors <- c("#08519C", "#3182BD", "#6BAED6")
  }
  
  fcast.all <- readRDS(paste0("./Data/", city.name, ".rds"))
  fcast.var <- fcast.all[[cliamte.var.ID]]
  
  fcast.df <- fcast.var[[paste0("st.", select.st.yr)]]
  
  fcast_2022.df <- fcast.var[[paste0("st.", 2022)]]
  
  y.limit.upp <- max(fcast.df$obs, fcast.df$all.obs, fcast_2022.df$upp95, fcast_2022.df$upp80, na.rm = TRUE)
  y.limit.low <- min(fcast.df$obs, fcast.df$all.obs, fcast_2022.df$low95, fcast_2022.df$low80, na.rm = TRUE)
  
  theme_set(theme_bw())
  
  fcast.p <- ggplot2::ggplot() + 
    ggplot2::geom_point(data = filter(fcast.df, is.na(obs) == FALSE), ggplot2::aes(x = year, y = obs, color = my.colors[4]), size = 2) +
    ggplot2::geom_point(data = filter(fcast.df, is.na(all.obs) == FALSE), ggplot2::aes(x = year, y = all.obs), color = paste0(my.colors[4], "60"), size = 2) +
    ggplot2::geom_line(data = filter(fcast.df, is.na(hist.fit) == FALSE), ggplot2::aes(x = year, y = hist.fit, color = my.colors[3]), size = 1) +
    ggplot2::geom_ribbon(data = filter(fcast.df, is.na(low95) == FALSE), ggplot2::aes(x = year, ymin = low95, ymax = upp95, fill = paste0(my.colors[2], "80"))) +
    ggplot2::geom_ribbon(data = filter(fcast.df, is.na(low80) == FALSE), ggplot2::aes(x = year, ymin = low80, ymax = upp80, fill = paste0(my.colors[3], "80"))) +
    ggplot2::geom_line(data = filter(fcast.df, is.na(median) == FALSE), ggplot2::aes(x = year, y = median, color =  my.colors[5]), size = 1) +
    ggplot2::scale_fill_manual(guide = ggplot2::guide_legend(override.aes = list(linetype = c("blank", "blank"), shape = c(NA, NA))),
                               labels = c("80%", "95%"), values = c(fill.colors) , name = "Prediction intervals") +
    ggplot2::scale_color_manual(guide = ggplot2::guide_legend(override.aes = list(linetype = c("blank", "solid", "solid"), shape = c(16, NA, NA), 
                                                                                  size = c(3, 1.5, 1.5),
                                                                                  color = c(my.colors[4], my.colors[3], my.colors[5]))),
                                labels = c("Historical observations", "ARIMA fitting", "Point forecasts"),
                                values = c(col.colors), name = " ") +
    xlab("Year") +
    ylab(paste0(var.unit[select.var])) +
    # scale_y_continuous(breaks = seq(80, 110, 2)) +
    coord_cartesian(xlim = c(1870, 2040), ylim = c(y.limit.low, y.limit.upp)) +
    scale_x_continuous(breaks = seq(1860, 2060, 20)) +
    # geom_vline(xintercept = 2022, linetype = "longdash") +
    theme(plot.title = element_text(size = 11), legend.position = "bottom", legend.key.size = unit(0.75, 'cm'), legend.text = element_text(size = 10)) + 
    ggtitle(paste0("The ARIMA forecasts starting from ", select.st.yr))
  
  return(fcast.p)
}
title_text <- function(climate.var, city.ID){
  en.dash <- substr(signs::signs(-2, accuracy = 1), 1, nchar(signs::signs(-2, accuracy = 1)) - 1)
  city.print.name <- paste0(cities.list$P.name[cities.list$ID %in% city.ID], " ", cities.list$State.ID[cities.list$ID %in% city.ID])
  print.text <- paste0(climate.var, " ", en.dash, " ", city.print.name)
  
  return(print.text) 
}
fcast_prepare <- function(city.ID, climate.var, select.st.yr) {
  
  city.name <- cities.list$Name[cities.list$ID %in% city.ID]
  select.var <- c(1:length(var.list))[var.list %in% climate.var]
  cliamte.var.ID <- var.ID[select.var]
  
  if (select.var <= 11) {
    my.colors <- RColorBrewer::brewer.pal(5, "Oranges")
    fill.colors <- c("#FD8D3C70", "#FDBE8570")
    col.colors <- c("#A63603", "#E6550D", "#FD8D3C")
  } else {
    my.colors <- RColorBrewer::brewer.pal(5, "Blues")
    fill.colors <- c("#6BAED6", "#BDD7E7")
    col.colors <- c("#08519C", "#3182BD", "#6BAED6")
  }
  
  fcast.all <- readRDS(paste0("./Data/", city.name, ".rds"))
  fcast.var <- fcast.all[[cliamte.var.ID]]
  
  fcast.df <- fcast.var[[paste0("st.", select.st.yr)]]
  
  fcast.df <- data.frame("Year" = fcast.df$year, "Observations" = fcast.df$all.obs, "ARIMA model Fitting" = round(fcast.df$hist.fit, digits = 3), 
                         "Point forecasts" = round(fcast.df$median, digits = 3), 
                         "Upper bound of 80% intervals" = round(fcast.df$upp80, digits = 3), "Lowe bound of 80% intervals" = round(fcast.df$low80, digits = 3),
                         "Upper bound of 95% intervals" = round(fcast.df$upp95, digits = 3), "Lowe bound of 95% intervals" = round(fcast.df$low95, digits = 3))
  
  colnames(fcast.df)[2] <- paste0("Observations: " , var.unit[select.var]) 
  colnames(fcast.df)[4] <- paste0("Point forecasts: starting from " , select.st.yr) 
  
  return(fcast.df)
}
dl_file.name <- function(climate.var, city.ID, select.st.yr){
  city.print.name <- paste0(cities.list$P.name[cities.list$ID %in% city.ID], " ", cities.list$State.ID[cities.list$ID %in% city.ID])
  print.text <- paste0(climate.var, " ", city.print.name, " (20-year forecasts starting from ", select.st.yr, ")")
  return(print.text) 
}

stn.list <- read.csv("./Data/Stn.List.93cities.csv")[-c(1)]
states.list <- unique(stn.list$State)
cities.list <- stn.list[!duplicated(stn.list$ID),]
var.ID <- c("Avg.Temp", "TXx", "TNx", "TXn", "TNn", "SU", "TR", "ID", "FD", "CoDD", "HDD", "Tot.Prcp", "Rx1day", "Rx5day", "R10mm", "R20mm")
var.list <- c("Annual average temperature", "Annual warmest daily Tmax", "Annual warmest daily Tmin", "Annual coldest daily Tmax", "Annual coldest daily Tmin",
              "Annual count of days when daily Tmax > 25ºC (77ºF)", "Annual count of days when daily Tmin > 20ºC (68ºF)", "Annual count of days when daily Tmax < 0ºC (32ºF)", "Annual count of days when Tmin <0 ºC (32ºF)",
              "Annual cooling degree days", "Annual heating degree days", 
              "Annual total precipitation", "Annual maximum 1 day precipitation", "Annual maximum consecutive 5-day precipitation", 
              "Annual count of days when Precip ≥ 10mm (0.39in.)", "Annual count of days when Precip ≥ 20mm (0.79in.)")
var.unit <- c("Temperature (ºF)", "Temperature (ºF)", "Temperature (ºF)", "Temperature (ºF)", "Temperature (ºF)", 
              "Count of days", "Count of days", "Count of days", "Count of days", "ºF-days", "ºF-days", "Precipitation (in.)", "Precipitation (in.)", "Precipitation (in.)", 
              "Count of days", "Count of days")

ui <- fluidPage(
  titlePanel("The ARIMA forecasts of annual temperature and precipitation indices at selected U.S. cities"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select the state", choices = states.list, selected = states.list[1]),
      selectInput("city", "Select the city", choices = cities.list$P.name[cities.list$State == states.list[1]], selected = cities.list$P.name[2]),
      selectInput("var", "Select the annual climate variable", choices = var.list, selected = var.list[1]),
      sliderInput(inputId = "st.yr", label = "Starting year for the 20-year forecasts:", min = 1977, max = 2022, step = 5, value = 1977, ticks = TRUE),
      downloadButton("download", "Download")
    ),
    mainPanel(
      h3(textOutput("select.var")),
      shinycssloaders::withSpinner(
        plotOutput("plot"), 
        hide.ui = FALSE, type = 3, color = "#666666", color.background = "#FFFFFF"
      )
    )
  )
  
)

server <- function(input, output, session) {
  
  observeEvent(input$state, {
    
    x <- input$state
    updateSelectInput(session, "city",
                      choices = cities.list$P.name[cities.list$State == x],
                      selected = cities.list$P.name[cities.list$State == x][1])
  })
  
  
  d <- reactive({
    cities.list$ID[cities.list$P.name == input$city]   
  }) 
  
  output$plot <- renderPlot({
    fcast_plot(d(), input$var, input$st.yr)
  })
  
  output$select.var <- renderText(title_text(input$var, d()))
  
  output$download <- downloadHandler(
    filename = function() {
      paste(dl_file.name(input$var, d(), input$st.yr), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(fcast_prepare(d(), input$var, input$st.yr), file)
    }
  )
}

shinyApp(ui = ui, server = server)


