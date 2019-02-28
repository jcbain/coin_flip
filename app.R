library(shiny)
library(tidyr)
library(dplyr)
library(nessy)
library(ggplot2)

flips <- crossing(trial = 1:1000, flip = 1:100) %>%
  mutate(heads = rbinom(n(), 1, .5)) %>% 
  group_by(trial) %>%
  mutate(next_flip = lead(heads),
         hh = heads & next_flip,
         ht = heads & !next_flip)


ui <- cartridge(
  title = "{nessy}",
  container_with_title(
    title = "Buttons",
    button("Normal", "Flip"),
    button_primary("Primary", "Primary"),
    button_success("Success", "Success"),
    button_warning("Warning", "Warning"),
    button_error("Error", "Error"),
    radio_buttons("sure", "Are you sure?", c("yes", "no"))
  ),
  cartridge(title= "something"),
  container_with_title(
    title = "params",
    text_input("pick_number", "Pick-a-Number", value = "10"), 
    container(plotOutput("plot2"))
    
  ),
  cartridge(title = "flipity"),
  container(
    container(imageOutput("flip_image")),
    button_primary("flip", "flipity")
  )
)

server <- function(input, output, session) {
  
  flip_count <- reactiveVal(1)
  
  # Actions that occur when flip button is pushed
  ## takes the first n rows determined by flip_count()
  ## increments flip_count() by 1
  flip_event <- eventReactive(input$flip, {
    flip <- reactiveVal(flips %>% head(flip_count()))
    flip_count(flip_count() + 1L)
    
    flip()
  })
  
  
  tmp_dat <- reactive({
    tmp_flips <- crossing(trial = 1:input$pick_number, 
                          flip = 1:100) %>%
      mutate(heads = rbinom(n(), 1, .5)) %>% 
      group_by(trial) %>%
      mutate(next_flip = lead(heads),
             hh = heads & next_flip,
             ht = heads & !next_flip) %>%
      summarize(first_hh = which(hh)[1] + 1,
                first_ht = which(ht)[1] + 1) %>%
      summarize(first_hh = mean(first_hh), first_ht = mean(first_ht))
    
    tmp_flips
    })
  
  output$plot2<-renderPlot({
    ggplot(tmp_dat()) +
      geom_bar(aes("first hh", first_hh), stat = 'identity') +
      geom_bar(aes("first ht", first_ht), stat = 'identity') +
      theme_bw()
    })
  
  output$flip_image <- renderImage({
    if (is.null(flip_event()))
      return(NULL)
    if (flip_event() %>% tail(1) %>% pull(heads) == 0) {
      return(list(
        src = "face.png",
        contentType = "image/png",
        alt = "Face"
      ))
    } else {
      return(list(
        src = "second.jpg",
        filetype = "image/jpeg",
        alt = "This is a chainring"
      ))
    }}, deleteFile = FALSE)
    
}

shiny::shinyApp(ui, server)
