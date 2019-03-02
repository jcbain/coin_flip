library(shiny)
library(tidyr)
library(dplyr)
library(nessy)
library(ggplot2)
library(gridExtra)
library(purrr)

# stat flip set
flips <- crossing(trial = 1:1000, flip = 1:100) %>%
  mutate(heads = rbinom(n(), 1, .5)) %>% 
  group_by(trial) %>%
  mutate(next_flip = lead(heads),
         hh = heads & next_flip,
         ht = heads & !next_flip)

# sayings for the smoking chihuahua 
textings <- tibble(sayings = c('Flip!!!', 'Again! Again!', 'Wowza!', 
                               'Flip again!', "I'm betting heads next!",
                               'Tails is in your future!', 'Is this coin weighted?',
                               "There's a heads and another heads!", 
                               "Look! A heads and then a tails!",
                               "Whoa! Flip number "))

ui <- cartridge(
  "Flip Flip",
  cartridge(
  cartridge(title = "flipity"),
  container(
    map(1:15, .f = function(x){
      imageOutput(paste0("flip_tracker_", x),
                  width = "2%",
                  inline = TRUE,
                  height = "20px")
    })
  ),
    
  #   imageOutput("flip_tracker_1", width = "2%", inline = T, height = "20px"),
  #   imageOutput("flip_tracker_2", width = "2%", inline = T, height = "20px"),
  #   imageOutput("flip_tracker_3", width = "2%", inline = T, height = "20px"),
  #   imageOutput("flip_tracker_4", width = "2%", inline = T, height = "20px"),
  #   imageOutput("flip_tracker_5", width = "2%", inline = T, height = "20px"),
  #   imageOutput("flip_tracker_6", width = "2%", inline = T, height = "20px"),
  #   imageOutput("flip_tracker_7", width = "2%", inline = T, height = "20px"),
  #   imageOutput("flip_tracker_8", width = "2%", inline = T, height = "20px"),
  #   imageOutput("flip_tracker_9", width = "2%", inline = T, height = "20px"),
  #   imageOutput("flip_tracker_10", width = "2%", inline = T, height = "20px"),
  #   imageOutput("flip_tracker_11", width = "2%", inline = T, height = "20px"),
  #   imageOutput("flip_tracker_12", width = "2%", inline = T, height = "20px"),
  #   imageOutput("flip_tracker_13", width = "2%", inline = T, height = "20px"),
  #   imageOutput("flip_tracker_14", width = "2%", inline = T, height = "20px"),
  #   imageOutput("flip_tracker_15", width = "2%", inline = T, height = "20px")
  # ),
  container(
    uiOutput("doge_image"),
    button_primary("flip", "flipity")
  ),
  container(
    container(uiOutput("flip_image", height = "175px", width = "175px") ),
    container(plotOutput("flip_collection", width = "100%")),
  container_with_title(
    title = "params",
    text_input("pick_number", "Pick-a-Number", value = "10"), 
    container(plotOutput("plot2", width = "100%"))
  ))
  


  
))

server <- function(input, output) {
  heads <- "www/heads.gif"
  tails <- "www/tails.gif"
  
  all_flips <- reactiveValues('flip_1' = 'empty', 'flip_2' = 'empty', 
                              'flip_3' = 'empty', 'flip_4' = 'empty',
                              'flip_5' = 'empty', 'flip_6' = 'empty',
                              'flip_7' = 'empty', 'flip_8' = 'empty',
                              'flip_9' = 'empty', 'flip_10' = 'empty',
                              'flip_11' = 'empty', 'flip_12' = 'empty',
                              'flip_13' = 'empty', 'flip_14' = 'empty',
                              'flip_15' = 'empty')
  # current flip count
  flip_count <- reactiveVal(1)
  flip_count2 <- reactiveVal(1)
  flip_count3 <- reactiveVal(1)
  flip_count4 <- reactiveVal(1)
  flip_count5 <- reactiveVal(1)
  trial_index <- reactiveVal(1)
  condition_met <- reactiveVal(FALSE)

  
  trial_data <- reactive({
    trial_flips <- flips %>% filter(trial == trial_index())
    
    trial_flips
  })
  
  
  # Actions that occur when flip button is pushed
  ## takes the first n rows determined by flip_count()
  ## increments flip_count() by 1
  flip_event <- eventReactive(input$flip, {
    flip <- reactiveVal(trial_data() %>% head(flip_count()))
    flip_count(flip_count() + 1L)
    
    #trial_reset()
    flip()
  })
  
  flip_gatherer <- eventReactive(input$flip, {
    flip_index <- paste0("flip_", flip_count2())
    flip <- reactiveVal(trial_data()[flip_count2(),] %>% pull(heads))
    all_flips[[flip_index]] <- flip()
    flip_count2(flip_count2() + 1L)
    #trial_reset()
    all_flips
  })
  
  
  observeEvent(input$flip, {
    hh <- trial_data() %>% head(flip_count3()) %>% pull(hh)
    ht <- trial_data() %>% head(flip_count3()) %>% pull(ht)
    if (flip_count3()  > 1){
      if (condition_met()){
        trial_index(trial_index() + 1L)
        flip_count(1L)
        flip_count2(1L)
        flip_count3(1L)
        flip_count4(1L)
      
        purrr::map(1:15, .f = function(x){
          v <- paste0("flip_", x)
          all_flips[[v]] <- "empty"
          })
        condition_met(FALSE)
        }
      else if((any(hh[1:flip_count3()-1]) == T & any(ht[1:flip_count3()-1]) == T)){
        condition_met(TRUE)
        }}
    flip_count3(flip_count3() + 1L)
    flip_count4(flip_count4() + 1L)
    flip_count5(flip_count5() + 1L)
    
  })
  
  flip_saying <- reactive({
    if (flip_count5() == 1){
      text = textings$sayings[1]
    }else{
      rand_index <- sample(6)[1]
      text = textings$sayings[rand_index]
    }
    text
  })
  
  # Generate a series of random flips
  ## used in bar chart section
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
  
  # Generate the plots for the individual filps
  flip_collection <- reactive({
    pre_tmp <- flips %>% filter(trial < trial_index())
    cur_tmp <- trial_data() %>% head(flip_count4() -1)
    tmp <- bind_rows(pre_tmp, cur_tmp) %>% 
      group_by(trial) %>% summarize(first_hh = which(hh)[1] + 1,
                                    first_ht = which(ht)[1] + 1)
    hh_mean <- mean(tmp$first_hh, na.rm = T)
    ht_mean <- mean(tmp$first_ht, na.rm = T)
    plt <- ggplot(data = tmp) + geom_histogram(aes(x = first_hh), 
                                               fill = "#bc5090",
                                               binwidth = 1,
                                               alpha = .5) + 
      geom_histogram(aes(x = first_ht), fill = "#ffa600",
                     binwidth = 1,
                     alpha = .5) +
      geom_vline(xintercept = hh_mean, color = "#bc5090") +
      geom_vline(xintercept = ht_mean, color = "#ffa600") + 
      theme_bw()
    
    plt + lims(x= c(0, 15), y = c(0,20))
  })
  hnt_count <- reactive({
    pre_tmp <- flips %>% filter(trial < trial_index()) %>% group_by(trial) %>%
      mutate(lag_ht= (lag(ht)), lag_hh = lag(hh)) %>%
      mutate(lag_ht = case_when(is.na(lag_ht) ~ FALSE, 
                                !is.na(lag_ht) ~ lag_ht), 
             lag_hh = case_when(is.na(lag_hh) ~ FALSE, 
                                !is.na(lag_hh) ~ lag_hh)) %>% 
      mutate(cs_laght = cumsum(lag_ht), cs_laghh = cumsum(lag_hh)) %>% 
      mutate(lagmult = cs_laghh * cs_laght) %>% 
      mutate(cs_mat= case_when(lagmult == 0 ~ FALSE, lagmult > 0 ~ TRUE)) %>% 
      mutate(cs_mat = cumsum(cs_mat)) %>% filter(cs_mat <= 1)
    cur_tmp <- trial_data() %>% head(flip_count4() -1)
    tmp <- bind_rows(pre_tmp, cur_tmp) %>% ungroup() %>%
      mutate(cumsum_heads = cumsum(heads), n = row_number()) %>% 
      mutate(cumsum_tails = n - cumsum_heads) %>% 
      ungroup %>% select(n, cumsum_heads, cumsum_tails) %>% 
      gather(side, count, -n)
    plt <- ggplot(data = tmp) + geom_area(aes(x = n, y = count, fill = side), position = "fill") +
      geom_hline(yintercept = .5) +
      scale_fill_manual(labels = c("heads", "tails"), values = c("#DAA520", "#C0C0C0")) +
      theme_bw()
    
    plt + theme(legend.position = c(0.8, 0.8),
                legend.background = element_rect(fill= alpha('white', 0.4)))
  })
  output$flip_collection <- renderPlot({
    ptlist <- list(flip_collection(), hnt_count())
    grid.arrange(grobs=ptlist, ncol=length(ptlist))
  })
  
  choose_heads_tails <- function(n){
    flip_num <- paste0("flip_", n)
    if(flip_gatherer()[[flip_num]] == "empty"){
      img_list <- list(
        src = "www/grey-circle-png-1.png",
        filetype = "image/png",
        width = 40,
        height = 40,
        alt = "blank_roll"
      )
    }else if(flip_gatherer()[[flip_num]] == 0){
        img_list <- list(
          src = tails,
          filetype = "image/gif",
          width = 40,
          height = 40,
          alt = "heads"
        )
    }else{
        img_list <- list(
          src = heads,
          filetype = "image/gif",
          width = 40,
          height = 40,
          alt = "tails"
        )
    }
    img_list
  }
  
  # dynamically create the coin tracker html object
  map(1:15, .f = function(x){
    tag_id <- paste0("flip_tracker_", x)
    output[[tag_id]] <- renderImage({choose_heads_tails(x)}, deleteFile = FALSE)
  })
  
  
  flip_image <- reactive({
    if(is.null(flip_event())){
      pic <- NULL
    }else if(flip_event() %>% tail(1) %>% pull(heads) == 0){
      pic <- "tails.gif"
    } else{
      pic <- "heads.gif"
    }
    pic
  })
  
  output$flip_image <- renderUI({
    tags$div(class = "flip_image_div", height = "800px",
      tags$img(src = flip_image(), filetype = "image/gif", height = "175px", 
               style = "margin-right: 100px; width: 175px; float: right;")
    )
    })
    
  
  # output$flip_image <- renderImage({
  #   if (is.null(flip_event())){
  #     return(NULL)
  #   }else if (flip_event() %>% tail(1) %>% pull(heads) == 0) {
  #     return(list(
  #       src = tails,
  #       filetype = "image/png",
  #       width = 128,
  #       height = 128,
  #       alt = "heads"
  #     ))
  #   } else {
  #     return(list(
  #       src = heads,
  #       filetype = "image/gif",
  #       width = 128,
  #       height = 128,
  #       alt = "tails"
  #     ))
  #   }}, deleteFile = FALSE)
  

  output$doge_image <- renderUI({
    tagList(
      balloon(flip_saying(), side = "left", style = "margin-left: 60px;"),
      tags$br(),
      tags$img(src = "chi_right.gif", filetype = "image/gif", height = "120px", 
               style = "margin-left: 20px; width: 120px; float: left;")
    )
  })
  
}

shiny::shinyApp(ui, server)
