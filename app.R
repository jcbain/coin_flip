library(shiny)
library(tidyr)
library(dplyr)
library(nessy)
library(ggplot2)
library(gridExtra)
library(purrr)

# ================
# HELPER FUNCTIONS
# ================
#
#' Function to optimize the number of flips necessary per trial.
#' 
#' @param m The mean number of flips.
#' @param s The variance.
#' @param z The z-score.
#' @return numeric vaalue for the simulated number of flips necessary to ensure
#'   the condition is met.
optimize_flips <- function(m, s, z){
  z * s + m
}

#' Solve for the mean number of flips for a coin.
#' 
#' @param n_heads The number of heads wanted in a row.
#' @param prob_heads The probability of tossing a heads.
#' @return The analytical mean number of heads.
analyze_flips <- function(n_heads, prob_heads) {
  (prob_heads ^ (- n_heads) - 1)/(1 - prob_heads)
}

#' Generate flips for any number of heads for any weigted coin.
#' 
#' @param n_heads The number of heads wanted in a row.
#' @param prob_heads The probability of tossing a heads.
#' @return a tibble with simulated flips.
flip_generator <- function(n_heads = 2, prob_heads = .5){
  mean_flips <- analyze_flips(n_heads, prob_heads)
  max_flips <- round(optimize_flips(mean_flips, mean_flips, 8))
  df <- crossing(trial = 1:1000, flip = 1:max_flips) %>% 
    mutate(heads = rbinom(n(), 1, prob_heads)) %>%
    group_by(trial)
  
  if (n_heads == 2){
    df <- df %>% mutate(next_flip = lead(heads))
  }else{
    new_cols <- map(1: (n_heads -1), ~ df %>%
                      select(heads) %>% mutate(lead(heads, .x)) %>%
                      ungroup() %>%
                      select(`lead(heads, .x)`) %>%
                      set_names(paste0("next_flip", .x))) %>% 
      bind_cols()
    df <- bind_cols(df, new_cols)
  }
  df %>% gather(flip_ind, hort, -c(trial, flip)) %>% 
    group_by(trial, flip) %>% summarize(condition_met = prod(hort)) %>%
    ungroup %>%
    group_by(trial) %>% summarize(first_cond = which(as.logical(condition_met))[1] + (n_heads - 1))
}
# ==========
# DATA 
# ==========
# 
# flip simulation data for heads and heads
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

# =============
# GLOBAL PARAMS
# =============
#
# Set the max number of flips to track
# TODO: This currently needs to be extended to manipulate the DOM
max_tracked_flip <- 15


ui <- cartridge(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "static/style.css")
  ),
  cartridge("The Heads-Heads Paradox",
            container_with_title(title = "The paradox", 
                                 title_tag = "p", 
                       "Let's say you have a fair coin, one side is heads, the 
                       other is tails. Now, when flipping this coin, how many 
                       times would you expect to flip the coin before you get a heads
                       and then another heads?...

                       That's the thing, on average it will take 6 flips. Think
                       about that for a second. Now think about it after considering
                       that, on average, it only takes four flips to get a heads then
                       a tails. 

                       Not wanting to work out the math but also don't want to take
                       my word for it? Let's run some simulations then...
                       Below is a fair coin flipper with a little added encouragment 
                       from Cory the dog. Press the 'Flip the Coin' button below..."
                       ),
            container(
              map(1:max_tracked_flip, .f = function(x){
                imageOutput(paste0("flip_tracker_", x),
                  width = "2%",
                  inline = TRUE,
                  height = "20px")
                })
              ),
            container(id = "dog_box",
                      uiOutput("dog_sprite"),
                      button_primary("flip_button", "Flip the Coin")
                      ),
            container(id = "flip_images",
                      container(uiOutput("flip_image", height = "175px", width = "175px")),
                      container(plotOutput("flip_collection", width = "100%"))),
            container(
                      container(withMathJax(),
                                helpText("Of course, this has an analytical solution
                                              if you are into those sort of things. Without
                                              getting into too much detail (because this is
                                              more about learning by simulation), but to find the average
                                              number of flips \\(X\\) for
                                              two heads in a row $$X_2 = (1 + p)/p^2$$ where \\(p\\) 
                                              is the probability of getting a heads in one flip. In 
                                              the case above the coin is fair and therefore \\(p=0.5\\). 
                                              Solving for this formula will result in a value of \\(6\\), 
                                              which is, of course, our average number of flips it takes 
                                              to get two heads in a row.")
                                ),
                      container(withMathJax(),
                                helpText("With a little bit more work, this can be generalized to solve for any
                                         number of \\(X_n\\) in a row with a slight tweak in the expression 
                                         $$X = (p^{-n}-1)/(1-p)$$ where \\(n\\) is the number of heads in a row.
                                         Again, we can simulate this below:"))
                      ),
  container(id = "simulation_plots",
    container(id = "num_pick",
              text_input("pick_number", "Number of heads in a row:", value = "2"),
              width = "30%"
    ),
    container(id = "prob_pick",
    text_input("pick_prob", "Probability of heads:", value = ".5"),
    width = "30%"
    ),
    container(plotOutput("plot2", width = "100%"))
  ), 
  tags$p(id = "closing",
    "App created by",
    tags$a(style = "color: #ff6e54;", href = "https://twitter.com/gymbrane", target = "_blank", HTML("&commat;gymbrane")),
    "with a huge shout out to",
    HTML(paste0(
      tags$a(style = "color: #ff6e54;", href = "https://twitter.com/drob", target = "_blank", HTML("&commat;drob")), 
      " and his ",
      tags$a(style = "color: #ff6e54;", href = "https://twitter.com/drob/status/1008409373423611904?ref_src=twsrc%5Etfw%7Ctwcamp%5Etweetembed%7Ctwterm%5E1008939111925809153&ref_url=https%3A%2F%2Fjcbain.github.io%2Fportfolio%2Fcoin-flip-probs-go%2F", 
                    "tweet", target = "_blank"), 
      " that inspired it all, along with ",
      tags$a(style = "color: #ff6e54;", href = "https://shiny.rstudio.com", "Shiny", target = "_blank"), " for providing the framework, and ",
      tags$a(style = "color: #ff6e54;", href = "https://github.com/ColinFay/nessy", target = "_blank", "nessy"), " for providing the NES theme!")
    ),
    "And if you are looking for a more detailed dive into derving the analytical solution, this ",
    tags$a(style = "color: #ff6e54;", href = "https://mindyourdecisions.com/blog/2015/02/16/monday-puzzle-two-heads-in-a-row/", "post", target = "_blank"),
    " by Presh Talwalkar is super helpful!"
  )
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

  # filter by trial
  trial_data <- reactive({
    trial_flips <- flips %>% filter(trial == trial_index())
    
    trial_flips
  })
  
  # generate the simulated data
  sim_data <- reactive({
    pick_number <- as.numeric(input$pick_number)
    pick_prob <- as.numeric(input$pick_prob)
    flip_sims <- flip_generator(pick_number, pick_prob)
    flip_sims
  })
  
  # find the analy
  analytical_mean <- reactive({
    pick_number <- as.numeric(input$pick_number)
    pick_prob <- as.numeric(input$pick_prob)
    num <- analyze_flips(pick_number, pick_prob)
    num
  })
  
  # Actions that occur when flip button is pushed
  ## takes the first n rows determined by flip_count()
  ## increments flip_count() by 1
  flip_event <- eventReactive(input$flip_button, {
    flip <- reactiveVal(trial_data() %>% head(flip_count()))
    flip_count(flip_count() + 1L)
    
    #trial_reset()
    flip()
  })
  
  flip_gatherer <- eventReactive(input$flip_button, {
    flip_index <- paste0("flip_", flip_count2())
    flip <- reactiveVal(trial_data()[flip_count2(),] %>% pull(heads))
    all_flips[[flip_index]] <- flip()
    flip_count2(flip_count2() + 1L)
    #trial_reset()
    all_flips
  })
  
  
  observeEvent(input$flip_button, {
    hh <- trial_data() %>% head(flip_count3()) %>% pull(hh)
    ht <- trial_data() %>% head(flip_count3()) %>% pull(ht)
    if (flip_count3()  > 1){
      if (condition_met()){
        trial_index(trial_index() + 1L)
        flip_count(1L)
        flip_count2(1L)
        flip_count3(1L)
        flip_count4(1L)
        flip_count5(1L)
      
        purrr::map(1:max_tracked_flip, .f = function(x){
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
      text <- textings$sayings[1]
    }else if(flip_count5() <= 16){
      rand_index <- sample(7)[1]
      text <- textings$sayings[rand_index]
    } else{
      text <- paste0(textings$sayings[8], " ", flip_count5(), " times!")
    }
    text
  })
  
  output$plot2<-renderPlot({
    dat <- sim_data()
    observed_mean <- mean(dat$first_cond, na.rm = T)
    analytic_mean <- analytical_mean()
    ggplot(data = sim_data()) +
      geom_histogram(aes(first_cond), fill = "#58508d") +
      geom_vline(xintercept = observed_mean, color = "#003f5c") +
      geom_vline(xintercept = analytic_mean, linetype = "dashed", color = "#ff6361") + 
      labs(x = "number of flips", y = "number of occurences") +
      #geom_label(aes(x = 20, y = 350), label = paste0("Analytical Mean: ", analytic_mean), color = "#ff6361", label.size = NA) +
      #geom_label(aes(x = 20, y = 325),label = paste0("Observed Mean: ", observed_mean), color = "#003f5c", label.size = NA)
      ggtitle(paste0("Average number of times it takes to get ", input$pick_number, " in a row with a probability of ", input$pick_prob, " throwing a heads."),
              subtitle = paste0("Analytical Mean: ", analytic_mean, "; Observed Mean: ", observed_mean)) +
      theme_bw() +
      theme(axis.title = element_text(color = "#ffa600"),
            title = element_text(color = "#ffa600", 
                                 size = 15))
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
    plt <- ggplot(data = tmp) + 
      geom_vline(xintercept = 6, color = "#ff6361", linetype = "dashed", alpha = .7) +
      geom_vline(xintercept = 4, color = "#58508d", linetype = "dashed", alpha = .7) + 
      geom_histogram(aes(x = first_hh), 
                     fill = "#ff6361",
                     binwidth = 1,
                     alpha = .7) + 
      geom_histogram(aes(x = first_ht), fill = "#58508d",
                     binwidth = 1,
                     alpha = .7) +
      geom_vline(xintercept = hh_mean, color = "#ff6361") +
      geom_vline(xintercept = ht_mean, color = "#58508d") + 
      geom_label(aes(hh_mean, y = 18), 
                 label = paste0("mean hh: ", round(hh_mean, 2)), 
                 label.size = NA,
                 color = "#ff6361") +
      geom_label(aes(ht_mean, y = 16), 
                 label = paste0("mean ht: ", round(ht_mean, 2)), 
                 label.size = NA,
                 color = "#58508d") +
      geom_label(aes(x = 6, y = 20), 
                 label = "6", 
                 label.size = NA,
                 color = "#ff6361") +
      geom_label(aes(x = 4, y = 20), 
                 label = "4", 
                 label.size = NA,
                 color = "#58508d") +
      ggtitle("The Paradox in Simulations", 
              subtitle = "Histograms of Occurences of HH and HT vs. the Number of Flips") + 
      labs(x = "number of flips", y = "number of occurences") +
      theme_bw() + 
      theme(axis.title = element_text(color = "#ffa600"),
            title = element_text(color = "#ffa600"))
    
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
      ggtitle("Yeah...but is this a fair coin?", subtitle = "Percentage of Heads vs. Tails") +
      labs(x = "number of flips", y = "percentage") + 
      theme_bw() + 
      theme(axis.title = element_text(color = "#ffa600"),
            title = element_text(color = "#ffa600")) + 
      scale_y_continuous(labels = scales::percent)
    
    plt + theme(legend.position = c(0.8, 0.8),
                legend.background = element_rect(fill= alpha('white', 0.4)))
  })
  output$flip_collection <- renderPlot({
    ptlist <- list(flip_collection(), hnt_count())
    grid.arrange(grobs=ptlist, ncol=length(ptlist))
  })
  
  # function to choose heads or tails image
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
  map(1:max_tracked_flip, .f = function(x){
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
               style = "margin-right: 100px; width: 175px;")
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
  

  output$dog_sprite <- renderUI({
    tagList(
      balloon(flip_saying(), side = "left", style = "margin-left: 60px;"),
      tags$br(),
      tags$img(src = "chi_right.gif", filetype = "image/gif", height = "120px", 
               style = "margin-left: 20px; width: 120px;")
    )
  })
  
}

shiny::shinyApp(ui, server)
