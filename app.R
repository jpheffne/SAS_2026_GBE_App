###### GBE Shiny App visualization ######

## Info: This shiny app does a few simple visualizations of a subset of 
#        public data from the Great Brain Experiment (Rutledge, 2021, Dryad). 
#        Currently, there are scripts for fitting happiness models according to 
#        Rutledge et al., 2014, PNAS and plans to add prospect theory in the 
#        future. 

## Author: Joseph Heffner
## Last updated: March 13th, 2026


library(shiny)
library(tidyverse)

# ── Load data ──────────────────────────────────────────────
df <- read.csv("GBE_subset.csv") |>
  # Make a few columns helpful for plotting and formatting
  mutate(
    happiness_raw = as.numeric(happiness_raw),
    gamble_ev     = (risky_gain + risky_loss) / 2,
    gamble_type   = case_when(
      certain_reward == 0 ~ "mixed",
      certain_reward >  0 ~ "gain",
      certain_reward <  0 ~ "loss"
    )) |>
  group_by(sub) |>
  mutate(total_score = cumsum(outcome)) |>
  ungroup()


# Happiness model fits
model_preds  <- readRDS("model_preds.rds")
model_params <- readRDS("model_params.rds")

# Import the happiness model
source("happiness_model.R")

# Helpful functions
sem <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

# ── UI ─────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("Great Brain Experiment Analysis App"),
  
  sidebarLayout(
    sidebarPanel(
      # Keep the participant selector global since it affects all tabs
      selectInput("participant", "Select Participant:",
                  choices = unique(df$sub))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Design Matrix",         plotOutput("designPlot",    height = "500px")),

        # Happiness over trials with optional comparisons
        tabPanel("Happiness over Trials", 
                 fluidRow(
                   column(4,
                          wellPanel(
                            h4("Happiness comparisons"),
                            helpText("Compare the happiness ratings to total score or model fits."),
                            
                            checkboxInput("show_score",  "Show Cumulative Score", value = FALSE),
                            checkboxInput("show_model_fit", "Show Model Fit", value = FALSE),
                            
                            hr(),
                            p(strong("Note:")),
                            p("The Cumulative Score is scaled to fit the 0-100 happiness range for visual comparison.", 
                              style = "font-size: 0.85em; color: #666;")
                          )
                   ),
                   column(8, # Plot column
                          plotOutput("happinessPlot", height = "450px")
                   )
                 )
        ),
        
        tabPanel("Happiness by Outcome",  plotOutput("outcomePlot",   height = "450px")),
        tabPanel("Model Parameters",      plotOutput("paramPlot",      height = "450px")),
        tabPanel("Gambling by Domain",    plotOutput("gamblePlot",     height = "450px")),
        #tabPanel("Gambling Curves",       plotOutput("curvePlot",      height = "450px")),
        
        # Interactive Tab
        tabPanel("Parameter Playground",
                 fluidRow(
                   column(4,
                          wellPanel(
                            h4("Manual Parameter Tuning"),
                            helpText("Adjust sliders to see effect on model's predictions."),
                            sliderInput("play_a",     "Certain Weight (a):", -50, 50, 0, step = 1),
                            sliderInput("play_b",     "EV Weight (b):",      -50, 50, 0, step = 1),
                            sliderInput("play_c",     "RPE Weight (c):",     -50, 50, 0, step = 1),
                            sliderInput("play_gamma", "Decay (gamma):",        0,  1, 0.5, step = 0.05),
                            sliderInput("play_const", "Baseline (const):",     0, 100, 50)
                          )
                   ),
                   column(8,
                          plotOutput("playgroundPlot", height = "500px"),
                          div(style = "text-align: center; margin-top: 20px;",
                              h3(textOutput("sse_val"), style = "color: #d62728; font-weight: bold;")
                          )
                   )
                 )
        ),
        
        # SSE space
        tabPanel("SSE Space",
                 fluidRow(
                   column(4,
                          wellPanel(
                            h4("SSE Surface Mapping"),
                            helpText("Visualize the graident for two parameters."),
                            
                            selectInput("param_x", "X-Axis Parameter:", 
                                        choices = c("certain", "ev", "rpe", "gamma", "const"), selected = "certain"),
                            selectInput("param_y", "Y-Axis Parameter:", 
                                        choices = c("certain", "ev", "rpe", "gamma", "const"), selected = "rpe"),
                            
                            hr(),
                            p("The map shows the SSE for combinations of two parameters."),
                            p("Other parameters held constant and pulled from the 'Playground' tab.")
                          )
                   ),
                   column(8,
                          plotOutput("sseSurfacePlot", height = "550px")
                   )
                 )
        )
      )
    )
  )
)
# ── Server ─────────────────────────────────────────────────
server <- function(input, output, session) { # 'session' is required to update sliders
  
  # Reactive: trials with happiness ratings + model predictions joined
  participant_data <- reactive({
    df |>
      filter(sub == input$participant,
             !is.na(happiness_raw)) |>
      left_join(model_preds |> select(sub, trial, happy_pred),
                by = c("sub", "trial"))
  })
  
  # Reactive: all trials for this participant
  participant_all <- reactive({
    df |> filter(sub == input$participant)
  })
  
  # Automatically update sliders to the subject's best-fit parameters
  observeEvent(input$participant, {
    p <- model_params |> filter(sub == input$participant)
    if (nrow(p) > 0) {
      updateSliderInput(session, "play_a",     value = p$certain)
      updateSliderInput(session, "play_b",     value = p$ev)
      updateSliderInput(session, "play_c",     value = p$rpe)
      updateSliderInput(session, "play_gamma", value = p$gamma)
      updateSliderInput(session, "play_const", value = p$const)
    }
  })
  
  # ── Plot 1: Design matrix ─────────────────────────────────
  output$designPlot <- renderPlot({
    
    d <- participant_all() |>
      distinct(certain_reward, gamble_ev, gamble_type)
    
    ggplot(d, aes(x = certain_reward, y = gamble_ev, color = gamble_type)) +
      geom_point(size = 3, alpha = 0.8) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
      geom_vline(xintercept = 0, linetype = "solid", color = "gray50") +
      scale_color_manual(
        name   = "Gamble Domain",
        values = c("gain"  = "#2ca02c",
                   "mixed" = "#ff7f0e",
                   "loss"  = "#d62728")
      ) +
      labs(title = paste("Participant:", input$participant),
           x     = "Safe Outcome",
           y     = "Gamble EV") +
      theme_classic(base_size = 20) +
      theme(plot.title   = element_text(size = 24, face = "bold"))
  })
  
  # ── Plot 2: Happiness over trials ────────────────────────
  output$happinessPlot <- renderPlot({
    
    d     <- participant_data()
    d_all <- participant_all()
    
    scale_factor <- 100 / diff(range(d_all$total_score, na.rm = TRUE))
    shift        <- -min(d_all$total_score, na.rm = TRUE) * scale_factor
    
    d_score <- d_all |>
      mutate(score_scaled = total_score * scale_factor + shift)
    
    d_long <- d |>
      select(trial, happiness_raw, happy_pred) |>
      pivot_longer(cols      = c(happiness_raw, happy_pred),
                   names_to  = "measure",
                   values_to = "value") |>
      mutate(measure = recode(measure,
                              "happiness_raw" = "Happiness",
                              "happy_pred"    = "Model Fit"))
    
    p <- ggplot() +
      geom_line(data = d_long |> filter(measure == "Happiness"),
                aes(x = trial, y = value, color = "Happiness"),
                linewidth = 1.2) +
      geom_point(data = d,
                 aes(x = trial, y = happiness_raw, color = "Happiness"),
                 size = 3,
                 show.legend = FALSE)
    
    if (input$show_score) {
      p <- p + geom_line(data = d_score,
                         aes(x = trial, y = score_scaled, color = "Score"),
                         linewidth = 1.2)
    }
    
    if (input$show_model_fit) {
      p <- p + geom_line(data = d_long |> filter(measure == "Model Fit"),
                         aes(x = trial, y = value, color = "Model Fit"),
                         linewidth = 1.2)
    }
    
    p +
      scale_color_manual(
        name   = NULL,
        values = c("Happiness" = "#1f77b4",
                   "Model Fit" = "#d62728",
                   "Score"  = "#2ca02c"),
        breaks = c("Happiness", "Model Fit", "Score")
      ) +
      scale_y_continuous(
        name     = "Happiness",
        limits   = c(0, 100),
        sec.axis = sec_axis(~ (. - shift) / scale_factor, name = "Cumulative Score")
      ) +
      labs(title = paste("Participant:", input$participant), x = "Trial") +
      theme_classic(base_size = 20) +
      theme(
        axis.title.y.left  = element_text(color = "#1f77b4"),
        axis.text.y.left   = element_text(color = "#1f77b4"),
        axis.title.y.right = element_text(color = "#2ca02c"),
        axis.text.y.right  = element_text(color = "#2ca02c"),
        plot.title         = element_text(size = 24, face = "bold"),
        legend.position    = "inside",
        legend.position.inside = c(0.2, 0.2), 
        legend.background  = element_rect(fill = alpha("white", 0.7), color = NA),
        legend.text        = element_text(size = 20)
      )
  })
  
  # ── Plot 3: Happiness by outcome type ────────────────────
  output$outcomePlot <- renderPlot({
    
    d <- participant_all() |>
      mutate(outcome_type = case_when(
        choice == 1 & outcome > 0  ~ "Win",
        choice == 1 & outcome <= 0 ~ "Lose",
        choice == 0                ~ "Safe"
      )) |>
      filter(!is.na(outcome_type)) |>
      mutate(outcome_type = factor(outcome_type, levels = c("Lose", "Win", "Safe"))) |>
      group_by(outcome_type) |>
      summarise(
        mean_happy = mean(happiness_fill_raw, na.rm = TRUE),
        sem_happy  = sem(happiness_fill_raw),
        .groups    = "drop"
      )
    
    ggplot(d, aes(x = outcome_type, y = mean_happy, fill = outcome_type)) +
      geom_col(width = 0.5, alpha = 0.85) +
      geom_errorbar(aes(ymin = mean_happy - sem_happy,
                        ymax = mean_happy + sem_happy),
                    width = 0.15, linewidth = 0.8) +
      scale_fill_manual(
        values = c("Win"  = "#2ca02c",
                   "Lose" = "#d62728",
                   "Safe" = "#1f77b4")
      ) +
      coord_cartesian(ylim = range(c(d$mean_happy - d$sem_happy - 5,
                                     d$mean_happy + d$sem_happy + 5))) +
      labs(title = paste("Participant:", input$participant),
           x = "Outcome", y = "Happiness (fill)") +
      theme_classic(base_size = 16) +
      theme(plot.title      = element_text(size = 18, face = "bold"),
            legend.position = "none")
  })
  
  # ── Plot 4: Model parameters ──────────────────────────────
  output$paramPlot <- renderPlot({
    
    params <- model_params |>
      filter(sub == input$participant) |>
      select(certain, ev, rpe, gamma, const) |>
      pivot_longer(everything(), names_to = "parameter", values_to = "value") |>
      mutate(parameter = factor(parameter,
                                levels = c("certain", "ev", "rpe", "gamma", "const"),
                                labels = c("Certain", "EV", "RPE", "Gamma", "Const")))
    
    ggplot(params, aes(x = parameter, y = value, fill = parameter)) +
      geom_col(width = 0.6, alpha = 0.85) +
      geom_hline(yintercept = 0, linewidth = 0.6,
                 linetype = "dashed", color = "gray40") +
      scale_fill_brewer(palette = "Set2") +
      labs(title = paste("Participant:", input$participant),
           x = NULL, y = "Parameter Value") +
      theme_classic(base_size = 16) +
      theme(legend.position = "none",
            plot.title      = element_text(size = 18, face = "bold"))
  })
  
  # ── Plot 5: Gambling by domain ────────────────────────────
  output$gamblePlot <- renderPlot({
    
    d <- participant_all() |>
      mutate(gamble_type = factor(gamble_type, levels = c("gain", "mixed", "loss"),
                                  labels = c("Gain", "Mixed", "Loss"))) |>
      group_by(gamble_type) |>
      summarise(
        prop_gamble = mean(choice, na.rm = TRUE),
        sem_gamble  = sem(choice),
        .groups     = "drop"
      )
    
    ggplot(d, aes(x = gamble_type, y = prop_gamble, fill = gamble_type)) +
      geom_col(width = 0.5, alpha = 0.85) +
      geom_errorbar(aes(ymin = prop_gamble - sem_gamble,
                        ymax = prop_gamble + sem_gamble),
                    width = 0.15, linewidth = 0.8) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40") +
      scale_fill_manual(
        values = c("Gain"  = "#2ca02c",
                   "Mixed" = "#ff7f0e",
                   "Loss"  = "#d62728")
      ) +
      scale_y_continuous(limits = c(0, 1),
                         labels = scales::percent_format()) +
      labs(title = paste("Participant:", input$participant),
           x = "Gamble Domain", y = "Proportion Gambled") +
      theme_classic(base_size = 16) +
      theme(plot.title      = element_text(size = 18, face = "bold"),
            legend.position = "none")
  })
  
  # ── Plot 6: Gambling curves by ratio quartile ─────────────
  output$curvePlot <- renderPlot({
    
    d <- participant_all() |>
      mutate(
        ratio = case_when(
          gamble_type == "gain"  ~ risky_gain / certain_reward,
          gamble_type == "loss"  ~ abs(risky_loss) / abs(certain_reward),
          gamble_type == "mixed" ~ risky_gain / abs(risky_loss)
        )
      ) |>
      filter(!is.na(ratio), is.finite(ratio)) |>
      mutate(gamble_type = factor(gamble_type,
                                  levels = c("gain", "mixed", "loss"),
                                  labels = c("Gain", "Mixed", "Loss"))) |>
      group_by(gamble_type) |>
      mutate(ratio_bin = {
        breaks <- unique(quantile(ratio, probs = seq(0, 1, 0.25), na.rm = TRUE))
        cut(ratio,
            breaks         = breaks,
            include.lowest = TRUE,
            labels         = seq_len(length(breaks) - 1))  # dynamic labels
      }) |>
      group_by(gamble_type, ratio_bin) |>
      summarise(
        prop_gamble = mean(choice, na.rm = TRUE),
        sem_gamble  = sem(choice),
        .groups     = "drop"
      ) |>
      filter(!is.na(ratio_bin))
    
    ggplot(d, aes(x = ratio_bin, y = prop_gamble,
                  color = gamble_type, group = gamble_type)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 4) +
      #geom_errorbar(aes(ymin = prop_gamble - sem_gamble,
      #                  ymax = prop_gamble + sem_gamble),
      #              width = 0.15, linewidth = 0.8) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40") +
      scale_color_manual(
        name   = "Gamble Domain",
        values = c("Gain"  = "#2ca02c",
                   "Mixed" = "#ff7f0e",
                   "Loss"  = "#d62728")
      ) +
      scale_y_continuous(limits = c(0, 1),
                         labels = scales::percent_format()) +
      labs(title = paste("Participant:", input$participant),
           x     = "Gamble Value Ratio (Quartile)",
           y     = "Proportion Gambled") +
      theme_classic(base_size = 16) +
      theme(plot.title   = element_text(size = 18, face = "bold"),
            legend.title = element_text(size = 14),
            legend.text  = element_text(size = 13))
  })
  
  # ── Plot 7: Parameter Playground ──────────────────────────
  
  # Reactive calculation using slider inputs
  play_results <- reactive({
    p_vec <- c(input$play_a, input$play_b, input$play_c, input$play_gamma, input$play_const)
    # This calls happiness function from happiness_model.R file
    happiness_model(p_vec, participant_all())
  })
  
  output$playgroundPlot <- renderPlot({
    res <- play_results()
    
    plot_df <- tibble(
      trial = res$happy_ind,
      obs   = res$happy_obs,
      pred  = res$happy_pred
    )
    
    ggplot(plot_df, aes(x = trial)) +
      # Vertical error lines (Residuals)
      geom_segment(aes(xend = trial, y = obs, yend = pred), 
                   color = "#d62728", linewidth = 1, alpha = 0.6) +
      # Observed data
      geom_point(aes(y = obs), color = "#1f77b4", size = 4) +
      # Slider-based predictions
      geom_point(aes(y = pred), color = "black", shape = 21, 
                 fill = "white", size = 3, stroke = 1.2) +
      geom_line(aes(y = pred), color = "black", linetype = "dotted") +
      labs(title = "Minimizing the Error (SSE)",
           y = "Happiness", x = "Trial") +
      ylim(0, 100) +
      theme_classic(base_size = 16)
  })
  
  output$sse_val <- renderText({
    res <- play_results()
    paste("Total SSE:", format(round(res$sse, 0), big.mark = ","))
  })
  
  
  # ── Plot 8: SSE Surface (Gradient Space) ──────────────────
  output$sseSurfacePlot <- renderPlot({
    # Fixed resolution
    res_val <- 40  
    zoom_val <- 20 
    
    # Get current baseline from the playground sliders
    base_params <- c(certain = input$play_a, ev = input$play_b, 
                     rpe = input$play_c, gamma = input$play_gamma, 
                     const = input$play_const)
    
    # Range function
    get_range <- function(p_name, val) {
      if(p_name == "gamma") {
        return(seq(max(0, val - 0.2), min(1, val + 0.2), length.out = res_val))
      }
      return(seq(val - zoom_val, val + zoom_val, length.out = res_val))
    }
    
    x_vals <- get_range(input$param_x, base_params[[input$param_x]])
    y_vals <- get_range(input$param_y, base_params[[input$param_y]])
    
    # Create grid and calculate SSE
    grid_df <- expand.grid(x = x_vals, y = y_vals)
    
    grid_df$sse <- apply(grid_df, 1, function(row) {
      tmp_params <- base_params
      tmp_params[[input$param_x]] <- row[["x"]]
      tmp_params[[input$param_y]] <- row[["y"]]
      # Happiness model from source file
      happiness_model(as.numeric(tmp_params), participant_all())$sse
    })
    
    ggplot(grid_df, aes(x = x, y = y, fill = sse)) +
      geom_tile() +
      scale_fill_gradientn(
        colors = c("#00007F", "blue", "#007FFF", "cyan", 
                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
        name = "SSE"
      ) +
      geom_vline(xintercept = base_params[[input$param_x]], linetype = "dashed", color = "white", alpha = 0.5) +
      geom_hline(yintercept = base_params[[input$param_y]], linetype = "dashed", color = "white", alpha = 0.5) +
      geom_point(aes(x = base_params[[input$param_x]], y = base_params[[input$param_y]]), 
                 color = "white", size = 5, shape = 3, stroke = 1.5) +
      labs(title = paste("Optimization Surface:", input$param_x, "vs", input$param_y),
           subtitle = "Blue valleys show better model fits for that parameter combination",
           x = paste(input$param_x, "Value"),
           y = paste(input$param_y, "Value")) +
      theme_minimal(base_size = 16)
  })
}

shinyApp(ui, server)