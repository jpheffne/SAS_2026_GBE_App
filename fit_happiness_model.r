###### Happiness model ######

## Info: This function fits a computational model of happiness according to 
#        Rutledge et al., 2014, PNAS using a subset of public data from the 
#        Great Brain Experiment. 

## Author: Joseph Heffner
## Last updated: March 11th, 2026


library(tidyverse)

# ── Load data ─────────────────────────────────────────────────────────────────
df <- read.csv("GBE_subset.csv")

# ── Happiness model ───────────────────────────────────────────────────────────
source("happiness_model.R")

# ── Fit model to one subject's data ───────────────────────────────────────────
fit_subject <- function(sub_data) {
  
  obj_fn <- function(x) happiness_model(x, sub_data)$sse
  
  # Par: initial starting values for parameters (a, b, c, gamma, const)
  #      note: for beginner simplicity we use neutral starting values, 
  #            but ideally we would use random starting values and repeat 
  #            model fitting multiple times per subject
  fit <- optim(
    par    = c(0, 0, 0, 0.5, 50),
    fn     = obj_fn,
    method = "L-BFGS-B",
    lower  = c(-100, -100, -100, 0,   -100),  # lower bound for parameters
    upper  = c( 100,  100,  100, 1,    100)   # upper bound for parameters
  )
  
  result <- happiness_model(fit$par, sub_data)
  
  tibble(
    sub     = sub_data$sub[1],
    certain = fit$par[1],
    ev      = fit$par[2],
    rpe     = fit$par[3],
    gamma   = fit$par[4],
    const   = fit$par[5],
    r2      = result$r2,
    sse     = result$sse
  )
}

# ── Fit all subjects and save results ─────────────────────────────────────────
subjects <- unique(df$sub)

model_params <- map_dfr(subjects, \(s) {
  cat("Fitting subject", s, "\n")
  fit_subject(df |> filter(sub == s))
})

model_preds <- map_dfr(subjects, \(s) {
  sub_data <- df |> filter(sub == s)
  params   <- model_params |> filter(sub == s)
  result   <- happiness_model(
    c(params$certain, params$ev, params$rpe, params$gamma, params$const),
    sub_data
  )
  
  tibble(
    sub        = s,
    trial      = result$happy_ind,
    happy_obs  = result$happy_obs,
    happy_pred = result$happy_pred
  )
})

saveRDS(model_params, "model_params.rds")
saveRDS(model_preds,  "model_preds.rds")