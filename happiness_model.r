# ── Happiness model ───────────────────────────────────────────────────────────
happiness_model <- function(x, sub_data) {
  
  # Free parameters
  a     <- x[1]  # certain weight
  b     <- x[2]  # EV weight
  c     <- x[3]  # RPE weight
  gamma <- x[4]  # decay of prior history
  const <- x[5]  # baseline constant
  
  # Indices of trials which had happiness ratings
  happy_ind  <- which(!is.na(sub_data$happiness_raw))
  happy_obs  <- sub_data$happiness_raw[happy_ind]
  
  if (length(happy_obs) < 2) return(list(sse = 1e10))
  
  n_ratings <- length(happy_ind)
  n_trials  <- nrow(sub_data)
  
  # Matrix of values for each trial, aligned for happiness model
  certain_mtx <- matrix(0, nrow = n_ratings, ncol = n_trials)
  ev_mtx      <- matrix(0, nrow = n_ratings, ncol = n_trials)
  rpe_mtx     <- matrix(0, nrow = n_ratings, ncol = n_trials)
  
  for (m in seq_len(n_ratings)) {
    t <- sub_data[1:happy_ind[m], ]
    
    temp_certain <- (t$choice == 0) * t$outcome
    temp_ev      <- (t$choice == 1) * (t$risky_gain + t$risky_loss) / 2
    temp_rpe     <- (t$choice == 1) * (t$outcome - temp_ev)
    
    certain_mtx[m, 1:happy_ind[m]] <- rev(temp_certain) / 100
    ev_mtx[m, 1:happy_ind[m]]      <- rev(temp_ev)      / 100
    rpe_mtx[m, 1:happy_ind[m]]     <- rev(temp_rpe)     / 100
  }
  
  # Happiness model is an integration of the history of prior events 
  # Apply decay or forgetting parameter
  decay_vec  <- gamma ^ (0:(n_trials - 1))
  happy_pred <- a * certain_mtx %*% decay_vec +
    b * ev_mtx %*% decay_vec +
    c * rpe_mtx %*% decay_vec + const
  
  if (any(!is.finite(happy_pred))) return(list(sse = 1e10))
  
  # Likelihood function
  # Goal is to minimize the difference between the model's happiness predictions
  # and the participant's true happiness ratings by adjusting the 
  # free parameters
  sse <- sum((happy_obs - happy_pred)^2)
  sst <- sum((happy_obs - mean(happy_obs))^2)
  r2  <- 1 - sse / sst
  
  # Return information
  list(sse        = sse,
       sst        = sst,
       happy_pred = as.vector(happy_pred),
       happy_obs  = happy_obs,
       r2         = r2,
       happy_ind  = happy_ind)
}