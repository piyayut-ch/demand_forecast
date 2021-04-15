if (!require("xfun")) install.packages("xfun")
pkgs <- c(
  'forecast', 'vars', 'urca', 'MLmetrics', 'yardstick',
  'lubridate', 'tsbox', 'timetk', 'rlist', 'here',
  'extrafont', 'patchwork', 'hrbrthemes', 'ggthemes', 'ggsci', 'scales', 
  'tidyverse', 'vroom', 'readxl', 'writexl', 'tsibble', "tidymodels"
)
xfun::pkg_attach2(pkgs, message = FALSE)


#################### UNIT ROOT TEST ####################
# get object name as a string
get_objname <- function(x) deparse(substitute(x))

# unit root test for a time series
ur_adf <- function(y, n_diff = 0, varname = NULL, ...) {
  
  varname <- if (is.null(varname)) deparse(substitute(y)) else varname
  y <- if(n_diff == 0) y else diff(y, n_diff)
  
  ur.trend <- ur.df(y, type='trend', lags = 10, selectlags = "BIC", ...)
  ur.drift <- ur.df(y, type='drift', lags = 10, selectlags = "BIC", ...)
  ur.none  <- ur.df(y, type='none' , lags = 10, selectlags = "BIC", ...)

  tstat.trend <- ur.trend@teststat
  tstat.drift <- ur.drift@teststat
  tstat.none  <- ur.none@teststat

  cv.trend <- ur.trend@cval
  cv.drift <- ur.drift@cval
  cv.none  <- ur.none@cval
  
  lags.trend <- dim(ur.trend@testreg$coefficients)[1] - 3
  lags.drift <- dim(ur.drift@testreg$coefficients)[1] - 2
  lags.none  <- dim(ur.none@testreg$coefficients)[1]  - 1

  df_test <- rbind(
    cbind(t(tstat.trend), cv.trend, lags.trend),
    cbind(t(tstat.drift), cv.drift, lags.drift),
    cbind(t(tstat.none) , cv.none , lags.none)
  ) %>% 
    as.data.frame() %>%
    rename(c("lag" = "lags.trend")) %>%
    rownames_to_column("hypo") %>%
    filter(str_starts(hypo, 'tau')) %>%
    mutate(
      result = ifelse(abs(statistic) >= abs(`5pct`), 'Reject', 'Accept'),
      variable = varname,
      level = paste0('d', n_diff),
      star = case_when(
        abs(statistic) > abs(`1pct`) ~ "***",
        abs(statistic) > abs(`5pct`) ~ "** ",
        abs(statistic) > abs(`10pct`) ~ "*  ",
        TRUE ~ "   "
      ),
      hypo = case_when(
        hypo == "tau3" ~ "trend",
        hypo == "tau2" ~ "constant",
        hypo == "tau1" ~ "none"
      ),
      statistic = format(round(statistic, 2), nsmall = 2) %>% 
        as.character %>% str_pad(6, "left")
    ) %>%
    select(variable, hypo, level, everything())
  
  return(df_test)
}

# unit root test in a table format
adf_table <- function(ur_adf) {
  ur_adf %>% 
    mutate(
      stat_star = glue::glue(
        "{statistic}{star}({lag})"
      ) %>% as.character()
    ) %>%
    pivot_wider(
      id_cols = c("variable"),
      names_from = c("level", "hypo"),
      values_from = c("stat_star")
    )
}


#################### Cross-Validate ####################
tsf_tscv <- function(data, 
                     initial, 
                     assess = 1, 
                     skip = 0, 
                     cumulative = TRUE) {
  
  n_obs <- length(tk_index(data))
  n_round <- floor(1 + (n_obs - initial - assess) / (skip + 1))
  
  res <- data.frame()
  idx_start <- 1
  idx_end <- initial
  for (i in 1:n_round) {
    res[i, 'id'] <- i
    res[i, 'idx_start'] <- idx_start
    res[i, 'idx_end'] <- idx_end
    idx_start <- ifelse(cumulative == TRUE, 1, idx_start + 1 + skip) 
    idx_end <- idx_end + 1 + skip
    }
  
  subet_ts <- function(data, idx_start, idx_end) {
    data %>% subset(start = idx_start, end = idx_end)
  }
  
  res <- res %>% 
    mutate(
      analysis = pmap(list(idx_start, idx_end), subet_ts, data = data),
      assess = pmap(list(idx_end + 1, idx_end + assess), subet_ts, data = data)
    )
  return(res)
}

#################### Model Evaluation ####################
tsf_eval_mod <- function(result, transform = "log") {
  if (transform == "log") {
    inverse_trans <- function(x) exp(x)
  }
  else {
    inverse_trans <- function(x) x
  }
  
  assess <- result$assess %>% unlist() %>% inverse_trans()
  pred <- result$pred %>% unlist() %>% inverse_trans()
  
  res <- data.frame(
      model = result$model[[1]],
      param = result$param[[1]] %>% paste(collapse = "-"),
      rmse = yardstick::rmse_vec(assess, pred) %>% round(),
      mae = yardstick::mae_vec(assess, pred) %>% round(), 
      mape = yardstick::mape_vec(assess, pred) %>% round(2)
    ) %>% 
    mutate(
      assess = list(result$assess),
      pred = list(result$pred)
    )
  return(res)
}

#################### Tuning ####################
tsf_tuning_grid <- function(cv, method, params_grid) {
  res <- list()
  n_params <- nrow(params_grid)
  pb <- txtProgressBar(0, n_params, style = 3)
  for (i in 1:n_params) {
    param <- flatten(param_grid[i,])
    tryCatch({
      res[[i]] <- method(cv, param) %>% tsf_eval_mod()
    }, error = function(e){})
    setTxtProgressBar(pb, i)
  }
  return(res %>% reduce(bind_rows))
}


#################### ETS ####################
tsf_ets <- function(cv, param) {
  
  fit_ets <- function(y, param) {
    do.call(forecast::ets, 
            list.append(y = y, param))
  }
  
  res <- cv
  
  n_ahead = length(res$assess[[1]])
  
  res <- res %>%
    mutate(
      fitted = map(analysis, fit_ets, param),
      pred = map(fitted, function(x) {
        x %>% forecast(h = n_ahead) %>% as.data.frame() %>% `[[`(1)
        }),
      coef = map(fitted, function(x) x[['par']]),
      param = list(param),
      model = "ets"
      ) %>% 
    select(-analysis, -fitted)
  
  return(res)
}
      
#################### ARIMA ####################