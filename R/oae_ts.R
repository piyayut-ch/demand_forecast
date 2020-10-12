library("vars")
library("tidyverse")
library("readxl")

adf_report = function(obj) {
  lag = (obj %>% slot("lags"))
  tstat = (t(obj %>% slot("teststat"))) %>% as.data.frame()
  colnames(tstat) = "tstat"
  cval = (obj %>% slot("cval")) %>% as.data.frame()
  colnames(cval) = c('cv01', 'cv05', 'cv10')
  res = data.frame(lag, tstat, cval)
  res['test'] = "ADF"
  res = res[1,]
  rownames(res) = NULL
  return(res)
}

kpss_report = function(obj) {
  lag = (obj %>% slot("lag"))
  tstat = obj %>% slot("teststat")
  cval = (obj %>% slot("cval")) %>% as.data.frame() %>% select(-3)
  colnames(cval) = c('cv10', 'cv05', 'cv01')
  res = data.frame(lag, tstat, cval)
  res['test'] = "KPSS"
  rownames(res) = NULL
  return(res)
}

unit_test = function(data, type=c("drift", "trend"), difference=FALSE) {
  kpss_map = list(drift = "mu", trend = "tau")
  col_names = colnames(data)
  res_1 = data.frame()
  res_2 = data.frame()
  
  for( i in seq_along(col_names) ) {
    ser = data[[col_names[i]]]
    ser = if (difference) diff(ser) else ser 
    level = if (difference) "d1" else "d0"
      
    adf = ur.df(ser, type = type, selectlags = "AIC")
    test_res = adf_report(adf)
    test_res['col_name']  = col_names[i]
    res_1 = rbind(res_1, test_res)
    
    kpss = ur.kpss(ser, type = kpss_map[[type]], lags = "short")
    test_res = kpss_report(kpss)
    test_res['col_name']  = col_names[i]
    res_2 = rbind(res_2, test_res)
    
    res = bind_rows(res_1, res_2)
    res['level'] = level
    res['type'] = type
  }
  return(res)
}

unit_test_report = function(data) {
  res1 = unit_test(data, type = "drift", difference = FALSE)
  res2 = unit_test(data, type = "trend", difference = FALSE)
  res3 = unit_test(data, type = "drift", difference = TRUE)
  res = bind_rows(res1, res2, res3)
  result = res %>%
    gather("key", "value", -col_name, -test, -level, -type) %>%
    mutate(key = paste(test, key, sep = "_")) %>%
    select(-test) %>%
    spread(key, value) %>%
    mutate(stationary_ADF  = ADF_tstat < ADF_cv05) %>%
    mutate(stationary_KPSS = KPSS_tstat < KPSS_cv05)  
  
  return(result)
}
