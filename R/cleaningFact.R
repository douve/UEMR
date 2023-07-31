

# This function adds a window to dispensation date and
# changes the first date for each drug (code, agr) to be the same as the first observed
# in prescription.

cleaningFact = function(fact,presc,wdays = 30,vars = c('idp','cod','agr','dat','datfi','env')){

  # Definir la finestra probable de facturació:
  fact$datfi = fact$dat + wdays + fact$env*wdays
  fact$dat = fact$dat-wdays

  # - Change dispensation date to first prescription if the dispensation is prior to that prescription
  p = presc %>% select(idp,agr,dat) %>%
    arrange(idp,agr,dat) %>% group_by(idp,agr) %>%
    summarise_all(first)
  p = p %>% select(idp,agr,dat0=dat)

  fact = fact %>% select(vars) %>% arrange(idp,agr,dat) %>% left_join(p)

  fact = fact %>% filter(!is.na(dat0)) %>% filter(datfi>dat0) %>%
    mutate(dat = anydate(ifelse(dat0>dat,dat0,dat))) %>%
    select(vars)

}
