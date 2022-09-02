


# tabulate regression models: HR
toHR = function(fit,digits=2){

  hr = coef(fit) %>% exp
  hr = hr %>% round(digits)
  ci = confint.default(fit) %>% exp %>% round(digits)
  or.ci = paste0(hr,' (',ci[,1],' to ',ci[,2],')')
  Pvalue = summary(fit)$coefficients[,5]

  # table with HR and 95% CI:
  hr = cbind.data.frame(or.ci,Pvalue)
  names(hr) = c('HR (CI 95%)','p-value')
  hr$vars = rownames(hr) %>% str_remove_all('[0-9]')
  hr = hr %>% select(3,1,2)
  hr$`p-value` = factor(round(hr$`p-value`,digits+1))

  return(hr)
}
