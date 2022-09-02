

# tabulate regression models: OR

toOR = function(fit,digits=2){

  or = coef(fit) %>% exp %>% na.omit %>% round(digits)
  ci = confint.default(fit) %>% exp %>% round(digits) %>% na.omit

  or.ci = paste0(or,' (',ci[,1],' to ',ci[,2],')')
  Pvalue = summary(fit)$coefficients[,4]

  # table:
  or = cbind.data.frame(or.ci,Pvalue)
  names(or) = c('OR (CI 95%)','p-value')
  or$vars = rownames(or) %>% str_remove_all('[0-9]')
  or = or %>% select(3,1,2)
  or$`p-value` = factor(round(or$`p-value`,digits+1))

  return(or)

}
