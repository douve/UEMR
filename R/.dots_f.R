.dots_f <- function(l, args) {
  y <- names(l)
  funs <- lapply(y, function(y) paste0(y,"::",l[[y]])) %>% unlist
  params <- lapply(funs,
                   function(x) eval(parse(text = x)) %>% formals %>% names) %>% unlist
  params <- unique(params)
  args[names(args) %in% params]
}
