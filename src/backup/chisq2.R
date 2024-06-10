
solution_stakeholder <- 
  as.matrix(
    read.table(text=
                 "  E     NE           
      S             14    4      
      NS            76    400",
               header=TRUE,
               row.names=1)
  )

names(dimnames(solution_stakeholder)) <- c('solution','stakeholder')
solution_stakeholder

original <- as.table(solution_stakeholder)

summary(solution_stakeholder)

fisher.test(solution_stakeholder)



model_stakeholder <- 
  as.matrix(
    read.table(text=
                 "  E     NE      
      S             11    7      
      NS            377   94",
               header=TRUE,
               row.names=1)
  )

names(dimnames(model_stakeholder)) <- c('solution','model')
model_stakeholder

original2 <- as.table(model_stakeholder)

summary(model_stakeholder)

fisher.test(model_stakeholder)
