# Function to 2x2 contingency table and calculate associated statistics
table_compare <- function(pred, actual){
  outlist <- vector('list', 8)
  names(outlist) <- c('cont.table', 'sens', 'spec', 'misclass.rate', 'false.pos', 'false.neg', 'ppv', 'npv')
  ind <- !(is.na(pred) | is.na(actual)) # indicate pairs where both values are non-missing
  (.Table <- table(pred = pred[ind], actual = actual[ind]))
  outlist$cont.table <- .Table
  outlist$sens <- .Table[2, 2] / sum(.Table[, 2]) # sensitivity
  outlist$spec <- .Table[1, 1] / sum(.Table[, 1]) # specificity
  outlist$misclass.rate <- (.Table[1, 2] + .Table[2, 1]) / sum(.Table) # misclassification rate
  outlist$false.pos <- .Table[2, 1] / sum(.Table[, 1]) # false positive rate
  outlist$false.neg <- .Table[1, 2] / sum(.Table[, 2]) # false negative rate
  outlist$ppv <- .Table[2, 2] / sum(.Table[2, ]) # positive predictive value
  outlist$npv <- .Table[1, 1] / sum(.Table[1, ]) # negative predictive value
  return(outlist)
}
