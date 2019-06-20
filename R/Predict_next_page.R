PredictNextPage <- function (current_URL){
  out <- try(predict(model,newdata = current_URL), silent = TRUE)
  if(inherits(out,"try_error")){
    out <- "None"
  }
    out
}PredictNextPage <- function (current_URL){
  out <- try(predict(model,newdata = current_URL), silent = TRUE)
  if(inherits(out,"try_error")){
    out <- "None"
  }
    out
}