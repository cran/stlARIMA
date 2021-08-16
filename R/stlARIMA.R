#' @importFrom forecast mstl auto.arima
#' @importFrom utils head tail
#' @importFrom graphics plot
#' @importFrom stats as.ts ts
#' @export
#'
STLARIMA <- function(data, stepahead=10){
  STLcomp <- mstl(data)
  STLcomp_plots<-plot(STLcomp)
  data_trn <- ts(head(data, round(length(data) - stepahead)))
  data_test <- ts(tail(data, stepahead))
  STLcomp_trn <- STLcomp[-c(((length(data)-stepahead)+1):length(data)),]
  Fcast_STLcomp <- NULL
  for (STLcomp in 2:ncol(STLcomp_trn)) {
    Indcomp <- NULL
    Indcomp <- STLcomp_trn[ ,STLcomp]
    stlARIMAFit <- forecast::auto.arima(as.ts(Indcomp))
    stlARIMA_fcast=forecast::forecast(stlARIMAFit, h=stepahead)
    stlARIMA_fcast_Mean=stlARIMA_fcast$mean
    Fcast_STLcomp <- cbind(Fcast_STLcomp, as.matrix(stlARIMA_fcast_Mean))
  }
  FinalstlARIMA_fcast <- ts(rowSums(Fcast_STLcomp, na.rm = T))
  MAE_stlARIMA=mean(abs(data_test - FinalstlARIMA_fcast))
  MAPE_stlARIMA=mean(abs(data_test - FinalstlARIMA_fcast)/data_test)
  rmse_stlARIMA=sqrt(mean((data_test - FinalstlARIMA_fcast)^2))
  return(list(data_test=data_test, STLcomp_forecast=Fcast_STLcomp,
              stlARIMA_forecast=FinalstlARIMA_fcast, MAE_stlARIMA=MAE_stlARIMA,
              MAPE_stlARIMA=MAPE_stlARIMA, rmse_stlARIMA=rmse_stlARIMA,
              STLcomp_plots=STLcomp_plots))
}
