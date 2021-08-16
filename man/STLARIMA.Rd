\name{STLARIMA}
\alias{STLARIMA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
STL Based ARIMA Forecasting Model
}
\description{
The STLARIMA function forecasts a time series using a hybrid model made of a decomposition technique called seasonal trend decomposition based on loess (STL) and a forecasting technique called ARIMA. The function further computes the values of different forecasting evaluation criteria.
}
\usage{
STLARIMA(data, stepahead=10)
}
\arguments{
  \item{data}{
Input univariate time series (ts) data.
}
  \item{stepahead}{
The forecast horizon.
}
}
\details{
This function decomposes a nonlinear, nonstationary and seasonal time series into trend-cycle, seasonal and remainder component using STL (Cleveland et al., 1990). ARIMA model is used to forecast these components individually (Box et al., 2015, Jha and Sinha, 2013). Finally, the prediction results of all the three components are aggregated to formulate an ensemble output for the input time series.
}
\value{
  \item{data_test }{Testing set used to measure the out of sample performance.}
  \item{STLcomp_forecast }{Forecasted value of all individual components.}
  \item{stlARIMA_forecast }{Final forecasted value of the stlARIMA model. It is obtained by combining the forecasted value of all individual components.}
  \item{MAE_stlARIMA }{Mean Absolute Error (MAE) for stlARIMA model.}
  \item{MAPE_stlARIMA }{Mean Absolute Percentage Error (MAPE) for stlARIMA model.}
  \item{rmse_stlARIMA }{Root Mean Square Error (RMSE) for stlARIMA model.}
}
\references{
Cleveland, R.B., Cleveland, W.S., McRae, J.E., Terpenning, I. (1990). STL: A seasonal-trend decomposition procedure based on loess, Journal of Official Statistics, 6, 3--73.

Box, G.E.P, Reinsel, G.C., Jenkins, G.M., Ljung, G.M. (2015). Time Series Analysis: Forecasting and Control. Wiley, Germany.

Jha, G.K., Sinha, K. (2013). Agricultural price forecasting using neural network model: An innovative information delivery system. Agricultural Economics Research Review, 26, 229--239.

}
\examples{
data("Data_potato")
STLARIMA(Data_potato)
}
\keyword{ stl }
