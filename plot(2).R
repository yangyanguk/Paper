library(readxl)
library(ggplot2)
library(wesanderson)

a <- read_excel("/Users/yangy/Desktop/Codes/Prediction Performance under different windows.xlsx",sheet="Sheet2")

ggplot(a, aes(fill=Model, y=RMSE, x=Window)) + 
  geom_bar(position="dodge", stat="identity",color="black")+
  scale_fill_manual(values=c("red", "yellow", "black","blue","green","pink","brown"))

b <- read_excel("/Users/yangy/Desktop/Codes/Prediction Performance under different windows.xlsx",sheet="Sheet3")

ggplot(b, aes(fill=Model, y=MAPE, x=Window)) + 
  geom_bar(position="dodge", stat="identity",color="black")









c <- read.csv("/Users/yangy/Desktop/Codes/pred.csv")
c$X.1 <- as.Date(c$X.1 , origin = "1970-01-01")
d <- c
c <- na.omit(c)

ggplot(data=c, aes(x=X.1, y=prediction.arima-X.2,fill=prediction.arima-X.2>0)) +
  geom_bar(stat = "identity")+
  xlab("Time")+ylab("Error")+
  ggtitle("Forecast Error: ARIMA")+
  guides(fill = FALSE)

ggplot(data=d, aes(x=X.1))+
  geom_line(aes(y=X.2,colour="SP 500"),size=0.2)+
  geom_line(aes(y=prediction.arima,colour="Forecast: ARIMA"),size=0.3)+
  xlab("Time")+ylab(" ")+
  ggtitle("Forecast: ARIMA")+
  theme(legend.position="bottom")



