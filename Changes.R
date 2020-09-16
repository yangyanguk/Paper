ggplot(data=d, aes(x=X.1))+
  geom_line(aes(y=X.2,colour="SP 500"),size=0.2)+
  geom_line(aes(y=prediction.arima,colour="Forecast: ARIMA"),size=0.3)+
  xlab("Time")+ylab(" ")+
  ggtitle("Forecast: ARIMA")+
  theme(legend.position="bottom")

ggplot(data=d, aes(x=X.1))+
  geom_line(aes(y=X.2,colour="SP 500"),size=0.2)+
  geom_line(aes(y=prediction.reg,colour="Forecast: Regression"),size=0.3)+
  xlab("Time")+ylab(" ")+
  ggtitle("Forecast: Regression")+
  theme(legend.position="bottom")+
  ylim(-0.3,0.3)

ggplot(data=d, aes(x=X.1))+
  geom_line(aes(y=X.2,colour="SP 500"),size=0.2)+
  geom_line(aes(y=prediction.pca,colour="Forecast: PCA"),size=0.3)+
  xlab("Time")+ylab(" ")+
  ggtitle("Forecast: PCA")+
  theme(legend.position="bottom")

ggplot(data=d, aes(x=X.1))+
  geom_line(aes(y=X.2,colour="SP 500"),size=0.2)+
  geom_line(aes(y=prediction.pca2,colour="Forecast: Square PCA"),size=0.3)+
  xlab("Time")+ylab(" ")+
  ggtitle("Forecast: Square PCA")+
  theme(legend.position="bottom")

ggplot(data=d, aes(x=X.1))+
  geom_line(aes(y=X.2,colour="SP 500"),size=0.2)+
  geom_line(aes(y=prediction.lasso,colour="Forecast: LASSO"),size=0.3)+
  xlab("Time")+ylab(" ")+
  ggtitle("Forecast: LASSO")+
  theme(legend.position="bottom")

ggplot(data=d, aes(x=X.1))+
  geom_line(aes(y=X.2,colour="SP 500"),size=0.2)+
  geom_line(aes(y=prediction.ridge,colour="Forecast: Ridge"),size=0.3)+
  xlab("Time")+ylab(" ")+
  ggtitle("Forecast: Ridge")+
  theme(legend.position="bottom")

ggplot(data=d, aes(x=X.1))+
  geom_line(aes(y=X.2,colour="SP 500"),size=0.2)+
  geom_line(aes(y=prediction.elnet,colour="Forecast: Elastic Net"),size=0.3)+
  xlab("Time")+ylab(" ")+
  ggtitle("Forecast: Elastic Net")+
  theme(legend.position="bottom")

