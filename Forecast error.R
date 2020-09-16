ggplot(data=c, aes(x=X.1, y=prediction.arima-X.2,fill=prediction.arima-X.2>0)) +
  geom_bar(stat = "identity")+
  xlab("Time")+ylab("Error")+
  ggtitle("Forecast Error: ARIMA")+
  guides(fill = FALSE)

ggplot(data=c, aes(x=X.1, y=prediction.reg-X.2,fill=prediction.reg-X.2>0)) +
  geom_bar(stat = "identity")+
  xlab("Time")+ylab("Error")+
  ggtitle("Forecast Error: Regression")+
  guides(fill = FALSE)+
  ylim(-0.15,0.15)

ggplot(data=c, aes(x=X.1, y=prediction.pca-X.2,fill=prediction.pca-X.2>0)) +
  geom_bar(stat = "identity")+
  xlab("Time")+ylab("Error")+
  ggtitle("Forecast Error: PCA")+
  guides(fill = FALSE)

ggplot(data=c, aes(x=X.1, y=prediction.pca2-X.2,fill=prediction.pca2-X.2>0)) +
  geom_bar(stat = "identity")+
  xlab("Time")+ylab("Error")+
  ggtitle("Forecast Error: Square PCA")+
  guides(fill = FALSE)

ggplot(data=c, aes(x=X.1, y=prediction.lasso-X.2,fill=prediction.lasso-X.2>0)) +
  geom_bar(stat = "identity")+
  xlab("Time")+ylab("Error")+
  ggtitle("Forecast Error: LASSO")+
  guides(fill = FALSE)

ggplot(data=c, aes(x=X.1, y=prediction.ridge-X.2,fill=prediction.ridge-X.2>0)) +
  geom_bar(stat = "identity")+
  xlab("Time")+ylab("Error")+
  ggtitle("Forecast Error: Ridge")+
  guides(fill = FALSE)

ggplot(data=c, aes(x=X.1, y=prediction.elnet-X.2,fill=prediction.elnet-X.2>0)) +
  geom_bar(stat = "identity")+
  xlab("Time")+ylab("Error")+
  ggtitle("Forecast Error: Elastic Net")+
  guides(fill = FALSE)
