set.seed(15)
rr<-density(faithful$eruptions)
dd<-data.frame(x=rr$x)
dd$y=rr$y+ runif(8,0,.05)

fit <- lm(y ~ poly(x,32,raw=TRUE), dd)
dd$fitted <- fitted(fit)

ggplot(dd, aes(x=x)) + 
  geom_line(aes(y = y), colour="red") + 
  geom_line(aes(y = fitted), colour="blue")
deriv_coef<-function(x) {
  x <- coef(x)
  stopifnot(names(x)[1]=="(Intercept)")
  y <- x[-1]
  stopifnot(all(grepl("^poly", names(y))))
  px <- as.numeric(gsub("poly\\(.*\\)","",names(y)))
  rr <- setNames(c(y * px, 0), names(x))
  rr[is.na(rr)] <- 0
  rr
}

dd$slope <- model.matrix(fit) %*% matrix(deriv_coef(fit), ncol=1)

ggplot(dd, aes(x=x)) + 
  geom_line(aes(y = y), colour="red") + 
  geom_line(aes(y = fitted), colour="blue") + 
  geom_line(aes(y = slope), colour="green")

install.packages(quantchem)
x = 1:10
y = jitter(x+x^2)
fit = lm(y~x+I(x^2))
derivative(fit,1:10)
