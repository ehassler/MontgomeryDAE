## ----setup, include = FALSE----------------------------------------------
source('_format.R')

## ------------------------------------------------------------------------
df <- Table6.10
colnames(df) <- c('z', 'x1', 'x2', 'x3', 'y', 'Block')
model <- lm(y ~ z * (x1 + x2 + x3), data=df)
print(summary(model))

## ------------------------------------------------------------------------
model <- lm(y ~ z * (x2 + x3), data=df)
print(summary(model))

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
x2 <- seq(from=-1, to=1, length.out=100)
x3 <- seq(from=-1, to=1, length.out=100)
z <- matrix(predict(model, cbind(
	'z'=rep(0, 100^2),
	expand.grid('x2'=x2, 'x3'=x3)
)), ncol=100)

poe.coefs <- (
	model.matrix(
		~ z * (x2 + x3),
		data=cbind(
			'z'=rep(1, 100^2),
			expand.grid('x2'=x2, 'x3'=x3)
		)
	) 
	%*% 
	matrix(c(0, coef(model)['z'], 0, 0, coef(model)[c('z:x2','z:x3')]))
)^2
z.poe <- matrix(sqrt(poe.coefs), nrow=100)

contour(x2, x3, z, col='blue', xlab='Concentration', ylab='StirringRate')
contour(x2, x3, z.poe, col='orange', add=TRUE)

## ------------------------------------------------------------------------
df <- Table12.3[,-1]
model <- lm(y ~ (z1 + z2 + z3) * (x1 + x2) + x1:x2 + I(x1^2) + I(x2^2), data=df)
print(summary(model))

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
x1 <- seq(from=-1, to=1, length.out=100)
x2 <- seq(from=-1, to=1, length.out=100)

mean.df <- cbind(
	data.frame(
		'z1'=rep(0, 100^2),
		'z2'=rep(0, 100^2),
		'z3'=rep(0, 100^2)
	),
	expand.grid('x1'=x1, 'x2'=x2)
)

z <- matrix(predict(model, mean.df), ncol=100)

var.df <- cbind(
	data.frame(
		'z1'=rep(1, 100^2),
		'z2'=rep(1, 100^2),
		'z3'=rep(1, 100^2)
	),
	expand.grid('x1'=x1, 'x2'=x2)
)

X.m <- model.matrix( ~ (z1 + z2 + z3) * (x1 + x2) + x1:x2 + I(x1^2) + I(x2^2), data=var.df)
b <- coef(model)
z1.terms <- matrix(c(0, b[2], 0, 0, 0, 0, 0, 0, b[9], b[10], 0, 0, 0, 0, 0))
z2.terms <- matrix(c(0, 0, b[3], 0, 0, 0, 0, 0, 0, 0, b[11], b[12], 0, 0, 0))
z3.terms <- matrix(c(0, 0, 0, b[4], 0, 0, 0, 0, 0, 0, 0, 0, b[13], b[14], 0))

poe.coefs <- sqrt((X.m %*% z1.terms)^2 + (X.m %*% z2.terms)^2 + (X.m %*% z3.terms)^2)
z.poe <- matrix(poe.coefs, nrow=100)

contour(x1, x2, z, col='blue', xlab='x1', ylab='x2')
contour(x1, x2, z.poe, col='orange', add=TRUE)

