## ----setup, include = FALSE----------------------------------------------
source('_format.R')

## ------------------------------------------------------------------------
expand.grid('A'=c(-1,1), 'B'=c(-1, 1), 'C'=c(-1, 1))

## ------------------------------------------------------------------------
fac2 <- function(k){
	as.data.frame(do.call(
		expand.grid,
		lapply(
			1:k,
			function(.){
				c(-1,1)
			}
		)
	))
}

fac2(4)

## ------------------------------------------------------------------------
model <- aov(Yield ~ A*B, data=Figure6.1)
summary(model)

## ------------------------------------------------------------------------
model.matrix(~A*B, data=Figure6.1)

## ----fig.align='center', fig.width=8, fig.height=4-----------------------
model <- lm(Yield ~ A*B, data=Figure6.1)

op <- par(mfrow=c(1,2))
qqnorm(resid(model), main='Normal Probability Plot')
qqline(resid(model), col='orange')
plot(x=predict(model), y=resid(model), main='Fitted vs. Residual')
par(op)

## ------------------------------------------------------------------------
model <- lm(Yield ~ A + B, data=Figure6.1)
summary(model)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
plot(x=Figure6.1$A, y=Figure6.1$B, main='Contours for Quadratic Model', pch=19)
x <- seq(from=-1, to=1, length.out=100)
y <- seq(from=-1, to=1, length.out=100)
df <- expand.grid('A'=x, 'B'=y)
z <- matrix(predict(model, df), nrow=length(x))
contour(x=x, y=y, z=z, add=TRUE)

## ------------------------------------------------------------------------
model <- aov(EtchRate ~ Gap * Flow * Power, data=Table6.4)
summary(model)

## ------------------------------------------------------------------------
2*coef(model)[-1]

## ------------------------------------------------------------------------
model <- aov(Filtration ~ Temperature * Pressure * Formaldehyde * StirringRate, data=Table6.10)
summary(model)

## ------------------------------------------------------------------------
df <- Table6.10
colnames(df) <- c('A', 'B', 'C', 'D', 'Filtration', 'Block')
X <- model.matrix(~ -1 + A*B*C*D, data=df)
effects <- apply(X, 2, function(w){ sum((w*df$Filtration) / (0.5 * dim(df)[1])) })
names(effects) <- colnames(X)
effects

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
locs <- qqnorm(effects, main='Normal Probability Plot')
qqline(effects, col='orange')
ix <- c(1,3,4,6,8)
text(x=locs$x[ix], y=locs$y[ix], labels=names(effects[ix]), pos=c(2, 4, 4, 4, 4))

## ----message=FALSE, fig.align='center', fig.width=5, fig.height=5--------
library(DoE.base)

Fo <- Table6.10$Formaldehyde
Te <- Table6.10$Temperature
St <- Table6.10$StirringRate
Pr <- Table6.10$Pressure
Fi <- Table6.10$Filtration
model <- aov(Fi ~ Te * Pr * Fo * St, data=Table6.10)
vals <- halfnormal(model, alpha=0.1)

## ------------------------------------------------------------------------
print(vals$signif)

## ----fig.align='center', fig.width=8, fig.height=4-----------------------
op <- par(mfrow=c(1,2))
model <- lm(Filtration ~ A * C * D, data=df)

# Temperature (A) vs. Concentration (C)
plot(x=df$A, y=df$C, pch=19, xlab='Temperature', ylab='Concentration')
x <- seq(from=-1, to=1, length.out=100)
y <- seq(from=-1, to=1, length.out=100)
pre.z <- expand.grid('A'=x, 'C'=y)
pre.z[,'D'] <- rep(1, 100)
z <- matrix(predict(model, pre.z), nrow=length(x))
contour(x=x, y=y, z=z, add=TRUE)

# Concentration (C) vs. Stirring Rate (D)
plot(x=df$C, y=df$D, pch=19, xlab='Concentration', ylab='Stirring Rate')
x <- seq(from=-1, to=1, length.out=100)
y <- seq(from=-1, to=1, length.out=100)
pre.z <- expand.grid('C'=x, 'D'=y)
pre.z[,'A'] <- rep(1, 100)
z <- matrix(predict(model, pre.z), nrow=length(x))
contour(x=x, y=y, z=z, add=TRUE)

par(op)

## ----message=FALSE, fig.align='center', fig.width=5, fig.height=5--------
model <- aov(Filtration ~ A * C * D, data=df)
vals <- halfnormal(model)
print(vals$signif)

## ----fig.align='center', fig.width=5, fig.height=5, message=FALSE--------
model <- lm(DrillRate ~ .*.*.*., data=Example6.3)
vals <- halfnormal(model)
print(vals$signif)

## ------------------------------------------------------------------------
model2 <- aov(DrillRate ~ Mud + RotationalSpeed + FlowRate + FlowRate:Mud + FlowRate:RotationalSpeed, data=Example6.3)
summary(model2)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
plot(predict(model2), resid(model2))

## ----fig.align='center', fig.width=5, fig.height=5, message=FALSE--------
df <- Example6.3
df[,'DrillRate'] <- log(df[,'DrillRate'])
model3 <- lm(DrillRate ~ .*.*.*., data=df)
vals <- halfnormal(model3)
print(vals$signif)

## ------------------------------------------------------------------------
model4 <- lm(DrillRate ~ FlowRate * RotationalSpeed * Mud, data=df)
anova(model4)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
model4 <- lm(DrillRate ~ FlowRate + RotationalSpeed + Mud, data=df)
qqnorm(resid(model4))
qqline(resid(model4), col='orange')

## ----fig.align='center', fig.width=5, fig.height=5, echo=FALSE, message=FALSE----
model <- lm(Defects ~ .*.*.*., data=Example6.4)
halfnormal(model)$signif

## ------------------------------------------------------------------------
model2 <- lm(Defects ~ Temperature * ResinFlow, data=Example6.4)
anova(model2)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
model2 <- lm(Defects ~ Temperature + ResinFlow, data=Example6.4)
plot(Example6.4$ClampTime, resid(model2))

## ----fig.align='center', fig.width=5, fig.height=5, message=FALSE--------
ix <- Example6.4$ClampTime == 1
X <- model.matrix(Defects ~ -1 + . * . * . * ., data=Example6.4)
Fi.star <- apply(
	X,
	2,
	function(w){
		log(
			sd(resid(model2)[w == 1])^2 
			/ 
			sd(resid(model2)[w != 1])^2
		)
	}
)
vals <- halfnormal(Fi.star)

## ------------------------------------------------------------------------
df <- aggregate(OxideThickness ~ ., data=Table6.18, FUN=mean)
df[,'SampleVariance'] <- aggregate(OxideThickness ~ ., data=Table6.18, FUN=var)[,'OxideThickness']

## ----echo=FALSE----------------------------------------------------------
kable(df) %>% kable_styling()

## ----echo=TRUE, message=FALSE--------------------------------------------
model <- lm(OxideThickness ~ Temperature * Time * Pressure * GasFlow, data=df)
model.ss <- anova(model)[['Sum Sq']]
effect.estimates <- data.frame(
	'EffectEstimate'=2 * coef(model)[-1],
	'SumOfSquares'=model.ss[-1],
	'Contribution'=sprintf('%0.2f%%', 100*model.ss[-1]/sum(model.ss[-1]))
)

## ----echo=FALSE----------------------------------------------------------
kable(effect.estimates) %>% kable_styling(font_size=10)

## ----fig.align='center', fig.width=5, fig.height=5, message=FALSE--------
vals <- halfnormal(model)
print(vals$signif)

## ------------------------------------------------------------------------
model2 <- lm(OxideThickness ~ Temperature + Time + Temperature:Time + Pressure + Temperature:Pressure, data=df)
anova(model2)

## ------------------------------------------------------------------------
summary(model2)

## ----fig.align='center', fig.width=8, fig.height=4-----------------------
op <- par(mfrow=c(1,2))

plot(x=df$Time, y=df$Temperature, pch=19, xlab='Time', ylab='Temperature', main='Pressure=-1')
x <- seq(from=-1, to=1, length.out=100)
y <- seq(from=-1, to=1, length.out=100)
pre.z <- expand.grid('Time'=x, 'Temperature'=y)
pre.z[,'Pressure'] <- rep(-1, 100)
z <- matrix(predict(model2, pre.z), nrow=length(x))
contour(x=x, y=y, z=z, add=TRUE)

plot(x=df$Time, y=df$Temperature, pch=19, xlab='Time', ylab='Temperature', main='Pressure=1')
x <- seq(from=-1, to=1, length.out=100)
y <- seq(from=-1, to=1, length.out=100)
pre.z <- expand.grid('Time'=x, 'Temperature'=y)
pre.z[,'Pressure'] <- rep(1, 100)
z <- matrix(predict(model2, pre.z), nrow=length(x))
contour(x=x, y=y, z=z, add=TRUE)

par(op)

## ----fig.align='center', fig.width=5, fig.height=5, message=FALSE--------
Y <- log(df[,'SampleVariance'])
model <- lm(Y ~ Temperature * Time * Pressure * GasFlow, data=df)
vals <- halfnormal(model)
print(vals$signif)

## ------------------------------------------------------------------------
model <- lm(Y ~ Temperature + Time + GasFlow + Time:GasFlow, data=df)
summary(model)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
plot(x=df$Time, y=df$Temperature, pch=19, xlab='Time', ylab='Temperature', main='GasFlow=+1')
x <- seq(from=-1, to=1, length.out=100)
y <- seq(from=-1, to=1, length.out=100)
pre.z <- expand.grid('Time'=x, 'Temperature'=y)
pre.z[,'GasFlow'] <- rep(1, 100)
z <- matrix(predict(model, pre.z), nrow=length(x))
contour(x=x, y=y, z=exp(z), add=TRUE)

## ----fig.align='center', fig.width=8, fig.height=6, message=FALSE--------
model <- lm(ResponseRate ~ .*.*.*., data=Table6.22)
vals <- halfnormal(model, alpha=0.1)
print(vals$signif)

## ------------------------------------------------------------------------
model <- lm(
	ResponseRate ~ AnnualFee + AccountOpeningFee + InitialInterestRate + LongTermInterestRate + AnnualFee:AccountOpeningFee,
	data=Table6.22
)
summary(model)

## ------------------------------------------------------------------------
str(Table6.10)

## ------------------------------------------------------------------------
Example6.7 <- rbind(
	Table6.10,
	c(0, 0, 0, 0, 73, NA),
	c(0, 0, 0, 0, 75, NA),
	c(0, 0, 0, 0, 66, NA),
	c(0, 0, 0, 0, 69, NA)
)
model1 <- lm(Filtration ~ Temperature*Pressure*Formaldehyde*StirringRate, data=Example6.7)
model2 <- lm(Filtration ~ Temperature*Pressure*Formaldehyde*StirringRate + I(Temperature^2) + I(Pressure^2) + I(Formaldehyde^2) + I(StirringRate^2), data=Example6.7)
anova(model1, model2)

## ------------------------------------------------------------------------
model1 <- lm(Filtration ~ Temperature + Formaldehyde + StirringRate + Temperature:Formaldehyde + Temperature:StirringRate, data=Example6.7)
model2 <- lm(Filtration ~ Temperature + Formaldehyde + StirringRate + Temperature:Formaldehyde + Temperature:StirringRate + I(Temperature^2) + I(Formaldehyde^2) + I(StirringRate^2), data=Example6.7)
anova(model1, model2)

## ------------------------------------------------------------------------
model <- lm(V ~ I * R, data=Table6.25)
summary(model)

## ------------------------------------------------------------------------
model <- lm(V ~ x1 * x2, data=Table6.25)
summary(model)

