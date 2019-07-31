## ----setup, include = FALSE----------------------------------------------
source('_format.R')

## ------------------------------------------------------------------------
str(Table5.1)

## ----message=FALSE-------------------------------------------------------
df <- Table5.1
df$Temperature <- factor(df$Temperature)

## ----message=FALSE-------------------------------------------------------
null.model <- aov(BatteryLife~1, data=df)
full.model <- aov(BatteryLife ~ MaterialType + Temperature + MaterialType:Temperature, data=df)
anova(null.model, full.model)

## ------------------------------------------------------------------------
summary(full.model)

## ----fig.align='center', fig.width=6, fig.height=5-----------------------
with(df, {
	interaction.plot(Temperature, MaterialType, BatteryLife)
})

## ----fig.align='center', fig.width=6, fig.height=5-----------------------
af <- aggregate(
	df$BatteryLife,
	by=list(
		'Temperature'=Table5.1$Temperature, 
		'MaterialType'=df$MaterialType
	),
	FUN=mean
)
plot(
	af[af[, 'MaterialType'] == 1, c('Temperature', 'x')],
	type='o',
	ylim=c(0,175),
	col='orange',
	pch=19
)
lines(
	af[af[, 'MaterialType'] == 2, c('Temperature', 'x')],
	type='o',
	col='skyblue',
	pch=19
)
lines(
	af[af[, 'MaterialType'] == 3, c('Temperature', 'x')],
	type='o',
	col='green',
	pch=19
)
legend(
	'bottomleft',
	legend=c('Material 1', 'Material 2', 'Material 3'),
	col=c('orange', 'skyblue', 'green'),
	lty=rep(1,3),
	pch=rep(19,3)
)

## ----fig.align='center', fig.width=8, fig.height=4-----------------------
op <- par(mfrow=c(1,2))
qqnorm(resid(full.model), main='Normal Probability Plot')
qqline(resid(full.model), col='orange')
plot(x=predict(full.model), y=resid(full.model), main='Fitted vs. Residual')
par(op)

## ----fig.align='center', fig.width=8, fig.height=4-----------------------
op <- par(mfrow=c(1,2))
plot(x=as.numeric(df$MaterialType), y=resid(full.model), main='MaterialType vs. Residual')
plot(x=Table5.1$Temperature, y=resid(full.model), main='Temperature vs. Residual')
par(op)

## ----message=FALSE-------------------------------------------------------
library(dae)

df <- Table5.10
df[,'fTemperature'] <- factor(df$Temperature)
df[,'fPressure'] <- factor(df$Pressure)
model <- aov(Impurity ~ fTemperature + fPressure, data=df)
tukey.1df(model, data=df)

## ------------------------------------------------------------------------
str(Table5.13)

## ------------------------------------------------------------------------
with(Table5.13, {
	A <- factor(PctCarbonation)
	B <- factor(OperatingPressure)
	C <- factor(LineSpeed)
	model <- aov(FillHeightDeviation ~ A*B*C)
	print(summary(model))
})

## ----fig.align='center', fig.width=7, fig.height=8-----------------------
op <- par(mfrow=c(2,2))
attach(Table5.13)
levels <- list(
	'Percent carbonation (A)'=PctCarbonation,
	'Pressure (B)'=OperatingPressure,
	'Line speed (C)'=LineSpeed
)
for(s in names(levels)){
	df <- aggregate(
		FillHeightDeviation,
		by=list('A'=levels[[s]]),
		FUN=mean
	)
	plot(df, type='o', pch=19, xlab=s, ylim=c(-2,8))
}
df <- aggregate(
	FillHeightDeviation,
	by=list('A'=PctCarbonation, 'B'=OperatingPressure),
	FUN=mean
)
detach(Table5.13)

plot(df[df[,'B'] == 25,c('A','x')], type='o', pch=19, col='orange', ylim=c(-2, 10), xlab='Interaction')
lines(df[df[,'B'] == 30,c('A','x')], type='o', pch=19, col='lightblue')
legend('topleft', legend=c('B=25','B=30'), lty=c(1,1), pch=c(19, 19), col=c('orange', 'lightblue'))
par(op)

## ----message=FALSE-------------------------------------------------------
df <- Table5.1
df$Temperature <- (df$Temperature - 70) / 55
contrasts(df$MaterialType) <- as.matrix(cbind(c(1,0,-1), c(0,1,-1)))
model <- lm(BatteryLife ~ Temperature + MaterialType + I(Temperature^2) + Temperature:MaterialType + I(Temperature^2):MaterialType, data=df)
anova(model)

## ------------------------------------------------------------------------
summary(model)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
temp <- seq(from=-1, to=1, length.out=100)
y1 <- predict(model, data.frame('Temperature'=temp, 'MaterialType'=rep('1', 100)))
y2 <- predict(model, data.frame('Temperature'=temp, 'MaterialType'=rep('2', 100)))
y3 <- predict(model, data.frame('Temperature'=temp, 'MaterialType'=rep('3', 100)))
plot(
	x=df$Temperature, 
	y=df$BatteryLife,
	pch=19,
	col=c('orange','lightblue','green')[as.numeric(df$MaterialType)],
	xlab='Coded Temperature',
	ylab='Battery Life'
)
lines(x=temp, y=y1, col='orange')
lines(x=temp, y=y2, col='lightblue')
lines(x=temp, y=y3, col='green')
legend(
	'topright',
	legend=c('Material 1', 'Material 2', 'Material 3'),
	col=c('orange', 'lightblue', 'green'),
	lty=c(1, 1, 1)
)

## ------------------------------------------------------------------------
df <- Table5.16
A <- factor(df$TotalAngle)
B <- factor(df$CuttingSpeed)
model <- lm(ToolLife ~ A * B, data=df)
print(anova(model))
print(summary(model))

## ----fig.align='center', fig.width=8, fig.height=4-----------------------
op <- par(mfrow=c(1,2))
qqnorm(resid(model), main='Normal Probability Plot')
qqline(resid(model), col='orange')
plot(x=predict(model), y=resid(model), main='Fitted vs. Residual')
par(op)

## ------------------------------------------------------------------------
attach(Table5.16)
df <- data.frame(
	'ToolLife' = ToolLife,
	'Angle'=TotalAngle,
	'Speed'=CuttingSpeed,
	'Angle.Angle'=(TotalAngle - 20)^2,
	'Angle.Speed'=(TotalAngle - 20)*(CuttingSpeed-150),
	'Speed.Speed'=(CuttingSpeed-150)^2,
	'Angle.Angle.Speed'=(TotalAngle - 20)^2 * (CuttingSpeed-150),
	'Speed.Speed.Angle'=(CuttingSpeed-150)^2 * (TotalAngle - 20),
	'Angle.Speed.Angle.Speed'=(TotalAngle - 20)^2 * (CuttingSpeed-150)^2
)
detach(Table5.16)
model2 <- lm(ToolLife ~ ., data=df)
summary(model2)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
plot(x=Table5.16$TotalAngle, y=Table5.16$CuttingSpeed, main='Contours for Example 5.5 Response Surface', pch=19)
x <- seq(from=min(Table5.16$TotalAngle), to=max(Table5.16$TotalAngle), length.out=100)
y <- seq(from=min(Table5.16$CuttingSpeed), to=max(Table5.16$CuttingSpeed), length.out=100)
df <- expand.grid('Angle'=x, 'Speed'=y)
df['Angle.Angle'] <- (df$Angle - 20)^2
df['Angle.Speed'] <- (df$Angle - 20) * (df$Speed - 150)
df['Speed.Speed'] <- (df$Speed - 150)^2
df['Angle.Angle.Speed'] <- (df$Angle - 20)^2 * (df$Speed - 150)
df['Speed.Speed.Angle'] <- (df$Speed - 150)^2 * (df$Angle - 20)
df['Angle.Speed.Angle.Speed'] <- (df$Angle - 20)^2 * (df$Speed - 150)^2
z <- matrix(predict(model2, df), nrow=length(x))
contour(x=x, y=y, z=z, add=TRUE)

## ----eval=FALSE----------------------------------------------------------
#  library(rgl)
#  
#  open3d()
#  pallete <- topo.colors(255)
#  colors <- pallete[as.numeric(cut(as.numeric(z), 255))]
#  persp3d(
#  	x, y, z, col=colors,
#  	xlab='ToolAngle', ylab='CuttingSpeed', zlab='ToolLife')

## ------------------------------------------------------------------------
model <- aov(Intensity ~ Error(Operator) + GroundClutter * FilterType, data=Table5.21)
summary(model)

## ----message=FALSE-------------------------------------------------------
library(lme4)
library(lmerTest)
model <- lmer(
	Intensity ~ GroundClutter * FilterType + (1|Operator),
	REML=TRUE,
	data=Table5.21
)
print(summary(model))
print(rand(model))

## ------------------------------------------------------------------------
model <- aov(Intensity ~ Error(Day + Operator) + GroundClutter * Filter, data=Table5.24)
summary(model)

