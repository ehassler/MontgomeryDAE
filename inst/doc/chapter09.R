## ----setup, include = FALSE----------------------------------------------
source('_format.R')

## ------------------------------------------------------------------------
model <- aov(SyrupLoss ~ NozzleType * factor(Speed) * factor(Pressure), data=Table9.1)
print(summary(model))

## ------------------------------------------------------------------------
code.speed <- function(x){
	(x - 120) / (140 - 120)
}

code.pressure <- function(x){
	(x - 15) / (20 - 15)
}

df.reg <- data.frame(
	'NozzleType'=Table9.1$NozzleType,
	'Speed'=code.speed(Table9.1$Speed),
	'Pressure'=code.pressure(Table9.1$Pressure),
	'SyrupLoss'=Table9.1$SyrupLoss
)

model1 <- lm(SyrupLoss ~ Speed * Pressure + I(Speed^2) + I(Pressure^2), data=df.reg[df.reg$NozzleType == 1,])
model2 <- lm(SyrupLoss ~ Speed * Pressure + I(Speed^2) + I(Pressure^2), data=df.reg[df.reg$NozzleType == 2,])
model3 <- lm(SyrupLoss ~ Speed * Pressure + I(Speed^2) + I(Pressure^2), data=df.reg[df.reg$NozzleType == 3,])

print(coef(model1))
print(coef(model2))
print(coef(model3))

## ----fig.align='center', fig.width=8, fig.height=8-----------------------
x <- seq(from=100, to=140, length.out=100)
y <- seq(from=10, to=20, length.out=100)
df0 <- expand.grid('Speed'=code.speed(x), 'Pressure'=code.pressure(y))

z1 <- matrix(predict(model1, df0), ncol=100)
z2 <- matrix(predict(model2, df0), ncol=100)
z3 <- matrix(predict(model3, df0), ncol=100)

par(mfrow=c(2,2))
contour(x, y, z1, main='Nozzle Type 1')
contour(x, y, z2, main='Nozzle Type 2')
contour(x, y, z3, main='Nozzle Type 3')
plot.new()  # This just uses up the last cell.

## ------------------------------------------------------------------------
model <- aov(Response ~ Error(Block) + A + B + A:B, data=Example9.2)
print(summary(model))

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
library(FrF2)
library(corrplot)

X.d <- FrF2(nfactors=6, nruns=16)
X.m <- model.matrix(~.*., data=X.d)
X.corr <- t(X.m) %*% X.m / 16
corrplot(X.corr, method='color', tl.col="black", tl.srt=45, tl.cex=0.7)

## ------------------------------------------------------------------------
model <- p.stepwise(
	lm(Thickness ~ 1, Table9.24),
	~ (A + B + C + D + E + F)^2,
	alpha.to.enter=0.20,
	alpha.to.leave=0.20
)
print(summary(model))
print(anova(model))

## ----example9.4.Iopt-----------------------------------------------------
library(AlgDesign)

candidate.list <- expand.grid(
	'A'=c(-1, 0, 1),
	'B'=c(-1, 0, 1),
	'C'=c(-1, 0, 1),
	'D'=c(-1, 0, 1),
	'E'=c(-1, 0, 1),
	'F'=c(-1, 0, 1)
)

opt.i <- optFederov(~A+B+C+D+E+F, data=candidate.list, nTrials=12, criterion='I', approximate=FALSE, nRepeats=1000)

## ----echo=FALSE----------------------------------------------------------
kable(opt.i$design) %>% kable_styling(bootstrap_options = "striped", full_width = F)

## ------------------------------------------------------------------------
eval.design(~ A + B + C + D + E + F, design=Table9.36, X=candidate.list)$I

