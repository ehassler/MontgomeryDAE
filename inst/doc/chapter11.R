## ----setup, include = FALSE----------------------------------------------
source('_format.R')
set.seed(98477112)

## ------------------------------------------------------------------------
library(rsm)

df <- Table11.1
cf <- coded.data(df, x1 ~ (Time - 35) / 5, x2 ~ (Temperature - 155) / 5)
model <- rsm(Yield ~ FO(x1, x2), data=cf)
summary(model)

## ------------------------------------------------------------------------
model <- rsm(Yield ~ FO(x1, x2) + TWI(x1, x2), data=cf)
summary(model)

## ------------------------------------------------------------------------
cf <- coded.data(Table11.6, x1 ~ (Time - 85) / 5, x2 ~ (Temperature - 175) / 5)
model <- rsm(Yield ~ FO(x1, x2), data=cf)
summary(model)$lof

## ------------------------------------------------------------------------
model <- rsm(Yield ~ FO(x1, x2) + TWI(x1, x2), data=cf)
summary(model)$lof

## ------------------------------------------------------------------------
model <- rsm(Yield ~ SO(x1, x2), data=cf)
summary(model)$lof

## ------------------------------------------------------------------------
model.fo <- rsm(Viscosity ~ FO(x1, x2), data=cf)
model.in <- rsm(Viscosity ~ FO(x1, x2) + TWI(x1, x2), data=cf)
model.so <- rsm(Viscosity ~ SO(x1, x2), data=cf)

rbind(
	'First Order LOF'=summary(model.fo)$lof[3,],
	'First Order Interaction LOF'=summary(model.in)$lof[4,],
	'Second Order LOF'=summary(model.so)$lof[5,]
)

## ------------------------------------------------------------------------
model.fo <- rsm(MolecularWeight ~ FO(x1, x2), data=cf)
model.in <- rsm(MolecularWeight ~ FO(x1, x2) + TWI(x1, x2), data=cf)
model.so <- rsm(MolecularWeight ~ SO(x1, x2), data=cf)

rbind(
	'First Order LOF'=summary(model.fo)$lof[3,],
	'First Order Interaction LOF'=summary(model.in)$lof[4,],
	'Second Order LOF'=summary(model.so)$lof[5,]
)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
 contour(model, ~ x1 + x2, image = TRUE)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
library(Vdgraph)

FDSPlot(Table11.13[,c('X1','X2','X3','X4')], mod=2)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
FDSPlot(Table11.14[,c('X1','X2','X3','X4')], mod=2)

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
library(DiceDesign)
df <- lhsDesign(10, 2)$design
colnames(df) <- c('x', 'R')
# Translate the points which are on [0,1] scale to the required scale
df[,'x'] <- df[,'x'] * 0.9 + 0.05
df[,'R'] <- df[,'R'] * 0.062
plot(df, main='LHS Design')

## ------------------------------------------------------------------------
library(DiceEval)
model <- modelFit(Table11.16[,c('x','R')], Table11.16[,'Temperature'], type='Kriging')

## ----fig.align='center', fig.width=5, fig.height=5-----------------------
x.range <- range(Table11.16[,'x'])
R.range <- range(Table11.16[,'R'])
x <- seq(from=x.range[1], to=x.range[2], length.out=100)
R <- seq(from=R.range[1], to=R.range[2], length.out=100)
z <- matrix(
	modelPredict(model, expand.grid('x'=x, 'R'=R)),
	nrow=length(x)
)
contour(x, R, z, main='Gaussian Process Mean Surface')

## ----message=FALSE-------------------------------------------------------
model <- lm(Elongation ~ -1 + (Polyethylene + Polystyrene + Polypropylene)^2, data=Table11.19)
print(summary(model))

## ----some-block, message=FALSE, fig.align='center', fig.width=5.5, fig.height=5----
library(qualityTools)

Y <- Table11.19$Elongation
mdo <- mixDesign(3, 2, center=FALSE, axial=FALSE, randomize=FALSE, replicates=c(1,1,2,3))
response(mdo) <- Y
contourPlot3(A, B, C, Y, data=mdo, form='quadratic')

## ------------------------------------------------------------------------
df <- Table11.20
for(s in c('Monomer', 'Crosslinker', 'Resin')){
	df[,s] <- df[,s] / 100
}

## ----message=FALSE-------------------------------------------------------
library(mixexp)

model <- MixModel(df, "Hardness", c("Monomer", "Crosslinker", "Resin"), 2)

## ------------------------------------------------------------------------
model <- aov(Hardness ~ -1 + (Monomer + Crosslinker + Resin)^2, data=df)
pe.summary(model)

## ------------------------------------------------------------------------
model <- MixModel(df, "Solids", c("Monomer", "Crosslinker", "Resin"), 2)

## ------------------------------------------------------------------------
model <- aov(Solids ~ -1 + (Monomer + Crosslinker + Resin)^2, data=df)
pe.summary(model)

