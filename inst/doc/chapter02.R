## ----setup, include = FALSE----------------------------------------------
library(knitr)
library(kableExtra)
library(MontgomeryDAE)
source('_format.R')

## ----echo=FALSE----------------------------------------------------------
kable(Table2.1) %>% kable_styling()

## ------------------------------------------------------------------------
str(Table2.1)

## ----message=FALSE-------------------------------------------------------
attach(Table2.1)
all(ModifiedMortar == Table2.1$ModifiedMortar)

## ----fig.align='center', fig.height=3, fig.width=5, message=FALSE--------
stripchart(
	list(
		'Modified'=ModifiedMortar, 
		'Unmodified'=UnmodifiedMortar
	),
	main="Dot Diagram for Table 2.1",
	xlab='Strength'
)

## ----fig.align='center', fig.width=5, fig.height=5, message=FALSE--------
data <- c(ModifiedMortar, UnmodifiedMortar)
hist(data)

## ----fig.align='center', fig.width=5, fig.height=5, message=FALSE--------
bp.returned <- boxplot(
	Table2.1,
	main="Box plots for Table 2.1",
	xlab='Mortar Formulation',
	ylab='Strength'
)

## ------------------------------------------------------------------------
print(bp.returned)

## ------------------------------------------------------------------------
summary(Table2.1)

## ------------------------------------------------------------------------
dstat <- data.frame(
	'Modified'=c(
		mean(ModifiedMortar),
		var(ModifiedMortar),
		sd(ModifiedMortar),
		length(ModifiedMortar)
	),
	'Unmodified'=c(
		mean(UnmodifiedMortar),
		var(UnmodifiedMortar),
		sd(UnmodifiedMortar),
		length(UnmodifiedMortar)		
	),
	row.names=c('mean', 'variance', 'sd','obs')
)

## ----echo=FALSE----------------------------------------------------------
kable(dstat) %>% 
	kable_styling(
		bootstrap_options="striped", 
		full_width=FALSE
	)

## ------------------------------------------------------------------------
n1 <- dstat['obs','Modified']
n2 <- dstat['obs','Unmodified']
S1sq <- dstat['var','Modified']
S2sq <- dstat['var','Unmodified']
pooled.var <- ((n1 - 1) * S1sq + (n2 - 1) * S2sq) / (n1 + n2 - 2)
print(pooled.var)

## ------------------------------------------------------------------------
ybar1 <- dstat['mean', 'Modified']
ybar2 <- dstat['mean', 'Unmodified']
t0 <- (ybar1 - ybar2) / sqrt(pooled.var * (1/n1 + 1/n2))
print(t0)

## ------------------------------------------------------------------------
critical.t <- qt(0.025, 18, lower.tail=FALSE)
print(critical.t)

## ------------------------------------------------------------------------
2 * (1 - pt(abs(t0), 18))

## ------------------------------------------------------------------------
df <- data.frame(
	'Strength'=c(ModifiedMortar, UnmodifiedMortar),
	'Mortar'=factor(c(rep('Modified', 10), rep('Unmodified', 10)))
)
t.test(Strength ~ Mortar, data=df, paired=FALSE, var.equal=TRUE)

## ----fig.align='center', fig.width=7, fig.height=4-----------------------
op <- par(mfrow=c(1,2))
qqnorm(ModifiedMortar, main="Modified Mortar")
qqline(ModifiedMortar, col='orange')
qqnorm(UnmodifiedMortar, main="Unmodified Mortar")
qqline(UnmodifiedMortar, col='blue')
par(op)

## ------------------------------------------------------------------------
detach(Table2.1)  # Clean up since we're done with this data.

## ------------------------------------------------------------------------
library(pwr)
pwr.t.test(n=10, d=2, sig.level=0.05, type='two.sample', alternative='two.sided')

## ------------------------------------------------------------------------
pwr.t.test(power=0.9, d=2, sig.level=0.05, type='two.sample', alternative='two.sided')

## ----echo=FALSE----------------------------------------------------------
kable(Table2.3) %>% kable_styling()

## ------------------------------------------------------------------------
summary(Table2.3[,c('Nerve','Muscle')])

## ------------------------------------------------------------------------
c('Nerve'=sd(Table2.3$Nerve), 'Muscle'=sd(Table2.3$Muscle))

## ----fig.align='center', fig.width=7, fig.height=4, message=FALSE--------
op <- par(mfrow=c(1,2))
qqnorm(Table2.3$Nerve, main="Nerve")
qqline(Table2.3$Nerve, col='orange')
qqnorm(Table2.3$Muscle, main="Muscle")
qqline(Table2.3$Muscle, col='blue')
par(op)

## ------------------------------------------------------------------------
df <- data.frame(
	'Flourescence'=c(Table2.3$Muscle, Table2.3$Nerve),
	'Tissue'=factor(c(rep('Muscle', 12), rep('Nerve', 12)))
)
t.test(Flourescence ~ Tissue, data=df, paired=FALSE, var.equal=FALSE, alternative='greater')

## ----echo=FALSE----------------------------------------------------------
kable(Table2.6) %>% kable_styling()

## ----message=FALSE-------------------------------------------------------
df <- data.frame(
	'Depth'=c(Table2.6$Tip1, Table2.6$Tip2),
	'Tip'=factor(c(rep('Tip1', 10), rep('Tip2', 10)))
)
t.test(Depth ~ Tip, data=df, paired=TRUE)

