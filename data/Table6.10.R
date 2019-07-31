Table6.10 <- cbind(
	expand.grid(
		'Temperature'=c(-1,1),
		'Pressure'=c(-1,1),
		'Formaldehyde'=c(-1,1),
		'StirringRate'=c(-1,1)
	),
	'Filtration'=c(45,71,48,65,68,60,80,65,43,100,45,104,75,86,70,96),
	'Block'=factor(c(1, 2, 2, 1, 2, 1, 1, 2, 2, 1, 1, 2, 1, 2, 2, 1))
)
