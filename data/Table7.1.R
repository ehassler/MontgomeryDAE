Table7.1 <- cbind(
	rbind(
		expand.grid(
			'ReactantConcentration'=c(-1, 1),
			'Catalyst'=c(-1, 1)
		),
		expand.grid(
			'ReactantConcentration'=c(-1, 1),
			'Catalyst'=c(-1, 1)
		),
		expand.grid(
			'ReactantConcentration'=c(-1, 1),
			'Catalyst'=c(-1, 1)
		)
	),
	'Block'=factor(c(rep(1, 4), rep(2, 4), rep(3, 4))),
	'Yield'=c(
		28, 26, 18, 31,
		25, 32, 19, 30,
		27, 32, 23, 29
	)
)
