Table14.16 <- data.frame(
	'Temperature'=rep(c(200, 225, 250, 275), 9),
	'Preperation'=factor(rep(c(rep(1, 4), rep(2, 4), rep(3, 4)), 3)),
	'Replicate'=factor(c(rep(1,12), rep(2,12), rep(3,12))),
	'TensileStrength'=c(30,35,37,36,
						34,41,38,42,
						29,26,33,36,
						28,32,40,41,
						31,36,42,40,
						31,30,32,40,
						31,37,41,40,
						35,40,39,44,
						32,34,39,45)
)
