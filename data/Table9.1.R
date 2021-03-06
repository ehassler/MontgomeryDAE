Table9.1 <- data.frame(
	'NozzleType'=factor(c(rep(1, 18), rep(2, 18), rep(3, 18))),
	'Speed'=rep(c(rep(100, 6), rep(120, 6), rep(140, 6)), 3),
	'Pressure'=rep(c(10, 10, 15, 15, 20, 20), 9),
	'SyrupLoss'=c(-35, -25, 110, 75, 4, 5,
				  -45, -60, -10, 30, -40, -30,
				  -40, 15, 80, 54, 31, 36,
				  17, 24, 55, 120, -23, -5,
				  -65, -58, -55, -44, -64, -62,
				  20, 4, 110, 44, -20, -31,
				  -39, -35, 90, 113, -30, -55,
				  -55, -67, -28, -26, -61, -52,
				  15, -30, 110, 135, 54, 4)
)
