Table8.3 <- expand.grid(
	'A'=c(-1, 1),
	'B'=c(-1, 1),
	'C'=c(-1, 1)
)
Table8.3[,'D'] = Table8.3[,'A'] * Table8.3[,'B'] * Table8.3[,'C']
Table8.3[,'FiltrationRate'] = c(45, 100, 45, 65, 75, 60, 80, 96)
