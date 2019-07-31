Table8.10 <- expand.grid(
	'A'=c(-1, 1),
	'B'=c(-1, 1),
	'C'=c(-1, 1),
	'D'=c(-1, 1)
)
Table8.10[,'E'] = Table8.10[,'A'] * Table8.10[,'B'] * Table8.10[,'C']
Table8.10[,'F'] = Table8.10[,'B'] * Table8.10[,'C'] * Table8.10[,'D']
Table8.10[,'Shrinkage'] = c(6, 10, 32, 60, 4, 15, 26, 60, 8, 12, 34, 60, 16, 5, 37, 52)
