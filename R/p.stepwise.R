p.stepwise <- function(model, scope, alpha.to.enter, alpha.to.leave, trace=TRUE, maxit=100){
	if(trace){ cat(sprintf('Beginning Stepwise Regression\n\tModel: %s\nSteps:\n', deparse(model$call[[2]]))) }
	# Addition helper
	try.addition <- function(model){
		additions <- add1(model, scope, test='F')
		i.add <- which.min(additions[['Pr(>F)']])
		p.add <- additions[['Pr(>F)']][i.add]
		if(p.add > alpha.to.enter){
			return(model)
		}
		s.add <- rownames(additions)[i.add]
		model <- update(model, sprintf('.~. + %s', s.add))
		s.model <- summary(model)
		if(trace){cat(sprintf('\tAdding %s at p=%0.4f. Model sigma=%f, R^2=%0.1f%%, Adj-R^2=%0.1f%%\n', s.add, p.add, s.model$sigma, 100*s.model$r.squared, 100*s.model$adj.r.squared))}
		return(model)
	}
	# Subtraction helper
	try.subtraction <- function(model){
		while(length(coef(model)) > 0 || !all(names(coef(model)) == '(Intercept)')){
			subtractions <- drop1(model, test='F')
			i.sub <- which.max(subtractions[['Pr(>F)']])
			p.sub <- subtractions[['Pr(>F)']][i.sub]
			if(p.sub < alpha.to.leave){
				return(model)
			}
			s.sub <- rownames(subtractions)[i.sub]
			model <-update(model, as.formula(sprintf('.~. - %s', s.sub)))
			s.model <- summary(model)
			if(trace){cat(sprintf('\tRemoving %s at p=%0.4f. Model sigma=%f, R^2=%0.1f%%, Adj-R^2=%0.1f%%\n', s.sub, p.sub, s.model$sigma, 100*s.model$r.squared, 100*s.model$adj.r.squared))}
		}
		return(model)
	}
	# Step it!
	for(i in 1:maxit){
		start.terms <- names(coef(model))
		model <- try.addition(model)
		added.term <- names(coef(model))[!(names(coef(model)) %in% start.terms)]
		end.terms <- names(coef(model))
		model <- try.subtraction(model)
		removed.terms <- names(coef(model))[!(names(coef(model)) %in% end.terms)]


		if(length(added.term) == 0 && length(removed.terms) ==0){
			return(model)
		}
	}
	stop('Failed to converge.')
}
