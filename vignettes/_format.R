library(knitr)
library(kableExtra)
library(MontgomeryDAE)


options(contrasts=c('unordered'='contr.sum', 'ordered'='contr.poly'))


if(Sys.getenv('RTEX_BUILD') != 'TRUE'){
	# Vignette version

	kable <- function(x, format, digits = getOption("digits"), row.names = NA,
					  col.names = NA, align, caption = NULL, label = NULL, format.args = list(),
					  escape = TRUE, ...){
		knitr::kable(x, format, digits, row.names, col.names, align, NULL, label, format.args, escape, ...)
	}

	kable_styling <- function(kable_input, bootstrap_options = "basic", latex_options = "basic",
							  full_width = FALSE, position = "center", font_size = NULL,
							  row_label_position = "l", repeat_header_text = "\\textit{(continued)}",
							  repeat_header_method = c("append", "replace"), repeat_header_continued = FALSE,
							  stripe_color = "gray!6", stripe_index = NULL, latex_table_env = NULL,
							  protect_latex = TRUE, table.envir = "table", fixed_thead = FALSE)
	{
		kableExtra::kable_styling(kable_input, bootstrap_options, latex_options, full_width, position, font_size,
								  row_label_position, repeat_header_text, repeat_header_method, repeat_header_continued,
								  stripe_color, stripe_index, latex_table_env, protect_latex, table.envir, fixed_thead)
	}

} else {
	# Latex version

	options(warn=-1)
	kable <- function(x, format, digits = getOption("digits"), row.names = NA,
					  col.names = NA, align, caption = NULL, label = NULL, format.args = list(),
					  escape = TRUE, ...)
	{
		knitr::kable(x, format, digits, row.names, col.names, align, caption, label, format.args, escape, booktabs=TRUE, linesep="", ...)
	}

	kable_styling <- function(kable_input, bootstrap_options = "basic", latex_options = "basic",
						   full_width = NULL, position = "center", font_size = NULL,
						   row_label_position = "l", repeat_header_text = "\\textit{(continued)}",
						   repeat_header_method = c("append", "replace"), repeat_header_continued = FALSE,
						   stripe_color = "gray!6", stripe_index = NULL, latex_table_env = NULL,
						   protect_latex = TRUE, table.envir = "table", fixed_thead = FALSE)
	{
		kableExtra::kable_styling(kable_input, bootstrap_options, latex_options, full_width, position, font_size,
								  row_label_position, repeat_header_text, repeat_header_method, repeat_header_continued,
								  stripe_color, stripe_index, latex_table_env, protect_latex, table.envir, fixed_thead)
	}

	opts_chunk$set(
		out.extra='scale=0.6'
	)
	pdf.options(useDingbats=FALSE)

}

opts_chunk$set(
	collapse = TRUE,
	comment = '#',
	fig.pos='H'
)

