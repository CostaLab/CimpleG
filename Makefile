
install_package:
	Rscript -e "devtools::install('.')"

build_package:
	Rscript -e "devtools::build('.')"
#devtools::document('.')

