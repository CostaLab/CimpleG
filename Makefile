
install_package:
	Rscript -e "devtools::install('.')"

ignore_inbuild:
	Rscript -e 'usethis::use_build_ignore(c("test_data","devel","update_version.sh"))'
	
check_package:
	Rscript -e "devtools::check('.')"

build_package:
	Rscript -e "devtools::build('.')"

check_build:
	Rscript -e "devtools::check('.')" -e "devtools::build('.')"

load_all_package:
	Rscript -e "devtools::load_all()"

document_package:
	Rscript -e "devtools::document()"
	Rscript -e "rmarkdown::render('README.Rmd',output_file='README.md')"

update_package_version:
	bash update_version.sh
