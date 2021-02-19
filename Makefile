
install_package:
	Rscript -e "devtools::install('.')"

build_package:
	Rscript -e 'usethis::use_build_ignore(c("test_data"))' -e "devtools::build('.')"

load_all_package:
	Rscript -e "devtools::load_all()"

document_package:
	Rscript -e "devtools::document()"

update_package_version:
	bash update_version.sh

