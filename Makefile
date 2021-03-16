
install_package:
	Rscript -e "devtools::install('.')"

check_package:
	Rscript -e "devtools::check(error_on='error')"

build_package:
	Rscript -e "devtools::build('.')"

test_package:
	Rscript -e "devtools::test()"

check_build:
	Rscript -e "devtools::check(error_on='error')" 
	Rscript -e "devtools::build('.')"

load_all_package:
	Rscript -e "devtools::load_all()"

document_package:
	Rscript -e "devtools::document()"
	Rscript -e "devtools::build_readme()"
#	Rscript -e "rmarkdown::render('README.Rmd',output_file='README.md')"

update_version:
	bash update_version.sh

githubactions_pkgdown_site:
	Rscript -e "usethis::use_pkgdown()"
	Rscript -e "pkgdown::build_site()"
	Rscript -e 'usethis::use_github_action("pkgdown")'

githubactions_check:
	Rscript -e 'usethis::use_github_actions()'

codetoolscheck:
	Rscript -e "library(CimpleG)" -e "codetools::checkUsagePackage('CimpleG')"

update_setup:
	Rscript setup_proj.R
