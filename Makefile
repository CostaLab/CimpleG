
pkg_pipeline: update_setup update_version check_package document_package build_package install_package

install_package:
	Rscript -e "devtools::install('.')"

check_package:
	Rscript -e "devtools::check(error_on='error')"

build_package:
	Rscript -e "devtools::build('.')"

test_package:
	Rscript --max-ppsize=500000 -e "devtools::test()"

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
	Rscript -e "library(CimpleG)" \
		-e "codetools::checkUsagePackage('CimpleG')" \
		-e "codetools::findGlobals('CimpleG', merge=FALSE)"

update_setup:
	Rscript setup_proj.R
