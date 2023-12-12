
pkg_pipeline: update_setup update_version\
	check_package document_package build_package install_package\
	githubactions_pkgdown_site

install_package:
	Rscript -e "pak::pkg_install('.')"

check_package:
	Rscript -e "devtools::check(error_on='error')"

fastcheck_package:
	Rscript -e "devtools::check(error_on='error', args = c('--timings', '--no-tests', '--no-examples'))"

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
	Rscript -e "devtools::build_vignettes()"
	# Rscript -e "devtools::build_manual()"

update_version:
	bash update_version.sh

githubactions_pkgdown_site:
	Rscript -e "usethis::use_pkgdown()"
	sed -i '/docs/d' .gitignore
	Rscript -e "pkgdown::build_site(lazy=TRUE)"
	Rscript -e "pkgdown::build_favicons()"
	# gitcreds::gitcreds_set() if creds are missing
	#Rscript -e 'usethis::use_pkgdown_github_pages()'
	#Rscript -e 'usethis::use_github_pages(branch="main", path="/docs")'
	#Rscript -e 'usethis::use_github_action("pkgdown")'

#githubactions_check:
	# Rscript -e 'usethis::use_github_action("check-release")'
	#Rscript -e 'usethis::use_github_actions()'

codetoolscheck:
	Rscript -e "library(CimpleG)" \
		-e "codetools::checkUsagePackage('CimpleG')" \
		-e "codetools::findGlobals('CimpleG', merge=FALSE)"

update_setup:
	Rscript setup_proj.R
