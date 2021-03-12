
install_package:
	Rscript -e "devtools::install('.')"

check_package:
	Rscript -e "devtools::check('.')"

build_package:
	Rscript -e "devtools::build('.')"

test_package:
	Rscript -e "devtools::test()"

check_build:
	Rscript -e "devtools::check('.')" -e "devtools::build('.')"

load_all_package:
	Rscript -e "devtools::load_all()"

document_package:
	Rscript -e "devtools::document()"
	Rscript -e "rmarkdown::render('README.Rmd',output_file='README.md')"

update_package_version:
	bash update_version.sh

use_pkgdown:
	Rscript -e "usethis::use_pkgdown()"

build_site:
	Rscript -e "pkgdown::build_site()"

githubactions_site:
	Rscript -e 'usethis::use_github_action("pkgdown")'

githubactions_check:
	Rscript -e 'usethis::use_github_actions()'

update_pkg_setup:
	Rscript setup_proj.R
