
# from https://stackoverflow.com/a/56220622
echo "••••••••••••••••••••••••••••••••••••••••••••"
echo "• Updating package date and version number •"
echo "••••••••••••••••••••••••••••••••••••••••••••"
sed -i -- "s/^Date: .*/Date: $(date '+%Y-%m-%d')/" DESCRIPTION
# get latest tags
git pull --tags --quiet
current_tag=`git describe --tags --abbrev=0 | sed 's/v//'`
current_commit=`git describe --tags | sed 's/.*-\(.*\)-.*/\1/'`
# combine tag (e.g. 0.1.0) and commit number (like 40) increased by 9000 to indicate beta version
new_version="$current_tag.$((current_commit + 9000))" # results in 0.1.0.9040
sed -i -- "s/^Version: .*/Version: ${new_version}/" DESCRIPTION
echo "First 3 lines of DESCRIPTION:"
head -5 DESCRIPTION
echo
# ... after here more commands like devtools::document() and git commit

