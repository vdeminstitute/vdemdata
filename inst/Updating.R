
# this script is about the yearly updating process of the package
# (vdem, vpart, codebook)

# PREP: fork the package / update your fork on your personal rep
# pull latest package version to your local RStudio/git (version control set up)
# do the following updates

# load the new datasets and save them as RData in the package folder "data"
# vdem
vdem <- readRDS("V-Dem-CY-Full+Others-v13.rds")
save("vdem", file = "data/vdem.RData")
# vparty (if there are updates)
#vparty <- readRDS("V-Dem-CPD-Party-V2.rds")
#save("vparty", file = "data/vparty.RData")

# load vdem codebook
# NOTE: clean out LaTeX code is NOT done yet (future versions might do so)
# save as RData in the package folder "data"
codebook <- readRDS("codebook.rds")
save("codebook", file = "data/codebook.RData")

# do any additional changes/updates of the scripts in
# package folder /R or DESCRIPTION or README.md if required
# e.g. update citation of datasets, package, etc.

# document and check new package version
devtools::document()
devtools::check()

# push to your personal rep
# do a pull request to the original rep

#### Release #####
# Go to the repository on Github at https://github.com/vdeminstitute/vdemdata

# Click on "releases" on the right; you see a list with all previous releases

# Click on "Draft a new release"

# Choose a new tag for the release (e.g., V11.1, V12)

# Select a title for the release and adjust the description
# (mostly copy-paste from last release to keep it consistent)
# You can use the "save draft" and "preview" buttons to make sure the release looks good

# Click on "publish release"

# all done and up-to-date :-)
