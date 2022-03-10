
# this script is about the yearly updating process of the package
# (vdem, vpart, codebook)

# PREP: fork the package / update your fork on your personal rep
# pull latest package version to your local RStudio/git (version control set up)
# do the following updates

# load the new datasets and save them as RData in the package folder "data"
# vdem
vdem <- readRDS("V-Dem-CY-Full+Others-v12.rds")
save("vdem", file = "data/vdem.RData")
# vparty
vparty <- readRDS("V-Dem-CPD-Party-V2.rds")
save("vparty", file = "data/vparty.RData")

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
# all done and up-to-date :-)
