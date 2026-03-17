
# this script is about the yearly updating process of the package
# (vdem, vpart, codebook)

# PREP: fork the package / update your fork on your personal rep
# pull latest package version to your local RStudio/git (version control set up)
# do the following updates

library(dplyr)

remove_latex <- function(x) {
    x <- gsub("\\\\textit\\{([^}]+)\\}", "\\1", x)
    x <- gsub("\\\\href\\{([^}]+)\\}\\{([^}]+)\\}", "\\2", x)
    x <- gsub("\\\\emph\\{([^}]+)\\}", "\\1", x)
    x <- gsub("\\\\underline\\{([^}]+)\\}", "\\1", x)
    x <- gsub("\\\\end\\{itemize\\}|\\\\begin\\{itemize\\}", "", x)
    x <- gsub("\\\\item", "", x)
    x <- gsub("\\\\nth\\{([^}]+)\\}", "\\1", x)
    x <- gsub("\\\\|\\n\t\t|\\t\t|\\t|\\n", "", x)
}

# Load the new datasets and save them as RData in the package folder "data"
# vdem
vdem <- readRDS("~/data/ds_construction/v16/dataset/V-Dem-CY-Full+Others/V-Dem-CY-Full+Others-v16.rds")

save("vdem", file = "data/vdem.RData")

# vparty (if there are updates)
vparty <- readRDS("V-Dem-CPD-Party-V2.rds")
save("vparty", file = "data/vparty.RData")

# Load vdem codebook
# save as RData in the package folder "data"
# -- fix for ellodiseff because formula is messing up clarification
ellodiseff <- "We have used different calculations to find the lower chamber election district effective magnitude value, depending on the electoral system. In electoral systems with reserved seats, reserved seats are treated as a second tier in a hybrid system. Effective magnitude is calculated separately for reserved seats. Effective magnitude in such systems is the weighted average where the weight is the proportion of seats allocated in each tier."

codebook <- readRDS("~/proj/reference_documents/refs/codebook.rds") %>%
    select(name, vartype, tag, projectmanager, question, clarification, responses,
        scale, notes, crosscoder_aggregation, cy_aggregation, datarelease, years, convergence) %>%
    filter(!grepl("commnt|coment|intro|v[23]zz", tag)) %>%
    # fix formulas (most formulas are in aggregation and therefore not included in the package)
    mutate(clarification = case_when(tag == "v2ellodiseff" ~ ellodiseff,
            TRUE ~ clarification),
        tag = case_when(tag == "v2histname" ~ "histname",
            tag == "codingstart_contemporary" ~ "codingstart_contemp",
            TRUE ~ tag)) %>%
    # remove LaTeX code
    mutate(question = remove_latex(question),
        clarification = remove_latex(clarification),
        responses = remove_latex(responses),
        notes = remove_latex(notes),
        crosscoder_aggregation = remove_latex(crosscoder_aggregation),
        datarelease = remove_latex(datarelease),
        convergence = remove_latex(convergence))

dif <- setdiff(codebook$tag, unique(names(vdem)))
select(vdem, starts_with(dif)) %>% names()

save("codebook", file = "data/codebook.RData")

# do any additional changes/updates of the scripts in
# package folder /R or DESCRIPTION or README.md if required
# e.g. update citation of datasets, package, etc.

# document and check new package version
devtools::document()
devtools::check()

# Update package tar.gz file
devtools::build(path = "~/proj/vdemdata")

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
