if (!require(devtools)) install.packages("pacman")
pacman::p_load(here, RefManageR, knitr)

pubs <- GetBibEntryWithDOI(
  c(
    "10.3758/bf03331287",
    "10.2307/1128859",
    "10.1007/s00426-015-0681-x",
    "10.3389/fpsyg.2015.01171",
    "10.3389/fpsyg.2019.02767",
    "10.1080/17470210902752096",
    "10.1080/17470218.2016.1182193",
    "10.1080/17470218.2016.1206130",
    "10.1037/xhp0000801",
    "10.3758/bf03195542",
    "10.1016/j.jml.2004.03.002",
    "10.18637/jss.v067.i01",
    "10.3758/s13428-018-1037-4",
    "10.1037/xhp0000656",
    "10.21105/joss.01686",
    "10.1214/ss/1177011136",
    "10.1037/xlm0000670"
  )
)

annyang_citation <- BibEntry(
  "manual",
  key = "js-annyang",
  author = "Tal Ater",
  title = "annyang! Easily add speech recognition to your site",
  year = "2017",
  url = "https://github.com/TalAter/annyang"
)

WriteBib(
  c(pubs, annyang_citation),
  here("manuscript", "article_refs.bib")
)


write_bib(
  c(
    "base",
    "bayestestR",
    "brms",
    "cmdstanr",
    "googleLanguageR",
    "googleCloudStorageR",
    "lme4",
    "papaja",
    "rmarkdown",
    "tidybayes",
    "tuneR",
    "VoiceExperiment"
  ),
  here("manuscript", "r_package_refs.bib")
)
