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
    "10.1016/j.jml.2004.03.002",                                                # WriteBib doesn't like the é symbol, so after running this sript, I manually fix the article_refs.bib file  
    "10.3758/bf03195542",                                                       # Ditto. Also, the ampersand in the journal name gets messed up, I have to manually fix that, too
    "10.18637/jss.v067.i01",
    "10.3758/s13428-018-1037-4",
    "10.1037/xhp0000656",
    "10.1214/ss/1177011136",
    "10.1177/1747021818787155",
    "10.1037/xlm0001075",
    "10.3758/BF03198265",
    "10.1016/j.tics.2019.07.002",
    "10.1037/0033-295X.108.3.624",
    "10.3758/BF03196526",
    "10.3758/BF03193850",
    "10.3758/BF03197535",
    "10.3389/fpsyg.2012.00367",
    "10.1037/0033-2909.109.2.163",
    "10.1037/h0054651",
    "10.1146/annurev.neuro.24.1.167",
    "10.1002/9781118920497.ch4",
    "10.1016/j.neubiorev.2019.01.019",
    "10.3758/s13423-012-0373-0",
    "10.1523/JNEUROSCI.0934-12.2012",
    "10.3758/s13423-016-1057-y",
    "10.1037/a0019957",
    "10.3758/s13423-018-1520-z",
    "10.3758/s13423-016-1167-6",
    "10.1016/j.neubiorev.2014.06.001",
    "10.1016/j.cognition.2020.104220",
    "10.1016/j.neuron.2018.03.042",
    "10.1037/xhp0000989"
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
    "tidyverse",
    "tuneR",
    "VoiceExperiment"
  ),
  here("manuscript", "r_package_refs.bib")
)
