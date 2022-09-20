libraries_needed<-c("data.table", "openxlsx", "magrittr", "stringr", "lubridate")
lapply(libraries_needed,require,character.only=TRUE)
# library(shiny)
setwd("C:/HY/Projects/Elections/BasicBars_by_region/")
# source("CDN_election_regional_PR.R")
party_colours_wp<-fread("wikipedia_cdn_party_colours.csv", encoding="UTF-8")
actual_vs_pr_results<-fread("FPTP_vs_regional_PR_all_elections.csv", encoding="UTF-8")
mp_cols<-which(grepl("MPs", names(actual_vs_pr_results)))
results_all_methods<-apply(actual_vs_pr_results[,.SD,.SDcols=mp_cols], MARGIN=1, sum)
has_elected<-actual_vs_pr_results[which(results_all_methods > 0),]

# Party colours per Wikipedia Project page --------
# https://en.wikipedia.org/w/index.php?title=Wikipedia:WikiProject_Political_parties_and_politicians_in_Canada/list_of_parties&oldid=1078477083#Federal_parties
parties_represented<-data.table(Party=unique(has_elected$Party))
setkey(parties_represented, "Party"); setkey(party_colours_wp, "Long_name")
parties_represented %<>% merge(party_colours_wp, by.x="Party", by.y="Long_name",
                               all.x=T)
# deal with diacritics in 2 QC parties
semi_manual_replace<-function(party_re) {
  parties_represented[grepl(party_re, Party, ignore.case=T),
                      Hexadecimal_colour:=unlist(party_colours_wp[
                        grepl(party_re, Long_name, ignore.case=T),
                        Hexadecimal_colour])]
}
semi_manual_replace("Bloc Q"); semi_manual_replace("Ralliement C")
# Bloc populaire canadien same colour as BQ
parties_represented[grepl("Bloc popul", Party, ignore.case=T),
                    Hexadecimal_colour:=unlist(party_colours_wp[
                      grepl("Bloc Q", Long_name, ignore.case=T),
                      Hexadecimal_colour])]
parties_represented<-parties_represented[,.(Party, Hexadecimal_colour)]

setkey(has_elected, "Party")
has_elected %<>% merge(parties_represented, all.x=T)
# fwrite(has_elected, "elected_only.csv")