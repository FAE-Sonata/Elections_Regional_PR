libraries_needed<-c("data.table", "openxlsx", "magrittr", "stringr", "lubridate", "ggplot2")
lapply(libraries_needed,require,character.only=TRUE)
# library(shiny)
setwd("C:/HY/Projects/Elections/datasets/")
# source("CDN_election_regional_PR.R")
actual_vs_pr_results<-fread("../BasicBars_by_region/FPTP_vs_regional_PR_all_elections.csv", encoding="UTF-8")
all_elxn<-fread("federal_elections_since_1921_cleaned.csv", encoding="UTF-8")
party_key<-fread("elected_only.csv", encoding="UTF-8") %>% unique(by="Party")
party_key<-party_key[,.(Party, Hexadecimal_colour)]
setkey(party_key, "Party")

calc_vote_discrepancy<-function(yr, region=NA, parties_re)  {
  stopifnot(is.na(region) | # region %in% all_elxn$Province |
              region %in% all_elxn$Region)
  # yr<-1993
  # parties_re<-"^(Liberal Party|Bloc Q|Reform|New Democratic|Progressive Conservative)"
  parties<-Filter(function(x) grepl(parties_re, x), unique(all_elxn$Party))
  if(length(parties) < 2) return(NA)
  votes_by_constituency<-NULL; fptp_results<-NULL
  if(is.na(region)) {
    votes_by_constituency<-all_elxn[election_year==yr,]
    fptp_results<-actual_vs_pr_results[election_year==yr & Region=="NATIONAL",]
  }
  # else if(region %in% all_elxn$Province)  {
  #   votes_by_constituency<-all_elxn[election_year==yr & Province==region,]
  #   fptp_results<-actual_vs_pr_results
  # }
  else  {
    votes_by_constituency<-all_elxn[election_year==yr & Region==region,]
    fptp_results<-actual_vs_pr_results[election_year==yr & Region==region,]
  }
  
  votes_by_constituency<-votes_by_constituency[,Party:=
                                                 ifelse(Party %in% parties,
                                                        Party,
                                                        "OTHER")]
  
  votes_by_party<-votes_by_constituency[,total_votes:=sum(Votes), by=.(Party)] %>%
    unique(by="Party")
  votes_by_party<-votes_by_party[,.(Party, total_votes)][
    ,vote_pct:=total_votes / sum(total_votes) * 100]
  
  setkey(votes_by_party, "Party")
  votes_by_party %<>% merge(party_key, all.x=T)
  
  fptp_results<-fptp_results[,Party:=
                               ifelse(Party %in% parties,
                                      Party,
                                      "OTHER")
  ][,MPs:=sum(FPTP_MPs), by=.(Party)
  ] %>% unique(by="Party")
  fptp_results<-fptp_results[,pct_seats:=MPs / sum(MPs) * 100][
    ,.(Party, MPs, pct_seats)]
  setkey(fptp_results, "Party")
  votes_by_party %<>% merge(fptp_results)
  votes_by_party<-votes_by_party[,discrepancy:=pct_seats - vote_pct]
  return(votes_by_party)
}

dt_1993<-calc_vote_discrepancy(yr=1993,
                               parties_re="^(Liberal Party|Bloc Q|Reform|New Democratic|Progressive Conservative)")
qc_1980<-calc_vote_discrepancy(yr=1980,
                               region = "QC",
                               parties_re="^(Liberal Party|New Democratic|Progressive Conservative|Social Credit)")

graph_vote<-function(dt, var_name)  {
  stopifnot(var_name %in% c("Share of vote", "discrepancy"))
  base_plot<-NULL
  if(var_name=="Share of vote")
    base_plot<-dt %>% ggplot(aes(x = Party, y = vote_pct, fill=Party))
  else
    base_plot<-dt %>% ggplot(aes(x = Party, y = discrepancy, fill=Party))
  p<-base_plot +
    labs(y=paste(var_name, "(%)")) +
    geom_bar(stat="identity", width=0.5) +
    scale_fill_manual(values = dt$Hexadecimal_colour) +
    guides(fill="none")
  if(var_name=="Share of vote")
    p<-p + geom_text(aes(label=paste0(round(vote_pct,1),"%")), size=4)
  else
    p<-p + geom_text(aes(y=discrepancy/2,
                         label=paste0(round(discrepancy,1),"%")), size=4)
  p + coord_flip(clip = "off")
}
graph_vote(dt_1993, "Share of vote")
graph_vote(dt_1993, "discrepancy")

graph_vote(qc_1980, "Share of vote")
graph_vote(qc_1980, "discrepancy")