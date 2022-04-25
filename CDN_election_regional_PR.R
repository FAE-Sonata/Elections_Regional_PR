libraries_needed<-c("data.table", "openxlsx", "magrittr", "stringr",
                    "lubridate")
lapply(libraries_needed,require,character.only=TRUE)
setwd("C:/HY/Elections")
PR_THRESHOLD_PCT<-5 / 100
region_definitions<-fread("CDN_regional_definitions.csv")

## TODO: use HFER_e.csv (combined file)
all_ridings_pre_2010<-fread("HFER_e.csv")
# cleaning; 1) extract election year; 2) "accl." is elected unopposed
# 3) "No affiliation" in Party is considered Independent
all_ridings_pre_2010[,`:=`(Party=ifelse(
  grepl("^No affiliation", Party, ignore.case=T), "Independent", Party),
  election_type_human=ifelse(`Election Type`=="Gen", "General", "By-election"),
  election_year=ifelse(grepl('Gen',
                             `Election Type`,
                             ignore.case=T),
                       `Election Date` %>%
                         ymd %>%
                         year,
                       NA),
  cleaned_votes=as.integer(Votes),
  `Votes (%)`=ifelse(grepl("accl", Votes,
                           ignore.case=T),
                     100,
                     `Votes (%)`))]
# join with regional definitions
all_ridings_pre_2010 %<>% merge(region_definitions, by="Province")
# TMP #
YEAR<-1984

# all_ridings<-fread("cdn_election_35_1993Oct_candidates_all ridings-CLEANED.csv")
# # "No affilitation to a recognised party" is considered Independent
# all_ridings[grepl("^No affiliation", `Political Affiliation`, ignore.case=T),
#             `Political Affiliation`:="Independent"]
elected_mps<-all_ridings[grepl("Elected", Result, ignore.case=T),
                         riding_count:=.N,
                         by=.(Region)][
                           !is.na(riding_count),]
# remove variable (to be JOINed back in)
all_ridings<-all_ridings[,riding_count:=NULL]
ridings_by_region<-elected_mps %>% unique(by=c("Region", "riding_count"))
ridings_by_region<-ridings_by_region[,.(Region, riding_count)]

all_ridings %<>% merge.data.table(ridings_by_region, by="Region")
party_votes_by_region<-all_ridings[,total_votes:=sum(Votes),
                                   by=.(Region, `Political Affiliation`)] %>%
  unique(by=c("Region",
              "Political Affiliation"))
# trim columns
party_votes_by_region<-party_votes_by_region[,.(Region,
                                                riding_count,
                                                `Political Affiliation`,
                                                total_votes)]
# ridings_by_raw_majority<-function(dt_ridings) {
#   # dt_ridings<-all_ridings
# }

calc_dhondt<-function(dt_region_party_totals) {
  # dt_region_party_totals<-party_votes_by_region
  total_votes_by_region<-dt_region_party_totals[
    ,all_votes:=sum(total_votes),
    by=.(Region)][
      ,threshold:=ceiling(PR_THRESHOLD_PCT * all_votes)
    ][
      ,met_threshold:=total_votes >= threshold
    ]
  calc_region<-function(region) {
    # region<-"ON"
    region_subset<-total_votes_by_region[Region==region,]
    idx_threshold<-which(region_subset$met_threshold)
    parties_threshold<-region_subset$`Political Affiliation`[idx_threshold]
    total_mps<-region_subset$riding_count[1]
    denominator<-seq(total_mps)
    region_subset$total_votes[1] / denominator
    divide_by_party<-function(party) {
      # party<-"Progressive Conservative Party"
      votes<-region_subset[`Political Affiliation`==party,.(total_votes)] %>%
        unlist
      names(votes)<-NULL
      return(votes / denominator)
    }
    lst_all_party_divisors<-lapply(parties_threshold, divide_by_party)
    dhondt<-do.call(rbind, lst_all_party_divisors)# %>% as.data.table
    dhondt_sorted<-dhondt %>% as.numeric %>% sort %>% rev
    CUTOFF<-dhondt_sorted[total_mps]
    row.names(dhondt)<-parties_threshold
    dhondt_cutoff<-dhondt >= CUTOFF
    with_representation<-apply(dhondt_cutoff, MARGIN=1, FUN=sum)
    mps_by_party<-rep(0, nrow(region_subset))
    mps_by_party[idx_threshold]<-with_representation
    res<-data.table(`Political Affiliation`=region_subset$`Political Affiliation`,
                    MPs=mps_by_party)
    res %<>% merge(region_subset, by="Political Affiliation")
    # trim columns
    res<-res[,.(Region,
                riding_count,
                `Political Affiliation`,
                MPs)]
    return(res)
  }
  lst_mps_by_region<-lapply(unique(dt_region_party_totals$Region), calc_region)
  return(rbindlist(lst_mps_by_region))
}
mps_by_party_region<-calc_dhondt(party_votes_by_region)
national_total_mps<-mps_by_party_region[,total_mps:=sum(MPs),
                                        by=.(`Political Affiliation`)] %>%
  unique(by=c("Political Affiliation",
              "total_mps"))
national_total_mps<-national_total_mps[,.(`Political Affiliation`,
                                          total_mps)]
## TODO: calculate Gallagher, aggreggate non-represented parties into OTHERS
national_total_votes<-all_ridings[,national_votes:=sum(Votes),
                                  by=.(`Political Affiliation`)] %>%
  unique(by="Political Affiliation")
national_total_votes<-national_total_votes[
  ,.(`Political Affiliation`, national_votes)][
    ,all_votes:=sum(all_ridings$Votes)
  ][
    ,pct_vote:=100 * (national_votes/all_votes)
  ]