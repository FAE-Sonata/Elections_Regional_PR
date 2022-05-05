libraries_needed<-c("data.table", "openxlsx", "magrittr", "stringr",
                    "lubridate", "ggplot2")
lapply(libraries_needed,require,character.only=TRUE)
setwd("C:/HY/Projects/Elections")
PR_THRESHOLD_PCT<-5 / 100
region_definitions<-fread("CDN_regional_definitions.csv")

## TODO: use HFER_e.csv (combined file)
all_ridings_pre_2010<-fread("HFER_e.csv")
# cleaning; 1) extract election year; 2) "accl." is elected unopposed
# 3) "No affiliation" in Party is considered Independent
all_ridings_pre_2010[,`:=`(Party=ifelse(
  grepl("^No affiliation", Party, ignore.case=T), "Independent", Party),
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

obtain_region_yr<-function(full_dt) {
  elected_mps<-full_dt[Elected & grepl(
    "Gen", `Election Type`,ignore.case=T),]
  region_ridings_yr<-elected_mps[,
                                 riding_count:=.N,
                                 ,by=.(election_year,Region)] %>%
    unique(by=c("election_year", "Region"))
  region_ridings_yr<-region_ridings_yr[,.(election_year,
                                          Region,
                                          riding_count)]
  return(region_ridings_yr)
}
region_ridings_yr<-obtain_region_yr(all_ridings_pre_2010)

viz_by_region_yr<-function(dt) {
  dt %>% ggplot(aes(election_year, riding_count)) +
    geom_line() +
    facet_wrap(~Region)
}
viz_by_region_yr(region_ridings_yr)

# the Alberta riding of Victoria is erroneously classified as "Gen"
all_ridings_pre_2010[grepl("Alberta", Province, ignore.case=T) &
                       grepl("VICTORIA", Riding, ignore.case=T) &
                       election_year==1909,
                     `Election Type`:="B/P"]
all_ridings_pre_2010[,election_type_human:=ifelse(
  `Election Type`=="Gen", "General", "By-election"),]

# re-calculate data tables
region_ridings_yr<-obtain_region_yr(all_ridings_pre_2010)
## fixed now
viz_by_region_yr(region_ridings_yr)

all_ridings_pre_2010 %<>% merge.data.table(region_ridings_yr,
                                           by=c("election_year",
                                                "Region"))
party_votes_by_region<-all_ridings_pre_2010[election_type_human=="General",
                                            total_votes:=sum(cleaned_votes,
                                                             na.rm=T),
                                            by=.(election_year,
                                                 Region,
                                                 Party)] %>%
  unique(by=c("election_year",
              "Region",
              "Party"))
# trim columns
party_votes_by_region<-party_votes_by_region[,.(election_year,
                                                Region,
                                                riding_count,
                                                Party,
                                                total_votes)]
# ridings_by_raw_majority<-function(dt_ridings) {
#   # dt_ridings<-all_ridings
# }
mulroney_landslide<-party_votes_by_region[election_year==1984,]

calc_dhondt<-function(dt_region_party_totals) {
  # dt_region_party_totals<-mulroney_landslide
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
    parties_threshold<-region_subset$Party[idx_threshold]
    total_mps<-region_subset$riding_count[1]
    denominator<-seq(total_mps)
    region_subset$total_votes[1] / denominator
    divide_by_party<-function(party) {
      # party<-"Progressive Conservative Party"
      votes<-region_subset[Party==party,.(total_votes)] %>%
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
    res<-data.table(Party=region_subset$Party,
                    MPs=mps_by_party)
    res %<>% merge(region_subset, by="Party")
    # trim columns
    res<-res[,.(election_year,
                Region,
                riding_count,
                Party,
                MPs)]
    return(res)
  }
  lst_mps_by_region<-lapply(unique(dt_region_party_totals$Region), calc_region)
  return(rbindlist(lst_mps_by_region))
}

three_party_elections<-party_votes_by_region[election_year > 1920,]
lst_three<-split(three_party_elections, three_party_elections$election_year)
lst_pr_by_yr<-lapply(lst_three, calc_dhondt)
pr_by_yr<-rbindlist(lst_pr_by_yr)

## TODO: update
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