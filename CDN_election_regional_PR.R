libraries_needed<-c("data.table", "openxlsx", "magrittr", "stringr",
                    "lubridate", "ggplot2")
lapply(libraries_needed,require,character.only=TRUE)

setwd("C:/HY/Projects/Elections")
PR_THRESHOLD_PCT<-5 / 100
region_definitions<-fread("CDN_regional_definitions.csv")

all_elxn<-read.xlsx("cdn_generalelection_1_44_candidates.xlsx") %>% as.data.table
parl_num_indices<-which(grepl("^Parliament:\\s*\\d+",
                              all_elxn$Province.or.Territory))
re_ymd<-"\\d+\\-[0-1]\\d\\-[0-3]\\d"
ge_date_indices<-which(grepl(paste0("^Date of Election:\\s*", re_ymd),
                             all_elxn$Province.or.Territory))
stopifnot(all(ge_date_indices - parl_num_indices == 2))
parl_num<-all_elxn$Province.or.Territory[parl_num_indices] %>%
  str_extract(pattern="\\d+") %>%
  as.integer
ge_dates<-all_elxn$Province.or.Territory[ge_date_indices] %>%
  str_extract(pattern=re_ymd)
# upper_limits<-c(parl_num_indices[-1]-1, nrow(all_elxn))
create_from_header<-function(indices, info) {
  # indices<-parl_num_indices
  # info<-parl_num
  stopifnot(length(info) == length(indices))
  upper_limits<-c(indices[-1]-1, nrow(all_elxn))
  return(lapply(seq(length(indices)), function(k) rep(
    info[k], upper_limits[k] - indices[k] + 1)) %>% unlist)
  # return(do.call(rbind, chunks))
}

all_elxn<-all_elxn[,`:=`(
  Parliament=create_from_header(parl_num_indices, parl_num),
  Date=create_from_header(parl_num_indices, ge_dates) %>% ymd
)][!is.na(Constituency),]
old_size<-nrow(all_elxn)

# "MacLean, John Angus" in PEI 1953 repeated (remove this & possible other similar cases)
all_elxn<-unique(all_elxn, by=c("Parliament", "Constituency",
                                "Political.Affiliation", "Candidate"))
new_size<-nrow(all_elxn)
new_size - old_size

all_elxn<-all_elxn[,num_candidates:=.N,
                   by=.(Political.Affiliation, Parliament)][
                     ,elected:=grepl("Elected",Result)][
                       ,all_seats:=sum(elected), by=.(Parliament)
                     ]
hoc_size_by_parl<-unique(all_elxn, by=c("Parliament", "all_seats"))
hoc_size_by_parl<-hoc_size_by_parl[,.(Parliament, all_seats)]
hoc_size_by_parl %>% ggplot(aes(x=Parliament, y=all_seats)) + geom_line()
# spot check: OK, postwar, only 27th to the 28th (1965 Pearson to 1968 Trudeau)
# had a decrease in seats
all_elxn<-all_elxn[year(Date) >= 1921,]
setnames(all_elxn, c("Province.or.Territory"), c("Province"))
all_elxn %<>% merge(region_definitions, by="Province")
all_elxn<-all_elxn[,election_year:=year(Date)]

# aggregates of seat counts by region, election
obtain_region_yr<-function(full_dt) {
  region_ridings_yr<-full_dt[
    elected==T,
    riding_count:=.N,
    ,by=.(election_year,Region)] %>%
    unique(by=c("election_year", "Region"))
  region_ridings_yr<-region_ridings_yr[,.(election_year,
                                          Region,
                                          riding_count)]
  return(region_ridings_yr)
}
region_ridings_yr<-obtain_region_yr(all_elxn)
setkeyv(region_ridings_yr,c("election_year", "Region"))

# another spot check
viz_by_region_yr<-function(dt) {
  dt %>% ggplot(aes(election_year, riding_count)) +
    geom_line() +
    facet_wrap(~Region)
}
viz_by_region_yr(region_ridings_yr)
setkeyv(all_elxn, c("election_year", "Region"))
all_elxn[region_ridings_yr, riding_count := i.riding_count] # use 2nd instance of riding_count

setnames(all_elxn, c("Political.Affiliation"), c("Party"))
Filter(function(x) grepl("ind|no", x, ignore.case = T),
       unique(all_elxn$Party))
non_affiliated<-Filter(function(x) grepl("affil", x, ignore.case = T),
                       unique(all_elxn$Party))
all_elxn[Party %in% c("Unknown", "Independent"),Party:=non_affiliated]
party_votes_by_region<-all_elxn[,
                                total_votes:=sum(Votes),
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

aggregate_fptp<-function(dt_seats)  {
  dt_seats<-all_elxn
  return(dt_seats[,FPTP_MPs:=sum(elected),
                  by=.(election_year, Region, Party)] %>%
           unique(by=c("election_year",
                       "Region",
                       "Party")))
}

calc_dhondt<-function(dt_region_party_totals) {
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

lst_three<-split(party_votes_by_region, party_votes_by_region$election_year)
lst_pr_by_yr<-lapply(lst_three, calc_dhondt)
pr_by_yr<-rbindlist(lst_pr_by_yr)
setnames(pr_by_yr, c("MPs"), c("Regional_PR_MPs"))

fptp_results<-aggregate_fptp(all_elxn)
keycols<-Filter(function(x) !grepl("riding", x),
                names(pr_by_yr)[-ncol(pr_by_yr)])
fptp_names<-c(keycols, "FPTP_MPs")
fptp_results<-fptp_results[,..fptp_names]

# takes one of the above data tables and calculates national totals
calc_national_totals<-function(dt)  {
  # dt<-fptp_results
  original_name<-Filter(function(s) grepl("MPs$", s), names(dt))
  tmp_name<-"MPs"
  INTERMEDIATE_NAME<-"national_total"
  setnames(dt, original_name, tmp_name)
  
  dt_national<-dt[,national_total:=sum(MPs), by=.(election_year, Party)]
  # remove "MPs" column from dt_national without affecting dt
  national_names<-Filter(function(s) s != tmp_name, names(dt_national))
  dt_national<-dt_national[,..national_names]
  # avoid in-place modification
  dt_national$Region<-"NATIONAL"
  
  modified_names<-Filter(function(s) s != INTERMEDIATE_NAME, names(dt))
  dt<-dt[,..modified_names]
  
  setnames(dt, tmp_name, original_name)
  setnames(dt_national, INTERMEDIATE_NAME, original_name)
  dt_national<-unique(dt_national, by=c("election_year", "Party"))
  return(rbind(dt, dt_national))
}
pr_by_yr<-calc_national_totals(pr_by_yr)
fptp_results<-calc_national_totals(fptp_results)

setkeyv(pr_by_yr, keycols); setkeyv(fptp_results, keycols)
actual_vs_pr_results<-merge(pr_by_yr, fptp_results, no.dups = T)
fwrite(actual_vs_pr_results, "FPTP_vs_regional_PR_all_elections.csv")
# fwrite()

## TODO: calculate Gallagher, aggreggate non-represented parties into OTHERS
# national_total_votes<-all_ridings[,national_votes:=sum(Votes),
#                                   by=.(`Political Affiliation`)] %>%
#   unique(by="Political Affiliation")
# national_total_votes<-national_total_votes[
#   ,.(`Political Affiliation`, national_votes)][
#     ,all_votes:=sum(all_ridings$Votes)
#   ][
#     ,pct_vote:=100 * (national_votes/all_votes)
#   ]