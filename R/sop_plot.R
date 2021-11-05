#' @export
plot_sop <- function(rule, stage_stor){

	rule_temp <- rule %>%
		mutate(dead_acft = elev_to_vol(elev = dead, stage_stor = stage_stor)) %>%
		mutate(hedge_acft = elev_to_vol(elev = hedge, stage_stor = stage_stor)) %>%
		mutate(conservation_acft = elev_to_vol(elev = conservation, stage_stor = stage_stor)) %>%
		mutate(spill_acft = elev_to_vol(elev = spill, stage_stor = stage_stor)) %>%
		select(month, dead_acft, hedge_acft, conservation_acft, spill_acft, release_cfs)
	
	for (j in seq(1,12)){
		month_j <- data.frame(month = j, elev_ft = seq(min(stage_stor$elev_ft), max(stage_stor$elev_ft), length.out = 100)) %>%
			mutate(stor_prerelease = elev_to_vol(elev = elev_ft, stage_stor = stage_stor)) %>%
			left_join(rule_temp, by = "month") %>%
			mutate(release_ft3 = release_cfs * 30*24*60*60) %>%
			mutate(release_acft = release_ft3/43560) %>%
			select(-release_cfs, -release_ft3)

		if(j == 1){
			sop_df <- month_j
		} else {
			sop_df <- sop_df %>% bind_rows(month_j)
		}
	}

		### Calculate release for all but hedging
		sop_df <- sop_df %>%
			mutate(release_decision_acft = case_when(stor_prerelease <= dead_acft ~ 0,
                        stor_prerelease <= hedge_acft  ~ 	-999	,
                        stor_prerelease <= conservation_acft  ~ release_acft,						
                        stor_prerelease <= spill_acft  ~ release_acft + (stor_prerelease - conservation_acft),
                        stor_prerelease > spill_acft  ~ release_acft + (stor_prerelease - conservation_acft)				
						))
						
		hedge_row <- seq(1, dim(sop_df)[1])[sop_df$release_decision_acft ==-999]
			
		for(k in seq(1,length(hedge_row))){
			row_k <- hedge_row[k]
			sop_df$release_decision_acft[[row_k]] <- hedge_calc(stor_prerelease = sop_df$stor_prerelease[[row_k]],
				hedge = sop_df$hedge_acft[[row_k]], 
				dead = sop_df$dead_acft[[row_k]], 
				release = sop_df$release_acft[[row_k]])
		
		}
	
		
	non_hedge_pt <- rule_temp %>% 
			mutate(release_ft3 = release_cfs * 30*24*60*60) %>%
			mutate(release_decision_acft = release_ft3/43560) %>%
		mutate(stor_prerelease = dead_acft + release_decision_acft)%>%
		select(month, stor_prerelease, release_decision_acft)

	return(list(sop  = sop_df, non_hedge = non_hedge_pt))
	
}