#' @export
release_dec <- function(res_temp){
	require(tidyverse)
	
	### Calculate inflow minus evap
	res_temp <- res_temp %>%
		mutate(in_evap_ft3 = flow_ft3 - evap_ft3)
		
	### Calculate stor without release
	res_temp <- res_temp %>%
		mutate(stor_init_ft3 = stor_init_acft * 43560) %>%
		mutate(stor_prerelease = stor_init_ft3 + in_evap_ft3) %>%
		mutate(hyp_stor = stor_prerelease - release_ft3)
		

	### Calculate release for all but hedging
	res_temp <- res_temp %>%
		mutate(release_decision_ft3 = case_when(stor_prerelease <= dead_ft3 ~ 0,
                        stor_prerelease <= hedge_ft3  ~ -999,
                        stor_prerelease <= conservation_ft3  ~ release_ft3,						
                        #stor_prerelease <= spill_ft3 & stor_init_ft3 < conservation_ft3  ~ release_ft3,
                        #stor_prerelease <= spill_ft3 & stor_init_ft3 >= conservation_ft3  ~ stor_prerelease - conservation_ft3,
						stor_prerelease <= spill_ft3  ~ release_ft3 + (stor_prerelease - conservation_ft3),
                        stor_prerelease > spill_ft3  ~ release_ft3 + (stor_prerelease - spill_ft3)						
						))
						
	### Calculate release for hedging
	if(res_temp$release_decision_ft3[[1]] == -999){
		res_temp$release_decision_ft3[[1]] <- hedge_calc(stor_prerelease = res_temp$stor_prerelease[[1]],
			hedge = res_temp$hedge_ft3[[1]], 
			dead = res_temp$dead_ft3[[1]], 
			release = res_temp$release_ft3[[1]])
	}

	return(unlist(res_temp$release_decision_ft3[[1]]))
}

#' @export
hedge_calc <- function(stor_prerelease, hedge, dead, release){
	require(tidyverse)
	require(zoo)
	
	hedge_df <- data.frame(stor_prerelease = c(dead,hedge), release = c(0, release))
	
	hedge_df <- hedge_df %>%
		mutate(col = "original") %>%
		bind_rows(data.frame(stor_prerelease = stor_prerelease, release = NA, col = "new")) %>%
		arrange(stor_prerelease) %>%
		mutate(release_est=zoo::na.approx(release, stor_prerelease))
	
	result_df <- hedge_df %>%
		filter(col == "new")

	return(unlist(result_df$release_est))
}
