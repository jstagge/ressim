#' @export
res_sim <- function(qin_cfs, stage_stor, rule, evap){
	### Required packages
	require(tidyverse)

	### Create inflow dataframe
	in_df <- qin_cfs %>%
		mutate(month = month(date), year = year(date)) %>%
		left_join(evap, by = "month") %>%
		mutate(flow_ft3 = flow_cfs * 60 * 60 * 24 * 30)

	### Add rule curve
	res_ts <- in_df %>%
		left_join(rule, by = "month")

	### Convert rule curve to vol
	res_ts <- res_ts %>%
		mutate(dead_ft3 = 43560*elev_to_vol(elev = dead, stage_stor = stage_stor)) %>%
		mutate(hedge_ft3 = 43560*elev_to_vol(elev = hedge, stage_stor = stage_stor)) %>%
		mutate(conservation_ft3 = 43560*elev_to_vol(elev = conservation, stage_stor = stage_stor)) %>%
		mutate(spill_ft3 = 43560*elev_to_vol(elev = spill, stage_stor = stage_stor)) %>%
		mutate(release_ft3 = release_cfs * 60*60*24*30)

	### Check that hedging is set up correctly
	res_ts <- res_ts %>%
		mutate(test = hedge_ft3 >= dead_ft3 + release_ft3)
	if(sum(res_ts$test == 0)){
		stop("Hedging volume must be greater than dead storage plus release volume")
	}
	res_ts <- res_ts %>%
		select(-test)


	### Set empty columns
	res_ts$stor_init_acft <- NA
	res_ts$area_init_acre <- NA
	res_ts$elev_init_ft <- NA
	
	res_ts$release_decision_ft3 <- NA
	res_ts$stor_final_acft <- NA
	res_ts$area_final_acre <- NA
	res_ts$elev_final_ft <- NA 
	
	### Set initial conditions, assume reservoir starts full
	res_ts$elev_init_ft[[1]] <- res_ts$conservation[[1]]
	
	### Calculate area and volume
	res_ts$area_init_acre[[1]] <- elev_to_area(elev = res_ts$conservation[[1]], stage_stor = stage_stor)
	res_ts$stor_init_acft[[1]] <- elev_to_vol(elev = res_ts$conservation[[1]], stage_stor = stage_stor)

	### Simulate each time step
	for(j in seq(1, dim(res_ts)[1])){
		### Copy to initial information if j > 1
		if (j > 1){
			res_ts$stor_init_acft[[j]] <- res_ts$stor_final_acft[[j-1]]
			res_ts$area_init_acre[[j]] <- res_ts$area_final_acre[[j-1]]
			res_ts$elev_init_ft[[j]] <- res_ts$elev_final_ft[[j-1]]	
		}

		### Extract row
		res_j <- res_ts[j,]
		
		### Calculate evap			
		res_j <- res_j %>%
			mutate(stor_init_ft3 = stor_init_acft * 43560) %>%
			mutate(area_init_ft2 = area_init_acre * 43560) %>%
			mutate(evap_ft3 = (evap_in/12)*area_init_ft2)	
		
		### Calculate decision	
		res_j$release_decision_ft3[[1]] <- release_dec(res_j)
		
		### Calculate final conditions
		res_j <- res_j %>%
			mutate(stor_final_ft3 = stor_init_ft3 + flow_ft3 - evap_ft3 - release_decision_ft3) %>%
			mutate(stor_final_acft = stor_final_ft3 / 43560) %>%
			mutate(elev_final_ft = vol_to_elev(vol = stor_final_acft, stage_stor = stage_stor)) %>%
			mutate(area_final_acre = elev_to_area(elev = elev_final_ft, stage_stor = stage_stor))
		
		### Insert in main dataframe
		res_ts$release_decision_ft3[[j]] <- res_j$release_decision_ft3[[1]]
		res_ts$stor_final_acft[[j]] <- res_j$stor_final_acft[[1]]
		res_ts$area_final_acre[[j]] <- res_j$area_final_acre[[1]]
		res_ts$elev_final_ft[[j]] <- res_j$elev_final_ft[[1]]
		
		### Delete res_ts
		rm(res_j)
	}

	res_ts <- res_ts %>%
		mutate(release_decision_cfs = release_decision_ft3 / (30*24*60*60))

	return(res_ts)
}