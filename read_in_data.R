library(rlist)
library(tidyverse)
#install.packages("factoextra")
library(factoextra)
#install.packages("corrplot")
library(corrplot)
library(ggplot2)
library(ggpubr)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("egg")
library(egg)
library(Rarity)

#install.packages("car") #An extremely useful/in-depth regression package
#install.packages("stargazer") #Produces easy to read regression results (similar to what you get in SPSS)
#install.packages("effects") #We will use this to create our interactions
#install.packages("ggplot2") #Our incredibly powerful and versatile graphing package
library(car)
library(stargazer)
library(effects)

#install.packages("interactions")
library(interactions)

# specify the number of replicates
n_rep <- 3

# generate a vector of the names of the simulations to be plotted
sim_names <- c( # all simulations
  "aa0010AMF",
  "aa0011AMF",
  "aa1010AMF",
  "aa1011AMF",
  "as0010AMF",
  "as0011AMF",
  "as1010AMF",
  "as1011AMF",
  "sa0010AMF",
  "sa0011AMF",
  "sa1010AMF",
  "sa1011AMF",
  "ss0010AMF",
  "ss0011AMF",
  "ss1010AMF",
  "ss1011AMF",
  "aa0110AMM",
  "aa0111AMM",
  "aa1110AMM",
  "aa1111AMM",
  "as0110AMM",
  "as0111AMM",
  "as1110AMM",
  "as1111AMM",
  "sa0110AMM",
  "sa0111AMM",
  "sa1110AMM",
  "sa1111AMM",
  "ss0110AMM",
  "ss0111AMM",
  "ss1110AMM",
  "ss1111AMM",
  "aa0100FFD",
  "aa0101FFD",
  "aa1100FFD",
  "aa1101FFD",
  "as0100FFD",
  "as0101FFD",
  "as1100FFD",
  "as1101FFD",
  "sa0100FFD",
  "sa0101FFD",
  "sa1100FFD",
  "sa1101FFD",
  "ss0100FFD",
  "ss0101FFD",
  "ss1100FFD",
  "ss1101FFD",
  "aa0000FFF",
  "aa0001FFF",
  "aa1000FFF",
  "aa1001FFF",
  "as0000FFF",
  "as0001FFF",
  "as1000FFF",
  "as1001FFF",
  "sa0000FFF",
  "sa0001FFF",
  "sa1000FFF",
  "sa1001FFF",
  "ss0000FFF",
  "ss0001FFF",
  "ss1000FFF",
  "ss1001FFF",
  "aa0100FFM",
  "aa0101FFM",
  "aa1100FFM",
  "aa1101FFM",
  "as0100FFM",
  "as0101FFM",
  "as1100FFM",
  "as1101FFM",
  "sa0100FFM",
  "sa0101FFM",
  "sa1100FFM",
  "sa1101FFM",
  "ss0100FFM",
  "ss0101FFM",
  "ss1100FFM",
  "ss1101FFM",
  "aa0110MDD",
  "aa0111MDD",
  "aa1110MDD",
  "aa1111MDD",
  "as0110MDD",
  "as0111MDD",
  "as1110MDD",
  "as1111MDD",
  "sa0110MDD",
  "sa0111MDD",
  "sa1110MDD",
  "sa1111MDD",
  "ss0110MDD",
  "ss0111MDD",
  "ss1110MDD",
  "ss1111MDD",
  "aa0010MDF",
  "aa0011MDF",
  "aa1010MDF",
  "aa1011MDF",
  "as0010MDF",
  "as0011MDF",
  "as1010MDF",
  "as1011MDF",
  "sa0010MDF",
  "sa0011MDF",
  "sa1010MDF",
  "sa1011MDF",
  "ss0010MDF",
  "ss0011MDF",
  "ss1010MDF",
  "ss1011MDF",
  "aa0010MMF",
  "aa0011MMF",
  "aa1010MMF",
  "aa1011MMF",
  "as0010MMF",
  "as0011MMF",
  "as1010MMF",
  "as1011MMF",
  "sa0010MMF",
  "sa0011MMF",
  "sa1010MMF",
  "sa1011MMF",
  "ss0010MMF",
  "ss0011MMF",
  "ss1010MMF",
  "ss1011MMF",
  "aa0110MMM",
  "aa0111MMM",
  "aa1110MMM",
  "aa1111MMM",
  "as0110MMM",
  "as0111MMM",
  "as1110MMM",
  "as1111MMM",
  "sa0110MMM",
  "sa0111MMM",
  "sa1110MMM",
  "sa1111MMM",
  "ss0110MMM",
  "ss0111MMM",
  "ss1110MMM",
  "ss1111MMM"
)

# create a vector recording the combination of independent variables for each similation
# note: no_extinction comes first, followed by extinction ones
nam_temp <- rep(NA, length(sim_names) * n_rep)
for ( rep_id in 1:n_rep) {
  nam_temp[(1:length(sim_names)) * n_rep - (n_rep - rep_id)] <- sim_names
}

nam <- c(
  paste(nam_temp, "no", sep = '_'),
  paste(nam_temp, "ex", sep = '_')
)


# read in the intermediate variables
# colonization
# prop_col_no <- read.csv("Output_no_extinction/prop_col.csv", header = T)
# prop_col_ex <- read.csv("Output_extinction/prop_col.csv", header = T)
# prop_col <- rbind(prop_col_no, prop_col_ex)
# 
# prop_col_host <- prop_col$prop_col_host
# prop_col_dep <- prop_col$prop_col_non

temp_list_no <- list.load("Output_no_extinction/prop_col.rds")
temp_list_ex <- list.load("Output_extinction/prop_col.rds")
temp_list <- c(temp_list_no, temp_list_ex)

# temp_list_no <- list.load("Output_no_extinction/prop_newborn.rds")
# temp_list_ex <- list.load("Output_extinction/prop_newborn.rds")
# temp_list <- c(temp_list_no, temp_list_ex)

prop_col_host <- rep(NA, length(temp_list))
prop_col_dep <- rep(NA, length(temp_list))

for (i in 1:length(prop_col_host)){
  prop_col_host[i] <- temp_list[[i]][[1]][[11]]
}

for (i in 1:length(prop_col_dep)){
  prop_col_dep[i] <- temp_list[[i]][[2]][[11]]
}


# age
# age_no <- read.csv("Output_no_extinction/age.csv", header = T)
# age_ex <- read.csv("Output_extinction/age.csv", header = T)
# age <- rbind(age_no, age_ex)
# 
# age_host <- age$age_host
# age_dep <- age$age_non

# # trait variation
# trait_var_no <- read.csv("Output_no_extinction/trait_var.csv", header = T)
# trait_var_ex <- read.csv("Output_extinction/trait_var.csv", header = T)
# trait_var <- rbind(trait_var_no, trait_var_ex)
# 
# trait_var_host <- trait_var$trait_var_host
# trait_var_dep <- trait_var$trait_var_non


#rang_siz_no <- read.csv("Output_no_extinction/rang_siz.csv", header = T)
#rang_siz_ex <- read.csv("Output_extinction/rang_siz.csv", header = T)
#rang_siz <- rbind(rang_siz_no, rang_siz_ex)

#rang_siz_mean_host <- rang_siz$rang_siz_host
#rang_siz_mean_dep <- rang_siz$rang_siz_non

#rang_siz_sum_host <- rang_siz$rang_siz_host_sum
#rang_siz_sum_dep <- rang_siz$rang_siz_non_sum

# demographic stochasticity
#dem_stoch_no <- read.csv("Output_no_extinction/demo_stoch.csv", header = T)
#dem_stoch_ex <- read.csv("Output_extinction/demo_stoch.csv", header = T)
#dem_stoch <- rbind(dem_stoch_no, dem_stoch_ex)

#dem_stoch_mean_host <- dem_stoch$demo_stoch_host
#dem_stoch_mean_dep <- dem_stoch$demo_stoch_non

# hybridization likelihood
# hybr_lik_no <- read.csv("Output_no_extinction/hybr_lik.csv", header = T)
# hybr_lik_ex <- read.csv("Output_extinction/hybr_lik.csv", header = T)
# hybr_lik <- rbind(hybr_lik_no, hybr_lik_ex)
# 
# hybr_lik_mean_host <- hybr_lik$hybr_lik_host
# hybr_lik_mean_dep <- hybr_lik$hybr_lik_non

temp_list_no <- list.load("Output_no_extinction/hybr_lik.rds")
temp_list_ex <- list.load("Output_extinction/hybr_lik.rds")
temp_list <- c(temp_list_no, temp_list_ex)

# temp_list_no <- list.load("Output_no_extinction/prop_newborn.rds")
# temp_list_ex <- list.load("Output_extinction/prop_newborn.rds")
# temp_list <- c(temp_list_no, temp_list_ex)

hybr_lik_mean_host <- rep(NA, length(temp_list))
hybr_lik_mean_dep <- rep(NA, length(temp_list))

for (i in 1:length(hybr_lik_mean_host)){
  hybr_lik_mean_host[i] <- temp_list[[i]][[1]]
}

for (i in 1:length(prop_col_dep)){
  hybr_lik_mean_dep[i] <- temp_list[[i]][[2]]
}


# trait temporl variation


temp_list_no <- list.load("Output_no_extinction/delta.rds")
temp_list_ex <- list.load("Output_extinction/delta.rds")
temp_list <- c(temp_list_no, temp_list_ex)

# temp_list_no <- list.load("Output_no_extinction/prop_newborn.rds")
# temp_list_ex <- list.load("Output_extinction/prop_newborn.rds")
# temp_list <- c(temp_list_no, temp_list_ex)

delta_host <- rep(NA, length(temp_list))
delta_dep <- rep(NA, length(temp_list))

for (i in 1:length(delta_host)){
  delta_host[i] <- temp_list[[i]][[1]]
}

for (i in 1:length(delta_dep)){
  delta_dep[i] <- temp_list[[i]][[2]]
}



# range or partner shift
# rang_part_shift_no <- read.csv("Output_no_extinction/rang_part_shift.csv", header = T)
# rang_part_shift_ex <- read.csv("Output_extinction/rang_part_shift.csv", header = T)
# rang_part_shift <- rbind(rang_part_shift_no, rang_part_shift_ex)
# 
# ana_rang_siz_bef_host <- rang_part_shift$ana_rang_siz_bef_host
# ana_rang_siz_aft_host <- rang_part_shift$ana_rang_siz_aft_host
# ana_rang_siz_prop_host <- rang_part_shift$ana_rang_siz_prop_host
# ana_rang_dist_gain_host <- rang_part_shift$ana_rang_dist_gain_host
# ana_rang_dist_loss_host <- rang_part_shift$ana_rang_dist_loss_host
# 
# clado_rang_siz_bef_host <- rang_part_shift$clado_rang_siz_bef_host
# clado_rang_siz_aft_host <- rang_part_shift$clado_rang_siz_aft_host
# clado_rang_siz_prop_host <- rang_part_shift$clado_rang_siz_prop_host
# clado_rang_dist_gain_host <- rang_part_shift$clado_rang_dist_gain_host
# clado_rang_dist_loss_host <- rang_part_shift$clado_rang_dist_loss_host
# 
# comb_rang_siz_bef_host <- rang_part_shift$comb_rang_siz_bef_host
# comb_rang_siz_aft_host <- rang_part_shift$comb_rang_siz_aft_host
# comb_rang_siz_prop_host <- rang_part_shift$comb_rang_siz_prop_host
# comb_rang_dist_gain_host <- rang_part_shift$comb_rang_dist_gain_host
# comb_rang_dist_loss_host <- rang_part_shift$comb_rang_dist_loss_host
# 
# ana_part_siz_bef_host <- rang_part_shift$ana_part_siz_bef_host
# ana_part_siz_aft_host <- rang_part_shift$ana_part_siz_aft_host
# ana_part_siz_prop_host <- rang_part_shift$ana_part_siz_prop_host
# ana_part_dist_gain_host <- rang_part_shift$ana_part_dist_gain_host
# ana_part_dist_loss_host <- rang_part_shift$ana_part_dist_loss_host
# 
# clado_part_siz_bef_host <- rang_part_shift$clado_part_siz_bef_host
# clado_part_siz_aft_host <- rang_part_shift$clado_part_siz_aft_host
# clado_part_siz_prop_host <- rang_part_shift$clado_part_siz_prop_host
# clado_part_dist_gain_host <- rang_part_shift$clado_part_dist_gain_host
# clado_part_dist_loss_host <- rang_part_shift$clado_part_dist_loss_host
# 
# comb_part_siz_bef_host <- rang_part_shift$comb_part_siz_bef_host
# comb_part_siz_aft_host <- rang_part_shift$comb_part_siz_aft_host
# comb_part_siz_prop_host <- rang_part_shift$comb_part_siz_prop_host
# comb_part_dist_gain_host <- rang_part_shift$comb_part_dist_gain_host
# comb_part_dist_loss_host <- rang_part_shift$comb_part_dist_loss_host
# 
# 
# 
# ana_rang_siz_bef_dep <- rang_part_shift$ana_rang_siz_bef_non
# ana_rang_siz_aft_dep <- rang_part_shift$ana_rang_siz_aft_non
# ana_rang_siz_prop_dep <- rang_part_shift$ana_rang_siz_prop_non
# ana_rang_dist_gain_dep <- rang_part_shift$ana_rang_dist_gain_non
# ana_rang_dist_loss_dep <- rang_part_shift$ana_rang_dist_loss_non
# 
# clado_rang_siz_bef_dep <- rang_part_shift$clado_rang_siz_bef_non
# clado_rang_siz_aft_dep <- rang_part_shift$clado_rang_siz_aft_non
# clado_rang_siz_prop_dep <- rang_part_shift$clado_rang_siz_prop_non
# clado_rang_dist_gain_dep <- rang_part_shift$clado_rang_dist_gain_non
# clado_rang_dist_loss_dep <- rang_part_shift$clado_rang_dist_loss_non
# 
# comb_rang_siz_bef_dep <- rang_part_shift$comb_rang_siz_bef_non
# comb_rang_siz_aft_dep <- rang_part_shift$comb_rang_siz_aft_non
# comb_rang_siz_prop_dep <- rang_part_shift$comb_rang_siz_prop_non
# comb_rang_dist_gain_dep <- rang_part_shift$comb_rang_dist_gain_non
# comb_rang_dist_loss_dep <- rang_part_shift$comb_rang_dist_loss_non
# 
# ana_part_siz_bef_dep <- rang_part_shift$ana_part_siz_bef_non
# ana_part_siz_aft_dep <- rang_part_shift$ana_part_siz_aft_non
# ana_part_siz_prop_dep <- rang_part_shift$ana_part_siz_prop_non
# ana_part_dist_gain_dep <- rang_part_shift$ana_part_dist_gain_non
# ana_part_dist_loss_dep <- rang_part_shift$ana_part_dist_loss_non
# 
# clado_part_siz_bef_dep <- rang_part_shift$clado_part_siz_bef_non
# clado_part_siz_aft_dep <- rang_part_shift$clado_part_siz_aft_non
# clado_part_siz_prop_dep <- rang_part_shift$clado_part_siz_prop_non
# clado_part_dist_gain_dep <- rang_part_shift$clado_part_dist_gain_non
# clado_part_dist_loss_dep <- rang_part_shift$clado_part_dist_loss_non
# 
# comb_part_siz_bef_dep <- rang_part_shift$comb_part_siz_bef_non
# comb_part_siz_aft_dep <- rang_part_shift$comb_part_siz_aft_non
# comb_part_siz_prop_dep <- rang_part_shift$comb_part_siz_prop_non
# comb_part_dist_gain_dep <- rang_part_shift$comb_part_dist_gain_non
# comb_part_dist_loss_dep <- rang_part_shift$comb_part_dist_loss_non



# read in all the calculated network-level variables
# read in quantitative modularity
#modularity_quant_no <- read.csv("Output_no_extinction/mod_quant.csv", header = T)[,2]
#modularity_quant_ex <- read.csv("Output_extinction/mod_quant.csv", header = T)[,2]
#modularity_quant <- c(modularity_quant_no, modularity_quant_ex)

# read in binary modularity
#modularity_bin_no <- read.csv("Output_no_extinction/mod_bin.csv", header = T)[,2]
#modularity_bin_ex <- read.csv("Output_extinction/mod_bin.csv", header = T)[,2]
#modularity_bin <- c(modularity_bin_no, modularity_bin_ex)

# read in quantitative nestedness
#nestedness_quant_no <- read.csv("Output_no_extinction/nest_quant.csv", header = T)[,2]
#nestedness_quant_ex <- read.csv("Output_extinction/nest_quant.csv", header = T)[,2]
#nestedness_quant <- c(nestedness_quant_no, nestedness_quant_ex)

# read in binary nestedness
#nestedness_bin_no <- read.csv("Output_no_extinction/nest_bin.csv", header = T)[,2]
#nestedness_bin_ex <- read.csv("Output_extinction/nest_bin.csv", header = T)[,2]
#nestedness_bin <- c(nestedness_bin_no, nestedness_bin_ex)

# read in interaction turnver data
# int_turn_no <- read.csv("Output_no_extinction/int_turn.csv", header = T)
# int_turn_ex <- read.csv("Output_extinction/int_turn.csv", header = T)
# int_turn <- rbind(int_turn_no, int_turn_ex)
# 
# gain_turn_both <- int_turn$gain_sp_both/int_turn$total_pairwise
# gain_turn_host <- int_turn$gain_sp_host/int_turn$total_pairwise
# gain_turn_dep <- int_turn$gain_sp_non/int_turn$total_pairwise
# 
# gain_rew <- int_turn$gain_rew/int_turn$total_pairwise
# 
# gain_total <- (gain_turn_both + gain_turn_host + gain_turn_dep + gain_rew) * int_turn$total_pairwise
# 
# loss_turn_both <- int_turn$loss_ex_both/int_turn$total_pairwise
# loss_turn_host <- int_turn$loss_ex_host/int_turn$total_pairwise
# loss_turn_dep <- int_turn$loss_ex_non/int_turn$total_pairwise
# 
# loss_rew <- int_turn$loss_rew/int_turn$total_pairwise
# 
# loss_total <- loss_turn_both + loss_turn_host + loss_turn_dep + loss_rew * int_turn$total_pairwise

# read in clade-level variables
# read in partnership conservatism
#part_cons_no <- read.csv("Output_no_extinction/part_cons.csv", header = T)
#part_cons_ex <- read.csv("Output_extinction/part_cons.csv", header = T)
#part_cons <- rbind(part_cons_no, part_cons_ex)

#part_cons_host_w <- part_cons$host_part_cons_w
#part_cons_dep_w <- part_cons$non_part_cons_w

#part_cons_host_u <- part_cons$host_part_cons_u
#part_cons_dep_u <- part_cons$non_part_cons_u

# read in trait conservatism
#trait_cons_no <- read.csv("Output_no_extinction/trait_cons.csv", header = T)
#trait_cons_ex <- read.csv("Output_extinction/trait_cons.csv", header = T)
#trait_cons <- rbind(trait_cons_no, trait_cons_ex)

#K_host <- trait_cons$host_K
#K_dep <- trait_cons$non_K

#lambda_host <- trait_cons$host_lambda
#lambda_dep <- trait_cons$non_lambda

# read in specificity data
# specificity_no <- read.csv("Output_no_extinction/specificity.csv", header = T)
# specificity_ex <- read.csv("Output_extinction/specificity.csv", header = T)
# specificity <- rbind(specificity_no, specificity_ex)
# 
# bas_spec_host <- specificity$bas_spec_host
# bas_spec_dep <- specificity$bas_spec_non
# 
# struct_spec_host <- specificity$struct_spec_host
# struct_spec_dep <- specificity$struct_spec_non
# 
# phylo_spec_host <- specificity$phylo_spec_host
# phylo_spec_dep <- specificity$phylo_spec_non
# 
# func_spec_host <- specificity$func_spec_host
# func_spec_dep <- specificity$func_spec_non

# read in species turnover and diversity
div_turn_no <- read.csv("Output_no_extinction/div_turn.csv", header = T)
div_turn_ex <- read.csv("Output_extinction/div_turn.csv", header = T)
div_turn <- rbind(div_turn_no, div_turn_ex)

# turn_host <- (div_turn$sp_host + div_turn$ex_host) / div_turn$div_host
# turn_dep <- (div_turn$sp_non + div_turn$ex_non) / div_turn$div_non
# turn_host <- div_turn$sp_host + div_turn$ex_host
# turn_dep <- div_turn$sp_non + div_turn$ex_non

div_host <- div_turn$div_host
div_dep <- div_turn$div_non

# read in mean phylogenetic distance among host lineages
#phylo_dist_host_no <- read.csv("Output_no_extinction/phylo_dist_host.csv", header = T)
#phylo_dist_host_ex <- read.csv("Output_extinction/phylo_dist_host.csv", header = T)
#phylo_dist_host <- rbind(phylo_dist_host_no, phylo_dist_host_ex)$pd_host

# store all data in a data frame
dat_raw <- data.frame(
  
  nam,
  
  # Colonization on new geographic sites
  1 - prop_col_host,
  1 - prop_col_dep,
  
  # age
  #log2(age_host),
  #log2(age_dep),
  
  # trait variation
  #trait_var_host,
  #trait_var_dep,
  
  # Range size fluctuation
  #rang_siz_mean_host , # rang_siz$rang_siz_host
  #rang_siz_mean_dep , # rang_siz$rang_siz_non
  
  #rang_siz_sum_host , # rang_siz$rang_siz_host_sum
  #rang_siz_sum_dep , # rang_siz$rang_siz_non_sum
  
  #comb_rang_dist_gain_host , # rang_part_shift$comb_rang_dist_gain_host
  #comb_rang_dist_loss_host , # rang_part_shift$comb_rang_dist_loss_host
  
  #comb_rang_dist_gain_dep , # rang_part_shift$comb_rang_dist_gain_dep
  #comb_rang_dist_loss_dep , # rang_part_shift$comb_rang_dist_loss_dep
  
  # Demographic stochasticity
  #dem_stoch_mean_host , # dem_stoch$dem_stoch_host
  #dem_stoch_mean_dep , # dem_stoch$dem_stoch_non
  
  # Hybridization
  as.numeric(hybr_lik_mean_host) , # hybr_lik$hybr_lik_host
  as.numeric(hybr_lik_mean_dep) , # hybr_lik$hybr_lik_non
  
  # trait temporal variation
  as.numeric(delta_host),
  as.numeric(delta_dep),
  
  # Speciation and extinction
  #turn_host,
  #turn_dep,
  div_host,
  div_dep#,
  
  # Partnership dynamics
  #comb_part_siz_aft_host , # rang_part_shift$comb_part_siz_aft_host
  #comb_part_siz_aft_dep , # rang_part_shift$comb_part_siz_aft_dep
  
  #comb_part_dist_gain_host , # rang_part_shift$comb_part_dist_gain_host
  #comb_part_dist_gain_dep #, #, rang_part_shift$comb_part_dist_gain_dep
  
  # phylogenetic distance among hosts
  #phylo_dist_host
  
)

# figure out which variables contain NAs
colSums(is.na(dat_raw))

# remove replicates with NAs
dat_na_rm <- dat_raw[!is.na(rowSums(dat_raw[,-1])),]
sum(!is.na(rowSums(dat_raw[,-1]))) / 4800

# add independent variables to the data frame
dat_na_rm$host_spec <- rep(NA, length(dat_na_rm$nam))
dat_na_rm$host_spec[substr(dat_na_rm$nam, 1, 1) == 'a' & substr(dat_na_rm$nam, 3, 3) == '1'] <- "neutral+"
dat_na_rm$host_spec[substr(dat_na_rm$nam, 1, 1) == 'a' & substr(dat_na_rm$nam, 3, 3) == '0'] <- "neutral-"
dat_na_rm$host_spec[substr(dat_na_rm$nam, 1, 1) == 's' & substr(dat_na_rm$nam, 3, 3) == '1'] <- "divergent+"
dat_na_rm$host_spec[substr(dat_na_rm$nam, 1, 1) == 's' & substr(dat_na_rm$nam, 3, 3) == '0'] <- "divergent-"

dat_na_rm$dep_spec <- rep(NA, length(dat_na_rm$nam))
dat_na_rm$dep_spec[substr(dat_na_rm$nam, 2, 2) == 'a' & substr(dat_na_rm$nam, 6, 6) == '1'] <- "neutral+"
dat_na_rm$dep_spec[substr(dat_na_rm$nam, 2, 2) == 'a' & substr(dat_na_rm$nam, 6, 6) == '0'] <- "neutral-"
dat_na_rm$dep_spec[substr(dat_na_rm$nam, 2, 2) == 's' & substr(dat_na_rm$nam, 6, 6) == '1'] <- "divergent+"
dat_na_rm$dep_spec[substr(dat_na_rm$nam, 2, 2) == 's' & substr(dat_na_rm$nam, 6, 6) == '0'] <- "divergent-"

dat_na_rm$host_fit_func <- rep(NA, length(dat_na_rm$nam))
dat_na_rm$host_fit_func[substr(dat_na_rm$nam, 5, 5) == 0] <- "Unaffected (0)"
dat_na_rm$host_fit_func[substr(dat_na_rm$nam, 5, 5) == '1' & substr(dat_na_rm$nam, 7, 7) == 'M' & substr(dat_na_rm$nam, 8, 8) == 'M'] <- "Trait matching (+)"
dat_na_rm$host_fit_func[substr(dat_na_rm$nam, 5, 5) == '1' & substr(dat_na_rm$nam, 7, 7) == 'M' & substr(dat_na_rm$nam, 8, 8) == 'D'] <- "Trait difference (+)"
dat_na_rm$host_fit_func[substr(dat_na_rm$nam, 5, 5) == '1' & substr(dat_na_rm$nam, 7, 7) == 'A' & substr(dat_na_rm$nam, 8, 8) == 'M'] <- "Trait matching (-)"
dat_na_rm$host_fit_func[substr(dat_na_rm$nam, 5, 5) == '1' & substr(dat_na_rm$nam, 7, 7) == 'A' & substr(dat_na_rm$nam, 8, 8) == 'D'] <- "Trait difference (-)"

dat_na_rm$dep_fit_func <- rep(NA, length(dat_na_rm$nam))
dat_na_rm$dep_fit_func[substr(dat_na_rm$nam, 4, 4) == 0] <- "Unaffected (0)"
dat_na_rm$dep_fit_func[substr(dat_na_rm$nam, 4, 4) == '1' & substr(dat_na_rm$nam, 9, 9) == 'M'] <- "Trait matching (+)"
dat_na_rm$dep_fit_func[substr(dat_na_rm$nam, 4, 4) == '1' & substr(dat_na_rm$nam, 9, 9) == 'D'] <- "Trait difference (+)"

dat_na_rm$catastrophe <- rep(NA, length(dat_na_rm$nam))
dat_na_rm$catastrophe[substr(dat_na_rm$nam, 11, 12) == "no"] <- "no"
dat_na_rm$catastrophe[substr(dat_na_rm$nam, 11, 12) == "ex"] <- "yes"


# Add independent variables based on scenario and reciprocity
dat_na_rm$scenario <- rep(NA, length(dat_na_rm$nam))

# 
dat_na_rm$scenario[dat_na_rm$host_fit_func == "Trait difference (+)" & dat_na_rm$dep_fit_func == "Trait difference (+)"] <- "a"

dat_na_rm$scenario[dat_na_rm$host_fit_func == "Trait difference (+)" & dat_na_rm$dep_fit_func == "Unaffected (0)"] <- "b"

dat_na_rm$scenario[dat_na_rm$host_fit_func == "Unaffected (0)" & dat_na_rm$dep_fit_func == "Trait difference (+)"] <- "c"

dat_na_rm$scenario[dat_na_rm$host_fit_func == "Trait matching (+)" & dat_na_rm$dep_fit_func == "Trait matching (+)"] <- "d"

dat_na_rm$scenario[dat_na_rm$host_fit_func == "Trait matching (+)" & dat_na_rm$dep_fit_func == "Unaffected (0)"] <- "e"

dat_na_rm$scenario[dat_na_rm$host_fit_func == "Unaffected (0)" & dat_na_rm$dep_fit_func == "Trait matching (+)"] <- "f"

dat_na_rm$scenario[dat_na_rm$host_fit_func == "Trait matching (-)" & dat_na_rm$dep_fit_func == "Trait matching (+)"] <- "g"

dat_na_rm$scenario[dat_na_rm$host_fit_func == "Trait matching (-)" & dat_na_rm$dep_fit_func == "Unaffected (0)"] <- "h"

dat_na_rm$scenario[dat_na_rm$host_fit_func == "Unaffected (0)" & dat_na_rm$dep_fit_func == "Unaffected (0)"] <- "i"

# add clade specific scenario order
dat_na_rm$scenario_host <- factor(dat_na_rm$scenario, levels=c('a','g','d','e','b','c','f','h','i'))
dat_na_rm$scenario_dep <- factor(dat_na_rm$scenario, levels=c('a','g','d','e','f','b','c','h','i'))

# Add highlight colors
dat_na_rm$mode_host <- rep('No change predicted', length(dat_na_rm$nam))

dat_na_rm$mode_host[dat_na_rm$scenario == "a" | dat_na_rm$scenario == "g"] <- "Destabilizing selection hypothesis\npredicts a decrease"
dat_na_rm$mode_host[dat_na_rm$scenario == "d" | dat_na_rm$scenario == "e"] <- "Stabilizing selection hypothesis\npredicts an increase"

dat_na_rm$mode_host <- factor(dat_na_rm$mode_host,levels=c("Destabilizing selection hypothesis\npredicts a decrease",  "Stabilizing selection hypothesis\npredicts an increase", "No change predicted"))

dat_na_rm$mode_dep <- rep('No change predicted', length(dat_na_rm$nam))

dat_na_rm$mode_dep[dat_na_rm$scenario == "a" | dat_na_rm$scenario == "g"] <- "Destabilizing selection hypothesis\npredicts a decrease"
dat_na_rm$mode_dep[dat_na_rm$scenario == "d" | dat_na_rm$scenario == "e" | dat_na_rm$scenario == "f"] <- "Stabilizing selection hypothesis\npredicts an increase"

dat_na_rm$mode_dep <- factor(dat_na_rm$mode_dep,levels=c("Destabilizing selection hypothesis\npredicts a decrease",  "Stabilizing selection hypothesis\npredicts an increase", "No change predicted"))

# subset the original dataset to neutral speciation entries only
#dat_na_rm <- dat_na_rm[dat_na_rm$host_spec=='neutral-' | dat_na_rm$host_spec=='neutral+',]
#dat_na_rm <- dat_na_rm[dat_na_rm$dep_spec=='neutral-' | dat_na_rm$dep_spec=='neutral+',]

# rename columns
colnames(dat_na_rm) <- c(
  'nam',
  'Proportion_of_locals_among_host_individuals',
  'Proportion_of_locals_among_dependent_individuals',
  #'Age_of_host_individuals',
  #'Age_of_dependent_individuals',
  #'Trait_variation_between_host_individuals_in_adjacent_sites',
  #'Trait_variation_between_dependent_individuals_in_adjacent_sites',
  #'Range_size_H',
  #'Range_size_D',
  #'Distance_of_geographic_range_change_H',
  #'Distance_of_geographic_range_change_D',
  #'Demographic_stochasticity_H',
  #'Demographic_stochasticity_D',
  'Difficulty_of_hybridization_H',
  'Difficulty_of_hybridization_D',
  'Delta_H',
  'Delta_D',
  #'Clade_turnover_H',
  #'Clade_turnover_D',
  'Clade_diversity_H',
  'Clade_diversity_D',
  #'Specificity_H',
  #'Specificity_D',
  #'Distance_of_partner_shifts_H',
  #'Distance_of_partner_shifts_D',
  #'Phylogenetic_distance_among_host_lineages',
  'Speciation_condition_H',
  'Speciation_condition_D',
  'Fitness outcome for the host',
  'Fitness outcome for the dependent',
  'Local_extinction_events',
  'Scenario',
  'Scenario_host',
  'Scenario_dep',
  'Mode of evolution for hosts',
  'Mode of evolution for dependents'
)

#(optional depending on what is needed)
# logit transfrom the proportional variables
# dat_na_rm$Proportion_of_colonists_H <- log(dat_na_rm$Proportion_of_colonists_H / (1 - dat_na_rm$Proportion_of_colonists_H))
# dat_na_rm$Proportion_of_colonists_D <- log(dat_na_rm$Proportion_of_colonists_D / (1 - dat_na_rm$Proportion_of_colonists_D))

# save the original dataset
dat_na_rm_original <- dat_na_rm





############################################################################################
# plot the relationship between colonist proportion and other variables
# subset the dataset
dat <- dat_na_rm_original[dat_na_rm_original$Fitness_function_H == "none" & dat_na_rm_original$Fitness_function_D == "none", ]

dat_na_rm_z_sub <- dat

dat_corr <- dat_na_rm_z_sub[,as.vector(lapply(dat_na_rm_z_sub, class) == "numeric")]

cor1 = cor(dat_corr)
cor <- corrplot(cor1, method = 'square')

pdf("corr_n_n.pdf", width = 8, height = 8)
corrplot(cor1, tl.col="black", method = 'square')
dev.off()




pdf("test.pdf", width = 8, height = 8)
plot(dat$Proportion_of_colonists_D, dat$Clade_turnover_rate_D)
abline(lm(dat$Clade_turnover_rate_D ~ dat$Proportion_of_colonists_D))
dev.off()



###########################################################################################
# below is an exploratory analysis

#(optional depending on what is needed)
# Z-standardize each variable to control for treatment
for(var_id in (1:length(dat_na_rm))[as.vector(lapply(dat_na_rm, class) == "numeric")]){
  
  # print progress
  print(var_id)
  
  # extract the treatment variable
  #nam <- paste(dat_na_rm$Fitness_function_H, dat_na_rm$Fitness_function_D, sep = '_')
  nam <- dat_na_rm$Scenario
  
  # extract the raw data
  raw <- dat_na_rm[,var_id]
  
  # create a vector for storing the transformed data
  z <- rep(NA, length(raw))
  
  for (sim_id in 1:length(raw)) {
    
    average <- mean(raw[nam==nam[sim_id]])
    std <- sd(raw[nam==nam[sim_id]])
    
    z[sim_id] <- (raw[sim_id] - average) / std
    
  }
  
  # replace the raw data with the z-transformed data
  dat_na_rm[,var_id] <- z
  
}

# save the z-transformed dataset
dat_na_rm_z <- dat_na_rm












# Path analysis

# a list of all variables
prop_col_host + prop_col_dep + rang_siz_mean_host + rang_siz_mean_dep + comb_rang_dist_gain_host + comb_rang_dist_gain_dep + dem_stoch_mean_host + dem_stoch_mean_dep + hybr_lik_mean_host + hybr_lik_mean_dep + turn_host + turn_dep + div_host + div_dep + comb_part_siz_aft_host + comb_part_dist_gain_host + comb_part_siz_aft_dep + comb_part_dist_gain_dep


#install.packages("lavaan")
#install.packages("OpenMx")
#install.packages("semPlot")
#install.packages("GGally")
#install.packages("corrplot")
#install.packages("lavaanPlot")
library(lavaan)
library(semPlot)
library(OpenMx)
library(GGally)
library(corrplot)
library(lavaanPlot)

# (caution) run this if no z-standardization is needed
#dat_na_rm_z <- dat_na_rm_original

# subset the dataset to control for treatments not to be plotted
# Subset the datset to the simplest scenario
#dat_na_rm_z <- dat_na_rm_z[dat_na_rm_z$Speciation_condition_H == "neutral-",]
#dat_na_rm_z <- dat_na_rm_z[dat_na_rm_z$Speciation_condition_D == "neutral-",]
#dat_na_rm_z <- dat_na_rm_z[dat_na_rm_z$Local_extinction_events == "no",]

# save the subsetted dataset
dat_na_rm_z_sub <- dat_na_rm_z
dat_na_rm_z_sub <- dat

dat_corr <- dat_na_rm_z_sub[,as.vector(lapply(dat_na_rm_z_sub, class) == "numeric")]

cor1 = cor(dat_corr)
cor <- corrplot(cor1, method = 'square')

pdf("corr_0218.pdf", width = 8, height = 8)
corrplot(cor1, tl.col="black", method = 'square')
dev.off()





# Path analysis for host shifts
####################################################
# Final model showing simplified indirect effects

model_pd = 'Distance_of_partner_shifts_D ~ Distance_of_geographic_range_change_H + Distance_of_geographic_range_change_D + Specificity_D
Distance_of_geographic_range_change_H ~ Difficulty_of_hybridization_H
Distance_of_geographic_range_change_D ~ Difficulty_of_hybridization_D
Specificity_D ~ Clade_diversity_H
Difficulty_of_hybridization_H ~ Range_size_H + Clade_turnover_rate_H
Difficulty_of_hybridization_D ~ Range_size_D + Clade_turnover_rate_D
Clade_diversity_H ~ Demographic_stochasticity_H + Range_size_H
Range_size_H ~ Specificity_H
Specificity_H ~ Clade_diversity_D
Clade_diversity_D ~ Range_size_D + Demographic_stochasticity_D
'

fit_pd = cfa(model_pd, data = dat_corr)
summary(fit_pd, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#semPaths(fit_pd, 'std', layout = 'circle')
pd <- lavaanPlot(model = fit_pd, coefs = TRUE, stand = TRUE, sig = 0.05)



# Path analysis for host specificity
###############################################
# Final model showing simplified indirect effects
model_spec = 'Specificity_D ~ Clade_diversity_H + Range_size_H'

model_spec = 'Specificity_D ~ Clade_diversity_H + Range_size_D
Clade_diversity_H ~ Range_size_H + Demographic_stochasticity_H
Specificity_H ~ Range_size_H + Clade_diversity_D
Clade_diversity_D ~ Range_size_D + Demographic_stochasticity_D
Range_size_H ~ Difficulty_of_hybridization_H
Difficulty_of_hybridization_H ~ Clade_turnover_rate_H
Range_size_D ~ Difficulty_of_hybridization_D
Difficulty_of_hybridization_D ~ Clade_turnover_rate_D
'

fit_spec = cfa(model_spec, data = dat_corr)
summary(fit_spec, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#semPaths(fit_spec, 'std', layout = 'tree')
lavaanPlot(model = fit_spec, coefs = TRUE, stand = TRUE, sig = 0.05)




# arrange
pdf("dependences.pdf", width = 24, height = 8)
par(mfrow = c(1, 3))
corrplot(cor1, method = 'square')
lavaanPlot(model = fit_pd, coefs = TRUE, stand = TRUE, sig = 0.05)
lavaanPlot(model = fit_spec, coefs = TRUE, stand = TRUE, sig = 0.05)
dev.off()



# old code below this line
#########################################################################
#########################################################################
#########################################################################



# Marginal correlation
# find the most important intermediate variable by looking for the variable most correlated with the variable of interest when conditional on treatments.

#dat_na_rm <- dat_na_rm_original
dat_na_rm <- dat_na_rm_z

# # subset dataset based on host speciation
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$host_spec == "neutral-", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$host_spec == "neutral+", ]
# 

# # subset dataset based on dependent speciation
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$dep_spec == "neutral-", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$dep_spec == "neutral+", ]
# 

# # subset dataset based on host fitness function
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$host_fit_func == "matching-", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$host_fit_func == "matching+", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$host_fit_func == "difference-", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$host_fit_func == "difference+", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$host_fit_func == "none", ]
# 

# # subset dataset based on dependent fitness function
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$dep_fit_func == "matching+", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$dep_fit_func == "difference+", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$dep_fit_func == "none", ]
# 

# # subset dataset based on whether there is extinction
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$catastrophe == "yes", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$catastrophe == "no", ]

# create a vector for recording the averaged correlation for each potential intermediate variable
corr_vec_all_var <- rep(NA, length(dat_na_rm))

# for each potential intermediate variable
for(var_id in (1:length(dat_na_rm))[as.vector(lapply(dat_na_rm, class) == "numeric")]){
  
  # print progress
  print(var_id)
  
  # for each unique combination of conditions
  
  # extract the intermediate variable
  var_pot <- dat_na_rm[,var_id]
  
  # extract the variable of interest
  var_int <- dat_na_rm$comb_part_dist_gain_dep
  #var_int <- dat_na_rm$comb_part_siz_aft_dep
  #var_int <- dat_na_rm$div_host
  #var_int <- dat_na_rm$rang_siz_mean_host
  #var_int <- dat_na_rm$hybr_lik_mean_host

  # calculate Pearson correlation coefficient, take its absolute value, and add it to the recorder vector
  corr_vec_all_var[var_id] <- cor(var_pot, var_int)
  
}

# make a data frame for easily reading variable names
dat <- data.frame(names(dat_na_rm), corr_vec_all_var)
dat

# save values to a .csv file for easily copying and pasting
write.csv(as.matrix(corr_vec_all_var, ncol=1), file = "temp.csv")





########################################################################
# Conditional correlation
# find the most important intermediate variable by looking for the variable most correlated with the variable of interest when conditional on treatments.

dat_na_rm <- dat_na_rm_original

# # subset dataset based on host speciation
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$host_spec == "neutral-", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$host_spec == "neutral+", ]
# 

# # subset dataset based on dependent speciation
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$dep_spec == "neutral-", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$dep_spec == "neutral+", ]
# 

# # subset dataset based on host fitness function
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$host_fit_func == "matching-", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$host_fit_func == "matching+", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$host_fit_func == "difference-", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$host_fit_func == "difference+", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$host_fit_func == "none", ]
# 

# # subset dataset based on dependent fitness function
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$dep_fit_func == "matching+", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$dep_fit_func == "difference+", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$dep_fit_func == "none", ]
# 

# # subset dataset based on whether there is extinction
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$catastrophe == "yes", ]
#dat_na_rm <- dat_na_rm_original[dat_na_rm_original$catastrophe == "no", ]

# create a vector for recording the averaged correlation for each potential intermediate variable
corr_vec_all_var <- rep(NA, length(dat_na_rm))

# for each potential intermediate variable
for(var_id in (1:length(dat_na_rm))[as.vector(lapply(dat_na_rm, class) == "numeric")]){
  
  # print progress
  print(var_id)
  
  # create a vector for recording the correlation when conditional on each unique combination of conditions
  corr_vec_each_var <- NULL
  
  # for each unique combination of conditions
  for (host_spec in unique(dat_na_rm$host_spec)) {
    for (dep_spec in unique(dat_na_rm$dep_spec)){
      for (host_fit_func in unique(dat_na_rm$host_fit_func)){
        for (dep_fit_func in unique(dat_na_rm$dep_fit_func)){
          for (catastrophe in unique(dat_na_rm$catastrophe)){
            
            # subset the dataset to this combination of conditions only
            dat_na_rm_sub <- dat_na_rm[dat_na_rm$host_spec == host_spec, ]
            dat_na_rm_sub <- dat_na_rm_sub[dat_na_rm_sub$dep_spec == dep_spec, ]
            dat_na_rm_sub <- dat_na_rm_sub[dat_na_rm_sub$host_fit_func == host_fit_func, ]
            dat_na_rm_sub <- dat_na_rm_sub[dat_na_rm_sub$dep_fit_func ==dep_fit_func, ]
            dat_na_rm_sub <- dat_na_rm_sub[dat_na_rm_sub$catastrophe == catastrophe, ]
            
            # extract the intermediate variable
            var_pot <- dat_na_rm_sub[,var_id]
            
            # extract the variable of interest
            var_int <- dat_na_rm_sub$comb_part_dist_gain_dep
            #var_int <- dat_na_rm_sub$comb_part_siz_aft_dep
            #var_int <- dat_na_rm_sub$div_host
            #var_int <- dat_na_rm_sub$rang_siz_mean_host
            #var_int <- dat_na_rm_sub$hybr_lik_mean_host
            
            # calculate Pearson correlation coefficient and add it to the recorder vector
            corr_vec_each_var <- c(corr_vec_each_var, cor(var_pot, var_int))
            # or
            # calculate Pearson correlation coefficient, take its absolute value, and add it to the recorder vector
            #corr_vec_each_var <- c(corr_vec_each_var, abs(cor(var_pot, var_int)))
            
          }
        }
      }
    }
  }
  
  # take the mean correlation across all combinations of conditions, and add the mean to the recorder vector
  corr_vec_all_var[var_id] <- mean(corr_vec_each_var)
  
}


# make a data frame for easily reading variable names
dat <- data.frame(names(dat_na_rm), corr_vec_all_var)
dat

# save values to a .csv file for easily copying and pasting
write.csv(as.matrix(corr_vec_all_var, ncol=1), file = "temp.csv")