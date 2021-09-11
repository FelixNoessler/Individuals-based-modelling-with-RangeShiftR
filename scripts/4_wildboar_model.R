library(dplyr)
library(raster)
library(ggplot2)
library(RangeShiftR)

dirpath <- "models/Wildboar/"

# Landscape ---------------------------------------------------------------
land <- ImportedLandscape(LandscapeFile = "Corine_2018_4km_France.asc",
                          PatchFile = "Corine_2018_4km_France_patches.asc", 
                          Resolution = 4000,
                          Nhabitats = 10,
                          K_or_DensDep = c(0, 0, 0, 0.04, 0, 0, 0, 0, 0, 0))



# Demography --------------------------------------------------------------

matrixes <- readRDS('data/TransitionMatrices.rds')
initial_trans_mat <- matrixes$wildboar$RS_TransitionMatrix
trans_mat <- initial_trans_mat
#trans_mat[2:4,2] <- trans_mat[3:5,2]

trans_mat[3,2] <- sum(initial_trans_mat[3:4,2])
trans_mat[4,2] <- 0
trans_mat[4,3] <- initial_trans_mat[4,2] + initial_trans_mat[4,3]

stg <- StageStructure(Stages = 5, 
                      TransMatrix = trans_mat,  
                      MaxAge = 27,  
                      FecDensDep = T)

demo <- Demography(StageStruct = stg, 
                   ReproductionType = 0)
 
plotProbs(stg)

# Dispersal ---------------------------------------------------------------

emig <- Emigration(DensDep=T, 
                   StageDep=T, 
                   SexDep = F,
                   EmigProb = cbind(0:4,c(0,0.8, 0.05,0,0),
                                    c(0, 10, 10, 0, 0),
                                    c(0,1,1,0,0))) 


transfer <-CorrRW(StepLength = 4000, 
                 StepMort = 0.01)

max_steps <- 20


settle <- Settlement(StageDep = F,
                     SexDep = F,
                     Settle = 0.9,
                     DensDep = F,
                     MaxSteps = max_steps)

disp <-  Dispersal(Emigration = emig,
                   Transfer = transfer,
                   Settlement = settle) 



# Simulation settings -----------------------------------------------------
sim <- Simulation(Simulation = 0, 
                  Replicates = 20, 
                  Years = 100,
                  OutIntPop = 1,
                  OutIntOcc = 1,
                  OutIntRange = 1)


# Initialisation ----------------------------------------------------------

selected_patchid <- values(
  raster(paste0(dirpath, 
                'Inputs/Corine_2018_4km_France_patches.asc'))) %>%
  table %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>%
  #slice_sample(n=1) %>%
  slice(2) %>%
  pull('.')


(init_df_multi <- data.frame(Year=0, 
                       Species=0,
                       PatchID=c(30,30,87,87),
                       Ninds=10,
                       Age=c(3,5),
                       Stage=c(3,4)))


(init_df_single <- data.frame(Year=0, 
                             Species=0,
                             PatchID=c(87,87),
                             Ninds=20,
                             Age=c(3,5),
                             Stage=c(3,4)))

(init_df_single1 <- data.frame(Year=0, 
                              Species=0,
                              PatchID=c(30,30),
                              Ninds=20,
                              Age=c(3,5),
                              Stage=c(3,4)))


write.table(init_df_single, 
            file=paste0(dirpath,'Inputs/InitInds_single.txt'), 
            sep='\t', 
            row.names=F, 
            quote=F)

write.table(init_df_single1, 
            file=paste0(dirpath,'Inputs/InitInds_single1.txt'), 
            sep='\t', 
            row.names=F, 
            quote=F)

write.table(init_df_multi, 
            file=paste0(dirpath,'Inputs/InitInds_multi.txt'), 
            sep='\t', 
            row.names=F, 
            quote=F)

init_single <- Initialise(InitType = 2,
                          InitIndsFile = 'InitInds_single.txt')

init_single1 <- Initialise(InitType = 2,
                          InitIndsFile = 'InitInds_single1.txt')

init_multi <- Initialise(InitType = 2,
                        InitIndsFile = 'InitInds_multi.txt')




# Run single site reintroduction ------------------------------------------

s_single <- RSsim(batchnum = 3, 
                 land = land, 
                 demog = demo, 
                 dispersal = disp, 
                 simul = sim, 
                 init = init_single)

RunRS(s_single, dirpath)


s_single1 <- RSsim(batchnum = 1, 
                  land = land, 
                  demog = demo, 
                  dispersal = disp, 
                  simul = sim, 
                  init = init_single1)

RunRS(s_single1, dirpath)


# Run multisite reintroduction --------------------------------------------
s_multi <- RSsim(batchnum = 2, 
               land = land, 
               demog = demo, 
               dispersal = disp, 
               simul = sim, 
               init = init_multi)

RunRS(s_multi, dirpath)



# Compare occupancy & abundance of single and multi -----------------------
abund_single_multi <- bind_rows(
                  # single site
                  readRange(s_single, dirpath) %>% 
                    group_by(Year) %>%
                    summarise(Abundance = mean(NInds), sd = sd(NInds)) %>%
                    mutate(Scenario = 'Single site SE'),
                  
                  # single site
                  readRange(s_single1, dirpath) %>% 
                    group_by(Year) %>%
                    summarise(Abundance = mean(NInds), sd = sd(NInds)) %>%
                    mutate(Scenario = 'Single site central'),
                  
                  # multi site
                  readRange(s_multi, dirpath) %>% 
                    group_by(Year) %>%
                    summarise(Abundance = mean(NInds), sd = sd(NInds)) %>%
                    mutate(Scenario = 'Multi-site'))


abund_single_multi %>%
  ggplot() +
  geom_line(aes(Year, Abundance, color=Scenario), size=1)+
  geom_ribbon(aes(x=Year, ymin=Abundance-sd, ymax=Abundance+sd, color=Scenario), 
              linetype=2, alpha=0.1)
ggsave('results/abu.svg')




occu_single_multi <- bind_rows(
  # single site
  readPop(s_single, dirpath) %>%
    dplyr::select(Year, PatchID, NInd, Rep) %>%
    mutate(NInd = ifelse(NInd > 0,1,0 )) %>%
    group_by(Year, Rep) %>%
    summarise(occu = sum(NInd), occu_sd = sd(NInd)) %>%
    group_by(Year) %>%
    summarise(occu = mean(occu), occu_sd = mean(occu_sd)) %>%
    mutate(Scenario = 'Single site SE'),
  
  # single site
  readPop(s_single1, dirpath) %>%
    dplyr::select(Year, PatchID, NInd, Rep) %>%
    mutate(NInd = ifelse(NInd > 0,1,0 )) %>%
    group_by(Year, Rep) %>%
    summarise(occu = sum(NInd), occu_sd = sd(NInd)) %>%
    group_by(Year) %>%
    summarise(occu = mean(occu), occu_sd = mean(occu_sd)) %>%
    mutate(Scenario = 'Single site central'),
  
  # multi-site
  readPop(s_multi, dirpath) %>%
    dplyr::select(Year, PatchID, NInd, Rep) %>%
    mutate(NInd = ifelse(NInd > 0,1,0 )) %>%
    group_by(Year, Rep) %>%
    summarise(occu = sum(NInd), occu_sd = sd(NInd)) %>%
    group_by(Year) %>%
    summarise(occu = mean(occu), occu_sd = mean(occu_sd)) %>%
    mutate(Scenario = 'Multi-site'))


occu_single_multi %>%
  ggplot() +
  geom_line(aes(Year, occu, color=Scenario), size=1)+
  geom_ribbon(aes(x=Year, ymin=occu-occu_sd, ymax=occu+occu_sd, color=Scenario), 
              linetype=2, alpha=0.1)+
  labs(y = 'Occupancy')
ggsave('results/occu.svg', dpi=300)

# Analyse -----------------------------------------------------------------



# no of patches
readRange(s_1, dirpath) %>% 
  group_by(Year) %>%
  summarise(patch = mean(NOccupPatches)) %>%
  ggplot()+
  geom_line(aes(Year, patch))

occupancy_probs <- ColonisationStats(s_single, dirpath)$occ_prob

RangeShiftR::plotOccupancy(s_single, dirpath, sd=T, replicates=F)

  
RangeShiftR::readPop()

# Maps --------------------------------------------------------------------

## general mapping data 

patches <-  raster(paste0(dirpath, 
                          'Inputs/Corine_2018_4km_France_patches.asc')) %>%
  as.data.frame(xy=T) %>%
  tidyr::drop_na() %>%
  rename(patch_id = Corine_2018_4km_France_patches)


background <- patches %>%
  mutate(patch_id = ifelse(patch_id == 0, 1, 0)) 

all_patches <- patches %>%
  filter(patch_id > 0)

selected_patches <- all_patches %>%
  filter(patch_id %in% c(30,87))

###### Map of patches

ggplot()+
  geom_tile(data=background, aes(x,y), fill='gray90')+
  geom_tile(data=all_patches, aes(x,y), fill='chartreuse4')+
  geom_tile(data=selected_patches, aes(x,y), fill='orange')+
  theme_classic()
ggsave('results/selected_patches.png', dpi=1000)

###### Map of occupancy

occupancy_probs <- ColonisationStats(s_multi, dirpath)$occ_prob

occupancy_data <- patches %>%
  left_join(occupancy_probs, by =c('patch_id' = 'patch')) %>%
  tidyr::drop_na() %>%
  rename(value = '100')

occu_multi_map <- ggplot()+
  geom_tile(data=background, aes(x,y), fill='gray90')+
  geom_tile(data=all_patches, aes(x,y), fill='beige')+
  geom_tile(data=occupancy_data, aes(x, y, fill=value))+
  scale_fill_gradient2('Mean occupancy',
                      low='beige',
                      mid='orange',
                      high='orangered2',
                      midpoint=0.5)+
  theme_classic()+
  theme(legend.position = 'none')

final_occu_map <- gridExtra::grid.arrange(occu_single_map, occu_single1_map, occu_multi_map, ncol=3)
ggsave(plot=final_occu_map, 'results/occu_map.png', width=12, height=4, dpi=1000)

###### Map of colonization time 

colonization_time <- ColonisationStats(s_multi, dirpath)$col_time
colonization_time$mean <- rowMeans(dplyr::select(colonization_time, -patch),
                                   na.rm = T)

colonization_data <- patches %>%
  left_join(colonization_time, by =c('patch_id' = 'patch')) %>%
  tidyr::drop_na()


colo_multi_map <- ggplot()+
  geom_tile(data=background, aes(x,y), fill='gray90')+
  geom_tile(data=all_patches, aes(x,y), fill='chartreuse4')+
  geom_tile(data=colonization_data, aes(x,y, fill=mean))+
  scale_fill_distiller('Mean time \n to colonization \n (if colonized)',
                       palette = "YlOrBr")+
  theme_classic()+
  theme(legend.position = 'none')
  


final_colo_map <- gridExtra::grid.arrange(colo_single_map, colo_single1_map, colo_multi_map, ncol=3)
ggsave(plot=final_colo_map, 'results/colo_map.png', width=12, height=4, dpi=800)



################ Extinction

Calc_ExtTime(readPop(s_single, dirpath))
Calc_ExtTime(readPop(s_single1, dirpath))
Calc_ExtTime(readPop(s_multi, dirpath))

Calc_ExtProb(readPop(s_single, dirpath), s_single)

# --> no extinctions


# Sensitivity analysis ----------------------------------------------------

sim_few <- Simulation(Simulation = 0, 
                      Replicates = 10, 
                      Years = 100,
                      OutIntPop = 1,
                      OutIntOcc = 1,
                      OutIntRange = 1)

### matrix

trans_mat_plus_5 <- trans_mat
trans_mat_plus_5[4,3] <- trans_mat[4,3] * 1.05


stg_plus_5 <- StageStructure(Stages = 5, 
                      TransMatrix = trans_mat_plus_5,  
                      MaxAge = 27,  
                      FecDensDep = T)

demo_plus_5 <- Demography(StageStruct = stg_plus_5, 
                   ReproductionType = 0)


trans_mat_minus_5 <- trans_mat
trans_mat_minus_5[4,3] <- trans_mat[4,3] * 0.95


stg_minus_5 <- StageStructure(Stages = 5, 
                             TransMatrix = trans_mat_minus_5,  
                             MaxAge = 27,  
                             FecDensDep = T)

demo_minus_5 <- Demography(StageStruct = stg_minus_5, 
                          ReproductionType = 0)

### max steps

settle_plus_5 <- Settlement(StageDep = F,
                            SexDep = F,
                            Settle = 0.9,
                            DensDep = F,
                            MaxSteps = max_steps*1.05)

disp_plus_5 <-  Dispersal(Emigration = emig,
                           Transfer = transfer,
                           Settlement = settle_plus_5) 


settle_minus_5 <- Settlement(StageDep = F,
                     SexDep = F,
                     Settle = 0.9,
                     DensDep = F,
                     MaxSteps = max_steps*0.95)

disp_minus_5 <-  Dispersal(Emigration = emig,
                           Transfer = transfer,
                           Settlement = settle_minus_5) 




### 
#s_1 <- RSsim(batchnum = 10, 
#             land = land, 
#             demog = demo, 
#             dispersal = disp, 
#             simul = sim_few, 
#             init = init_1)
#RunRS(s_1, dirpath)


##
s_demo_plus_5 <- RSsim(batchnum = 11, 
                       land = land, 
                       demog = demo_plus_5, 
                       dispersal = disp, 
                       simul = sim_few, 
                       init = init_single1)
RunRS(s_demo_plus_5, dirpath)


##
s_demo_minus_5 <- RSsim(batchnum = 12, 
                       land = land, 
                       demog = demo_minus_5, 
                       dispersal = disp, 
                       simul = sim_few, 
                       init = init_single1)
RunRS(s_demo_minus_5, dirpath)


##
s_steps_plus_5 <- RSsim(batchnum = 13, 
                       land = land, 
                       demog = demo, 
                       dispersal = disp_plus_5, 
                       simul = sim_few, 
                       init = init_single1)
RunRS(s_steps_plus_5, dirpath)

##
s_steps_minus_5 <- RSsim(batchnum = 14, 
                        land = land, 
                        demog = demo, 
                        dispersal = disp_minus_5, 
                        simul = sim_few, 
                        init = init_single1)
RunRS(s_steps_minus_5, dirpath)


##########

abund_sens <- bind_rows(
  
  # basic
  readRange(s_single1,dirpath) %>%
    group_by(Year) %>%
    summarise(Abundance = mean(NInds), sd = sd(NInds)) %>% mutate(Scenario = "1 - Single central"), 
  
  # demo +5%
  readRange(s_demo_plus_5,dirpath) %>%
    group_by(Year) %>%
    summarise(Abundance = mean(NInds), sd = sd(NInds)) %>% mutate(Scenario = "2 - Surv +5%"),
  
  # demo -5%
  readRange(s_demo_minus_5,dirpath) %>%
    group_by(Year) %>%
    summarise(Abundance = mean(NInds), sd = sd(NInds)) %>% mutate(Scenario = "3 - Surv -5%"),
  
  # steps +5%
  readRange(s_steps_plus_5, dirpath) %>%
    group_by(Year) %>%
    summarise(Abundance = mean(NInds), sd = sd(NInds)) %>% mutate(Scenario = "4 - Max steps +5%"),
  
  # steps -5%
  readRange(s_steps_minus_5, dirpath) %>%
    group_by(Year) %>%
    summarise(Abundance = mean(NInds), sd = sd(NInds)) %>% mutate(Scenario = "5 - Max steps -5%")) 


abund_sens %>%
  ggplot() +
  geom_line(aes(Year, Abundance, color=Scenario), size=2)+
  geom_ribbon(aes(x=Year, ymin=Abundance-sd, ymax=Abundance+sd, color=Scenario), 
              linetype=2, alpha=0.1)
ggsave('results/abund_sens.svg', width=10)


# Functions ---------------------------------------------------------------



Calc_ExtProb <- function(pop_df,s) {
  require(dplyr)
  
  pop_df %>%
    group_by(Rep,Year) %>%
    # Sum individuals over all cells per year and replicate
    summarise(sumPop = sum(NInd), .groups='keep') %>%
    group_by(Year) %>%
    # Average extinction probability (1 minus the proportion of replicates with surviving populations)
    summarise(extProb = 1-sum(sumPop>0, na.rm=T)/RepNb) %>%
    # Make sure that data frame is filled until last year of simulation
    right_join(tibble(Year = seq_len(s@simul@Years)), by='Year') %>% tidyr::replace_na(list(extProb=1))
}

# Define a function for calculating mean time to extinction
Calc_ExtTime <- function(pop_df) {
  require(dplyr)
  
  pop_df %>%
    group_by(Rep,Year) %>%
    # Sum individuals over all cells per year and replicate    
    summarise(sumPop = sum(NInd), .groups='keep') %>% 
    # Identify in which year they go extinct
    filter(sumPop==0) %>% 
    pull(Year) %>% mean
}
