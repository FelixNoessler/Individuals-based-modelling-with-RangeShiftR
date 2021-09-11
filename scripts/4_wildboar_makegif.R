
### create gif


mean_Ind_patch <- readPop(s_multi, dirpath) %>%
  dplyr::select(Year, PatchID, NInd, Rep)  %>%
  group_by(Year, PatchID) %>%
  summarise(mean_ind = mean(NInd))

all_patches <- patches %>%
  filter(patch_id > 0)

data_patch_year <- data.frame(Year = 0:100, 
           PatchID = rep(unique(all_patches$patch_id), each=101)) %>%
  left_join(mean_Ind_patch) %>%
  mutate(mean_ind = tidyr::replace_na(mean_ind, 0))




all_patches_years <- all_patches %>%
  slice(rep(row_number(), 101)) %>%
  mutate(Year = rep(0:100, each=nrow(all_patches)))

data <- all_patches_years %>%
  left_join(data_patch_year, by = c('Year' = 'Year', 'patch_id' = 'PatchID'))



base_path <- 'results/time_series/year_'


for (i in 1:100){
  img <- data %>%
    filter(Year == i) %>%
    ggplot()+
    geom_tile(data=background, aes(x,y), fill='gray90')+
    geom_tile(aes(x,y, fill=log(mean_ind)))+
    scale_fill_distiller(
      'log mean individuals\nper patch',
      palette = "YlOrRd",
      direction= 1,
      limits = c(0, 12))+
    theme_classic()+
    labs(title = paste0('Year: ', i, '    - theoretical wild boar reintroduction in France'))
  
  
  img_no <- stringr::str_sub(paste0('00', i), -3, -1)
  
  path <- paste0(base_path, img_no, '.png')
  ggsave(filename = path,
         plot = img)
}




patches <-  raster(paste0(dirpath, 
                          'Inputs/Corine_2018_4km_France_patches.asc')) %>%
  as.data.frame(xy=T) %>%
  tidyr::drop_na() %>%
  rename(patch_id = Corine_2018_4km_France_patches)


background <- patches %>%
  mutate(patch_id = ifelse(patch_id == 0, 1, 0)) 



selected_patches <- all_patches %>%
  filter(patch_id %in% c(30,87))

###### Map of patches

ggplot()+
  geom_tile(data=background, aes(x,y), fill='gray90')+
  geom_tile(data=all_patches, aes(x,y), fill='chartreuse4')+
  geom_tile(data=selected_patches, aes(x,y), fill='orange')+
  theme_classic()



#%>%
  mutate(NInd = ifelse(NInd > 0,1,0 )) %>%
  group_by(Year, Rep) %>%
  summarise(occu = sum(NInd), occu_sd = sd(NInd)) %>%
  group_by(Year) %>%
  summarise(occu = mean(occu), occu_sd = mean(occu_sd)) %>%
  mutate(Scenario = 'Multi-site')