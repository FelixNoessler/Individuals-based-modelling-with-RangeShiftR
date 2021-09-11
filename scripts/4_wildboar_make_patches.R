

######  R Script to create patch maps

library(raster)
dirpath <- "models/Wildboar/"

landscape_r <- raster(paste0(dirpath, "Inputs/Corine_2018_4km_France.asc"))
values(landscape_r) <- ifelse(values(landscape_r)==4, 1, 0)

#plot(landscape_r)




# Detect patches ----------------------------------------------------------

patches <- clump(landscape_r, directions=4)

#n_patches <- max(values(patches), na.rm = T)
#plot(patches, col = rep_len(rainbow(50),n_patches))




# Split large patches -----------------------------------------------------

maxPatchSize <- 25
(largePatchIDs <- which(table(values(patches))>maxPatchSize))

for (i in seq_len(length(largePatchIDs))){
  
  nsplits <- ceiling(sum(values(patches)==largePatchIDs[i], na.rm=T) / maxPatchSize)
  split1 <- nsplits %/% 2
  split2 <- max(2,ceiling(nsplits / 2))

  min_frac <- 1 / nsplits * 0.8
  
  patch_splits <- sperrorest::partition_tiles(
    na.omit(coordinates(patches)[values(patches)==largePatchIDs[i],]), 
    nsplit=c(1,2), 
    min_frac=min_frac)
  
  for (s in seq_len(length(patch_splits[[1]]))) {
    if (s>1) {
      values(patches)[values(patches)==largePatchIDs[i] & !is.na(values(patches))][patch_splits[[1]][[s]]$train] <- (max(values(patches),na.rm=T) + 1 )}
  }
}

#n_patches <- max(values(patches), na.rm = T)
#plot(patches, col = rep_len(rainbow(5),n_patches))



# Delete small patches ----------------------------------------------------

minPatchSize <- 10

freq.patches <- freq(patches)
del.patches <- freq.patches[freq.patches[,2] < minPatchSize,]
keep.patches <- freq.patches[freq.patches[,2] >= minPatchSize,]
values(patches)[values(patches) %in% del.patches[,1]] <- NA

for (i in 1: nrow(keep.patches-1)){
  values(patches)[values(patches) == keep.patches[i,1]] <- i
}

#n_patches <- max(values(patches), na.rm = T)
#plot(patches, col = rep_len(rainbow(25),n_patches))




# Write file --------------------------------------------------------------

values(patches)[is.na(values(patches))&!is.na(values(landscape_r))] <- 0

writeRaster(patches,
            format="ascii",
            filename = paste0(dirpath, "Inputs/Corine_2018_4km_France_patches.asc"),
            NAflag = -9L,
            datatype='INT1S',
            overwrite = T)

