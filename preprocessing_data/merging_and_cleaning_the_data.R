#to use str_replace_all
#install.packages("stringr")
#######################
#to use mutate
#install.packages("dplyr")
library(dplyr)
library(stringr)

ariana_grande_dataset = read.csv("ArianaGrande.csv")
beyonce_dataset = read.csv("Beyonce.csv")
billie_eilish_dataset = read.csv("BillieEilish.csv")
cardi_b_dataset = read.csv("CardiB.csv")
charlie_puth_dataset = read.csv("CharliePuth.csv")
cold_play_dataset = read.csv("ColdPlay.csv")
drake_dataset = read.csv("Drake.csv")
duaLipa_dataset = read.csv("DuaLipa.csv")
ed_sheeran_dataset = read.csv("EdSheeran.csv")
eminem_dataset = read.csv("Eminem.csv")
justin_bieber_dataset = read.csv("JustinBieber.csv")
katy_perry_dataset = read.csv("KatyPerry.csv")
khalid_dataset = read.csv("Khalid.csv")
lady_gaga_dataset = read.csv("LadyGaga.csv")
maroon5_dataset = read.csv("Maroon5.csv")
nicki_minaj_dataset = read.csv("NickiMinaj.csv")
post_malonee_dataset = read.csv("PostMalone.csv")
rihanna_dataset = read.csv("Rihanna.csv")
selena_gomez_dataset = read.csv("SelenaGomez.csv")
taylor_swift_dataset = read.csv("TaylorSwift.csv")


#replaces empty spaces with unknown for every cell
replacing_empty_spaces_with_unknown <- function(temp_obj) {
  temp_obj$artist = ifelse(temp_obj$artist == "", "unknown", temp_obj$artist)
  temp_obj$title = ifelse(temp_obj$title == "", "unknown", temp_obj$title)
  temp_obj$lyric = ifelse(temp_obj$lyric == "", "unknown", temp_obj$lyric)
  temp_obj$album = ifelse(temp_obj$album == "", "unknown", temp_obj$album)
  
  return(temp_obj)
}


#replaces all symbols for empty spaces
replacing_symbols_with_empty_spots <- function(temp_obj) {
  
  refined_temp_obj_dataset <- temp_obj  %>%
    transmute(artist = str_replace(string = Artist, 
                                       pattern = "[[:punct:]]" , 
                                       replacement =  "")
              ,title = str_replace(string = Title , 
                                       pattern = "[[:punct:]]" , 
                                       replacement =  "")
              ,lyric = str_replace(string = Lyric, 
                                       pattern = "[[:punct:]]" , 
                                       replacement =  "")
              ,album = str_replace(string = Album, 
                                       pattern = "[[:punct:]]" , 
                                       replacement =  ""))
  
  refined_temp_obj_dataset <- replacing_empty_spaces_with_unknown(refined_temp_obj_dataset)
  
  return(refined_temp_obj_dataset)
}



#passing the data to cleaning functions
refined_ariana_grande_dataset <- replacing_symbols_with_empty_spots(ariana_grande_dataset)
refined_beyonce_dataset <- replacing_symbols_with_empty_spots(beyonce_dataset)
refined_billie_eilish_dataset <- replacing_symbols_with_empty_spots(billie_eilish_dataset)
refined_cardi_b_dataset <- replacing_symbols_with_empty_spots(cardi_b_dataset)
refined_charlie_puth_dataset <- replacing_symbols_with_empty_spots(charlie_puth_dataset)
refined_cold_play_dataset <- replacing_symbols_with_empty_spots(cold_play_dataset)
refined_drake_dataset <- replacing_symbols_with_empty_spots(drake_dataset)
refined_duaLipa_dataset <- replacing_symbols_with_empty_spots(duaLipa_dataset)
refined_ed_sheeran_dataset <- replacing_symbols_with_empty_spots(ed_sheeran_dataset)
refined_eminem_dataset <- replacing_symbols_with_empty_spots(eminem_dataset)
refined_justin_bieber_dataset <- replacing_symbols_with_empty_spots(justin_bieber_dataset)
refined_katy_perry_dataset<- replacing_symbols_with_empty_spots(katy_perry_dataset)
refined_khalid_dataset <- replacing_symbols_with_empty_spots(khalid_dataset)
refined_lady_gaga_dataset  <- replacing_symbols_with_empty_spots(lady_gaga_dataset )
refined_maroon5_dataset  <- replacing_symbols_with_empty_spots(maroon5_dataset)
refined_nicki_minaj_dataset <- replacing_symbols_with_empty_spots(nicki_minaj_dataset)
refined_post_malonee_dataset  <- replacing_symbols_with_empty_spots(post_malonee_dataset)
refined_rihanna_dataset <- replacing_symbols_with_empty_spots(rihanna_dataset)
refined_selena_gomez_dataset <- replacing_symbols_with_empty_spots(selena_gomez_dataset)
refined_taylor_swift_dataset  <- replacing_symbols_with_empty_spots(taylor_swift_dataset)


#merging all the data into one big object
refined_artists_dataset <- rbind(refined_ariana_grande_dataset,
                            refined_beyonce_dataset,
                            refined_billie_eilish_dataset,
                            refined_cardi_b_dataset,
                            refined_charlie_puth_dataset,
                            refined_cold_play_dataset,
                            refined_drake_dataset,
                            refined_duaLipa_dataset,
                            refined_ed_sheeran_dataset,
                            refined_eminem_dataset,
                            refined_justin_bieber_dataset,
                            refined_katy_perry_dataset,
                            refined_khalid_dataset,
                            refined_lady_gaga_dataset,
                            refined_maroon5_dataset,
                            refined_nicki_minaj_dataset,
                            refined_post_malonee_dataset,
                            refined_rihanna_dataset,
                            refined_selena_gomez_dataset,
                            refined_taylor_swift_dataset)


#writing to a csv fill. Empty string will have the form of unicode zero width
#space,therefore, it needs to be clean on the preprocessing step of the data
write.csv(refined_artists_dataset, 
          "your directory path\\artists_songs.csv",
          row.names = FALSE)








  

