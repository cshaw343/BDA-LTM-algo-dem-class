# Set path to the data file "*.da"
data_path <- paste0("/Users/CrystalShaw/Box/NIA_F31_April2020/Data/ADAMS/",
                    "adams1trk/ADAMS1TRK_R.da")

# Set path to the dictionary file "*.dct"
dict_path <- paste0("/Users/CrystalShaw/Box/NIA_F31_April2020/Data/ADAMS/",
                    "adams1trk/adams1trksta/ADAMS1TRK_R.dct")

# Read the dictionary file
df_dict <- read.table(dict_path, skip = 2, fill = TRUE,
                      stringsAsFactors = FALSE)

#Set column names for dictionary dataframe
colnames(df_dict) <- c("col.num", "col.type", "col.name", "col.width",
                       "col.lbl")

#Remove last row which only contains a closing}
df_dict <- df_dict[-nrow(df_dict), ]

#Extract numeric value from column width field
df_dict$col.width <- as.integer(sapply(df_dict$col.width, gsub,
                                       pattern = "[^0-9\\.]",
                                       replacement = ""))

#Convert column types to format to be used with read_fwf function
df_dict$col.type <-
  sapply(df_dict$col.type,
         function(x) ifelse(x %in% c("int","byte","long"), "i",
                            ifelse(x == "float", "n",
                                   ifelse(x == "double", "d", "c"))))

#Read the data file into a dataframe
adams_tracker <- read_fwf(file = data_path,
                          fwf_widths(widths = df_dict$col.width,
                                     col_names = df_dict$col.name),
                          col_types = paste(df_dict$col.type, collapse = ""))

# Add column labels to headers
attributes(adams_tracker)$variable.labels <- df_dict$col.lbl
