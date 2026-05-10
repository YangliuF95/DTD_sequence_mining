# Load necessary library
library(TraMineR)
#library(agnes)
version

# Load the data
data <- read.csv("sample_for_traminer2.csv", header=TRUE,  sep = ",")

# Print the first few rows of the data to verify the content
print(head(data))
data <- reshape(
  # Select data and specific columns that should be kept 
  # (dyad ID: id, time variable: turn, 
  # categorical variable: turn_type)
  data = data[, c("user_id", "turn", "activity_type")],
  # Identify time variable ("turn")
  timevar = "turn",           
  # Identify dyad ID variable ("id")
  idvar = "user_id",             
  # Identify categorical variable ("turn_type")
  v.names = "activity_type",             
  # Note direction of data reformat
  direction = "wide",      
  # No spaces in new column names
  sep = "")                    

# View the first 10 rows and first 10 columns of the wide data
head(data[1:10, 1:10])

# Create a vector that contains the categories that appear in the wide data set
turntype_alphabet <- c("facebook_searches",
                       "facebook_reactions",
                       "facebook_last_seen_content",
                       "facebook_posts",
                       "facebook_likes_and_follows",
                       
                       "youtube_watch_history",
                       "youtube_search_history",
                       "tiktok_watched_videos",
                       "tiktok_favorite_videos_effects_hashtags_sounds",
                       "tiktok_search_history", 
                       "tiktok_shared_videos",
                       
                       "instagram_likes",
                       "instagram_shared_links",
                       "instagram_saved_posts",
                       "instagram_comment_history",
                       "instagram_search_history")

# Create a vector that allows for more helpful labels if applicable 
turntype_labels <- c("searches(facebook)",
                      "reactions(facebook)",
                      "last_seen_content(facebook)",
                      "posts(facebook)",
                      "likes_and_follows(facebook)",
                     "search_history(youtube)", 
                     "shared_videos(youtube)",
                      
                      "watch_history(tiktok)",
                      "search_history(tiktok)",
                      "watched_videos(tiktok)",
                      "favorite_videos_effects_hashtags_sounds(tiktok)",
                   
                      
                      "likes(instagram)",
                      "shared_links(instagram)",
                      "saved_posts(instagram)",
                      "comment_history(instagram)",
                      "search_history(instagram)")


facebook_searches <-"#1461A8"
facebook_reactions <-"#7DB8D9"
facebook_last_seen_content  <-"#3F8FC4"
facebook_posts <-"#BCD7EB"
facebook_likes_and_follows  <-"#E3EEF8"
youtube_watch_history <- "#9D3203"
youtube_search_history <- "#FEE9D4"
tiktok_watched_videos <- "#404040" 
tiktok_favorite_videos_effects_hashtags_sounds  <- "#B5B5B5" 
tiktok_search_history<- "#E2E2E2" 
tiktok_shared_videos <- "#FFFFFF" 

instagram_likes <- "#D85908"
instagram_shared_links<- "#F27F1C"
instagram_saved_posts<- "#feab46"
instagram_comment_history<- "#FED069"
instagram_search_history <- "#FEEBA2"


           # Purple
data.seq  <- seqdef(data,                      # Select data   
                   var = 2:990,                    # Columns containing repeated measures data
                   alphabet = turntype_alphabet,   # Alphabet  
                   labels = turntype_labels,       # Labels
                   xtstep = 25, cpal=c(
                     facebook_searches,
                     facebook_reactions,
                     facebook_last_seen_content,
                     facebook_posts,
                     facebook_likes_and_follows,
                     
                     youtube_watch_history,
                     youtube_search_history,
                     tiktok_watched_videos,
                     tiktok_favorite_videos_effects_hashtags_sounds,
                     tiktok_search_history, 
                     tiktok_shared_videos,
                     
                     instagram_likes,
                     instagram_shared_links,
                     instagram_saved_posts,
                     instagram_comment_history,
                     instagram_search_history
                   ))


seqIplot(data.seq,                                             # Sequence object
         with.legend = "right",                                # Display legend on right side of plot
         cex.legend = 0.6, sortv = "from.start",                              # Change size of legend
         main = "activity type")

# Create substitution cost matrix and save to the object "costmatrix"
costmatrix <- seqsubm(data.seq,            # Sequence object
                      method = "CONSTANT",  # Method to determine costs
                      cval = 2,             # Substitution cost
                      with.missing = TRUE,  # Allows for missingness state
                      miss.cost = 1,        # Cost for substituting a missing state
                      time.varying = FALSE, # Does not allow the cost to vary over time
                      weighted = TRUE)   
costmatrix

# Conduct sequence analysis 
dist_om <- seqdist(data.seq ,            # Sequence object
                   method = "OM",       # Optimal matching algorithm
                   indel = 1.0,         # Insert/deletion costs set to 1
                   sm = costmatrix,     # Substitution cost matrix
                   with.missing = TRUE)

# Examine the top left corner of the dissimilarity matrix
dist_om[1:10, 1:10]
# Insert dissimilarity matrix ("dist_om"), 
# indicate that we are using a dissimilarity matrix, and
# indicate that we want to use Ward's single linkage clustering method


#clusterward <- agnes(dist_om, diss = TRUE, method = "ward")
write.table(dist_om, file="file.csv", row.names =F,  sep = ',', col.names = F)
# Plot the results of the cluster analysis using a dendrogram
# Insert cluster analysis results object ("clusterward")
#plot(clusterward, which.plot = 2)







png("output_plot.png", width = 800, height = 800)
# your plotting commands here
# divide the graphic area in 2 rows and 2 columns 
par(mfrow = c(2, 2))

seqiplot(data.seq, with.legend = FALSE, border = NA)

seqIplot(data.seq, with.legend = FALSE, border = NA)

seqfplot(data.seq, with.legend = FALSE,   border = NA)

seqlegend(data.seq)

dev.off()