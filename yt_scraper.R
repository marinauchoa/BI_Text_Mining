# Adaptation: Marina Ferreira Uchoa - h16mferr@du.se
# main code source:
# http://stackoverflow.com/questions/29692972/youtube-comment-scraper-returns-limited-results


# You will need httr package to scrape using yt_scraper class.
# To change the video to get comments from, change the videoId in initialize().
# The key value is your personal google developer key.
# To set your own Google developer key and manage API allowances:
# console.developers.google.com


# install.packages("httr")
# library(httr)

yt_scraper <- setRefClass(
  "yt_scraper",
  fields = list(
    base_url = "character",
    api_opts = "list",
    nextPageToken = "character",
    data = "list",
    unique_count = "numeric",
    done = "logical",
    core_df = "data.frame"),
  
  methods = list(
    initialize = function() {
      base_url <<- "https://www.googleapis.com/youtube/v3/commentThreads/"
      api_opts <<- list(
        part = "snippet", #if you want top comments and replies, use part = "snippet,replies",
        maxResults = 100,
        textFormat = "plainText",
        videoId = "V9eeg8d9XEg",  ############################ BuzzFeed - Worth It - Salmon: V9eeg8d9XEg
        key = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", ##### input your personal key
        fields = "items,nextPageToken",
        orderBy = "published")
      nextPageToken <<- ""
      data <<- list()
      unique_count <<- 0
      done <<- FALSE
      core_df <<- data.frame()
    },
    
    scrape = function() {
      opts <- api_opts
      if (nextPageToken != "") {
        opts$pageToken <- nextPageToken
      }
      
      res <- httr::content(
        httr::GET(base_url, query = opts))
      if (!(is.null(res$nextPageToken))){
        nextPageToken <<- res$nextPageToken
      }
      else {
        nextPageToken <<- ""
      }
      data <<- c(data, res$items)
      unique_count <<- length(unique(data))
    },
    
    scrape_all = function() {
      while (TRUE) {
        old_count <- unique_count
        scrape()
        if (unique_count == old_count | is.null(nextPageToken)) {
          done <<- TRUE
          nextPageToken <<- ""
          data <<- unique(data)
          break
        }
      }
      cache_core_data()
    },
    
    
    reset = function() {
      data <<- list()
      nextPageToken <<- ""
      unique_count <<- 0
      done <<- FALSE
      core_df <<- data.frame()
    },
    
    cache_core_data = function() {
      if (nrow(core_df) < unique_count) {
        sub_data <- lapply(data, function(x) {
          data.frame(
            Comment = x$snippet$topLevelComment$snippet$textDisplay,
            User = x$snippet$topLevelComment$snippet$authorDisplayName,
            ReplyCount = x$snippet$totalReplyCount,
            LikeCount = x$snippet$topLevelComment$snippet$likeCount,
            PublishTime = x$snippet$topLevelComment$snippet$publishedAt,
            CommentId = x$snippet$topLevelComment$id,
            stringsAsFactors=FALSE)
        })
        core_df <<- do.call("rbind", sub_data)
      } else {
        message("\n`core_df` is already up to date.\n")
      } 
    }
  )
)
