## elasticsearch

library(elastic)
connect()
graylog <- Search(asdf = TRUE)
graylog$hits$hits$`_source`

graylog <- Search(asdf = TRUE, fields = c("geoip_country_name", "timestamp", 
                                                   "domain"))
graylog$hits$hits$fields

graylog <- Search(asdf = TRUE, fields = c("geoip_country_name", "timestamp", 
                                          "domain"), sort = "timestamp")
graylog$hits$hits$fields

query <- '{
  "from": 0,
"size": 1000000,
"query": {
"bool": {
"must": {
"query_string": {
"query": "geoip_country_name:\\"United States\\" AND -agent:\\"\\\\\\"Mozilla/5.0 (compatible; bingbot/2.0; +http://www.bing.com/bingbot.htm)\\\\\\"\\"",
"allow_leading_wildcard": true
}
},
"filter": {
  "bool": {
    "must": [
      {
        "range": {
          "timestamp": {
            "from": "2016-12-13 12:54:01.738",
            "to": "2016-12-13 13:54:01.738",
            "include_lower": true,
            "include_upper": true
          }
        }
      },
      {
        "query_string": {
          "query": "streams:57f3b1a1edd76c34aeb02c5d"
        }
      }
      ]
  }
}
}
},
"sort": [
  {
    "timestamp": {
      "order": "desc"
    }
  }
  ]
}'


results <- Search(body = query, fields = c("timestamp", "agent"), sort = "timestamp", asdf = TRUE, scroll = "5m")
results$hits$hits$fields %>% glimpse()


queries <- paste0('{
  "from": 0,
"size": 1000000,
"query": {
"bool": {
"must": {
"query_string": {
"query": "geoip_country_name:\\"', c("United States", "France", "Germany"), '\\" AND -agent:\\"\\\\\\"Mozilla/5.0 (compatible; bingbot/2.0; +http://www.bing.com/bingbot.htm)\\\\\\"\\"",
"allow_leading_wildcard": true
}
},
"filter": {
"bool": {
"must": [
{
  "range": {
  "timestamp": {
  "from": "2016-12-13 12:54:01.738",
  "to": "2016-12-13 13:54:01.738",
  "include_lower": true,
  "include_upper": true
  }
  }
},
  {
  "query_string": {
  "query": "streams:57f3b1a1edd76c34aeb02c5d"
  }
  }
  ]
}
}
}
},
"sort": [
{
  "timestamp": {
  "order": "desc"
  }
}
]
}')

results <- list()

# on fait la même requête trois fois pour trois pays différents

for (i in 1:length(queries)) {
  results[[i]] <- Search(body = queries[i], fields = c("timestamp", "agent", "domain"), sort = "timestamp", asdf = TRUE, scroll = "5m")
  # bien penser à laisser scroll = "5m", sinon ça plante
  results[[i]]$hits$hits$fields %>% glimpse()
}
