# cckey <- readLines("~/Dropbox/dotfiles/cyclestreets-api-key-rl")
# after Sys.setenv(CYCLESTREET = cckey) # see http://www.cyclestreets.net/api/

collisions <- function(bounds){
  if(!is.null(bounds)){
    resp <- GET('https://api.cyclestreets.net/v2/collisions.locations',
                query=list(bbox=cycle_street_bbox(bounds)
                           , casualtiesinclude='cyclist'
                           , key=cckey
                           , limit=20
                )
    )
    if(status_code(resp)==200){ return(content(resp, 'parsed')) }
  }
  empty_geojson
}

pois <- function(bounds, type){
  if(!is.null(bounds)){
    resp <- GET('https://api.cyclestreets.net/v2/pois.locations',
                query=list(bbox=cycle_street_bbox(bounds)
                           , type=type
                           , key=cckey
                           , limit=20
                           , fields='id,latitude,longitude,name,osmTags'
                )
    )
    if(status_code(resp)==200){ return(content(resp, 'parsed')) }
  }
  empty_geojson
}

from_cycle_streets <- function(bounds, type){
  switch(type
         ,"collisions"=collisions(bounds)
         , pois(bounds, type)
  )
}

cckey <- Sys.getenv('CYCLESTREET')

empty_geojson <- '{"type": "Point","coordinates": [-1.5492,53.7997]}'

cycle_street_bbox <- function(bounds){
  paste(bounds$west, bounds$south, bounds$east, bounds$north, sep=",")
}
