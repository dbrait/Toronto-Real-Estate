library(RCurl)

#need to sort through the json, and specify searches

musicUrl <- getURL("https://api.foursquare.com/v2/venues/search?ll=43.76,-79.41&query=music&client_id=ZTJBAJIVU5EQGFIQXJUMGGTDNKEB3QUKOKBXXW5MIVCBI10T&client_secret=NA1FCOAIJWCLZLPT2YQDPVPOCONEOVV5INQPNL03BFDLU0GQ&v=20160201")



crossfiturl <- getURL("https://api.foursquare.com/v2/venues/search?ll=43.76,-79.41&query=crossfit&client_id=ZTJBAJIVU5EQGFIQXJUMGGTDNKEB3QUKOKBXXW5MIVCBI10T&client_secret=NA1FCOAIJWCLZLPT2YQDPVPOCONEOVV5INQPNL03BFDLU0GQ&v=20160201")