data Quote = Quote { name :: String, value :: Int } deriving (Show)
data QuoteService = QuoteService { fetch :: IO [Quote] }

realQuoteService url = QuoteService undefined

fakeQuoteService = QuoteService $ return [ Quote "NOK" 0 ]
