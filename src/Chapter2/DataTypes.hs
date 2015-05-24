{-# LANGUAGE ViewPatterns #-}

module Chapter2.DataTypes where

data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show

data ClientR = GovOrgR     { clientRName :: String }
             | CompanyR    { clientRName :: String
                           , companyId   :: Integer
                           , person      :: PersonR
                           , duty        :: String
                           }
             | IndividualR { person      :: PersonR }
             deriving Show

data PersonR = PersonR { firstName       :: String
                       , lastName        :: String
                       } deriving Show

clientName :: Client -> String
clientName client = case client of
    GovOrg name        -> name
    Company name _ _ _ -> name
    Individual person  -> case person of
                              Person fName lName _ -> fName ++ " " ++ lName

clientName' :: Client -> String
clientName' (GovOrg name)        = name
clientName' (Company name _ _ _) = name
clientName' (Individual person)  = personName person

personName :: Person -> String
personName (Person fName lName _) =  fName ++ " " ++ lName

companyName :: Client -> Maybe String
companyName client = case client of
    GovOrg name -> Just name
    _           -> Nothing

fibonacci :: Integer -> Integer
fibonacci n = case n of
    0 -> 0
    1 -> 1
    _ -> fibonacci (n-1) + fibonacci (n-2)

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Me") = True
specialClient _ = False

greet :: ClientR -> String
greet IndividualR { person = PersonR {firstName = fn}} = "hi, " ++ fn
greet CompanyR { clientRName = c }                     = "Hello, " ++ c
greet GovOrgR {}                                       = "Welcome"
