module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry, AddressBook)
import Data.List (filter, head, null, nubByEq)
import Data.Maybe (Maybe)

-- Note to reader: Add your solutions to this file
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.address.street == street

findEntryByStreet' :: String -> AddressBook -> Maybe Entry
findEntryByStreet' street = head <<< filter ((==) street <<< _.address.street)

isInBook :: String -> String -> AddressBook -> Boolean
isInBook first last = not null <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == first && entry.lastName == last

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq (\a b -> a.firstName == b.firstName && a.lastName == b.lastName)
