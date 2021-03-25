module Stripe where

type Balance = Int
type UserId = Int 
type SubscriptionId = String

getUserBalance :: UserId -> IO Balance
getUserBalance = undefined 

getLastPaymentDate :: UserId -> IO String
getLastPaymentDate = undefined 

getSubscriptionsFor :: UserId -> IO SubscriptionId
getSubscriptionsFor = undefined 

cancelSubscription :: SubscriptionId -> IO ()
cancelSubscription = undefined 