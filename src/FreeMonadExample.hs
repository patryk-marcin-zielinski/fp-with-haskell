module FreeMonadExample where

import Stripe    

newtype PlanId = PlanId Int deriving Show
newtype Email = Email String deriving Show
newtype Day = Day Double deriving Show


data User = User {
    userId :: UserId,
    email :: Email
}

data Subscription = Subscription {
    subscriptionId :: SubscriptionId,
    price :: Double,
    planId :: PlanId
}

data BillingProgram
    = GetUserBalance UserId
    | GetUserLastPaymentDate UserId
    | CancelSubscription UserId PlanId
    | ChargeUser UserId Double
    | SendLateNotice PlanId Email


interpret :: BillingProgram -> IO ()
interpret = undefined 
-- interpret (GetUserBalance userId) = do
--     Stripe.getUserBalance userId
--     return ()
-- interpret (GetUserLastPaymentDate userId) = do
--     Stripe.getLastPaymentDate userId
--     return ()
-- interpret (CancelSubscription userId planId) = do
--     subscriptions <- Stripe.getSubscriptionsFor userId
--     for_ subscriptions $ \sub -> do
--         when (sunPlan sun == planId) $ do
--             Stripe.cancelSubscription (subId sub)

data BillingProgram' ret
    = GetUserBalance' UserId            (Double -> BillingProgram' ret)
    | GetUserLastPaymentDate' UserId    (Day -> BillingProgram' ret)
    | CancelSubscription' UserId PlanId (BillingProgram' ret)
    | ChargeUser' UserId Double         (BillingProgram' ret)
    | SendLateNotice' PlanId Email      (BillingProgram' ret)
    | Done' ret


chargeOrEmail :: User -> Subscription -> BillingProgram' ()
chargeOrEmail user subscription = GetUserBalance' (userId user) (\balance ->
                                    if balance >  (price subscription) then ChargeUser' (userId user) (price subscription) (Done' ())
                                    else SendLateNotice' (planId subscription) (email user) (Done' ()))


flatMap :: BillingProgram' a -> (a -> BillingProgram' b) -> BillingProgram' b
flatMap (Done' a) runNextProgram = runNextProgram a
flatMap (GetUserBalance' userId nextCommand) runNextProgram = GetUserBalance' userId (\balance -> flatMap (nextCommand balance) runNextProgram )
flatMap (GetUserLastPaymentDate' userId day2next) runNextProgram = GetUserLastPaymentDate' userId (\day -> flatMap (day2next day) runNextProgram )
flatMap (CancelSubscription' userId planId next) runNextProgram = CancelSubscription' userId planId (flatMap next runNextProgram)
flatMap (ChargeUser' userId price next) runNextProgram = ChargeUser' userId price (flatMap next runNextProgram)
flatMap (SendLateNotice' userId price next) runNextProgram = SendLateNotice' userId price (flatMap next runNextProgram)    


billingProgram' :: User -> [Subscription] -> BillingProgram' ()
billingProgram' _ [] = Done' ()
billingProgram' user (sub:subs) =
    GetUserBalance' uid $ \balance ->
        if balance > (price sub)  then
            ChargeUser' uid (price sub) theRest
        else
            SendLateNotice' plan (email user)
                $ GetUserLastPaymentDate' uid
                $ \(Day day) -> if day < 60
                    then CancelSubscription' uid plan theRest
                    else theRest
  where
    uid = userId user
    plan = planId sub
    theRest = billingProgram' user subs


interpret' :: BillingProgram' a -> IO ()
interpret' (Done' a) = putStrLn "DONE"
interpret' (ChargeUser' uid price next) = do
    putStrLn $ "Charge User " ++ (show uid) ++ " and "++ " price " ++ show price
    interpret' next
interpret' (SendLateNotice' plan email next) = do
    putStrLn $ "Send late notice " ++ (show plan) ++ " and "++ " email address " ++ show email
    interpret' next
interpret' (GetUserBalance' uid next) = do
    putStrLn $ "Get balance " ++ (show uid)
    interpret' (next 2)
interpret' (GetUserLastPaymentDate' uid next) = do
    putStrLn $ "Get last payment " ++ (show uid)
    interpret' (next $ Day 2)  
interpret' (CancelSubscription' userId planId next) = do
    putStrLn $ "Cancel subscription " ++ (show userId) ++ " and "++ " subscription plan " ++ show planId
    interpret' next  


getUserBalance :: UserId -> BillingProgram' Double
getUserBalance userId =
    GetUserBalance' userId (\amount -> Done' amount)

end :: BillingProgram' ()
end = Done' ()

getLastPaymentDate :: UserId -> BillingProgram' Day
getLastPaymentDate userId =
    GetUserLastPaymentDate' userId (\day -> Done' day)

cancelSubscription :: UserId -> PlanId -> BillingProgram' ()
cancelSubscription userId planId =
    CancelSubscription' userId planId end

chargeUser :: UserId -> Double -> BillingProgram' ()
chargeUser userId price = ChargeUser' userId price end    

sendLateNotice :: PlanId -> Email -> BillingProgram' ()
sendLateNotice planId email = SendLateNotice' planId email end    

billingProgram'' :: User -> [Subscription] -> BillingProgram' ()
billingProgram'' _ [] = end
billingProgram'' user (sub:subs) =
    FreeMonadExample.getUserBalance uid `flatMap` \balance ->
    if balance > (price sub) then
        FreeMonadExample.chargeUser uid (price sub)
            `flatMap` \_ -> theRest
    else
        FreeMonadExample.sendLateNotice plan (email user) `flatMap` \_ ->    
        FreeMonadExample.getLastPaymentDate uid `flatMap` \(Day day) ->
        if day < 60
           then FreeMonadExample.cancelSubscription uid plan
                    `flatMap` \_ -> theRest
           else theRest
  where
    uid = userId user
    plan = planId sub
    theRest = billingProgram'' user subs


