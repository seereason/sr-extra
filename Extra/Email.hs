module Extra.Email 
    ( sendEmails
    ) where

import		 Control.Monad
import		 Data.List
import		 Data.Time
import		 System.Exit
import qualified System.IO as IO
import		 Text.MIME.Compose
import		 Text.MIME.Type hiding (Binary)


-- currently, failure is non-fatal, and unreported :-(
sendEmails :: (String, String) -> [(String, String)] -> [(String, [String])] -> IO ()
sendEmails _ _ [] = return ()
sendEmails _ [] _ = return ()
sendEmails sender addrs messages = 
    do now <- getZonedTime
       mapM_ sendEmail (map (uncurry (buildEmail sender addrs now)) messages)

sendEmail :: Message String -> IO ()
sendEmail msg =
    do (outs, errs, ec) <- sendmail msg
       case ec of
         ExitSuccess -> return ()
         (ExitFailure _n) -> IO.putStrLn outs >> IO.putStrLn errs

buildEmail :: (String, String) -> [(String, String)] -> ZonedTime -> String -> [String] -> Message String
buildEmail sender addrs origDate subjectText bodyText =
    (message
     (from (uncurry addrSpec sender))
     origDate
     [ to (map (uncurry addrSpec) addrs), subject $ subjectText]
     (RFC2822Body  bodyText))
