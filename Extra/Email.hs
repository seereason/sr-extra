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
sendEmails :: [(String, String)] -> [(String, [String])] -> IO ()
sendEmails _ [] = return ()
sendEmails [] _ = return ()
sendEmails addrs messages = 
    do now <- getZonedTime
       mapM_ sendEmail (map (uncurry (buildEmail addrs now)) messages)

sendEmail :: Message String -> IO ()
sendEmail msg =
    do (outs, errs, ec) <- sendmail msg
       case ec of
         ExitSuccess -> return ()
         (ExitFailure _n) -> IO.putStrLn outs >> IO.putStrLn errs

buildEmail :: [(String, String)] -> ZonedTime -> String -> [String] -> Message String
buildEmail addrs origDate subjectText bodyText =
    (message
     (from (addrSpec "autobuilder" "somewhere"))
     origDate
     [ to (map (uncurry addrSpec) addrs), subject $ subjectText]
     (RFC2822Body  bodyText))
