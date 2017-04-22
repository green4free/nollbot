import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import System.Random

server = 
port = 6667
chan = 
nick = "nollbot"

main = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    write h "NICK" nick
    write h "USER" (nick ++ " 8 * :" ++ nick)
    write h "JOIN" chan
    listen h

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf  "> %s %s\r\n" s t

listen :: Handle -> IO ()
listen h = forever $ do
    t <- hGetLine h
    let s = init t
    if ping s then pong s else eval h (clean s)
    putStrLn s
  where
    forever a = a >> forever a
    
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write h  "PONG" (':' : drop 6 x)


eval :: Handle -> String -> IO ()
eval h "!quit" = write h "QUIT" ":Exiting" >> exitWith ExitSuccess
eval h x | "!id " `isPrefixOf` x = privmsg h (drop 4 x)
eval h x | "!sjung " `isPrefixOf` x = do
    let t = (drop 7 x)
    let songs = [["Rida rida get", "En glad analfabet", "Le-eva leva livet", "ute på savan-nen"], ["Till bolaget så ränner jag", "och bankar på dess port", "Jag vill ha nått som bränner bra", "och gör mig sketfull fort", "Expediten fråga och sa", "Hur gammal kan min herre va?", "Har du nått leg Ditt fula drägg?", "Kom hit igen när du fått skägg", "Nej, detta var ju inte bra", "Jag vill bli full ikväll igen", "Då plötsligt en ide jag fick", "de har ju sprit på shell, OK", "Många flaskor stod där på rad", "Så nu kan jag bli full och glad", "Den röda drycken åkte ner", "Nu kan jag inte se nått mer"]]
    b <- shuffle (songs !! (read t :: Int))
    sing h b

eval _ _ = return ()

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)

sing :: Handle -> [String] -> IO ()
sing h [] = return ()
sing h (s:xs) = (privmsg h s) >> (sing h xs)

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do randomPosition <- getStdRandom (randomR (0, length xs - 1))
                let (left, (a:right)) = splitAt randomPosition xs
                fmap (a:) (shuffle (left ++ right))
