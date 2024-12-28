import Data.IORef
import Data.List (find)

--定義資料型別

data Sheet = Sheet { sheetName :: String
                    , content :: [[Int]] -- 3x3 2D list
                    , right :: Int -- Sheet的Access Right, 0=Readable, 1=Editable
}    deriving (Show) -- 派生不用管他實際上在幹嘛, 大家都這樣定義一個data, 跟著做就對了

data User = User { userName :: String -- UserName
                    , userSheet :: [Sheet] -- User持有的sheet
} deriving (Show)

showMenu = do -- 印出Menu
    putStrLn "---------------Menu---------------"
    putStrLn "0. End program"
    putStrLn "1. Create a user"
    putStrLn "2. Create a sheet"
    putStrLn "3. Check a sheet"
    putStrLn "4. Change a value in a sheet"
    putStrLn "5. Change a sheet's access right."
    putStrLn "6. Collaborate with an other user"
    putStrLn "7. Print out all the users"
    putStrLn "----------------------------------"

createUser :: IORef [User] -> IO ()
createUser pUser = do
    name <- getLine
    users <- readIORef pUser -- readIORef 把 pUser 讀取給 users, 不提供重名user存在
    if any (\user -> userName user == name) users --Lambda 函數: https://learnyouahaskell.mno2.org/zh-tw/ch06/high-order-function#lambda
        then putStrLn "Sorry, The user has existed."
            else do
                let newUser = User { userName = name, userSheet = [] }
                modifyIORef pUser (newUser :) -- 把 newUser 加入 pUser (newUser :) 意義上等於 newUser : pUser, 不過我們不能直接修改函數, 得透過IORef
                putStrLn $ "Create a user named \"" ++ name ++ "\"." 

getUserSheet :: IO(String, String)
getUserSheet = do
    inputStr <- getLine
    let [a, b] = words inputStr
    return (a, b)

createSheet :: IORef [User] -> IO ()
createSheet pUser = do
    (uName, sName) <- getUserSheet -- 得到輸入的userName 跟 sheetName
    users <- readIORef pUser
    case find (\user -> userName user == uName) users of
        Nothing -> putStrLn "Sorry, The user doesn't exist."
        Just user -> do
            let newSheet = Sheet { sheetName = sName, content = [[0,0,0], [0,0,0], [0,0,0]], right = 0}
            let updatedUser = user { userSheet = newSheet : userSheet user }
            writeIORef pUser (updatedUser : filter(\u -> userName u /= uName) users) --把舊的user排泄掉
            putStrLn $ "Create a sheet named \"" ++ uName ++ "\" for \"" ++ sName ++ "\"."

printUsers :: IORef [User] -> IO ()
printUsers pUser = do
    users <- readIORef pUser
    if null users
        then do
            putStrLn "There's no any users."
            else
                print users


operationLoop :: IORef [User] -> IO ()
operationLoop pUser = do --當我收到 pUser 這個 IORef User list 時會做...
    showMenu
    command <- getLine
    case command of
        "0" -> putStrLn "End of Program"
        "1" -> createUser pUser >> operationLoop pUser
        "2" -> createSheet pUser >> operationLoop pUser
        "7" -> printUsers pUser >> operationLoop pUser

main :: IO ()
main = do
    pUser <- newIORef [] -- Haskell裡面都是不可變的function, 用IORef令function的內容變更, 當然也可以不斷創建新function來更新function內容, 但目前不太想做
    operationLoop pUser