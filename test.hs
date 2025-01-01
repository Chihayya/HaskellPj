import Data.IORef
import Data.List (find, intercalate)
import Text.Read (readMaybe)

--定義資料型別

data Sheet = Sheet { sheetName :: String
                    , content :: [[String]] -- 3x3 2D list
                    , sharedUser :: [String]
                    , right :: Int -- Sheet的Access Right, 1=Readable, 0=Editable
}    deriving (Show) -- 派生不用管他實際上在幹嘛, 大家都這樣定義一個data, 跟著做就對了

data User = User { userName :: String -- UserName
                    , userSheet :: [Sheet] -- User持有的sheet
} deriving (Show)

showMenu :: IO ()
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
            let newSheet = Sheet { sheetName = sName, content = [["0","0","0"], ["0","0","0"], ["0","0","0"]], sharedUser = [uName], right = 0}
            let updatedUser = user { userSheet = newSheet : userSheet user }
            writeIORef pUser (updatedUser : filter(\u -> userName u /= uName) users) --把舊的user排泄掉
            putStrLn $ "Create a sheet named \"" ++ uName ++ "\" for \"" ++ sName ++ "\"."

matchSheetPrintContent :: String -> String -> [User] -> IO ()
matchSheetPrintContent uName sName users =
    case find (\user -> userName user == uName) users of
        Nothing -> putStrLn "Sorry, The user doesn't exist."
        Just user -> do
            case find (\sheet -> sheetName sheet == sName) (userSheet user) of
                Nothing -> putStrLn "Sorry, The sheet doesn't exist."
                Just sheet -> do
                    mapM_ (putStrLn . intercalate ", ") (content sheet)

checkSheet :: IORef [User] -> IO ()
checkSheet pUser = do
    (uName, sName) <- getUserSheet
    users <- readIORef pUser
    matchSheetPrintContent uName sName users

printUsers :: IORef [User] -> IO ()
printUsers pUser = do
    users <- readIORef pUser
    if null users
        then do
            putStrLn "There's no any users."
            else
                print users

getUserInput :: IO (Int, Int, String)
getUserInput = do
    inputStr <- getLine
    let [rStr, cStr, expr] = words inputStr
    let r = read rStr :: Int
    let c = read cStr :: Int
    let result = evaluateExprToString expr -- 計算表達式的結果並轉成字串
    return (r, c, result)

-- 定義 evaluateExprToString 函數
evaluateExprToString :: String -> String
evaluateExprToString expr = 
    case break (`elem` "+-*/") expr of
        (a, '+':b) -> calculateAndConvert a b (+)
        (a, '-':b) -> calculateAndConvert a b (-)
        (a, '*':b) -> calculateAndConvert a b (*)
        (a, '/':b) -> case (readMaybe b :: Maybe Double) of
            Just 0 -> "Division by zero" -- 處理除以零的特殊情況
            _      -> calculateAndConvert a b (/)
        _ -> expr

-- 定義 calculateAndConvert 函數
calculateAndConvert :: String -> String -> (Double -> Double -> Double) -> String
calculateAndConvert a b op =
    case (readMaybe a :: Maybe Double, readMaybe b :: Maybe Double) of
        (Just left, Just right) -> 
            if isInt left && isInt right
                then show (truncate (op left right) :: Int) -- 結果是整數
                else show (op left right) -- 結果是浮點數
        _ -> "Invalid numbers" -- 無效的數字

-- 定義 isInt 函數檢查是否為整數
isInt :: Double -> Bool
isInt x = x == fromIntegral (round x)

changeSheet :: IORef [User] -> IO ()
changeSheet pUser = do
    (uName, sName) <- getUserSheet
    users <- readIORef pUser
    matchSheetPrintContent uName sName users

    -- 獲取修改值
    (row, col, inputVal) <- getUserInput
    -- 確認 user's sheet 是否屬於 0 (editable)
    case find (\user -> userName user == uName) users of
        Nothing -> putStrLn "Sorry, The user doesn't exist."
        Just user -> do
            case find (\sheet -> sheetName sheet == sName) (userSheet user) of
                Nothing -> putStrLn "Sorry, The sheet doesn't exist."
                Just sheet -> do
                    if right sheet == 0 then do
                        -- 如果是可編輯的，修改工作表中的數值
                        let updatedSheet = sheet { content = updateContent (content sheet) row col inputVal }
                        -- 找到 sharedUsers 中的所有用戶，更新他們的工作表
                        let sharedUsers = sharedUser sheet
                        let updatedUsers = map (updateSharedSheet sName updatedSheet) (filter (\u -> userName u `elem` sharedUsers) users)
                        -- 過濾出未被修改的用戶
                        let unaffectedUsers = filter (\u -> userName u `notElem` map userName updatedUsers) users
                        -- 合併更新後的用戶和未修改的用戶
                        let finalUsers = unaffectedUsers ++ updatedUsers
                        -- 更新全域的 pUser
                        writeIORef pUser finalUsers
                        users <- readIORef pUser
                        matchSheetPrintContent uName sName users

                    else
                        putStrLn "This sheet is not accessible for editing."

-- 更新共享此工作表的用戶資料
updateSharedSheet :: String -> Sheet -> User -> User
updateSharedSheet sName updatedSheet user =
    if any (\sheet -> sheetName sheet == sName) (userSheet user)
        then user { userSheet = map (replaceSheet sName updatedSheet) (userSheet user) }
        else user

-- 替換用戶的某個工作表
replaceSheet :: String -> Sheet -> Sheet -> Sheet
replaceSheet sName updatedSheet sheet =
    if sheetName sheet == sName
        then updatedSheet
        else sheet

updateContent :: [[String]] -> Int -> Int -> String -> [[String]]
updateContent content r c newVal = 
    let (beforeRow, targetRow:afterRow) = splitAt r content 
        (beforeCol, _:afterCol) = splitAt c targetRow
    in beforeRow ++ (beforeCol ++ newVal : afterCol) : afterRow

getSheetAccess :: IO(String, String, String)
getSheetAccess = do
    inputStr <- getLine
    let [user, sheet, right] = words inputStr
    return (user, sheet, right) 

updateSharedSheetRight :: String -> Sheet -> User -> User
updateSharedSheetRight sName updatedSheet user =
    user
        { userSheet = map
            (\s -> if sheetName s == sName then updatedSheet else s)
            (userSheet user)
        }

changeRight :: IORef [User] -> IO ()
changeRight pUser = do
    (uName, sName, inputRight) <- getSheetAccess
    users <- readIORef pUser
    case find (\user -> userName user == uName) users of
        Nothing -> putStrLn "Sorry, The user doesn't exist."
        Just user -> do
            case find (\sheet -> sheetName sheet == sName) (userSheet user) of
                Nothing -> putStrLn "Sorry, The sheet doesn't exist."
                Just sheet -> do
                    let newRight = case inputRight of
                                    "ReadOnly" -> Just 1
                                    "Editable" -> Just 0
                                    _          -> Nothing
                    case newRight of
                        Nothing -> putStrLn "Invalid access right. Please input 'ReadOnly' or 'Editable'."
                        Just rightValue -> do
                            -- 更新當前用戶的工作表權限
                            let updatedSheet = sheet { right = rightValue }
                            -- 找到 sharedUser 裡的所有用戶，更新他們的工作表
                            let sharedUsers = sharedUser sheet
                            let updatedUsers = map (updateSharedSheetRight sName updatedSheet) (filter (\u -> userName u `elem` sharedUsers) users)
                            -- 過濾出未被修改的用戶
                            let unaffectedUsers = filter (\u -> userName u `notElem` map userName updatedUsers) users
                            -- 合併更新後的用戶和未修改的用戶
                            let finalUsers = unaffectedUsers ++ updatedUsers
                            -- 更新全域的 pUser
                            writeIORef pUser finalUsers
                            putStrLn $ "The access right of sheet \"" ++ sName ++ "\" has been changed to " ++ inputRight ++ " for all shared users."

getSharedUser :: IO(String, String, String)
getSharedUser = do
    inputStr <- getLine
    let [u, s, p] = words inputStr
    return (u, s, p)

addSharedUser :: IORef [User] -> IO ()
addSharedUser pUser = do
    (uName, sName, pName) <- getSharedUser
    users <- readIORef pUser

    case find (\user -> userName user == uName) users of
        Nothing -> putStrLn "Sorry, The user doesn't exist."
        Just user -> do
            case find (\sheet -> sheetName sheet == sName) (userSheet user) of
                Nothing -> putStrLn "Sorry, The sheet doesn't exist."
                Just sheet -> do
                    case find (\user -> userName user == pName) users of
                        Nothing -> putStrLn "Sorry, The specified user to share with doesn't exist."
                        Just targetUser -> do
                            -- 更新原始用戶的共享列表
                            let updatedSheet = sheet { sharedUser = pName : sharedUser sheet }
                            let updatedUser = user {
                                userSheet = map (\s -> if sheetName s == sName then updatedSheet else s) (userSheet user)
                            }

                            -- 如果共享用戶中不存在該工作表，則添加
                            let targetUserUpdated = if any (\s -> sheetName s == sName) (userSheet targetUser)
                                    then targetUser -- 如果已經存在，不變
                                    else targetUser { userSheet = updatedSheet : userSheet targetUser }

                            -- 更新全局用戶數據
                            let updatedUsers = map (\u -> 
                                    if userName u == uName then updatedUser
                                    else if userName u == pName then targetUserUpdated
                                    else u ) users

                            -- 寫回到 IORef
                            writeIORef pUser updatedUsers
                            putStrLn $ "Shared sheet \"" ++ sName ++ "\" with user \"" ++ pName ++ "\" successfully."

-- 更新共享工作表用戶的資料
updateSharedSheetForUser :: String -> Sheet -> User -> User
updateSharedSheetForUser sName updatedSheet user =
    if any (\sheet -> sheetName sheet == sName) (userSheet user)
        then user { userSheet = map (replaceSheet sName updatedSheet) (userSheet user) }
        else user

operationLoop :: IORef [User] -> IO ()
operationLoop pUser = do --當我收到 pUser 這個 IORef User list 時會做...
    showMenu
    command <- getLine
    case command of
        "0" -> putStrLn "End of Program"
        "1" -> createUser pUser >> operationLoop pUser
        "2" -> createSheet pUser >> operationLoop pUser
        "3" -> checkSheet pUser >> operationLoop pUser
        "4" -> changeSheet pUser >> operationLoop pUser
        "5" -> changeRight pUser >> operationLoop pUser
        "6" -> addSharedUser pUser >> operationLoop pUser
        "7" -> printUsers pUser >> operationLoop pUser

main :: IO ()
main = do
    pUser <- newIORef [] -- Haskell裡面都是不可變的function, 用IORef令function的內容變更, 當然也可以不斷創建新function來更新function內容, 但目前不太想做
    operationLoop pUser