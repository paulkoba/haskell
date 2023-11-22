import Control.Monad
import Control.Monad.State
import Database.HDBC
import Database.HDBC.MySQL
import System.Exit

data AppState = AppState { 
    dbConnection :: Connection,
    
    getAllResourcesRequest :: Statement,
    getAllUsersRequest :: Statement,
    getAllStatisticsRequest :: Statement,
    getAllResourceTypesRequest :: Statement,
    getAllResourceStatisticLinksRequest :: Statement,

    deleteFromResourcesRequest :: Statement, 
    deleteFromUsersRequest :: Statement, 
    deleteFromStatisticsRequest :: Statement, 
    deleteFromResourceTypesRequest :: Statement, 
    
    insertIntoResourcesRequest :: Statement, 
    insertIntoUsersRequest :: Statement, 
    insertIntoStatisticsRequest :: Statement, 
    insertIntoResourceTypesRequest :: Statement, 
    insertIntoResourceStatisticLinksRequest :: Statement
}

type AppStateWithIO = StateT AppState IO

emptyState :: IO AppState
emptyState = do 
    conn <- liftIO $ connectMySQL defaultMySQLConnectInfo {
        mysqlHost     = "127.0.0.1",
        mysqlUser     = "root",
        mysqlPassword = "EYpBXFus0S6qkfj",
        mysqlDatabase = "InformationResourcesDeptDB"
    }

    lGetAllResourcesRequest <- prepare conn "SELECT * FROM Resources"
    lGetAllUsersRequest <- prepare conn "SELECT * FROM Users"
    lGetAllStatisticsRequest <- prepare conn "SELECT * FROM DailyUsageStatistics"
    lGetAllResourceTypesRequest <- prepare conn "SELECT * FROM ResourceTypes"
    lGetAllResourceStatisticLinksRequest <- prepare conn "SELECT * FROM ResourceStatistics"

    lDeleteFromResourcesRequest <- prepare conn "DELETE FROM Resources WHERE resource_id = ?"
    lDeleteFromUsersRequest <- prepare conn "DELETE FROM Users WHERE user_id = ?"
    lDeleteFromStatisticsRequest <- prepare conn "DELETE FROM DailyUsageStatistics WHERE stat_id = ?"
    lDeleteFromResourceTypesRequest <- prepare conn "DELETE FROM ResourceTypes WHERE type_id = ?"

    lInsertIntoResourcesRequest <- prepare conn "INSERT INTO Resources (resource_name, author_id, annotation, creation_date, expiration_date, terms_and_conditions, url_address, resource_type_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
    lInsertIntoUsersRequest <- prepare conn "INSERT INTO Users (user_name, user_email) VALUES (?, ?)"
    lInsertIntoStatisticsRequest <- prepare conn "INSERT INTO DailyUsageStatistics (unique_visitors, page_views, peak_load) VALUES (?, ?, ?)"
    lInsertIntoResourceStatisticLinksRequest <- prepare conn "INSERT INTO ResourceStatistics (resource_id, stat_id) VALUES (?, ?)"
    lInsertIntoResourceTypesRequest <- prepare conn "INSERT INTO ResourceTypes (resource_type) VALUES (?)"

    return AppState {
        dbConnection = conn,
        getAllResourcesRequest = lGetAllResourcesRequest,
        getAllUsersRequest = lGetAllUsersRequest,
        getAllStatisticsRequest = lGetAllStatisticsRequest,
        getAllResourceTypesRequest = lGetAllResourceTypesRequest,
        getAllResourceStatisticLinksRequest = lGetAllResourceStatisticLinksRequest,

        deleteFromResourcesRequest = lDeleteFromResourcesRequest,
        deleteFromUsersRequest = lDeleteFromUsersRequest,
        deleteFromStatisticsRequest = lDeleteFromStatisticsRequest,
        deleteFromResourceTypesRequest = lDeleteFromResourceTypesRequest,

        insertIntoResourcesRequest = lInsertIntoResourcesRequest,
        insertIntoUsersRequest = lInsertIntoUsersRequest,
        insertIntoStatisticsRequest = lInsertIntoStatisticsRequest,
        insertIntoResourceTypesRequest = lInsertIntoResourceTypesRequest,
        insertIntoResourceStatisticLinksRequest = lInsertIntoResourceStatisticLinksRequest
    }

-- User table boilerplate

printUserRow :: [SqlValue] -> AppStateWithIO ()
printUserRow row = do
    let id = fromSql $ row!!0
    let name = fromSql $ row!!1
    let email = fromSql $ row!!2
    liftIO $ putStrLn $ show (id :: Int) ++ "\t" ++ show (name :: String) ++ "\t" ++ show (email :: String)

viewUsers :: AppStateWithIO ()
viewUsers = do
    request <- gets getAllUsersRequest
    liftIO $ execute request []
    results <- liftIO $ fetchAllRows request
    liftIO $ putStrLn "Users Table:"
    liftIO $ putStrLn "ID \t Name \t\t Email"
    mapM_ printUserRow results

createUser :: AppStateWithIO ()
createUser = do
    liftIO $ putStrLn "Enter user's name: "
    user_name <- liftIO $ (getLine :: IO String)
    liftIO $ putStrLn "Enter user's email: "
    user_email <- liftIO $ (getLine :: IO String)
    
    request <- gets insertIntoUsersRequest
    liftIO $ execute request [toSql (user_name :: String), toSql (user_email :: String)]

    liftIO $ putStrLn "Updated table: "
    viewUsers

deleteFromUsers :: AppStateWithIO ()
deleteFromUsers = do
    liftIO $ putStrLn "Enter user ID to delete: "
    id <- liftIO $ readLn
    request <- gets deleteFromUsersRequest
    liftIO $ execute request [toSql (id :: Int)]

    liftIO $ putStrLn "Updated table: "
    viewUsers

usersTableLoop :: AppStateWithIO ()
usersTableLoop = do
    liftIO $ putStrLn "Select one of the following options:"
    liftIO $ putStrLn "(1) to see all users."
    liftIO $ putStrLn "(2) to create user."
    liftIO $ putStrLn "(3) to delete user."
    liftIO $ putStrLn "(0) to go back."

    choice <- liftIO $ getLine;

    case choice of
        "1" -> viewUsers
        "2" -> createUser
        "3" -> deleteFromUsers
        "0" -> return ()
        _  -> liftIO $ putStrLn ("Unexpected string: " ++ choice)
    
    if choice == "0" then
        return ()
    else do
        mainLoop


-- Resource table boilerplate

printResourcesRow :: [SqlValue] -> AppStateWithIO ()
printResourcesRow row = do
    let resource_id = fromSql $ row!!0
    let resource_name = fromSql $ row!!1
    let author_id = fromSql $ row!!2
    let annot = fromSql $ row!!3
    let creation_date = fromSql $ row!!4
    let expiration_date = fromSql $ row!!5
    let terms_and_conditions = fromSql $ row!!6
    let url_address = fromSql $ row!!7
    let resource_type_id = fromSql $ row!!8

    liftIO $ putStrLn $ show (resource_id :: Int) ++ "\t" ++ show (resource_name :: String) ++ "\t" ++ show (author_id :: Int) ++ "\t" ++ show (annot :: String) ++ "\t" ++ show (creation_date :: String) ++ "\t" ++ show (expiration_date :: String) ++ "\t" ++ show (terms_and_conditions :: String) ++ "\t" ++ show (url_address :: String) ++ "\t" ++ show (resource_type_id :: String)

viewResources :: AppStateWithIO ()
viewResources = do
    request <- gets getAllResourcesRequest
    liftIO $ execute request []
    results <- liftIO $ fetchAllRows request
    liftIO $ putStrLn "Resources Table:"
    mapM_ printResourcesRow results

deleteFromResources :: AppStateWithIO ()
deleteFromResources = do
    liftIO $ putStrLn "Enter resource ID to delete: "
    id <- liftIO $ readLn
    request <- gets deleteFromResourcesRequest
    liftIO $ execute request [toSql (id :: Int)]

    liftIO $ putStrLn "Updated table: "
    viewResources

createResourceItem :: AppStateWithIO ()
createResourceItem = do
    liftIO $ putStrLn "Enter resource name: "
    resource_name <- liftIO $ (getLine :: IO String)
    liftIO $ putStrLn "Enter author id: "
    author_id <- liftIO $ (readLn :: IO Int)
    liftIO $ putStrLn "Enter annotation: "
    annotation <- liftIO $ (getLine :: IO String)
    liftIO $ putStrLn "Enter creation date: "
    creation_date <- liftIO $ (getLine :: IO String)
    liftIO $ putStrLn "Enter expiration date: "
    expiration_date <- liftIO $ (getLine :: IO String)
    liftIO $ putStrLn "Enter terms and conditions: "
    terms_and_conditions <- liftIO $ (getLine :: IO String)
    liftIO $ putStrLn "Enter url address: "
    url_address <- liftIO $ (getLine :: IO String)
    liftIO $ putStrLn "Enter resource type id: "
    resource_type_id <- liftIO $ (readLn :: IO Int)
    
    request <- gets insertIntoResourcesRequest
    liftIO $ execute request [toSql (resource_name :: String), toSql (author_id :: Int), toSql (annotation :: String), toSql (creation_date :: String), toSql (expiration_date :: String), toSql (terms_and_conditions :: String), toSql (url_address :: String), toSql (resource_type_id :: Int)]

    liftIO $ putStrLn "Updated table: "
    viewResources

resourceTableLoop :: AppStateWithIO ()
resourceTableLoop = do
    liftIO $ putStrLn "Select one of the following options:"
    liftIO $ putStrLn "(1) to see all resources."
    liftIO $ putStrLn "(2) to create resource."
    liftIO $ putStrLn "(3) to delete resource."
    liftIO $ putStrLn "(0) to go back."

    choice <- liftIO $ getLine;

    case choice of
        "1" -> viewResources
        "2" -> createResourceItem
        "3" -> deleteFromResources
        "0" -> return ()
        _  -> liftIO $ putStrLn ("Unexpected string: " ++ choice)
    
    if choice == "0" then
        return ()
    else do
        mainLoop



-- Resource statistics link


printResourceStatisticsLinksRow :: [SqlValue] -> AppStateWithIO ()
printResourceStatisticsLinksRow row = do
    let resource_id = fromSql $ row!!0
    let stat_id = fromSql $ row!!1
    liftIO $ putStrLn $ show (resource_id :: Int) ++ "\t\t" ++ show (stat_id :: String)

viewResourceStatisticsLinks :: AppStateWithIO ()
viewResourceStatisticsLinks = do
    request <- gets getAllResourceStatisticLinksRequest
    liftIO $ execute request []
    results <- liftIO $ fetchAllRows request
    liftIO $ putStrLn "Resource statistics link Table:"
    liftIO $ putStrLn "Resource ID \t Stat item id"
    mapM_ printResourceStatisticsLinksRow results

createStatisticsLinkItem :: AppStateWithIO ()
createStatisticsLinkItem = do
    liftIO $ putStrLn "Enter resource id: "
    resource_id <- liftIO $ (readLn :: IO Int)
    liftIO $ putStrLn "Enter stat id: "
    stat_id <- liftIO $ (readLn :: IO Int)
    
    request <- gets insertIntoResourceStatisticLinksRequest
    liftIO $ execute request [toSql (resource_id :: Int), toSql (stat_id :: Int)]

    liftIO $ putStrLn "Updated table: "
    viewResourceStatisticsLinks

resourceStatisticsLinkLoop :: AppStateWithIO ()
resourceStatisticsLinkLoop = do
    liftIO $ putStrLn "Select one of the following options:"
    liftIO $ putStrLn "(1) to see all resource statistic link items."
    liftIO $ putStrLn "(2) to create resource statistic link item."
    liftIO $ putStrLn "(0) to go back."

    choice <- liftIO $ getLine;

    case choice of
        "1" -> viewResourceStatisticsLinks
        "2" -> createStatisticsLinkItem
        "0" -> return ()
        _  -> liftIO $ putStrLn ("Unexpected string: " ++ choice)
    
    if choice == "0" then
        return ()
    else do
        mainLoop


-- Resource statistics

printResourceStatisticsRow :: [SqlValue] -> AppStateWithIO ()
printResourceStatisticsRow row = do
    let stat_id = fromSql $ row!!0
    let unique_visitors = fromSql $ row!!1
    let page_views = fromSql $ row!!2
    let peak_load = fromSql $ row!!3
    liftIO $ putStrLn $ show (stat_id :: Int) ++ "\t" ++ show (unique_visitors :: Int) ++ "\t\t" ++ show (page_views :: Int) ++ "\t\t" ++ show (peak_load :: Int)

viewResourceStatistics :: AppStateWithIO ()
viewResourceStatistics = do
    request <- gets getAllStatisticsRequest
    liftIO $ execute request []
    results <- liftIO $ fetchAllRows request
    liftIO $ putStrLn "Resource statistics Table:"
    liftIO $ putStrLn "Stat ID\tUnique visitors\tPage views\tPeak load"
    mapM_ printResourceStatisticsRow results

createResourceStatisticsItem :: AppStateWithIO ()
createResourceStatisticsItem = do
    liftIO $ putStrLn "Enter unique visitors: "
    unique_visitors <- liftIO $ (readLn :: IO Int)
    liftIO $ putStrLn "Enter page views: "
    page_views <- liftIO $ (readLn :: IO Int)
    liftIO $ putStrLn "Enter peak load: "
    peak_load <- liftIO $ (readLn :: IO Int)
    
    request <- gets insertIntoResourcesRequest
    liftIO $ execute request [toSql (unique_visitors :: Int), toSql (page_views :: Int), toSql (peak_load :: Int)]

    liftIO $ putStrLn "Updated table: "
    viewResources

deleteFromStatistics :: AppStateWithIO ()
deleteFromStatistics = do
    liftIO $ putStrLn "Enter statistic ID to delete: "
    id <- liftIO $ readLn
    request <- gets deleteFromStatisticsRequest
    liftIO $ execute request [toSql (id :: Int)]

    liftIO $ putStrLn "Updated table: "
    deleteFromStatistics

statisticsLoop :: AppStateWithIO ()
statisticsLoop = do
    liftIO $ putStrLn "Select one of the following options:"
    liftIO $ putStrLn "(1) to see all resource statistic items."
    liftIO $ putStrLn "(2) to create resource statistic item."
    liftIO $ putStrLn "(3) to delete resource statistic item."
    liftIO $ putStrLn "(0) to go back."

    choice <- liftIO $ getLine;

    case choice of
        "1" -> viewResourceStatistics
        "2" -> createResourceStatisticsItem
        "3" -> deleteFromStatistics
        "0" -> return ()
        _  -> liftIO $ putStrLn ("Unexpected string: " ++ choice)
    
    if choice == "0" then
        return ()
    else do
        mainLoop

-- Resource types

printResourceTypeRow :: [SqlValue] -> AppStateWithIO ()
printResourceTypeRow row = do
    let resource_type_id = fromSql $ row!!0
    let desc = fromSql $ row!!1
    liftIO $ putStrLn $ show (resource_type_id :: Int) ++ "\t" ++ show (desc :: String)

viewResourceType :: AppStateWithIO ()
viewResourceType = do
    request <- gets getAllResourceTypesRequest
    liftIO $ execute request []
    results <- liftIO $ fetchAllRows request
    liftIO $ putStrLn "Resource statistics Table:"
    liftIO $ putStrLn "Type ID\tResource type"
    mapM_ printResourceTypeRow results

createResourceTypeItem :: AppStateWithIO ()
createResourceTypeItem = do
    liftIO $ putStrLn "Enter resource type name: "
    resource_name <- liftIO $ (getLine :: IO String)

    request <- gets insertIntoResourceTypesRequest
    liftIO $ execute request [toSql (resource_name :: String)]

    liftIO $ putStrLn "Updated table: "
    viewResourceType


deleteFromResourceTypes :: AppStateWithIO ()
deleteFromResourceTypes = do
    liftIO $ putStrLn "Enter resource type ID to delete: "
    id <- liftIO $ readLn
    request <- gets deleteFromResourceTypesRequest
    liftIO $ execute request [toSql (id :: Int)]

    liftIO $ putStrLn "Updated table: "
    viewResourceType

resourceTypeLoop :: AppStateWithIO ()
resourceTypeLoop = do
    liftIO $ putStrLn "Select one of the following options:"
    liftIO $ putStrLn "(1) to see all resource type items."
    liftIO $ putStrLn "(2) to create resource type item."
    liftIO $ putStrLn "(3) to delete resource type item."
    liftIO $ putStrLn "(0) to go back."

    choice <- liftIO $ getLine;

    case choice of
        "1" -> viewResourceType
        "2" -> createResourceTypeItem
        "3" -> deleteFromResourceTypes
        "0" -> return ()
        _  -> liftIO $ putStrLn ("Unexpected string: " ++ choice)
    
    if choice == "0" then
        return ()
    else do
        mainLoop

mainLoop :: AppStateWithIO ()
mainLoop = do
    liftIO $ putStrLn "Select one of the following options:"
    liftIO $ putStrLn "(1) to work with resources table."
    liftIO $ putStrLn "(2) to work with users table."
    liftIO $ putStrLn "(3) to work with resource / daily usage statistics link table."
    liftIO $ putStrLn "(4) to work with daily usage statistics table."
    liftIO $ putStrLn "(5) to work with resource types table."
    liftIO $ putStrLn "(0) to exit."

    choice <- liftIO $ getLine;

    case choice of
        "1" -> resourceTableLoop
        "2" -> usersTableLoop
        "3" -> resourceStatisticsLinkLoop
        "4" -> statisticsLoop
        "5" -> resourceTypeLoop
        "0" -> return ()
        _  -> liftIO $ putStrLn ("Unexpected string: " ++ choice)
    
    if choice == "0" then
        liftIO $ putStrLn "Exit requested."
    else do
        mainLoop

main :: IO ()
main = do
    initialState <- emptyState
    ((), finalState) <- runStateT mainLoop initialState
    
    let conn = dbConnection finalState
    disconnect conn