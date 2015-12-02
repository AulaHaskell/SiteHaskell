{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Import
import Yesod
import Yesod.Static
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio { connPool :: ConnectionPool,
                     getStatic :: Static }

staticFiles "."

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Livro
   cd_ISBN Text
   nm_Livro Text
   nm_Autor Text
   dt_Ano Text
   nm_Genero Text
   vl_Avaliação Int
   ds_Sinopse Text
   deriving Show

Genero
   nm_Genero Text
   deriving Show

Usuario
   nome Text
   pass Text
   nm_usuario Text
   deriving Show

Avaliacao
   vl_Nota Int
   cd_Usuario UsuarioId
   cd_Livro LivroId
   opniao Text
   deriving Show

Lista
   cd_Usuario UsuarioId
   cd_Livro LivroId
|]

mkYesodData "Sitio" pRoutes

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where
    authRoute _ = Just $ LoginR
    isAuthorized UsuarioR _ = isVisita
    isAuthorized LoginR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized _ _ = isUser

isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "Soh o admin acessa aqui!"

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
      --  Nothing -> Authorized
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

isVisita = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> Authorized
        Just _ -> Authorized

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage
