{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where
import Import
import Yesod
import Yesod.Static
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" pRoutes

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Text -> Widget
widgetForm x enctype widget y val = do
     msg <- getMessage
     $(whamletFile "form.hamlet")
     toWidget $(luciusFile "teste.lucius")

-- widget de cadastro de usuario
widgetFormCad :: Route Sitio -> Enctype -> Widget-> Text -> Text -> Widget
widgetFormCad  x enctype widget y val = do
    msg <- getMessage
    $(whamletFile "cadastro.hamlet")
    toWidget $(luciusFile "teste.lucius")

-- so precisa do entype e do widget e o retorno do ultimo widget
widgetFormLiv :: Route Sitio -> Enctype -> Widget-> Text -> Text -> Widget
widgetFormLiv  x enctype widget y val = do
    msg <- getMessage
    $(whamletFile "livro.hamlet")
    toWidget $(luciusFile "teste.lucius")

widgetFormAvaliar :: Route Sitio -> Enctype -> Widget -> Text -> Text -> Widget
widgetFormAvaliar  x enctype widget y val = do
    msg <- getMessage
    $(whamletFile "avaliar.hamlet")
    toWidget $(luciusFile "teste.lucius")

formAv ::  Form Avaliacao
formAv  = renderDivs $ Avaliacao <$>
    areq intField "Nota" Nothing <*>
   -- areq (selectField us) "Usuario:" Nothing <*>
    pure us <*>
    areq (selectField lv) "Livro:" Nothing <*>
    areq textField "Avaliação: " Nothing

 

us = do 
    xy  <- lookupSession "_ID"
    case xy of
     Just x -> runDB $ selectList [UsuarioNome ==. x] []
     Nothing -> redirect LoginR
   -- optionsPairs $ fmap(\ent -> (usuarioNome $ entityVal ent, entityKey ent)) entidades
lv = do
    entidades <- runDB $ selectList [] [Asc LivroNm_Livro]
    optionsPairs $ fmap(\ent -> (livroNm_Livro $ entityVal ent, entityKey ent)) entidades
    

formLiv :: Form Livro
formLiv = renderDivs $ Livro  <$>
    areq textField "Codigo ISBN:" Nothing <*>
    areq textField "Titulo:" Nothing <*>
    areq textField "Autor:" Nothing <*>
    areq textField "Ano:" Nothing <*>
    areq textField "Genero:" Nothing <*>
    pure 0 <*>
    areq textField "Sinopse:" Nothing


formUsu :: Form Usuario
formUsu = renderDivs $ Usuario <$>
    areq textField "Login:" Nothing <*>
    areq textField "Senha:" Nothing <*>
    pure ""

formCad :: Form Usuario
formCad = renderDivs $ Usuario <$>
    areq textField "Login:" Nothing <*>
    areq textField "Senha:" Nothing <*>
    areq textField "Nome: " Nothing


getListLivR :: Handler Html
getListLivR = do
    liU <- runDB $ selectList [] [Asc LivroNm_Livro]
    defaultLayout $(whamletFile "livros.hamlet")

getVisualizarR :: Key Livro -> Handler Html
getVisualizarR x = do
    liU <- runDB $ selectFirst [LivroId ==. x] []
    defaultLayout [whamlet|
        $forall Entity pid livro <- liU
         <div id="divlivros">
            <p>#{livroNm_Livro livro}
            <p>#{livroCd_ISBN livro}
            <p>#{livroNm_Autor livro}
            <p>#{livroDt_Ano livro}
            <p>#{livroNm_Genero livro}
            <p>#{livroDs_Sinopse livro}
    |]
getAvaliarR :: Handler Html
getAvaliarR = do
    (wid,enc) <- generateFormPost formAv
    defaultLayout $ widgetFormAvaliar AvaliarR enc wid "LivroMania" "Avaliar"
    
postAvaliarR :: Handler Html
postAvaliarR = do
    ((result,_),_) <- runFormPost formAv
    case result of
        FormSuccess cadliv -> do
            runDB $ insert cadliv
            setMessage $ [shamlet| <p> Obrigado por Avaliar o Livro! |]
            redirect AvaliarR
        _ -> redirect AvaliarR
getLivroR :: Handler Html
getLivroR = do
    (wid,enc) <- generateFormPost formLiv
    defaultLayout $ widgetFormLiv LivroR enc wid "LivroMania" "Cadastrar"

postLivroR :: Handler Html
postLivroR = do
    ((result,_),_) <- runFormPost formLiv
    case result of
        FormSuccess cadliv -> do
            runDB $ insert cadliv
            setMessage $ [shamlet| <p> Livro inserido com sucesso! |]
            redirect LivroR
        _ -> redirect LivroR
-- esta modificado para cadastrar o usuario em outro form/codigo html, esta ok!
getUsuarioR :: Handler Html
getUsuarioR = do
    (wid,enc) <- generateFormPost formCad
    defaultLayout $ widgetFormCad UsuarioR enc wid "LivroMania" "Cadastrar"

--getImgR :: Handler Html
--getImgR = defaultLayout [whamlet| 
--    <img src=@{StaticR empolgou_jpg}> 


getWelcomeR :: Handler Html
getWelcomeR = do
     usr <- lookupSession "_ID"
     defaultLayout $(whamletFile "inicio.hamlet")


getLoginR :: Handler Html
getLoginR = do
    (wid,enc) <- generateFormPost formUsu
    defaultLayout $ widgetForm LoginR enc wid "Digite seus dados para acessar o LivroMania" "Entrar"


postLoginR :: Handler Html
postLoginR = do
    ((result,_),_) <- runFormPost formUsu
    case result of
        FormSuccess usr -> do
            usuario <- runDB $ selectFirst [UsuarioNome ==. usuarioNome usr, UsuarioPass ==. usuarioPass usr ] []
            case usuario of
                Just (Entity uid usr) -> do
                    setSession "_ID" (usuarioNome usr)
                    redirect WelcomeR
                Nothing -> do
                    setMessage $ [shamlet| Usuário ou senha incorretos |]
                    redirect LoginR 
        _ -> redirect LoginR

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost formUsu
    case result of
        FormSuccess usr -> do
            runDB $ insert usr
            setMessage $ [shamlet| <p> Usuario inserido com sucesso! |]
            redirect UsuarioR
        _ -> redirect UsuarioR

getListUserR :: Handler Html
getListUserR = do
    listaU <- runDB $ selectList [] [Asc UsuarioNome]
    defaultLayout $(whamletFile "list.hamlet")

getByeR :: Handler Html
getByeR = do
    deleteSession "_ID"
    redirect LoginR

getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet| <h1> Bem-vindo ADMIN!! |]


connStr = "dbname=dcho5ccc91qoht host=ec2-107-21-224-11.compute-1.amazonaws.com user=jwsayvsuldjcme password=8NwEM_qIT-QhFagXxQlc-KhBsH port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       s <- static "."
       warpEnv (Sitio pool s)