{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
import Yesod.Static
 
pRoutes = [parseRoutes|
   /user UsuarioR GET POST
   /listar ListUserR GET
   /static StaticR Static getStatic
   -- /ima ImgR GET
   /login LoginR GET POST
   / WelcomeR GET
   /bye ByeR GET
   /admin AdminR GET
   /livro LivroR GET POST
   /listalivro ListLivR GET
   /visualizar/#LivroId VisualizarR GET
   /avaliar AvaliarR GET POST
|]