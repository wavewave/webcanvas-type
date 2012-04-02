{-# LANGUAGE DeriveDataTypeable, 
             TemplateHaskell, 
             TypeFamilies, 
             TypeSynonymInstances, 
             OverloadedStrings  #-}

module Application.WebCanvas.Type where

import Control.Applicative 
import Control.Monad.Reader
import Control.Monad.State
import Data.Typeable
import Data.Data
import Data.SafeCopy
import qualified Data.Map as M

import Data.Acid 
import Data.UUID
import Data.UUID.Instances
import Data.Aeson
import Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Time.Clock

data WebCanvasItem = WebCanvasItem { 
  webcanvas_uuid :: UUID, 
  webcanvas_creationtime :: UTCTime 
} deriving (Show,Typeable,Data)

instance FromJSON WebCanvasItem where
  parseJSON (Object v) = WebCanvasItem <$>  v .: "uuid" <*> v .: "creationtime"

instance ToJSON WebCanvasItem where
  toJSON (WebCanvasItem uuid ctime) = object [ "uuid" .= uuid , "creationtime" .= ctime ] 

$(deriveSafeCopy 0 'base ''WebCanvasItem)

type WebCanvasItemRepository = M.Map UUID WebCanvasItem 

addWebCanvasItem :: WebCanvasItem -> Update WebCanvasItemRepository WebCanvasItem 
addWebCanvasItem minfo = do 
  m <- get 
  let (r,m') = M.insertLookupWithKey (\_k _o n -> n) (webcanvas_uuid minfo) minfo m
  put m'
  return minfo
 
queryWebCanvasItem :: UUID -> Query WebCanvasItemRepository (Maybe WebCanvasItem) 
queryWebCanvasItem uuid = do 
  m <- ask 
  return (M.lookup uuid m)

queryAll :: Query WebCanvasItemRepository [WebCanvasItem]
queryAll = do m <- ask   
              return (M.elems m)


updateWebCanvasItem :: WebCanvasItem -> Update WebCanvasItemRepository (Maybe WebCanvasItem)
updateWebCanvasItem minfo = do 
  m <- get 
  let (r,m') = M.updateLookupWithKey (\_ _ -> Just minfo) (webcanvas_uuid minfo) m
  put m'
  maybe (return Nothing) (const (return (Just minfo))) r 

deleteWebCanvasItem :: UUID -> Update WebCanvasItemRepository (Maybe WebCanvasItem)
deleteWebCanvasItem uuid = do 
  m <- get
  let r = M.lookup uuid m  
  case r of 
    Just _ -> do  
      let m' = M.delete uuid m  
      put m' 
      return r
    Nothing -> return Nothing


$(makeAcidic ''WebCanvasItemRepository [ 'addWebCanvasItem, 'queryWebCanvasItem, 'queryAll, 'updateWebCanvasItem, 'deleteWebCanvasItem] )
