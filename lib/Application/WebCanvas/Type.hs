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
import Data.Aeson
import Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B

data WebcanvasInfo = WebcanvasInfo { 
  webcanvas_uuid :: UUID, 
  webcanvas_name :: String
} deriving (Show,Typeable,Data)


instance FromJSON UUID where
  parseJSON x = do r <- return . fromString . C.unpack . E.encodeUtf8 =<< parseJSON x
                   case r of 
                     Nothing -> fail ("UUID parsing failed " ++ show x )
                     Just uuid -> return uuid 

instance ToJSON UUID where
  toJSON = toJSON . E.decodeUtf8 . C.pack . toString 

instance FromJSON WebcanvasInfo where
  parseJSON (Object v) = WebcanvasInfo <$>  v .: "uuid" <*> v .: "name"

instance ToJSON WebcanvasInfo where
  toJSON (WebcanvasInfo uuid name) = object [ "uuid" .= uuid , "name" .= name ] 


instance SafeCopy UUID where 
  putCopy uuid = contain $ safePut (toByteString uuid) 
  getCopy = contain 
            $ maybe (fail "cannot parse UUID") return . fromByteString 
              =<< safeGet

$(deriveSafeCopy 0 'base ''WebcanvasInfo)

type WebcanvasInfoRepository = M.Map UUID WebcanvasInfo 

addWebcanvas :: WebcanvasInfo -> Update WebcanvasInfoRepository WebcanvasInfo 
addWebcanvas minfo = do 
  m <- get 
  let (r,m') = M.insertLookupWithKey (\_k _o n -> n) (webcanvas_uuid minfo) minfo m
  put m'
  return minfo
 
queryWebcanvas :: UUID -> Query WebcanvasInfoRepository (Maybe WebcanvasInfo) 
queryWebcanvas uuid = do 
  m <- ask 
  return (M.lookup uuid m)

queryAll :: Query WebcanvasInfoRepository [WebcanvasInfo]
queryAll = do m <- ask   
              return (M.elems m)


updateWebcanvas :: WebcanvasInfo -> Update WebcanvasInfoRepository (Maybe WebcanvasInfo)
updateWebcanvas minfo = do 
  m <- get 
  let (r,m') = M.updateLookupWithKey (\_ _ -> Just minfo) (webcanvas_uuid minfo) m
  put m'
  maybe (return Nothing) (const (return (Just minfo))) r 

deleteWebcanvas :: UUID -> Update WebcanvasInfoRepository (Maybe WebcanvasInfo)
deleteWebcanvas uuid = do 
  m <- get
  let r = M.lookup uuid m  
  case r of 
    Just _ -> do  
      let m' = M.delete uuid m  
      put m' 
      return r
    Nothing -> return Nothing


$(makeAcidic ''WebcanvasInfoRepository [ 'addWebcanvas, 'queryWebcanvas, 'queryAll, 'updateWebcanvas, 'deleteWebcanvas] )
