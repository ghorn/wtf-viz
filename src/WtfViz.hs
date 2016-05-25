{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

module WtfViz
       ( module WV
         -- * helpers
       , Mesh(..), newMesh, updateMesh
       , Line(..), newLine, updateLine
       ) where

import Linear ( V3(..), V4(..), Quaternion(..) )

import WtfViz.FFI.FFI as WV

data Mesh =
  Mesh
  { meshSceneNode :: WV.SceneNode
  , meshEntity :: WV.Entity
  }

data Line =
  Line
  { lineManualObject :: WV.ManualObject
  , lineSceneNode :: WV.SceneNode
  }

newLine :: String -> Int -> IO Line
newLine name k = do
  let objName = "Lines__" ++ name ++ "__obj__" ++ show k
      nodeName = "Lines__" ++ name ++ "__node__" ++ show k
  -- make ManualObject
  obj <- WV.manualObjectCreate objName
  WV.manualObjectSetDynamic obj True

  -- make SceneNode and attach object
  root <- WV.sceneNodeGetRoot
  node <- WV.sceneNodeCreateChild root nodeName
  WV.sceneNodeAttachManualObject node obj

  let line' =
        Line
        { lineManualObject = obj
        , lineSceneNode = node
        }
  return line'

updateLine :: Line -> String -> RenderOp -> [(V3 Double, V4 Double)] -> IO ()
updateLine (Line {lineManualObject = obj}) material renderOp nodes = do
  WV.manualObjectClear obj
  WV.manualObjectEstimateVertexCount obj (length nodes)
  -- TODO(greg): beginUpdate instead of begin
  WV.manualObjectBegin obj material renderOp
  let posCol (V3 x y z, V4 r g b a) = do
        WV.manualObjectPosition obj x y z
        WV.manualObjectColour obj r g b a
  mapM_ posCol nodes
  WV.manualObjectEnd obj

newMesh :: String -> String -> IO Mesh
newMesh description meshName = do
  let entityName = "Mesh__" ++ meshName ++ "__entity__" ++ description
      nodeName = "Mesh__" ++ meshName ++ "__node__" ++ description
  entity <- WV.entityCreate entityName meshName
  root <- WV.sceneNodeGetRoot
  node <- WV.sceneNodeCreateChild root nodeName
  WV.sceneNodeAttachEntity node entity
  let mesh =
        Mesh
        { meshEntity = entity
        , meshSceneNode = node
        }
  return mesh

updateMesh :: Mesh -> (V3 Double, V3 Double, Quaternion Double) -> IO ()
updateMesh (Mesh {meshSceneNode = sn}) (V3 px py pz, V3 sx sy sz, quat) = do
  WV.sceneNodeSetPosition sn px py pz
  WV.sceneNodeSetScale sn sx sy sz
  WV.sceneNodeSetOrientation sn quat
