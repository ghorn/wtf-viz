module WtfViz.Object
  ( Mesh(..), newMesh, updateMesh
  , Line(..), newLine, updateLine
  , GridSpec(..), createGridMesh
  , createConeMesh, createCylinderMesh
  ) where

import Linear ( Quaternion(..), V3(..), V4(..), cross )
import WtfViz.FFI.FFI ( Entity, ManualObject, RenderOp, SceneNode )
import qualified WtfViz.FFI.FFI as WV

-- | Wrapper around a mesh including its own scene node.
data Mesh =
  Mesh
  { meshSceneNode :: SceneNode
  , meshEntity :: Entity
  , meshDescription :: String
  }

-- | Wrapper around a ManualObject including its own scene node.
data Line =
  Line
  { lineManualObject :: ManualObject
  , lineSceneNode :: SceneNode
  }

newLine :: SceneNode -> String -> Int -> IO Line
newLine parentNode name k = do
  let objName = "Lines__" ++ name ++ "__obj__" ++ show k
      nodeName = "Lines__" ++ name ++ "__node__" ++ show k
  -- make ManualObject
  obj <- WV.manualObjectCreate objName
  WV.manualObjectSetDynamic obj True

  -- make SceneNode and attach object
--  root <- WV.sceneNodeGetRoot
  node <- WV.sceneNodeCreateChildSceneNode parentNode nodeName
  WV.sceneNodeAttachManualObject node obj

  let line' =
        Line
        { lineManualObject = obj
        , lineSceneNode = node
        }
  return line'

updateLine :: Line -> String -> RenderOp -> [(V3 Double, V4 Double)] -> IO ()
updateLine Line {lineManualObject = obj} material renderOp nodes = do
  WV.manualObjectClear obj
  WV.manualObjectEstimateVertexCount obj (length nodes)
  -- TODO(greg): beginUpdate instead of begin
  WV.manualObjectBegin obj material renderOp
  let posCol (pos, V4 r g b a) = do
        WV.manualObjectPosition obj pos
        WV.manualObjectColour obj r g b a
  mapM_ posCol nodes
  WV.manualObjectEnd obj

newMesh :: SceneNode -> String -> String -> IO Mesh
newMesh parentNode description meshName = do
  let entityName = "Mesh__" ++ meshName ++ "__entity__" ++ description
      nodeName = "Mesh__" ++ meshName ++ "__node__" ++ description
  entity <- WV.entityCreate entityName meshName
--  root <- WV.sceneNodeGetRoot
  node <- WV.sceneNodeCreateChildSceneNode parentNode nodeName
  WV.sceneNodeAttachEntity node entity
  let mesh =
        Mesh
        { meshEntity = entity
        , meshSceneNode = node
        , meshDescription = meshName ++ " (" ++ description ++ ")"
        }
  return mesh

updateMesh :: Mesh -> (V3 Double, V3 Double, Quaternion Double) -> IO ()
updateMesh Mesh {meshSceneNode = sn} (pos, scale, quat) = do
  WV.sceneNodeSetPosition sn pos
  WV.sceneNodeSetScale sn scale
  WV.sceneNodeSetOrientation sn quat


createConeMesh :: String -> Int -> Maybe (Double, Double, Double, Double) -> IO ()
createConeMesh name n mcolour = do
  mo <- WV.manualObjectCreate (name ++ "Object")

  let position pos (V3 u v w) = do
        WV.manualObjectPosition mo pos
        let norm = sqrt (u*u + v*v + w*w)
        WV.manualObjectNormal mo (V3 (u/norm) (v/norm) (w/norm))
        case mcolour of
          Nothing -> return ()
          Just (r, g, b, a) -> WV.manualObjectColour mo r g b a

  -- draw the cone
  WV.manualObjectBegin mo "FlatVertexColour" WV.OT_TRIANGLE_FAN
  let tip = V3 1 0 0

  position tip (V3 1 1 0)

  let cone :: Int -> IO ()
      cone k = position pos normal
        where
          q = 2*pi*fromIntegral k/fromIntegral n
          qdot = V3 0 (-sin q) (cos q)
          pos = V3 0 (cos q) (sin q)
          normal = cross qdot (tip - pos)

  mapM_ cone (take (n + 1) [0..])
  WV.manualObjectEnd mo

  -- draw the cap
  WV.manualObjectBegin mo "FlatVertexColour" WV.OT_TRIANGLE_FAN
  position (V3 0 0 0) (V3 (-1) 0 0)
  let cap :: Int -> IO ()
      cap k = position (V3 0 (cos q) (sin q)) (V3 (-1) 0 0)
        where
          q = - 2*pi*fromIntegral k/fromIntegral n
  mapM_ cap (take (n + 1) [0..])
  WV.manualObjectEnd mo

  WV.manualObjectConvertToMesh mo (name ++ "Mesh")
  WV.manualObjectDelete mo


createCylinderMesh :: String -> Int -> Maybe (Double, Double, Double, Double) -> IO ()
createCylinderMesh name n mcolour = do
  mo <- WV.manualObjectCreate (name ++ "Object")

  let position pos normal = do
        WV.manualObjectPosition mo pos
        WV.manualObjectNormal mo normal
        case mcolour of
          Nothing -> return ()
          Just (r, g, b, a) -> WV.manualObjectColour mo r g b a

  -- draw the body
  WV.manualObjectBegin mo "FlatVertexColour" WV.OT_TRIANGLE_STRIP
  let body :: Int -> IO ()
      body k = do
        let q0 = -2*pi*fromIntegral k/fromIntegral n
            q1 = q0 - pi / fromIntegral n
            y0 = cos q0
            z0 = sin q0
            y1 = cos q1
            z1 = sin q1
        position (V3 0 y0 z0) (V3 0 y0 z0)
        position (V3 1 y1 z1) (V3 0 y1 z1)
  mapM_ body (take (n + 1) [0..])
  WV.manualObjectEnd mo

  -- draw the bottom cap
  WV.manualObjectBegin mo "FlatVertexColour" WV.OT_TRIANGLE_FAN
  let bottomNormal = V3 (-1) 0 0
  position (V3 0 0 0) bottomNormal
  let bottomCap :: Int -> IO ()
      bottomCap k = position pos bottomNormal
        where
          pos = V3 0 (cos q) (sin q)
          q = - 2*pi*fromIntegral k/fromIntegral n
  mapM_ bottomCap (take (n + 1) [0..])
  WV.manualObjectEnd mo

  -- draw the top cap
  WV.manualObjectBegin mo "FlatVertexColour" WV.OT_TRIANGLE_FAN
  let topNormal = V3 1 0 0
  position (V3 0 0 0) topNormal
  let topCap :: Int -> IO ()
      topCap k = position pos topNormal
        where
          pos = V3 1 (cos q) (sin q)
          q = 2*pi*(0.5 + fromIntegral k)/fromIntegral n
  mapM_ topCap (take (n + 1) [0..])
  WV.manualObjectEnd mo

  WV.manualObjectConvertToMesh mo (name ++ "Mesh")
  WV.manualObjectDelete mo

data GridSpec =
  GridSpec
  { gsNumLines :: Int
  , gsLineSpacing :: Double
  , gsBoldInterval :: Int
  }

createGridMesh :: GridSpec -> IO ()
createGridMesh GridSpec {gsNumLines = n, gsLineSpacing = r, gsBoldInterval = boldInterval} = do
  mo <- WV.manualObjectCreate "GridObject"

  -- draw the body
  WV.manualObjectBegin mo "YeOldeTransparentLine" WV.OT_LINE_LIST

  let alpha :: Int -> Double
      alpha k
        | mod k boldInterval == 0 = 0.5
        | otherwise = 0.2

      xline :: Int -> IO ()
      xline k = do
        let x0 = -x1
            x1 = fromIntegral n * r
            y = fromIntegral k * r
        WV.manualObjectPosition mo (V3 x0 y 0)
        WV.manualObjectColour mo 1 1 1 (alpha k)
        WV.manualObjectPosition mo (V3 x1 y 0)
        WV.manualObjectColour mo 1 1 1 (alpha k)

      yline :: Int -> IO ()
      yline k = do
        let x = fromIntegral k * r
            y0 = -y1
            y1 = fromIntegral n * r
        WV.manualObjectPosition mo (V3 x y0 0)
        WV.manualObjectColour mo 1 1 1 (alpha k)
        WV.manualObjectPosition mo (V3 x y1 0)
        WV.manualObjectColour mo 1 1 1 (alpha k)
  mapM_ xline [-n..n]
  mapM_ yline [-n..n]

  WV.manualObjectEnd mo

  WV.manualObjectConvertToMesh mo "GridMesh"
  WV.manualObjectDelete mo
