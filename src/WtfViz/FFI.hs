{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

module WtfViz.FFI
       ( -- * main
         runViz

         -- * Entity
       , Entity
       , entityCreate
       , entitySetMaterialName

         -- * Light
       , createLight
       , setAmbientLight
       , lightSetPosition
       , lightSetDirection
       , lightSetDiffuseColour
       , lightSetSpecularColour

         -- * ManualObject
       , ManualObject
       , manualObjectCreate
       , manualObjectDelete
       , RenderOp(..)
       , manualObjectBegin
       , manualObjectBeginUpdate
       , manualObjectPosition
       , manualObjectNormal
       , manualObjectTangent
       , manualObjectColour
       , manualObjectEnd
       , manualObjectClear
       , manualObjectEstimateVertexCount
       , manualObjectEstimateIndexCount
       , manualObjectConvertToMesh
       , manualObjectSetDynamic
       , manualObjectSetVisible

         -- * Material
       , getMaterialManager
       , materialManagerGetByName
       , materialClone
       , materialGetTechnique

         -- * Pass
       , SceneBlendType(..)
       , passSetSceneBlending
       , passSetDepthWriteEnabled

         -- * SceneNode
       , SceneNode
       , sceneNodeGetRoot
       , sceneNodeCreateChild
       , sceneNodeAttachEntity
       , sceneNodeAttachManualObject
       , sceneNodeSetPosition
       , sceneNodeSetScale
       , sceneNodeSetOrientation
       , sceneNodeSetVisible

         -- * Technique
       , Technique
       , techniqueGetPass

         -- * misc
       , setExtraMessages
       , setSkybox
       ) where

import GHC.Generics ( Generic )

import Data.IORef ( newIORef, readIORef, writeIORef )
import Foreign.C.Types ( CInt(..), CDouble(..) )
import Foreign.C.String ( CString, withCString )
import Foreign.Ptr ( FunPtr, Ptr )
import Linear ( Quaternion(..), V3(..) )

data RenderOp
  = OT_POINT_LIST -- ^ A list of points, 1 vertex per point
  | OT_LINE_LIST -- ^ A list of lines, 2 vertices per line
  | OT_LINE_STRIP -- ^  A strip of connected lines, 1 vertex per line plus 1 start vertex
  | OT_TRIANGLE_LIST -- ^ A list of triangles, 3 vertices per triangle
  | OT_TRIANGLE_STRIP -- ^ A strip of triangles, 3 vertices for the first triangle, and 1 per triangle after that
  | OT_TRIANGLE_FAN -- ^ A fan of triangles, 3 vertices for the first triangle, and 1 per triangle after that
  deriving (Eq, Ord, Show, Enum, Generic)

data SceneBlendType
  = SBT_TRANSPARENT_ALPHA -- ^ Make the object transparent based on the final alpha values in the texture
  | SBT_TRANSPARENT_COLOUR -- ^ Make the object transparent based on the colour values in the texture (brighter = more opaque)
  | SBT_ADD -- ^ Add the texture values to the existing scene content
  | SBT_MODULATE -- ^ Multiply the 2 colours together
  | SBT_REPLACE -- ^ The default blend mode where source replaces destination
  deriving (Eq, Ord, Show, Enum, Generic)

data Entity'
newtype Entity = Entity (Ptr Entity')

data Light'
newtype Light = Light (Ptr Light')

data ManualObject'
newtype ManualObject = ManualObject { unManualObject :: Ptr ManualObject' }

data Material'
newtype Material = Material (Ptr Material')

data MaterialManager'
newtype MaterialManager = MaterialManager (Ptr MaterialManager')

data Pass'
newtype Pass = Pass (Ptr Pass')

data SceneNode'
newtype SceneNode = SceneNode (Ptr SceneNode')

data Technique'
newtype Technique = Technique (Ptr Technique')


-- main
foreign import ccall "wrapper"
  c_wrapOgreCall :: IO () -> IO (FunPtr (IO ()))

foreign import ccall safe "wv2_run_viz"
  c_runViz :: FunPtr (IO ()) -> FunPtr (IO ()) -> IO ()

runViz :: IO a -> (a -> IO a) -> IO ()
runViz initialize update = do
  stateRef <- newIORef (error "should never be accessed")

  initPtr <- c_wrapOgreCall $ do
    initialState <- initialize
    writeIORef stateRef initialState

  updatePtr <- c_wrapOgreCall $ do
    state0 <- readIORef stateRef
    state1 <- update state0
    writeIORef stateRef state1

  c_runViz initPtr updatePtr



-- Entity
foreign import ccall unsafe "wv2_entity_create"
  c_entityCreate :: CString -> CString -> IO (Ptr Entity')

entityCreate :: String -> String -> IO Entity
entityCreate name meshname =
  withCString name $ \cname ->
  withCString meshname $ \cmeshname ->
  Entity <$> c_entityCreate cname cmeshname


foreign import ccall unsafe "wv2_entity_set_material_name"
  c_entitySetMaterialName :: Ptr Entity' -> CString -> IO ()

entitySetMaterialName :: Entity -> String -> IO ()
entitySetMaterialName (Entity e) name =
  withCString name (c_entitySetMaterialName e)



-- Light
foreign import ccall unsafe "wv2_create_light"
  c_createLight :: CString -> IO (Ptr Light')

-- TODO(greg): this really goes under SceneManager
createLight :: String -> IO Light
createLight name = Light <$> withCString name c_createLight


foreign import ccall unsafe "wv2_set_ambient_light"
  c_setAmbientLight :: CDouble -> CDouble -> CDouble -> IO ()

-- TODO(greg): this really goes under SceneManager
setAmbientLight :: Double -> Double -> Double -> IO ()
setAmbientLight r g b = c_setAmbientLight (realToFrac r) (realToFrac g) (realToFrac b)


foreign import ccall unsafe "wv2_light_set_position"
  c_lightSetPosition :: Ptr Light' -> CDouble -> CDouble -> CDouble -> IO ()

lightSetPosition :: Light -> Double -> Double -> Double -> IO ()
lightSetPosition (Light obj) x y z = c_lightSetPosition obj (realToFrac x) (realToFrac y) (realToFrac z)


foreign import ccall unsafe "wv2_light_set_direction"
  c_lightSetDirection :: Ptr Light' -> CDouble -> CDouble -> CDouble -> IO ()

lightSetDirection :: Light -> Double -> Double -> Double -> IO ()
lightSetDirection (Light obj) x y z = c_lightSetDirection obj (realToFrac x) (realToFrac y) (realToFrac z)


foreign import ccall unsafe "wv2_light_set_diffuse_colour"
  c_lightSetDiffuseColour :: Ptr Light' -> CDouble -> CDouble -> CDouble -> IO ()

lightSetDiffuseColour :: Light -> Double -> Double -> Double -> IO ()
lightSetDiffuseColour (Light obj) x y z = c_lightSetDiffuseColour obj (realToFrac x) (realToFrac y) (realToFrac z)


foreign import ccall unsafe "wv2_light_set_specular_colour"
  c_lightSetSpecularColour :: Ptr Light' -> CDouble -> CDouble -> CDouble -> IO ()

lightSetSpecularColour :: Light -> Double -> Double -> Double -> IO ()
lightSetSpecularColour (Light obj) x y z = c_lightSetSpecularColour obj (realToFrac x) (realToFrac y) (realToFrac z)





-- ManualObject
foreign import ccall unsafe "wv2_manual_object_create"
  c_manualObjectCreate :: CString -> IO (Ptr ManualObject')

manualObjectCreate :: String -> IO ManualObject
manualObjectCreate name = ManualObject <$> withCString name c_manualObjectCreate


foreign import ccall unsafe "wv2_manual_object_delete"
  c_manualObjectDelete :: Ptr ManualObject' -> IO ()

manualObjectDelete :: ManualObject -> IO ()
manualObjectDelete = c_manualObjectDelete . unManualObject


foreign import ccall unsafe "wv2_manual_object_begin"
  c_manualObjectBegin :: Ptr ManualObject' -> CString -> CInt -> IO ()

manualObjectBegin :: ManualObject -> String -> RenderOp -> IO ()
manualObjectBegin (ManualObject mo) name ro =
  withCString name $ \cname ->
  c_manualObjectBegin mo cname (fromIntegral (fromEnum ro))


foreign import ccall unsafe "wv2_manual_object_begin_update"
  c_manualObjectBeginUpdate :: Ptr ManualObject' -> CInt -> IO ()

manualObjectBeginUpdate :: ManualObject -> Int -> IO ()
manualObjectBeginUpdate (ManualObject mo) k =
  c_manualObjectBeginUpdate mo (fromIntegral k)


foreign import ccall unsafe "wv2_manual_object_position"
  c_manualObjectPosition :: Ptr ManualObject' -> CDouble -> CDouble -> CDouble -> IO ()

manualObjectPosition :: ManualObject -> Double -> Double -> Double -> IO ()
manualObjectPosition (ManualObject mo) x y z =
  c_manualObjectPosition mo (realToFrac x) (realToFrac y) (realToFrac z)


foreign import ccall unsafe "wv2_manual_object_normal"
  c_manualObjectNormal :: Ptr ManualObject' -> CDouble -> CDouble -> CDouble -> IO ()

manualObjectNormal :: ManualObject -> Double -> Double -> Double -> IO ()
manualObjectNormal (ManualObject mo) x y z =
  c_manualObjectNormal mo (realToFrac x) (realToFrac y) (realToFrac z)


foreign import ccall unsafe "wv2_manual_object_tangent"
  c_manualObjectTangent :: Ptr ManualObject' -> CDouble -> CDouble -> CDouble -> IO ()

manualObjectTangent :: ManualObject -> Double -> Double -> Double -> IO ()
manualObjectTangent (ManualObject mo) x y z =
  c_manualObjectTangent mo (realToFrac x) (realToFrac y) (realToFrac z)


foreign import ccall unsafe "wv2_manual_object_colour"
  c_manualObjectColour :: Ptr ManualObject' -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

manualObjectColour :: ManualObject -> Double -> Double -> Double -> Double -> IO ()
manualObjectColour (ManualObject mo)  r g b a =
  c_manualObjectColour mo (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)


foreign import ccall unsafe "wv2_manual_object_end"
  c_manualObjectEnd :: Ptr ManualObject' -> IO ()

manualObjectEnd :: ManualObject -> IO ()
manualObjectEnd = c_manualObjectEnd . unManualObject


foreign import ccall unsafe "wv2_manual_object_clear"
  c_manualObjectClear :: Ptr ManualObject' -> IO ()

manualObjectClear :: ManualObject -> IO ()
manualObjectClear = c_manualObjectClear . unManualObject


foreign import ccall unsafe "wv2_manual_object_estimate_vertex_count"
  c_manualObjectEstimateVertexCount :: Ptr ManualObject' -> CInt -> IO ()

manualObjectEstimateVertexCount :: ManualObject -> Int -> IO ()
manualObjectEstimateVertexCount (ManualObject mo) n = c_manualObjectEstimateVertexCount mo (fromIntegral n)


foreign import ccall unsafe "wv2_manual_object_estimate_index_count"
  c_manualObjectEstimateIndexCount :: Ptr ManualObject' -> CInt -> IO ()

manualObjectEstimateIndexCount :: ManualObject -> Int -> IO ()
manualObjectEstimateIndexCount (ManualObject mo) n = c_manualObjectEstimateIndexCount mo (fromIntegral n)


foreign import ccall unsafe "wv2_manual_object_convert_to_mesh"
  c_manualObjectConvertToMesh :: Ptr ManualObject' -> CString -> IO ()

manualObjectConvertToMesh :: ManualObject -> String -> IO ()
manualObjectConvertToMesh (ManualObject mo) name = withCString name $ c_manualObjectConvertToMesh mo


foreign import ccall unsafe "wv2_manual_object_set_dynamic"
  c_manualObjectSetDynamic :: Ptr ManualObject' -> CInt -> IO ()

manualObjectSetDynamic :: ManualObject -> Bool -> IO ()
manualObjectSetDynamic (ManualObject mo) dynamic = c_manualObjectSetDynamic mo (fromIntegral (fromEnum dynamic))


foreign import ccall unsafe "wv2_manual_object_set_visible"
  c_manualObjectSetVisible :: Ptr ManualObject' -> CInt -> IO ()

manualObjectSetVisible :: ManualObject -> Bool -> IO ()
manualObjectSetVisible (ManualObject mo) visible = c_manualObjectSetVisible mo (fromIntegral (fromEnum visible))



-- Material
foreign import ccall unsafe "wv2_get_material_manager"
  c_getMaterialManager :: IO (Ptr MaterialManager')

getMaterialManager :: IO MaterialManager
getMaterialManager = MaterialManager <$> c_getMaterialManager


foreign import ccall unsafe "wv2_material_manager_get_by_name"
  c_materialManagerGetByName :: Ptr MaterialManager' -> CString -> IO (Ptr Material')

materialManagerGetByName :: MaterialManager -> String -> IO Material
materialManagerGetByName (MaterialManager mm) name =
  Material <$> withCString name (c_materialManagerGetByName mm)


foreign import ccall unsafe "wv2_material_clone"
  c_materialClone :: Ptr Material' -> CString -> IO (Ptr Material')

materialClone :: Material -> String -> IO Material
materialClone (Material m) name =
  Material <$> withCString name (c_materialClone m)


foreign import ccall unsafe "wv2_material_get_technique"
  c_materialGetTechnique :: Ptr Material' -> CInt -> IO (Ptr Technique')

materialGetTechnique :: Material -> Int -> IO Technique
materialGetTechnique (Material m) k =
  Technique <$> c_materialGetTechnique m (fromIntegral k)



-- SceneNode
foreign import ccall unsafe "wv2_scene_node_get_root"
  c_sceneNodeGetRoot :: IO (Ptr SceneNode')

sceneNodeGetRoot :: IO SceneNode
sceneNodeGetRoot = SceneNode <$> c_sceneNodeGetRoot


foreign import ccall unsafe "wv2_scene_node_create_child"
  c_sceneNodeCreateChild :: Ptr SceneNode' -> CString -> IO (Ptr SceneNode')

sceneNodeCreateChild :: SceneNode -> String -> IO SceneNode
sceneNodeCreateChild (SceneNode sn) name = SceneNode <$> (withCString name $ c_sceneNodeCreateChild sn)


foreign import ccall unsafe "wv2_scene_node_attach_entity"
  c_sceneNodeAttachEntity :: Ptr SceneNode' -> Ptr Entity' -> IO ()

sceneNodeAttachEntity :: SceneNode -> Entity -> IO ()
sceneNodeAttachEntity (SceneNode sn) (Entity en) = c_sceneNodeAttachEntity sn en


foreign import ccall unsafe "wv2_scene_node_attach_manual_object"
  c_sceneNodeAttachManualObject :: Ptr SceneNode' -> Ptr ManualObject' -> IO ()

sceneNodeAttachManualObject :: SceneNode -> ManualObject -> IO ()
sceneNodeAttachManualObject (SceneNode sn) (ManualObject mo) = c_sceneNodeAttachManualObject sn mo


foreign import ccall unsafe "wv2_scene_node_set_position"
  c_sceneNodeSetPosition :: Ptr SceneNode' -> CDouble -> CDouble -> CDouble -> IO ()

sceneNodeSetPosition :: SceneNode -> Double -> Double -> Double -> IO ()
sceneNodeSetPosition (SceneNode sn) x y z =
  c_sceneNodeSetPosition sn (realToFrac x) (realToFrac y) (realToFrac z)


foreign import ccall unsafe "wv2_scene_node_set_scale"
  c_sceneNodeSetScale :: Ptr SceneNode' -> CDouble -> CDouble -> CDouble -> IO ()

sceneNodeSetScale :: SceneNode -> Double -> Double -> Double -> IO ()
sceneNodeSetScale (SceneNode sn) x y z =
  c_sceneNodeSetScale sn (realToFrac x) (realToFrac y) (realToFrac z)


foreign import ccall unsafe "wv2_scene_node_set_orientation"
  c_sceneNodeSetOrientation :: Ptr SceneNode' -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

sceneNodeSetOrientation :: SceneNode -> Quaternion Double -> IO ()
sceneNodeSetOrientation (SceneNode sn) (Quaternion w (V3 x y z)) =
  c_sceneNodeSetOrientation sn (realToFrac w) (realToFrac x) (realToFrac y) (realToFrac z)


foreign import ccall unsafe "wv2_scene_node_set_visible"
  c_sceneNodeSetVisible :: Ptr SceneNode' -> CInt -> IO ()

sceneNodeSetVisible :: SceneNode -> Bool -> IO ()
sceneNodeSetVisible (SceneNode sn) visible =
  c_sceneNodeSetVisible sn (fromIntegral (fromEnum visible))



-- Pass
foreign import ccall unsafe "wv2_pass_set_scene_blending"
  c_passSetSceneBlending :: Ptr Pass' -> CInt -> IO ()

passSetSceneBlending :: Pass -> SceneBlendType -> IO ()
passSetSceneBlending (Pass p) sbt = c_passSetSceneBlending p (fromIntegral (fromEnum sbt))


foreign import ccall unsafe "wv2_pass_set_depth_write_enabled"
  c_passSetDepthWriteEnabled :: Ptr Pass' -> CInt -> IO ()

passSetDepthWriteEnabled :: Pass -> Bool -> IO ()
passSetDepthWriteEnabled (Pass p) enabled = c_passSetDepthWriteEnabled p (fromIntegral (fromEnum enabled))


-- Technique
foreign import ccall unsafe "wv2_technique_get_pass"
  c_techniqueGetPass :: Ptr Technique' -> CInt -> IO (Ptr Pass')

techniqueGetPass :: Technique -> Int -> IO Pass
techniqueGetPass (Technique t) k = Pass <$> c_techniqueGetPass t (fromIntegral k)



-- misc
foreign import ccall unsafe "wv2_set_extra_messages"
  c_setExtraMessages :: CString -> IO ()

setExtraMessages :: String -> IO ()
setExtraMessages msgs = withCString msgs c_setExtraMessages


foreign import ccall unsafe "wv2_set_skybox"
  c_setSkybox :: CInt -> CString -> IO ()

setSkybox :: Bool -> String -> IO ()
setSkybox b name = withCString name (c_setSkybox (fromIntegral (fromEnum b)))
