#include "WtfViz2.hpp"
#include "OgreFramework.hpp"

#ifdef __cplusplus
extern "C" {
#endif

  void wv2_run_viz(HsFunPtr haskell_init, HsFunPtr haskell_update);
  void wv2_set_extra_messages(char *msgs);
  void wv2_set_skybox(int some_bool, char *name);

  // Entity
  Ogre::Entity* wv2_entity_create(char *name, char *mesh_name);
  void wv2_entity_set_material_name(Ogre::Entity *obj, char *name);

  // Light
  void wv2_set_ambient_light(double r, double g, double b);
  Ogre::Light* wv2_create_light(char *name);
  void wv2_light_set_position(Ogre::Light *obj, double x, double y, double z);
  void wv2_light_set_direction(Ogre::Light *obj, double x, double y, double z);
  void wv2_light_set_diffuse_colour(Ogre::Light *obj, double r, double g, double b);
  void wv2_light_set_specular_colour(Ogre::Light *obj, double r, double g, double b);

  // Material
  Ogre::MaterialManager * wv2_get_material_manager();
  Ogre::Material* wv2_material_manager_get_by_name(Ogre::MaterialManager *obj, char *name);
  Ogre::Material* wv2_material_clone(Ogre::Material *obj, char *name);
  Ogre::Technique* wv2_material_get_technique(Ogre::Material *obj, int k);

  // ManualObject
  Ogre::ManualObject* wv2_manual_object_create(char *name);
  void wv2_manual_object_delete(Ogre::ManualObject *obj);
  void wv2_manual_object_begin(Ogre::ManualObject *obj, char *name, int renderOpInt);
  void wv2_manual_object_begin_update(Ogre::ManualObject* obj, int index);
  void wv2_manual_object_position(Ogre::ManualObject *obj, double x, double y, double z);
  void wv2_manual_object_normal(Ogre::ManualObject *obj, double x, double y, double z);
  void wv2_manual_object_tangent(Ogre::ManualObject *obj, double x, double y, double z);
  void wv2_manual_object_colour(Ogre::ManualObject *obj, double r, double g, double b, double a);
  void wv2_manual_object_end(Ogre::ManualObject *obj);
  void wv2_manual_object_clear(Ogre::ManualObject *obj);
  void wv2_manual_object_estimate_vertex_count(Ogre::ManualObject *obj, int n);
  void wv2_manual_object_estimate_index_count(Ogre::ManualObject *obj, int n);
  void wv2_manual_object_convert_to_mesh(Ogre::ManualObject *obj, char *name);
  void wv2_manual_object_set_dynamic(Ogre::ManualObject *obj, int dynamic);
  void wv2_manual_object_set_visible(Ogre::ManualObject *obj, int visible);

  // Pass
  void wv2_pass_set_scene_blending(Ogre::Pass *obj, int k);
  void wv2_pass_set_depth_write_enabled(Ogre::Pass *obj, int k);

  // SceneNode
  Ogre::SceneNode* wv2_scene_node_get_root();
  Ogre::SceneNode* wv2_scene_node_create_child(Ogre::SceneNode *node, char *name);
  void wv2_scene_node_attach_entity(Ogre::SceneNode *node, Ogre::Entity *entity);
  void wv2_scene_node_attach_manual_object(Ogre::SceneNode *node, Ogre::ManualObject *obj);
  void wv2_scene_node_set_position(Ogre::SceneNode *node, double x, double y, double z);
  void wv2_scene_node_set_scale(Ogre::SceneNode *node, double x, double y, double z);
  void wv2_scene_node_set_orientation(Ogre::SceneNode *node, double q0, double q1, double q2, double q3);
  void wv2_scene_node_set_visible(Ogre::SceneNode *node, int visible);

  // Technique
  Ogre::Pass* wv2_technique_get_pass(Ogre::Technique *obj, int k);

#ifdef __cplusplus
}
#endif

int num_running = 0;

void wv2_run_viz(HsFunPtr haskell_init, HsFunPtr haskell_update) {
  if (num_running > 0) {
    printf("You can only run one of these at a time. I believe it's an OGRE limitation.");
  } else {
      try {
        num_running++;
        WtfViz2 viz;
        viz.startViz(haskell_init, haskell_update);
      } catch (std::exception& e) {
        fprintf(stderr, "An exception has occurred: %s\n", e.what());
      }
  }
}

void wv2_set_extra_messages(char *cmsgs) {
  std::string msgs(cmsgs);
  OgreFramework::getSingletonPtr()->setExtraMessages(cmsgs);
}

void wv2_set_skybox(int some_bool, char *name) {
  OgreFramework::getSingletonPtr()->m_pSceneMgr->setSkyBox(some_bool, std::string(name));
}

// Entity
Ogre::Entity* wv2_entity_create(char *name, char *mesh_name) {
  return OgreFramework::getSingletonPtr()->m_pSceneMgr->createEntity(std::string(name), std::string(mesh_name));
}

void wv2_entity_set_material_name(Ogre::Entity *obj, char *name) {
  obj->setMaterialName(std::string(name));
}


// Light
void wv2_set_ambient_light(double r, double g, double b) {
  OgreFramework::getSingletonPtr()->m_pSceneMgr->setAmbientLight(Ogre::ColourValue(r, g, b));
}

Ogre::Light* wv2_create_light(char *name) {
  return OgreFramework::getSingletonPtr()->m_pSceneMgr->createLight(std::string(name));
}

void wv2_light_set_position(Ogre::Light *obj, double x, double y, double z) {
  obj->setPosition(x, y, z);
}

void wv2_light_set_direction(Ogre::Light *obj, double x, double y, double z) {
  obj->setDirection(x, y, z);
}

void wv2_light_set_diffuse_colour(Ogre::Light *obj, double r, double g, double b) {
  obj->setDiffuseColour(r, g, b);
}

void wv2_light_set_specular_colour(Ogre::Light *obj, double r, double g, double b) {
  obj->setSpecularColour(r, g, b);
}



// ManualObject
Ogre::ManualObject* wv2_manual_object_create(char *name) {
  return OgreFramework::getSingletonPtr()->m_pSceneMgr->createManualObject(std::string(name));
}

void wv2_manual_object_delete(Ogre::ManualObject *obj) {
  delete obj;
}

Ogre::RenderOperation::OperationType fromHaskellRenderOp(int haskellRenderOp) {
  // do this manually in case ogre changes
  Ogre::RenderOperation::OperationType renderOp;
  switch(haskellRenderOp) {
  case 0:
    renderOp = Ogre::RenderOperation::OT_POINT_LIST;
    break;
  case 1:
    renderOp = Ogre::RenderOperation::OT_LINE_LIST;
    break;
  case 2:
    renderOp = Ogre::RenderOperation::OT_LINE_STRIP;
    break;
  case 3:
    renderOp = Ogre::RenderOperation::OT_TRIANGLE_LIST;
    break;
  case 4:
    renderOp = Ogre::RenderOperation::OT_TRIANGLE_STRIP;
    break;
  case 5:
    renderOp = Ogre::RenderOperation::OT_TRIANGLE_FAN;
    break;
  default:
    throw "wv2_begin: got unknown render operation!";
  }
  return renderOp;
}

void wv2_manual_object_begin(Ogre::ManualObject* obj, char *name, int haskellRenderOp) {
  Ogre::RenderOperation::OperationType renderOp = fromHaskellRenderOp(haskellRenderOp);
  obj->begin(std::string(name), renderOp);
}

void wv2_manual_object_begin_update(Ogre::ManualObject* obj, int index) {
  obj->beginUpdate(index);
}

void wv2_manual_object_position(Ogre::ManualObject *obj, double x, double y, double z) {
  obj->position(x, y, z);
}

void wv2_manual_object_normal(Ogre::ManualObject *obj, double x, double y, double z) {
  obj->normal(x, y, z);
}

void wv2_manual_object_tangent(Ogre::ManualObject *obj, double x, double y, double z) {
  obj->tangent(x, y, z);
}

void wv2_manual_object_colour(Ogre::ManualObject *obj, double r, double g, double b, double a) {
  obj->colour(r, g, b, a);
}

void wv2_manual_object_end(Ogre::ManualObject *obj) {
  obj->end();
}

void wv2_manual_object_clear(Ogre::ManualObject *obj) {
  obj->clear();
}

void wv2_manual_object_estimate_vertex_count(Ogre::ManualObject *obj, int n) {
  obj->estimateVertexCount(n);
}

void wv2_manual_object_estimate_index_count(Ogre::ManualObject *obj, int n) {
  obj->estimateIndexCount(n);
}

void wv2_manual_object_convert_to_mesh(Ogre::ManualObject *obj, char *name) {
  obj->convertToMesh(std::string(name));
}

void wv2_manual_object_set_dynamic(Ogre::ManualObject *obj, int dynamic) {
  obj->setDynamic(dynamic);
}

void wv2_manual_object_set_visible(Ogre::ManualObject *obj, int visible) {
  obj->setVisible(visible);
}


// Material
Ogre::MaterialManager * wv2_get_material_manager() {
  return Ogre::MaterialManager::getSingletonPtr();
}

Ogre::Material* wv2_material_manager_get_by_name(Ogre::MaterialManager *obj, char *name) {
  return obj->getByName(std::string(name)).get();
}

Ogre::Material* wv2_material_clone(Ogre::Material *obj, char *name) {
  return obj->clone(std::string(name)).get();
}

Ogre::Technique* wv2_material_get_technique(Ogre::Material *obj, int k) {
  return obj->getTechnique(k);
}


// Pass
Ogre::SceneBlendType fromHaskellSceneBlendType(int haskellSceneBlendType) {
  // do this manually in case ogre changes
  Ogre::SceneBlendType sbt;
  switch(haskellSceneBlendType) {
  case 0:
    sbt = Ogre::SceneBlendType::SBT_TRANSPARENT_ALPHA;
    break;
  case 1:
    sbt = Ogre::SceneBlendType::SBT_TRANSPARENT_COLOUR;
    break;
  case 2:
    sbt = Ogre::SceneBlendType::SBT_ADD;
    break;
  case 3:
    sbt = Ogre::SceneBlendType::SBT_MODULATE;
    break;
  case 4:
    sbt = Ogre::SceneBlendType::SBT_REPLACE;
    break;
  default:
    throw "fromHaskellSceneBlendType: got unknown scene blend type!";
  }
  return sbt;
}

void wv2_pass_set_scene_blending(Ogre::Pass *obj, int k) {
  return obj->setSceneBlending(fromHaskellSceneBlendType(k));
}

void wv2_pass_set_depth_write_enabled(Ogre::Pass *obj, int k) {
  obj->setDepthWriteEnabled(k);
}


// scene
Ogre::SceneNode* wv2_scene_node_get_root() {
  return OgreFramework::getSingletonPtr()->m_pSceneMgr->getRootSceneNode();
}

Ogre::SceneNode* wv2_scene_node_create_child(Ogre::SceneNode *node, char *name) {
  return node->createChildSceneNode(std::string(name));
}

void wv2_scene_node_attach_entity(Ogre::SceneNode *node, Ogre::Entity *entity) {
  node->attachObject(entity);
}

void wv2_scene_node_attach_manual_object(Ogre::SceneNode *node, Ogre::ManualObject *obj) {
  node->attachObject(obj);
}

void wv2_scene_node_set_visible(Ogre::SceneNode *obj, int visible) {
  obj->setVisible(visible);
}

void wv2_scene_node_set_position(Ogre::SceneNode *node, double x, double y, double z) {
  node->setPosition(x, y, z);
}

void wv2_scene_node_set_scale(Ogre::SceneNode *node, double x, double y, double z) {
  node->setScale(x, y, z);
}

void wv2_scene_node_set_orientation(Ogre::SceneNode *node, double q0, double q1, double q2, double q3) {
  node->setOrientation(q0, q1, q2, q3);
}


// Technique
Ogre::Pass* wv2_technique_get_pass(Ogre::Technique *obj, int k) {
  return obj->getPass(k);
}
