#pragma once

#include <string>

// save diagnostic state
#pragma GCC diagnostic push

// turn off the specific warning. Can also use "-Wall"
#pragma GCC diagnostic ignored "-Wunused-but-set-parameter"
#pragma GCC diagnostic ignored "-Wunused-parameter"

#include <OgreCamera.h>
#include <OgreEntity.h>
#include <OgreLogManager.h>
#include <OgreManualObject.h>
#include <Overlay/OgreOverlay.h>
#include <Overlay/OgreOverlayElement.h>
#include <Overlay/OgreOverlayManager.h>
#include <OgreRoot.h>
#include <OgreViewport.h>
#include <OgreSceneManager.h>
#include <OgreRenderWindow.h>
#include <OgreConfigFile.h>
#include <OgreFrameListener.h>
#include "Overlay/OgreOverlaySystem.h"

#include <OISEvents.h>
#include <OISInputManager.h>
#include <OISKeyboard.h>
#include <OISMouse.h>

// turn the warnings back on
#pragma GCC diagnostic pop

class OgreFramework : public Ogre::Singleton<OgreFramework>, OIS::KeyListener, OIS::MouseListener {
public:
  OgreFramework();
  ~OgreFramework();

  bool initOgre(Ogre::String wndTitle,
                OIS::KeyListener *pKeyListener = 0,
                OIS::MouseListener *pMouseListener = 0);
  void updateOgre(double timeSinceLastFrame);
  void setCamera();
  void getInput();

  bool isOgreToBeShutDown() const {return m_bShutDownOgre;}

  bool keyPressed(const OIS::KeyEvent &keyEventRef);
  bool keyReleased(const OIS::KeyEvent &keyEventRef);

  bool mouseMoved(const OIS::MouseEvent &evt);
  bool mousePressed(const OIS::MouseEvent &evt, OIS::MouseButtonID id);
  bool mouseReleased(const OIS::MouseEvent &evt, OIS::MouseButtonID id);

  void setExtraMessages(char *msgs);

  Ogre::Overlay* m_debugOverlay;
  Ogre::TextAreaOverlayElement* m_debugText;

  Ogre::Root*         m_pRoot;
  Ogre::SceneManager* m_pSceneMgr;
  Ogre::RenderWindow* m_pRenderWnd;
  Ogre::Camera*       m_pCamera;
  Ogre::Viewport*     m_pViewport;
  Ogre::Log*          m_pLog;
  Ogre::Timer*        m_pTimer;

  OIS::InputManager*  m_pInputMgr;
  OIS::Keyboard*      m_pKeyboard;
  OIS::Mouse*         m_pMouse;

private:
  OgreFramework(const OgreFramework&);
  OgreFramework& operator= (const OgreFramework&);

  //  OgreBites::SdkTrayManager* m_pTrayMgr;
  Ogre::FrameEvent  m_FrameEvent;
  int               m_iNumScreenShots;

  bool              m_bShutDownOgre;

  Ogre::Vector3     m_TranslateVector;
  Ogre::Real        m_MoveSpeed;
  Ogre::Degree      m_RotateSpeed;
  float             m_MoveScale;
  Ogre::Degree      m_RotScale;

  Ogre::Entity *m_focusSphere;
  Ogre::SceneNode *m_focusSphereNode;

  Ogre::Vector3 m_cameraFocus;
  double m_cameraAzimuth;
  double m_cameraElevation;
  double m_cameraRadius;

  std::string extra_messages;
};
