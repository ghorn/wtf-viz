#include "WtfViz2.hpp"
#include <OgreWindowEventUtilities.h>
#include <stdint.h>
#include <map>
#include <string>
#include <vector>

#include "HsFFI.h"

WtfViz2::WtfViz2() {
}

WtfViz2::~WtfViz2() {
  delete OgreFramework::getSingletonPtr();
}

typedef void (*hs_init_callback)();
typedef void (*hs_update_callback)(double);
typedef void (*hs_keyboard_event)(int, unsigned int);
typedef void (*hs_mouse_pressed_or_released)(int);
typedef void (*hs_mouse_moved)(const OIS::MouseState*, int, int, int, int, int, int);

void WtfViz2::startViz(HsFunPtr haskell_init, HsFunPtr haskell_update,
                       HsFunPtr haskell_key_pressed, HsFunPtr haskell_key_released,
                       HsFunPtr haskell_mouse_pressed, HsFunPtr haskell_mouse_released,
                       HsFunPtr haskell_mouse_moved) {
  m_haskell_key_pressed = haskell_key_pressed;
  m_haskell_key_released = haskell_key_released;
  m_haskell_mouse_pressed = haskell_mouse_pressed;
  m_haskell_mouse_released = haskell_mouse_released;
  m_haskell_mouse_moved = haskell_mouse_moved;

  new OgreFramework();
  if (!OgreFramework::getSingletonPtr()->initOgre("wtf-viz v2.0", this, this))
    return;

  m_bShutdown = false;

  OgreFramework::getSingletonPtr()->m_pLog->logMessage("wtf-viz initialized!");

  OgreFramework::getSingletonPtr()->m_pLog->logMessage("Calling haskell init...");
  ((hs_init_callback)haskell_init)();
  OgreFramework::getSingletonPtr()->m_pLog->logMessage("Start main loop...");

  double timeSinceLastFrame = 0;
  double startTime = 0;

  OgreFramework::getSingletonPtr()->m_pRenderWnd->resetStatistics();

  while (!m_bShutdown && !OgreFramework::getSingletonPtr()->isOgreToBeShutDown()) {
    if (OgreFramework::getSingletonPtr()->m_pRenderWnd->isClosed())
      m_bShutdown = true;

    Ogre::WindowEventUtilities::messagePump();

    if (OgreFramework::getSingletonPtr()->m_pRenderWnd->isActive()) {
      startTime = OgreFramework::getSingletonPtr()->m_pTimer->getMillisecondsCPU();

      OgreFramework::getSingletonPtr()->m_pKeyboard->capture();
      OgreFramework::getSingletonPtr()->m_pMouse->capture();

      OgreFramework::getSingletonPtr()->updateOgre(timeSinceLastFrame);
      OgreFramework::getSingletonPtr()->m_pRoot->renderOneFrame();

      // get messages
      ((hs_update_callback)haskell_update)(timeSinceLastFrame);

      timeSinceLastFrame =
        OgreFramework::getSingletonPtr()->m_pTimer->getMillisecondsCPU() - startTime;
    } else {
      sleep(1);
    }
  }

  OgreFramework::getSingletonPtr()->m_pLog->logMessage("Main loop quit");
  OgreFramework::getSingletonPtr()->m_pLog->logMessage("Shutdown OGRE...");
}


bool WtfViz2::keyPressed(const OIS::KeyEvent &keyEventRef) {
  ((hs_keyboard_event)m_haskell_key_pressed)(static_cast<int>(keyEventRef.key), keyEventRef.text);

  OgreFramework::getSingletonPtr()->keyPressed(keyEventRef);

  if (OgreFramework::getSingletonPtr()->m_pKeyboard->isKeyDown(OIS::KC_F)) {
    // do something
  }

  return true;
}

bool WtfViz2::keyReleased(const OIS::KeyEvent &keyEventRef) {
  ((hs_keyboard_event)m_haskell_key_released)(static_cast<int>(keyEventRef.key), keyEventRef.text);
  OgreFramework::getSingletonPtr()->keyReleased(keyEventRef);

  return true;
}

bool WtfViz2::mouseMoved(const OIS::MouseEvent &evt) {
  const OIS::MouseState *mouse_state = &(OgreFramework::getSingletonPtr()->m_pMouse->getMouseState());
  ((hs_mouse_moved)m_haskell_mouse_moved)(mouse_state,
                                          evt.state.X.rel, evt.state.X.abs,
                                          evt.state.Y.rel, evt.state.Y.abs,
                                          evt.state.Z.rel, evt.state.Z.abs);
  OgreFramework::getSingletonPtr()->mouseMoved(evt);

  return true;
}



bool WtfViz2::mousePressed(const OIS::MouseEvent &evt,
                                 OIS::MouseButtonID id) {
  ((hs_mouse_pressed_or_released)m_haskell_mouse_pressed)(static_cast<int>(id));
  OgreFramework::getSingletonPtr()->mousePressed(evt, id);
  return true;
}

bool WtfViz2::mouseReleased(const OIS::MouseEvent &evt,
                                  OIS::MouseButtonID id) {
  ((hs_mouse_pressed_or_released)m_haskell_mouse_released)(static_cast<int>(id));
  OgreFramework::getSingletonPtr()->mouseReleased(evt, id);
  return true;
}
