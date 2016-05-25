#pragma once

#include <map>
#include <string>
#include <vector>

#include "OgreFramework.hpp"

#include "HsFFI.h"

class WtfViz2 : public OIS::KeyListener, public OIS::MouseListener {
public:
  WtfViz2();
  ~WtfViz2();

  void startViz(HsFunPtr haskell_init, HsFunPtr haskell_update,
                HsFunPtr haskell_key_pressed, HsFunPtr haskell_key_released,
                HsFunPtr haskell_mouse_pressed, HsFunPtr haskell_mouse_released,
                HsFunPtr haskell_mouse_moved);

  bool keyPressed(const OIS::KeyEvent &keyEventRef);
  bool keyReleased(const OIS::KeyEvent &keyEventRef);

  bool mouseMoved(const OIS::MouseEvent &evt);
  bool mousePressed(const OIS::MouseEvent &evt, OIS::MouseButtonID id);
  bool mouseReleased(const OIS::MouseEvent &evt, OIS::MouseButtonID id);

private:
  bool m_bShutdown;

  HsFunPtr m_haskell_key_pressed;
  HsFunPtr m_haskell_key_released;
  HsFunPtr m_haskell_mouse_pressed;
  HsFunPtr m_haskell_mouse_released;
  HsFunPtr m_haskell_mouse_moved;
};
